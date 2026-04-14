import { useEffect, useRef } from 'react'

function EnvelopeVisualizer({ synth, activeNotes }) {
  const canvasRef = useRef(null)
  const animationRef = useRef(null)
  const noteInstancesRef = useRef([]) // Array of { id, note, startTime, releaseTime }
  const previousActiveNotesRef = useRef(new Set())
  const nextIdRef = useRef(0)

  useEffect(() => {
    if (!synth) return

    const canvas = canvasRef.current
    const ctx = canvas.getContext('2d')
    const width = canvas.width
    const height = canvas.height

    // Get the accent color from CSS variables
    const accentColor = getComputedStyle(document.documentElement)
      .getPropertyValue('--accent').trim()

    const dimColor = getComputedStyle(document.documentElement)
      .getPropertyValue('--accent-dim').trim()

    // Detect newly started notes (in activeNotes but not in previousActiveNotes)
    activeNotes.forEach(note => {
      if (!previousActiveNotesRef.current.has(note)) {
        // New note started
        noteInstancesRef.current.push({
          id: nextIdRef.current++,
          note,
          startTime: Date.now(),
          releaseTime: null
        })
      }
    })

    // Detect released notes (in previousActiveNotes but not in activeNotes)
    previousActiveNotesRef.current.forEach(note => {
      if (!activeNotes.has(note)) {
        // Note was released - mark the most recent active instance
        const instances = noteInstancesRef.current
          .filter(inst => inst.note === note && inst.releaseTime === null)
        if (instances.length > 0) {
          instances[instances.length - 1].releaseTime = Date.now()
        }
      }
    })

    // Update previous active notes
    previousActiveNotesRef.current = new Set(activeNotes)

    const draw = () => {
      // Clear canvas
      ctx.fillStyle = '#1a1a1a'
      ctx.fillRect(0, 0, width, height)

      // Get envelope settings from synth (using the current settings)
      const envelope = synth.options.envelope
      const attack = envelope.attack * 1000 // Convert to ms
      const decay = envelope.decay * 1000
      const sustain = envelope.sustain // 0-1
      const release = envelope.release * 1000

      const padding = 40
      const graphWidth = width - padding * 2
      const graphHeight = height - padding * 2
      const graphTop = padding
      const graphLeft = padding

      // Draw axes
      ctx.strokeStyle = '#333'
      ctx.lineWidth = 1
      ctx.beginPath()
      ctx.moveTo(graphLeft, graphTop)
      ctx.lineTo(graphLeft, graphTop + graphHeight)
      ctx.lineTo(graphLeft + graphWidth, graphTop + graphHeight)
      ctx.stroke()

      // Draw ADSR envelope curve
      ctx.strokeStyle = dimColor
      ctx.lineWidth = 2
      ctx.beginPath()

      // Scale time for better visualization (make short phases visible)
      const visualAttack = Math.max(attack, 20) // At least 20ms visual width
      const visualDecay = Math.max(decay, 50)   // At least 50ms visual width
      const visualSustain = 100                  // 100ms sustain display
      const visualRelease = Math.max(release, 50) // At least 50ms visual width

      const totalTime = visualAttack + visualDecay + visualSustain + visualRelease
      const timeScale = graphWidth / totalTime

      // Start point
      ctx.moveTo(graphLeft, graphTop + graphHeight)

      // Attack
      const attackX = graphLeft + visualAttack * timeScale
      ctx.lineTo(attackX, graphTop)

      // Decay
      const decayX = attackX + visualDecay * timeScale
      const sustainY = graphTop + graphHeight * (1 - sustain)
      ctx.lineTo(decayX, sustainY)

      // Sustain
      const sustainX = decayX + visualSustain * timeScale
      ctx.lineTo(sustainX, sustainY)

      // Release
      const releaseX = sustainX + visualRelease * timeScale
      ctx.lineTo(releaseX, graphTop + graphHeight)

      ctx.stroke()

      // Draw labels
      ctx.fillStyle = '#666'
      ctx.font = '10px monospace'
      ctx.textAlign = 'center'

      ctx.fillText('A', graphLeft + (visualAttack * timeScale) / 2, height - 10)
      ctx.fillText('D', attackX + (visualDecay * timeScale) / 2, height - 10)
      ctx.fillText('S', decayX + (visualSustain * timeScale) / 2, height - 10)
      ctx.fillText('R', sustainX + (visualRelease * timeScale) / 2, height - 10)

      // Draw note positions on the envelope
      const currentTime = Date.now()

      noteInstancesRef.current = noteInstancesRef.current.filter(instance => {
        const { startTime, releaseTime } = instance
        let x, y

        if (releaseTime === null) {
          // Note is still held - in Attack, Decay, or Sustain
          const elapsed = currentTime - startTime

          if (elapsed < attack) {
            // Attack phase
            const progress = Math.min(1, elapsed / attack)
            x = graphLeft + (visualAttack * timeScale * progress)
            y = graphTop + graphHeight * (1 - progress)
          } else if (elapsed < attack + decay) {
            // Decay phase
            const decayProgress = Math.min(1, (elapsed - attack) / decay)
            x = attackX + (visualDecay * timeScale * decayProgress)
            const currentLevel = 1 - (1 - sustain) * decayProgress
            y = graphTop + graphHeight * (1 - currentLevel)
          } else {
            // Sustain phase - animate across the sustain section
            const sustainElapsed = elapsed - attack - decay
            const sustainProgress = Math.min(1, sustainElapsed / 1000) // Move across in 1 second
            x = decayX + (visualSustain * timeScale * sustainProgress)
            y = sustainY
          }
        } else {
          // Note was released - in Release phase
          const releaseElapsed = currentTime - releaseTime

          if (releaseElapsed < release) {
            // Release phase
            const releaseProgress = Math.min(1, releaseElapsed / release)
            x = sustainX + (visualRelease * timeScale * releaseProgress)
            y = sustainY + (graphHeight - sustainY + graphTop) * releaseProgress
          } else {
            // Release finished - remove this instance
            return false
          }
        }

        // Draw position indicator
        ctx.fillStyle = accentColor
        ctx.shadowBlur = 10
        ctx.shadowColor = accentColor
        ctx.beginPath()
        ctx.arc(x, y, 4, 0, Math.PI * 2)
        ctx.fill()
        ctx.shadowBlur = 0

        return true // Keep this instance
      })

      animationRef.current = requestAnimationFrame(draw)
    }

    draw()

    return () => {
      if (animationRef.current) {
        cancelAnimationFrame(animationRef.current)
      }
    }
  }, [synth, activeNotes])

  return (
    <div className="visualizer-container">
      <h3 className="visualizer-label">Envelope (ADSR)</h3>
      <canvas
        ref={canvasRef}
        width={400}
        height={200}
        className="envelope-canvas"
      />
    </div>
  )
}

export default EnvelopeVisualizer
