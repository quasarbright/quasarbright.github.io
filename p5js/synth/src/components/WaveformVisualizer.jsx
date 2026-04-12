import { useEffect, useRef } from 'react'
import * as Tone from 'tone'

function WaveformVisualizer({ synth }) {
  const canvasRef = useRef(null)
  const analyserRef = useRef(null)
  const animationRef = useRef(null)

  useEffect(() => {
    if (!synth) return

    // Create analyser
    analyserRef.current = new Tone.Analyser('waveform', 2048)
    synth.connect(analyserRef.current)

    const canvas = canvasRef.current
    const ctx = canvas.getContext('2d')
    const width = canvas.width
    const height = canvas.height

    // Get the accent color from CSS variables
    const accentColor = getComputedStyle(document.documentElement)
      .getPropertyValue('--accent').trim()

    const draw = () => {
      const waveform = analyserRef.current.getValue()

      // Clear canvas
      ctx.fillStyle = '#1a1a1a'
      ctx.fillRect(0, 0, width, height)

      // Draw waveform
      ctx.strokeStyle = accentColor
      ctx.lineWidth = 2
      ctx.beginPath()

      const sliceWidth = width / waveform.length
      let x = 0

      for (let i = 0; i < waveform.length; i++) {
        const v = (waveform[i] + 1) / 2 // Normalize from [-1, 1] to [0, 1]
        const y = v * height

        if (i === 0) {
          ctx.moveTo(x, y)
        } else {
          ctx.lineTo(x, y)
        }

        x += sliceWidth
      }

      ctx.stroke()

      // Draw center line
      ctx.strokeStyle = '#333'
      ctx.lineWidth = 1
      ctx.beginPath()
      ctx.moveTo(0, height / 2)
      ctx.lineTo(width, height / 2)
      ctx.stroke()

      animationRef.current = requestAnimationFrame(draw)
    }

    draw()

    return () => {
      if (animationRef.current) {
        cancelAnimationFrame(animationRef.current)
      }
      if (analyserRef.current) {
        analyserRef.current.dispose()
      }
    }
  }, [synth])

  return (
    <div className="visualizer-container">
      <canvas
        ref={canvasRef}
        width={800}
        height={200}
        className="waveform-canvas"
      />
    </div>
  )
}

export default WaveformVisualizer
