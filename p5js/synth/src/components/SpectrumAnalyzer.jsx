import { useEffect, useRef } from 'react'
import * as Tone from 'tone'

// Convert note name to frequency
const noteToFrequency = (note) => {
  return Tone.Frequency(note).toFrequency()
}

// Calculate filter response at a given frequency
const getFilterResponse = (freq, filterType, cutoff, Q) => {
  // Proper biquad filter magnitude response
  const r = freq / cutoff
  const r2 = r * r
  const denominator = Math.sqrt(Math.pow(1 - r2, 2) + Math.pow(r / Q, 2))

  switch (filterType) {
    case 'lowpass':
      return 1 / denominator
    case 'highpass':
      return r2 / denominator
    case 'bandpass':
      return (r / Q) / denominator
    case 'notch':
      return Math.abs(1 - r2) / denominator
    default:
      return 1
  }
}

function SpectrumAnalyzer({ synth, activeNotes, activeFilters, filterFrequency, filterQ }) {
  const canvasRef = useRef(null)
  const analyserRef = useRef(null)
  const animationRef = useRef(null)

  // Use refs to track current filter params so draw loop isn't stale
  const activeFiltersRef = useRef(activeFilters)
  const filterFrequencyRef = useRef(filterFrequency)
  const filterQRef = useRef(filterQ)

  // Update refs when props change
  useEffect(() => {
    activeFiltersRef.current = activeFilters
    filterFrequencyRef.current = filterFrequency
    filterQRef.current = filterQ
  }, [activeFilters, filterFrequency, filterQ])

  useEffect(() => {
    if (!synth) return

    // Create FFT analyser
    // 4096 samples = 48000/4096 = ~11.7 Hz per bin
    analyserRef.current = new Tone.Analyser('fft', 4096)
    synth.connect(analyserRef.current)

    const canvas = canvasRef.current
    const ctx = canvas.getContext('2d')
    const width = canvas.width
    const height = canvas.height

    // Get the accent color from CSS variables
    const accentColor = getComputedStyle(document.documentElement)
      .getPropertyValue('--accent').trim()

    const dimColor = getComputedStyle(document.documentElement)
      .getPropertyValue('--accent-dim').trim()

    const draw = () => {
      const values = analyserRef.current.getValue()

      // Clear canvas
      ctx.fillStyle = '#1a1a1a'
      ctx.fillRect(0, 0, width, height)

      const activeFrequencies = Array.from(activeNotes).map(note => noteToFrequency(note))
      const minFreq = 20
      const maxFreq = Tone.context.sampleRate / 2
      const logMin = Math.log10(minFreq)
      const logMax = Math.log10(maxFreq)

      // Create logarithmic frequency bands
      const numBands = 100
      const bands = []

      for (let i = 0; i < numBands; i++) {
        const freqRatio = i / numBands
        const logFreq = logMin + freqRatio * (logMax - logMin)
        const freq = Math.pow(10, logFreq)
        bands.push(freq)
      }

      // Draw bars for each logarithmic band
      const barWidth = width / numBands

      for (let i = 0; i < numBands; i++) {
        const freq = bands[i]
        const nextFreq = i < numBands - 1 ? bands[i + 1] : maxFreq

        // Find FFT bins in this frequency range and take the maximum
        const binStart = Math.floor((freq / maxFreq) * values.length)
        const binEnd = Math.floor((nextFreq / maxFreq) * values.length)

        let maxDb = -100
        for (let j = binStart; j < binEnd && j < values.length; j++) {
          maxDb = Math.max(maxDb, values[j])
        }

        // Convert dB to 0-1 range
        const value = Math.max(0, Math.min(1, (maxDb + 60) / 60))
        const barHeight = value * height

        const x = i * barWidth
        const y = height - barHeight

        // Check if this frequency bin corresponds to an active note
        const binFrequency = freq
        const isActiveFrequency = activeFrequencies.some(freq => {
          // Highlight if within 5% of the note frequency or its harmonics (up to 3rd)
          for (let harmonic = 1; harmonic <= 3; harmonic++) {
            const harmonicFreq = freq * harmonic
            if (Math.abs(binFrequency - harmonicFreq) / harmonicFreq < 0.05) {
              return true
            }
          }
          return false
        })

        // Use brighter color for active note frequencies
        if (isActiveFrequency && value > 0.1) {
          ctx.fillStyle = accentColor
          ctx.shadowBlur = 10
          ctx.shadowColor = accentColor
        } else {
          ctx.fillStyle = dimColor
          ctx.shadowBlur = 0
        }

        ctx.fillRect(x, y, barWidth - 1, barHeight)
      }

      // Reset shadow
      ctx.shadowBlur = 0

      // Draw vertical lines for active note frequencies
      activeFrequencies.forEach(freq => {
        // Draw fundamental frequency line
        const logFreq = Math.log10(freq)
        const x = ((logFreq - logMin) / (logMax - logMin)) * width

        ctx.strokeStyle = accentColor
        ctx.lineWidth = 2
        ctx.shadowBlur = 15
        ctx.shadowColor = accentColor
        ctx.globalAlpha = 0.8

        ctx.beginPath()
        ctx.moveTo(x, 0)
        ctx.lineTo(x, height)
        ctx.stroke()

        ctx.shadowBlur = 0
        ctx.globalAlpha = 1
      })

      // Draw filter response curve
      if (activeFiltersRef.current && activeFiltersRef.current.size > 0) {
        ctx.strokeStyle = '#ff00ff'
        ctx.lineWidth = 3
        ctx.globalAlpha = 0.7
        ctx.shadowBlur = 10
        ctx.shadowColor = '#ff00ff'
        ctx.beginPath()

        const numPoints = 200
        for (let i = 0; i < numPoints; i++) {
          const freqRatio = i / numPoints
          const logFreq = logMin + freqRatio * (logMax - logMin)
          const freq = Math.pow(10, logFreq)
          const x = freqRatio * width

          // Calculate combined filter response
          let response = 1
          activeFiltersRef.current.forEach(filterType => {
            response *= getFilterResponse(freq, filterType, filterFrequencyRef.current, filterQRef.current)
          })

          // Convert response to dB and scale to canvas height
          const responseDb = 20 * Math.log10(Math.max(response, 0.001))
          const normalizedResponse = Math.max(0, Math.min(1, (responseDb + 40) / 40))
          const y = height - (normalizedResponse * height)

          if (i === 0) {
            ctx.moveTo(x, y)
          } else {
            ctx.lineTo(x, y)
          }
        }

        ctx.stroke()
        ctx.shadowBlur = 0
        ctx.globalAlpha = 1
      }

      // Draw frequency labels with logarithmic spacing
      ctx.fillStyle = '#666'
      ctx.font = '10px monospace'
      ctx.textAlign = 'center'

      const labelFreqs = [60, 250, 1000, 4000, 16000]

      labelFreqs.forEach(freq => {
        if (freq <= maxFreq) {
          // Logarithmic position
          const logFreq = Math.log10(freq)
          const x = ((logFreq - logMin) / (logMax - logMin)) * width

          const label = freq >= 1000 ? `${freq/1000}k` : freq
          ctx.fillText(label, x, height - 5)
        }
      })

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
  }, [synth, activeNotes])

  return (
    <div className="visualizer-container">
      <h3 className="visualizer-label">Frequency Spectrum</h3>
      <canvas
        ref={canvasRef}
        width={400}
        height={200}
        className="spectrum-canvas"
      />
    </div>
  )
}

export default SpectrumAnalyzer
