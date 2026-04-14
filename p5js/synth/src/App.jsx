import { useState, useEffect, useRef } from 'react'
import * as Tone from 'tone'
import WaveformVisualizer from './components/WaveformVisualizer'
import SpectrumAnalyzer from './components/SpectrumAnalyzer'
import EnvelopeVisualizer from './components/EnvelopeVisualizer'
import './App.css'

// Keyboard mapping: AWSEDFTGYHUJK layout for piano
const KEY_TO_NOTE = {
  'a': 'C4',
  'w': 'C#4',
  's': 'D4',
  'e': 'D#4',
  'd': 'E4',
  'f': 'F4',
  't': 'F#4',
  'g': 'G4',
  'y': 'G#4',
  'h': 'A4',
  'u': 'A#4',
  'j': 'B4',
  'k': 'C5',
}

const WAVEFORMS = ['sine', 'square', 'sawtooth', 'triangle']
const FILTER_TYPES = ['lowpass', 'highpass', 'bandpass', 'notch']

function App() {
  const [waveform, setWaveform] = useState('sine')
  const [volume, setVolume] = useState(-12) // dB
  const [activeNotes, setActiveNotes] = useState(new Set())
  const [isStarted, setIsStarted] = useState(false)

  // ADSR envelope parameters
  const [attack, setAttack] = useState(0.005)
  const [decay, setDecay] = useState(0.1)
  const [sustain, setSustain] = useState(0.3)
  const [release, setRelease] = useState(0.1)

  // Filter parameters
  const [activeFilters, setActiveFilters] = useState(new Set())
  const [filterFrequency, setFilterFrequency] = useState(2000)
  const [filterQ, setFilterQ] = useState(1)

  const [outputNode, setOutputNode] = useState(null)

  const synthRef = useRef(null)
  const filtersRef = useRef({}) // Map of filter type to filter instance
  const activeVoicesRef = useRef(new Map())

  // Initialize synth and filters
  useEffect(() => {
    // Create output node for visualization (0dB passthrough)
    const output = new Tone.Gain(1).toDestination()
    setOutputNode(output)

    // Create all filter instances
    FILTER_TYPES.forEach(type => {
      filtersRef.current[type] = new Tone.Filter({
        type,
        frequency: filterFrequency,
        Q: filterQ,
      })
    })

    // Create synth - will connect chain in another effect
    synthRef.current = new Tone.PolySynth(Tone.Synth, {
      oscillator: { type: waveform },
      envelope: {
        attack,
        decay,
        sustain,
        release,
      },
    })

    synthRef.current.volume.value = volume

    return () => {
      synthRef.current.dispose()
      Object.values(filtersRef.current).forEach(filter => filter.dispose())
      output.dispose()
    }
  }, [])

  // Update waveform
  useEffect(() => {
    if (synthRef.current) {
      synthRef.current.set({ oscillator: { type: waveform } })
    }
  }, [waveform])

  // Update volume
  useEffect(() => {
    if (synthRef.current) {
      synthRef.current.volume.value = volume
    }
  }, [volume])

  // Update envelope
  useEffect(() => {
    if (synthRef.current) {
      synthRef.current.set({
        envelope: { attack, decay, sustain, release }
      })
    }
  }, [attack, decay, sustain, release])

  // Rebuild filter chain when active filters change
  useEffect(() => {
    if (!synthRef.current || !outputNode) return

    // Disconnect everything first
    synthRef.current.disconnect()
    Object.values(filtersRef.current).forEach(filter => filter.disconnect())

    // Build the chain based on active filters
    const activeFilterList = FILTER_TYPES.filter(type => activeFilters.has(type))

    if (activeFilterList.length === 0) {
      // No filters active - connect directly to output node
      synthRef.current.connect(outputNode)
    } else {
      // Chain filters: synth -> filter1 -> filter2 -> ... -> output node
      synthRef.current.connect(filtersRef.current[activeFilterList[0]])

      for (let i = 0; i < activeFilterList.length - 1; i++) {
        filtersRef.current[activeFilterList[i]].connect(filtersRef.current[activeFilterList[i + 1]])
      }

      filtersRef.current[activeFilterList[activeFilterList.length - 1]].connect(outputNode)
    }
  }, [activeFilters, outputNode])

  // Update filter frequency for all filters
  useEffect(() => {
    Object.values(filtersRef.current).forEach(filter => {
      filter.frequency.value = filterFrequency
    })
  }, [filterFrequency])

  // Update filter Q for all filters
  useEffect(() => {
    Object.values(filtersRef.current).forEach(filter => {
      filter.Q.value = filterQ
    })
  }, [filterQ])

  // Start audio context on first interaction
  const startAudio = async () => {
    if (!isStarted) {
      await Tone.start()
      setIsStarted(true)
    }
  }

  // Play note
  const playNote = (note) => {
    if (!synthRef.current || !isStarted) return

    synthRef.current.triggerAttack(note)
    setActiveNotes(prev => new Set(prev).add(note))
  }

  // Stop note
  const stopNote = (note) => {
    if (!synthRef.current) return

    synthRef.current.triggerRelease(note)
    setActiveNotes(prev => {
      const newSet = new Set(prev)
      newSet.delete(note)
      return newSet
    })
  }

  // Keyboard event handlers
  useEffect(() => {
    const handleKeyDown = async (e) => {
      const key = e.key.toLowerCase()
      if (KEY_TO_NOTE[key] && !activeNotes.has(KEY_TO_NOTE[key])) {
        await startAudio()
        playNote(KEY_TO_NOTE[key])
      }
    }

    const handleKeyUp = (e) => {
      const key = e.key.toLowerCase()
      if (KEY_TO_NOTE[key]) {
        stopNote(KEY_TO_NOTE[key])
      }
    }

    window.addEventListener('keydown', handleKeyDown)
    window.addEventListener('keyup', handleKeyUp)

    return () => {
      window.removeEventListener('keydown', handleKeyDown)
      window.removeEventListener('keyup', handleKeyUp)
    }
  }, [activeNotes, isStarted])

  return (
    <div className="app">
      <header>
        <h1>Synthesizer</h1>
        {!isStarted && (
          <p className="start-hint">Click anywhere or press a key to start</p>
        )}
      </header>

      <main>
        <section className="controls">
          {/* Waveform and Volume */}
          <div className="controls-row">
            <div className="control-group">
              <label>Waveform</label>
              <div className="waveform-buttons">
                {WAVEFORMS.map(type => (
                  <button
                    key={type}
                    className={waveform === type ? 'active' : ''}
                    onClick={async () => {
                      await startAudio()
                      setWaveform(type)
                    }}
                  >
                    {type}
                  </button>
                ))}
              </div>
            </div>

            <div className="control-group">
              <label>
                Volume: {volume} dB
              </label>
              <input
                type="range"
                min="-40"
                max="0"
                value={volume}
                onChange={(e) => {
                  setVolume(Number(e.target.value))
                  startAudio()
                }}
              />
            </div>
          </div>

          {/* ADSR Envelope */}
          <div className="controls-row">
            <div className="control-group">
              <label>Attack: {attack.toFixed(3)}s</label>
              <input
                type="range"
                min="0.001"
                max="2"
                step="0.001"
                value={attack}
                onChange={(e) => {
                  setAttack(Number(e.target.value))
                  startAudio()
                }}
              />
            </div>

            <div className="control-group">
              <label>Decay: {decay.toFixed(3)}s</label>
              <input
                type="range"
                min="0.001"
                max="2"
                step="0.001"
                value={decay}
                onChange={(e) => {
                  setDecay(Number(e.target.value))
                  startAudio()
                }}
              />
            </div>

            <div className="control-group">
              <label>Sustain: {sustain.toFixed(2)}</label>
              <input
                type="range"
                min="0"
                max="1"
                step="0.01"
                value={sustain}
                onChange={(e) => {
                  setSustain(Number(e.target.value))
                  startAudio()
                }}
              />
            </div>

            <div className="control-group">
              <label>Release: {release.toFixed(3)}s</label>
              <input
                type="range"
                min="0.001"
                max="5"
                step="0.001"
                value={release}
                onChange={(e) => {
                  setRelease(Number(e.target.value))
                  startAudio()
                }}
              />
            </div>
          </div>

          {/* Filter */}
          <div className="controls-row">
            <div className="control-group">
              <label>Filter Types (click to toggle)</label>
              <div className="waveform-buttons">
                {FILTER_TYPES.map(type => (
                  <button
                    key={type}
                    className={activeFilters.has(type) ? 'active' : ''}
                    onClick={async () => {
                      await startAudio()
                      setActiveFilters(prev => {
                        const newSet = new Set(prev)
                        if (newSet.has(type)) {
                          newSet.delete(type)
                        } else {
                          newSet.add(type)
                        }
                        return newSet
                      })
                    }}
                  >
                    {type}
                  </button>
                ))}
              </div>
            </div>

            <div className="control-group">
              <label>Cutoff: {filterFrequency.toFixed(0)} Hz</label>
              <input
                type="range"
                min="0"
                max="1000"
                step="1"
                value={(() => {
                  // Convert frequency to logarithmic slider position
                  const minLog = Math.log(20)
                  const maxLog = Math.log(20000)
                  return ((Math.log(filterFrequency) - minLog) / (maxLog - minLog)) * 1000
                })()}
                onChange={(e) => {
                  // Convert slider position to logarithmic frequency
                  const minLog = Math.log(20)
                  const maxLog = Math.log(20000)
                  const freq = Math.exp(minLog + (Number(e.target.value) / 1000) * (maxLog - minLog))
                  setFilterFrequency(freq)
                  startAudio()
                }}
              />
            </div>

            <div className="control-group">
              <label>Resonance: {filterQ.toFixed(1)}</label>
              <input
                type="range"
                min="0.1"
                max="30"
                step="0.1"
                value={filterQ}
                onChange={(e) => {
                  setFilterQ(Number(e.target.value))
                  startAudio()
                }}
              />
            </div>
          </div>
        </section>

        <section className="visualizers">
          <WaveformVisualizer synth={outputNode} />
          <SpectrumAnalyzer
            synth={outputNode}
            activeNotes={activeNotes}
            activeFilters={activeFilters}
            filterFrequency={filterFrequency}
            filterQ={filterQ}
          />
          <EnvelopeVisualizer synth={synthRef.current} activeNotes={activeNotes} />
        </section>

        <section className="keyboard">
          <div className="keys">
            {/* Define all keys with explicit positions */}
            {[
              // White keys
              { key: 'a', note: 'C4', type: 'white', left: 0 },
              { key: 's', note: 'D4', type: 'white', left: 60 },
              { key: 'd', note: 'E4', type: 'white', left: 120 },
              { key: 'f', note: 'F4', type: 'white', left: 180 },
              { key: 'g', note: 'G4', type: 'white', left: 240 },
              { key: 'h', note: 'A4', type: 'white', left: 300 },
              { key: 'j', note: 'B4', type: 'white', left: 360 },
              { key: 'k', note: 'C5', type: 'white', left: 420 },
              // Black keys
              { key: 'w', note: 'C#4', type: 'black', left: 42 },
              { key: 'e', note: 'D#4', type: 'black', left: 102 },
              { key: 't', note: 'F#4', type: 'black', left: 222 },
              { key: 'y', note: 'G#4', type: 'black', left: 282 },
              { key: 'u', note: 'A#4', type: 'black', left: 342 },
            ].map(({ key, note, type, left }) => {
              const isActive = activeNotes.has(note)
              return (
                <div
                  key={key}
                  className={`key ${type} ${isActive ? 'active' : ''}`}
                  style={{ left: `${left}px` }}
                  onMouseDown={async () => {
                    await startAudio()
                    playNote(note)
                  }}
                  onMouseUp={() => stopNote(note)}
                  onMouseLeave={() => stopNote(note)}
                >
                  <span className="key-label">{key.toUpperCase()}</span>
                  <span className="note-label">{note}</span>
                </div>
              )
            })}
          </div>
        </section>
      </main>
    </div>
  )
}

export default App
