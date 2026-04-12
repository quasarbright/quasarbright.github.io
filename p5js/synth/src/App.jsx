import { useState, useEffect, useRef } from 'react'
import * as Tone from 'tone'
import WaveformVisualizer from './components/WaveformVisualizer'
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

function App() {
  const [waveform, setWaveform] = useState('sine')
  const [volume, setVolume] = useState(-12) // dB
  const [activeNotes, setActiveNotes] = useState(new Set())
  const [isStarted, setIsStarted] = useState(false)

  const synthRef = useRef(null)
  const activeVoicesRef = useRef(new Map())

  // Initialize synth
  useEffect(() => {
    synthRef.current = new Tone.PolySynth(Tone.Synth, {
      oscillator: { type: waveform },
      envelope: {
        attack: 0.005,
        decay: 0.1,
        sustain: 0.3,
        release: 0.1,
      },
    }).toDestination()

    synthRef.current.volume.value = volume

    return () => {
      synthRef.current.dispose()
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
        <h1>🎹 Synthesizer (BLUE THEME TEST)</h1>
        {!isStarted && (
          <p className="start-hint">Click anywhere or press a key to start</p>
        )}
      </header>

      <main>
        <section className="controls">
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
              onChange={async (e) => {
                await startAudio()
                setVolume(Number(e.target.value))
              }}
            />
          </div>
        </section>

        <WaveformVisualizer synth={synthRef.current} />

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
