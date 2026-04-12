// Audio context and nodes
let audioContext;
let oscillator;
let gainNode;
let isPlaying = false;

// Waveform state
let currentWaveform = 'sine';
let targetWaveform = 'sine';
let morphProgress = 1.0; // 1.0 = fully morphed to target
let morphSpeed = 0.005; // Speed of morphing (starts at minimum)

// Frequency and volume
let frequency = 440;
let volume = 0.15; // Default 15%

// Fourier series size (more harmonics = more accurate waveforms)
const harmonics = 32;

// Calculate RMS power from Fourier coefficients (using Parseval's theorem)
function calculateRMS(real, imag) {
    let sumSquares = 0;
    for (let i = 1; i < harmonics; i++) {
        sumSquares += real[i] * real[i] + imag[i] * imag[i];
    }
    return Math.sqrt(sumSquares / 2);
}

// Generate Fourier coefficients for different waveforms
function generateWaveformCoefficients(type) {
    const real = new Float32Array(harmonics);
    const imag = new Float32Array(harmonics);

    // DC component
    real[0] = 0;
    imag[0] = 0;

    switch(type) {
        case 'sine':
            // Pure sine wave - only fundamental
            real[1] = 0;
            imag[1] = 1;
            break;

        case 'square':
            // Square wave - odd harmonics with 1/n amplitude
            for (let n = 1; n < harmonics; n++) {
                if (n % 2 === 1) { // odd harmonics only
                    real[n] = 0;
                    imag[n] = 4 / (Math.PI * n);
                }
            }
            break;

        case 'sawtooth':
            // Sawtooth wave - all harmonics with 1/n amplitude
            for (let n = 1; n < harmonics; n++) {
                real[n] = 0;
                imag[n] = -2 / (Math.PI * n) * Math.pow(-1, n);
            }
            break;

        case 'triangle':
            // Triangle wave - odd harmonics with 1/n² amplitude, alternating phase
            for (let n = 1; n < harmonics; n++) {
                if (n % 2 === 1) { // odd harmonics only
                    real[n] = 0;
                    imag[n] = 8 / (Math.PI * Math.PI * n * n) * Math.pow(-1, (n-1)/2);
                }
            }
            break;
    }

    // Calculate RMS and normalize to match sine wave's RMS
    const currentRMS = calculateRMS(real, imag);
    const sineRMS = Math.sqrt(0.5); // RMS of a sine wave with amplitude 1
    const normFactor = sineRMS / currentRMS;

    for (let i = 0; i < harmonics; i++) {
        real[i] *= normFactor;
        imag[i] *= normFactor;
    }

    return { real, imag };
}

// Interpolate between two sets of coefficients
function interpolateCoefficients(coeff1, coeff2, t) {
    const real = new Float32Array(harmonics);
    const imag = new Float32Array(harmonics);

    for (let i = 0; i < harmonics; i++) {
        real[i] = coeff1.real[i] * (1 - t) + coeff2.real[i] * t;
        imag[i] = coeff1.imag[i] * (1 - t) + coeff2.imag[i] * t;
    }

    return { real, imag };
}

// Initialize audio context
function initAudio() {
    if (!audioContext) {
        audioContext = new (window.AudioContext || window.webkitAudioContext)();
        gainNode = audioContext.createGain();
        gainNode.gain.value = volume;
        gainNode.connect(audioContext.destination);
    }
}

// Start playing
function startPlaying() {
    initAudio();

    if (oscillator) {
        oscillator.stop();
    }

    oscillator = audioContext.createOscillator();
    oscillator.frequency.value = frequency;

    // Set initial waveform
    updateOscillatorWaveform();

    oscillator.connect(gainNode);
    oscillator.start();

    isPlaying = true;
    updatePlayButton();

    // Start animation loop for morphing
    animate();
}

// Stop playing
function stopPlaying() {
    if (oscillator) {
        oscillator.stop();
        oscillator = null;
    }
    isPlaying = false;
    updatePlayButton();
}

// Update oscillator waveform based on current morph state
function updateOscillatorWaveform() {
    if (!oscillator) return;

    const currentCoeff = generateWaveformCoefficients(currentWaveform);
    const targetCoeff = generateWaveformCoefficients(targetWaveform);
    const morphedCoeff = interpolateCoefficients(currentCoeff, targetCoeff, morphProgress);

    const wave = audioContext.createPeriodicWave(morphedCoeff.real, morphedCoeff.imag);
    oscillator.setPeriodicWave(wave);
}

// Animation loop for smooth morphing
function animate() {
    if (!isPlaying) return;

    // Update morph progress
    if (morphProgress < 1.0) {
        morphProgress = Math.min(1.0, morphProgress + morphSpeed);
        updateOscillatorWaveform();
        drawMainWaveform();
        updateMorphIndicator();

        // When morph is complete, update current waveform
        if (morphProgress >= 1.0) {
            currentWaveform = targetWaveform;
            updateMorphIndicator();
        }
    }

    requestAnimationFrame(animate);
}

// Draw waveform on canvas
function drawWaveform(canvas, type, isInterpolated = false, progress = 1.0) {
    const ctx = canvas.getContext('2d');
    const width = canvas.width;
    const height = canvas.height;

    ctx.fillStyle = '#000';
    ctx.fillRect(0, 0, width, height);

    let coeff;
    if (isInterpolated && progress < 1.0) {
        const currentCoeff = generateWaveformCoefficients(currentWaveform);
        const targetCoeff = generateWaveformCoefficients(type);
        coeff = interpolateCoefficients(currentCoeff, targetCoeff, progress);
    } else {
        coeff = generateWaveformCoefficients(type);
    }

    // Draw waveform
    ctx.beginPath();
    ctx.strokeStyle = '#00d4ff';
    ctx.lineWidth = 2;

    const cycles = 2; // Number of cycles to display
    const samples = width;

    for (let x = 0; x < samples; x++) {
        const t = (x / samples) * cycles * 2 * Math.PI;
        let y = 0;

        // Sum up harmonics
        for (let n = 1; n < harmonics; n++) {
            y += coeff.imag[n] * Math.sin(n * t) + coeff.real[n] * Math.cos(n * t);
        }

        // Normalize and scale to canvas
        const yPos = height / 2 - (y * height / 4);

        if (x === 0) {
            ctx.moveTo(x, yPos);
        } else {
            ctx.lineTo(x, yPos);
        }
    }

    ctx.stroke();

    // Draw center line
    ctx.beginPath();
    ctx.strokeStyle = 'rgba(255, 255, 255, 0.2)';
    ctx.lineWidth = 1;
    ctx.moveTo(0, height / 2);
    ctx.lineTo(width, height / 2);
    ctx.stroke();
}

// Draw main canvas with current morphed waveform
function drawMainWaveform() {
    const canvas = document.getElementById('mainCanvas');
    drawWaveform(canvas, targetWaveform, true, morphProgress);
}

// Update morph indicator text
function updateMorphIndicator() {
    const indicator = document.getElementById('morphIndicator');
    if (morphProgress < 1.0) {
        const percent = Math.round(morphProgress * 100);
        indicator.textContent = `Morphing: ${currentWaveform} → ${targetWaveform} (${percent}%)`;
    } else if (isPlaying) {
        indicator.textContent = `Current: ${currentWaveform}`;
    } else {
        indicator.textContent = '';
    }
}

// Update play button appearance
function updatePlayButton() {
    const btn = document.getElementById('playBtn');
    if (isPlaying) {
        btn.textContent = 'Stop';
        btn.classList.add('playing');
    } else {
        btn.textContent = 'Play';
        btn.classList.remove('playing');
    }
}

// Initialize preview canvases
function initPreviews() {
    const previewCanvases = document.querySelectorAll('.preview');
    previewCanvases.forEach(canvas => {
        const type = canvas.dataset.type;
        canvas.width = 300;
        canvas.height = 80;
        drawWaveform(canvas, type);
    });

    // Initialize main canvas
    const mainCanvas = document.getElementById('mainCanvas');
    mainCanvas.width = 600;
    mainCanvas.height = 200;
    drawMainWaveform();
}

// Event listeners
document.getElementById('playBtn').addEventListener('click', () => {
    if (isPlaying) {
        stopPlaying();
    } else {
        startPlaying();
    }
});

document.getElementById('frequency').addEventListener('input', (e) => {
    frequency = parseFloat(e.target.value);
    document.getElementById('freqDisplay').textContent = `${frequency} Hz`;

    if (oscillator) {
        oscillator.frequency.value = frequency;
    }
});

document.getElementById('volume').addEventListener('input', (e) => {
    volume = parseFloat(e.target.value) / 100; // Convert percentage to 0-1 range
    document.getElementById('volumeDisplay').textContent = `${e.target.value}%`;

    if (gainNode) {
        gainNode.gain.value = volume;
    }
});

document.getElementById('morphSpeed').addEventListener('input', (e) => {
    const speedValue = parseInt(e.target.value);
    // Map 1-10 to morphSpeed values (slower to faster)
    // 1 = 0.01 (very slow), 5 = 0.05 (medium), 10 = 0.15 (very fast)
    morphSpeed = 0.005 + (speedValue - 1) * 0.015;

    const speedLabels = ['Very Slow', 'Very Slow', 'Slow', 'Slow', 'Medium', 'Medium', 'Fast', 'Fast', 'Very Fast', 'Very Fast'];
    document.getElementById('speedDisplay').textContent = speedLabels[speedValue - 1];
});

// Waveform selection
document.querySelectorAll('.waveform-card').forEach(card => {
    card.addEventListener('click', () => {
        const waveform = card.dataset.waveform;

        // Update active state
        document.querySelectorAll('.waveform-card').forEach(c => c.classList.remove('active'));
        card.classList.add('active');

        // Set target waveform and start morphing
        targetWaveform = waveform;
        morphProgress = 0.0;

        if (isPlaying) {
            updateMorphIndicator();
        } else {
            // If not playing, just update the main canvas immediately
            currentWaveform = waveform;
            morphProgress = 1.0;
            drawMainWaveform();
        }
    });
});

// Initialize on page load
window.addEventListener('load', () => {
    initPreviews();
    updateMorphIndicator();
});
