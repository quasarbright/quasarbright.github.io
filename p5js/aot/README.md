# Attack on Titan - ODM Gear Simulator

A browser-based 3D game where you swing through a Manhattan-like city using Omni-Directional Mobility (ODM) gear from Attack on Titan.

## Features

- **Realistic Swinging Physics**: Spring-based grappling hook mechanics with momentum and swing dynamics
- **Dual Grappling Hooks**: Fire left and right hooks independently for advanced maneuvering
- **Gas Propulsion System**: Boost forward while grappled, with limited gas that regenerates
- **Procedural City**: Manhattan-style city with varied building heights and lit windows
- **First-Person View**: Full 360° camera control for immersive gameplay
- **Visual Effects**: Grappling hook lines, speed trails, and dynamic UI

## Controls

- **Mouse**: Look around (360° camera control)
- **Left Click**: Fire left grappling hook
- **Right Click**: Fire right grappling hook
- **Space**: Gas boost (while grappled, consumes gas)
- **R**: Detach all hooks
- **WASD**: Air control for directional influence
- **Shift**: Reel in / shorten rope length
- **Click to Start**: Click anywhere to lock the pointer and begin

## How to Play

1. Open `index.html` in a modern web browser (Chrome, Firefox, Edge recommended)
2. Click anywhere to start and lock the mouse pointer
3. Look at a building and click to fire a grappling hook
4. Use both hooks strategically to control your swing direction
5. Press Space while grappled to boost forward (watch your gas meter!)
6. Use WASD for subtle air control during flight
7. Hold Shift to reel in and shorten your rope for tighter swings
8. Press R to release hooks and free-fall

## Tips for Mastering ODM Gear

- **Momentum is Key**: Build speed by swinging in arcs rather than pulling straight
- **Dual Hooks**: Use both hooks for more stable swinging and better directional control
- **Reel In**: Shorten your rope when approaching a building to avoid crashing
- **Gas Management**: Your gas regenerates when not using ODM gear, so take breaks
- **Aim High**: Hook to the upper parts of tall buildings for longer, faster swings
- **Speed Trails**: Green trails appear when you're moving fast - that's when the ODM gear shines!

## Technical Details

- **Engine**: Three.js (r128) for 3D rendering
- **Physics**: Custom spring-based rope physics with damping
- **City Generation**: Procedural grid-based system with 400+ buildings
- **No Build Required**: Pure HTML/CSS/JS - just open and play!

## Browser Requirements

- Modern browser with WebGL support
- Pointer Lock API support
- Recommended: Chrome 90+, Firefox 88+, Edge 90+

## File Structure

- `index.html` - Main HTML structure and library imports
- `style.css` - UI styling and HUD elements
- `game.js` - Game initialization, render loop, and visual effects
- `player.js` - Player controller and ODM gear mechanics
- `city.js` - Procedural city generation and collision detection

## Performance

The game is optimized for smooth 60 FPS gameplay. If you experience lag:
- Close other browser tabs
- Reduce browser window size
- Update your graphics drivers

Enjoy swinging through the city! 🚀

