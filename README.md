# Game clock

## Development

Make sure you have spago installed:

```
npm install -g spago
```

Install dependencies:

```
npm install
```

To run a development server, run:

```
npm run start
```

### Build structure (development)

Note that you must run `spago build` before the very first time you run `npm run
start` (and any time the `output/` directory is removed).

CSS files are bundled with parcel (which uses PostCSS to bundle Tailwind) to
`public/.css_bundle/`.

JS files are bundled with esbuild to `public/bundle.js`.

Purescript files are built with spago (this is not necessary if your editor
integration already auto runs spago build).

For development, the HTML is served with live-server.

If there is an error relating to Tailwind, just re-run `npm run start`.

## Directory structure

Purescript source code is in the `src/` folder. Tests are in the `test/` folder.
Spago will build to the `output/` folder. Parcel will bundle the files in
`public/` to `dist/`.

## Specifications

- Each player can see how much time they have left
- The clock is only running for one player at a time
- The current player is displayed
- The current player can be easily switched at any time
- There can be multiple players
- The game can be paused, at which point no player's clock will run
- Each move can add an increment of time to the player's clock
- A sound may play when the player's clock is active
- A sound may play when the player's time is up
- There should be settings for:
  - number of players
  - player time (different for each player)
  - time increment on each move
  - sound (type and volume)
