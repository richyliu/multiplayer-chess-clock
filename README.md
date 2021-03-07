# Game clock

## Development

Make sure you have spago and parcel installed:

```
npm install -g spago parcel
```

Install dependencies:

```
npm install
```

To run a development server, run:

```
npm run start
```

This will run spago to compile purescript and parcel to bundle the files.

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
