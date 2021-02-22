

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

This will run both spago to compile purescript and parcel to bundle the files.

## Directory structure

Purescript source code is in the `src/` folder. Tests are in the `test/` folder.
Spago will build to the `output/` folder. Parcel will bundle the files in
`public/` to `dist/`.
