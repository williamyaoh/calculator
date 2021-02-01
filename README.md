# calculator

Calculator SPA in PureScript and Haskell. Displays previous calculations that
other people have made. Works with the keyboard too!

Check it out at [calculator.williamyaoh.com](https://calculator.williamyaoh.com).

## Building and running

To build and run, you'll need the following things installed:

* GHC 8.8.*
* cabal >= 2.4.1.0
* The PureScript compiler, `purs`
* `spago`
* A local Postgres database

To build, run `cabal v2-run calculator -- bundle-dev`. This should then create
an `output/` folder, where you'll find a server executable you can run. Make sure
to set up the database configuration in `server/src/bin/env/Env.hs` correctly,
and set up the tables according to `tables.sql`.
