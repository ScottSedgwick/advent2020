# Advent of code: 2020

Solutions in Haskell.  

## Repository structure

* Actual solutions are in the `src/Advent` folder.
* Unit tests are in the `test/AdventSpec` folder.
* Daily data sets are in the `data` folder.
* The `app` folder contains a constantly changing application entry point, that calls the code for whatever day I happen to be working on.

## Building

Install Haskell if required. Use [ghcup](https://www.haskell.org/ghcup/).

I'm using ghc version 8.8.4 and cabal version 3.2.0.0.  If you are using ghcup and your versions are not currently set to those, you can do it like this:

```{bash}
ghcup install ghc 8.8.4
ghcup set ghc 8.8.4
ghcup install cabal 3.2.0.0
ghcup set cabal 3.2.0.0
```

| What you want to do               | Command                 |
| --------------------------------- | ----------------------- |
| Build the library and application | `cabal build`           |
| Run the application               | `cabal exec advent.exe` |
| Build and run the tests           | `cabal test`            |

There is also a Rakefile to make some tasks faster:

| What you want to do               | Command     |
| --------------------------------- | ----------- |
| Build and run the application     | `rake`      |
| Build and run the tests           | `rake test` |
