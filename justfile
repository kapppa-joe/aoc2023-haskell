set positional-arguments

rebuild:
  cabal build

run filepath:
  cabal exec runghc {{filepath}}