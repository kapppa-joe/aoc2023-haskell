set positional-arguments

rebuild:
  cabal build

run day:
  cabal exec runghc 'src/Day{{day}}.hs'
