set positional-arguments

rebuild:
  cabal build

run day:
  cabal exec runghc 'src/Day{{day}}.hs'

compile day:
  cabal exec ghc 'src/Day{{day}}.hs' -- -O2 -outputdir bin -o 'bin/{{day}}'

clean_dir:
  rm src/*.o src/*.hi

ghci:
  cabal exec ghci
