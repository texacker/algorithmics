# The Categorical (Metamorphic) Approach to Algorithms

## Algebraic Theories

## Algorithm

## Programming

### Functional Programming
FP = ADT + ( AG + Laziness )

## Appendix
### Haskell Toolchain and Tools

#### ghcup
```bash
sudo debfoster build-essential curl libffi-dev libffi6 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5

curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# or

curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org > ./bootstrap-haskell && \
chmod a+x ./bootstrap-haskell && \
proxychains ./bootstrap-haskell
```

#### haskell-platform
```bash
sudo debfoster haskell-platform haskell-platform-doc haskell-platform-prof
```

#### ghc & cabal
```bash
sudo debfoster ghc ghc-doc ghc-prof haskell-doc cabal-install
```

##  References
1. Fethi Rabhi, Guy Lapalme, _Algorithms: A Functional Programming Approach_, Addison-Wesley, 2nd ed., 1999.
1. [Benjamin C. Pierce](http://www.cis.upenn.edu/~bcpierce/), _Basic Category Theory for Computer Scientists_, The MIT Press, 1991.
1. [Steve Awodey](http://www.andrew.cmu.edu/user/awodey/), _Category Theory_, Oxford University Press, 2nd ed., 2010.
1. [Richard Bird](http://www.cs.ox.ac.uk/richard.bird/), Oege de Moor, _The Algebra of Programming_, Prentice Hall, 1997.
1. Erik Meijer, Maarten Fokkinga, Ross Paterson, _[Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire](http://eprints.eemcs.utwente.nl/7281/01/db-utwente-40501F46.pdf)_, Springer-Verlag, 1991.
1. [Richard Bird](http://www.cs.ox.ac.uk/richard.bird/), _Pearls of Functional Algorithm Design_, Cambridge University Press, 2010.
1. Miran Lipovaca, _[Learn You a Haskell for Great Good! A Beginner's Guide](http://learnyouahaskell.com/)_, No Starch Press, 2011.
1. _[The Haskell Cabal](https://www.haskell.org/cabal/)_
1. _[GHCup](https://gitlab.haskell.org/haskell/ghcup-hs)_
