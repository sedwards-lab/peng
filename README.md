# The peng compiler

This depends on the Haskell Stack.

See <https://docs.haskellstack.org/en/stable/GUIDE/> for Haskell Stack
documentation


Build with

````
stack build
````


Test with, e.g.,

````
cd regression-tests ; ./runtests.sh
stack test peng:scanner-tests
stack build peng:parser-tests
````
