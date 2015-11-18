libconfig-haskell
=================

[![Hackage](https://img.shields.io/hackage/v/bindings-nlopt.svg)](https://hackage.haskell.org/package/bindings-nlopt) [![Build Status](https://secure.travis-ci.org/peddie/hs-bindings-nlopt.png?branch=master)](http://travis-ci.org/peddie/hs-bindings-nlopt)
![Hackage Dependencies][badge-hackage-deps]
[badge-hackage-deps]:https://img.shields.io/hackage-deps/v/bindings-nlopt.svg

Low-level bindings to the
[NLOpt](http://ab-initio.mit.edu/wiki/index.php/NLopt) library.

This package is still in development and has not yet been released on
hackage.

Please see the [NLOpt
Reference](http://ab-initio.mit.edu/wiki/index.php/NLopt_Reference)
and [Tutorial](http://ab-initio.mit.edu/wiki/index.php/NLopt_Tutorial)
for more information.

The library is designed and developed with libnlopt version 2.4.2 on
Debian `sid`.  The travis tests run against libnlopt version 2.4.1 on
Ubuntu 12.04 `precise`.  I don't currently have a good way to test
against other versions or systems, but I am happy to make changes to
improve compatibility.
