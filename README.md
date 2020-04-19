# EVM-Why3

EVM in Why3 from a state transition perspective

## how to build

This package requires *why3* to verify the correctness of the EVM specification and *ocamlfind* to 
build the automatically extracted EVM implementation. It is strongly suggested to use *opam* to 
install the dependencies.

For example, under Debian-based linux distributions, you can use

```
sudo apt-get install opam
opam init
```
to install opam and initialize it.

For the users who have already installed and properly configured *opam*, use
```
opam install why3 ocamlfind
```
to install the Why3 [Automated Theorem Prover](http://why3.lri.fr/).

Now typing `make run` will extract the OCaml implemention, compile it and run it as a RPC server (for now not implemented yet).
