# EVM-Why3

EVM in Why3 from a state transition perspective

## how to build

This package requires *why3* to verify the correctness of the EVM specification and *ocamlfind* to 
build the automatically extracted EVM implementation. It is strongly suggested to use *opam* to 
install the dependencies.

For example, under Debian-based linux distributions, you can use

```
$ sudo apt-get install opam
$ opam init
```
to install opam and initialize it.

For the users who have already installed and properly configured *opam*, use
```
$ opam install why3 ocamlfind yojson
```
to install the Why3 [Automated Theorem Prover](http://why3.lri.fr/).

Now typing `make server` will extract the OCaml implemention, compile it and the compiled target
runs as a RPC server (for now not implemented yet).

## running tests

First make sure that this repository is cloned and the executable *server* is properly built by

```
$ cd <EVM-Why3>
$ make server
```


clone the modified version of cita-vm at [Cita-VM](https://github.com/liyi-david/cita-vm). And running

```
$ cd <Cita-VM>
$ git submodule init && git submodule update
$ export WHY3EVM=<EVM-Why3>
$ cargo build
$ cargo test
```
will show the test result.
