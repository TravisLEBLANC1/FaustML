# FaustML

FaustML is a **first order simply typed ML**, with OCaml-compatible concrete syntax.
The project contains a typecheck and polytime check

Using a tiering algorithm, we guarantee that the program will execute in PTIME 
if we use total memoization + sharing of subterm.

## Build

to build the project use `dune build`
the only depedency is *OCamlgraph* and *UnionFind*

## Usage

```
usage: ./faustml - [itsenap] prog.ml
  - condence version of the other options
  -a enable all (equiv to - tsien)
  -p enable polytime check (equiv to - tsi)
  -t enable type check
  -s enable syntax check
  -i enable tier check
  -e enable standar execution
  -n enable naive execution
  -help  Display this list of options
  --help  Display this list of options
``` 

if one of the option: n/e/a is enable, the program will listen to user input, then parse it as an input for the first function of the program.
The common usages are:

- `./faustml -a prog.ml < input.ml` to check everything and execute
- `./faustml -p prog.ml` to check everything without executing
- `./faustml -e prog.ml < input.ml` to execute without checking anything


the input should have the form a list of values 
`[v1;..;vn]` with n the number of arguments of the first function of the program 
if there is only 1 input, a simple value without bracket also works
`[v]` or `v`

## Graphs

we can translate the constraints graph to a pdf using dot:
`dot -Tpdf graphs/constraints.dot > graphs/constraints.pdf`
