# FaustML

FaustML is a **first order simply typed ML**, with OCaml-compatible concrete syntax.
The project contains a polytime check and interpretor for those programs.

Using a data-tiering algorithm, we guarantee that the program will execute in PTIME 
if the execution make use of full memoization + full sharing of subterm.

This algorithm is inspired by 
> M. Avanzini and U. Dal Lago. On sharing, memoization, and polynomial time. Inf. Comput., 261:3–22, 2018.


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

if one of the option: n/e/a is enable, the program will listen to user input, then parse it as a program input for the function called "main" in the program.
The common usages are:

- `./faustml -a prog.ml < input.ml` to check everything and execute
- `./faustml -p prog.ml` to check everything without executing
- `./faustml -e prog.ml < input.ml` to execute without checking anything


the input should have the form a list of values 
`[v1;..;vn]` with n the number of arguments of the first function of the program 
if there is only 1 input, a simple value without bracket also works
i.e: `[v]` or `v`

**careful!** the program for the moment require the linearity of variable inside function. If the is multiple times the same variable typecheck and tiering will (probably) not work properly

## Graphs

we can translate the constraints graph to a pdf using dot:
`dot -Tpdf graphs/constraints.dot > graphs/constraints.pdf`

or the small bash program will do the above line automatically:
`./graph constraints`

## Tiering

As it is still in developpement, multiple version of the tiering are accessible,
There is a choice to be made before compilation, in the file bin/faust.ml, between:
- `Tier.tier_prog` strict version of the tiering
- `Tierwithdupl.tier_prog` allow duplication and downward coercion
- `Tierformal.tier_prog` allow duplication and downward coercion but also duplicate computations (instead of storing each graph)

The first one follow the strict inference rules of tiering with no duplication of function.
The second one take a bit of freedom because it evaluate function in a "depedency order" to avoid multiple computation of tiering
The last one follow an algorithm closer to the inference rules of tiering with the "coercion" rule added.

## Tests

The test/ folder contains 
- **test/neg/** for the negative tests. Thoses are programs that don't pass one of the restriction for good reasons (like nbleaves of exp)
or for bad reasons (like insert sort)
- **test/pos/** for positive tests. Programs that pass all the restrictions