# J< Starter project

Read the assignment, follow the instructions.

```shell
opam install dune menhir ppx_deriving

dune build

dune exec jaysub forward examples/fib.jsub
```

## CLI usage

```
jaysub [forward|backward|invert|optimize] <filename.jsub>
```

