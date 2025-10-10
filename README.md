# PLN-SOLVER

This is a mini-language based on **Ocaml** to solve linear programming problems. The language is very simple.

## Syntax

Example of syntax :

```
Obj 
    min t = 8x + 5y + 6z
Cons
    2x + 3y + 2z <= 90
    x + 2y + z <= 81
    4x + 3y + z <= 120

```
always begin with "Obj"  
..then "max" or "min"  
....then an expression of the objectives "t = 8x + 5y + 6z"  
....**it has to be [id] = [expression] and not [expression] = [id]**

section constraints with "Cons"  
..then constraints in the form :  
....- **[expression] <= [int]**  
....- **[int] <= [expression]**  
.... and so on


## Compile

To compile you just have to use the command below.

```
dune build
dune exec ./bin/main.exe <input_file>
```

## Dependencies

```
name                   lp-glpk
all-installed-versions 0.5.0 [default]
```

## Authors

Hadriwer :)
