# SequenTT

## Examples
```ocaml
-- Universe hierarchies
define A : Type 0;
define x : A;

define Int : Type 0;
define Bool: Type 0;

-- inductive type introduction
data nat : A where
| zero : nat
| suc : nat -> nat
.

let addTen x : Int -> Int = x + 10;


let addexplicit : Int list =
    map addTen [3,4,2];

-- Match statements
let f m : Int -> Bool = match m with
       | 3 -> True
       | _ -> False
 .

f 3;
```

## References
- Implementation of Dependent Type Theory, by Andrej Bauer: https://math.andrej.com/2012/11/08/how-to-implement-dependent-type-theory-i/
- A tutorial implementation of a dependently typed lambda calculus, Andres Loh et al.: https://www.andres-loeh.de/LambdaPi/LambdaPi.pdf
- Introduction to Homotopy Type Theory, by Egbert Rijke: https://arxiv.org/abs/2212.11082
