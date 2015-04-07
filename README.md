What I modified
---------------

I modified the definitions of Lambda.lambda and Clambda.ulambda for better inlining.  
The original compiler doesn't pass `array_kind` when inlining. For example,

    let make e = [|e|]
    let get a i = a.(i - 1) (* 1-origin get *)
    let () =
      let a = make 1. in
      print_float (get a 1)

Functions `make` and `get` are simple. So the compiler inlines them to void costs of function call.
OCaml usually doesn't pass type information in run-time. But generics array.get or array.set are exceptions.

In this case, we don't need to pass any type information for `make` and `get` because we inline them and we know
`make` creates float array and `get` gets from float array. We can think the compiler can optimize the bodies of inlined functions.
Because every expression is represented as untyped intermediate languages when inlining,
The original compiler cannot avoid run-time type checks.

I extend the intermediate languages, lambda and clambda, to represent how polymorphic functions are specialized by types.

Benchmark
---------
I just tried one following benchmark program.

a.ml
    let get a i = a.(i - 1)
     
    let sum a =
      get a 1 + get a 2 + get a 3 +
      get a 4 + get a 5 + get a 6 +
      get a 7 + get a 8 + get a 9
     
    let arr () =
      Array.init 10000000 (fun i -> Array.make 9 i)

b.ml
    let () =
      let r = ref 0 in
      let arr = A.arr () in
      for i = 1 to Array.length arr do
        r := !r + A.sum (A.get arr i)
      done; print_int !r; print_endline ""


result
    [~/ocaml/bench] time ./my 
    449999955000000
       
    real    0m2.361s
    user    0m1.976s
    sys     0m0.352s

     [~/ocaml/bench] time ./vanilla 
     449999955000000
      
     real	0m3.905s
     user	0m3.592s
     sys	0m0.268s
