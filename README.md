What I modified
---------------

I modified the definitions of `Lambda.lambda` and `Clambda.ulambda` for better inlining.
The original compiler doesn't pass `array_kind` when inlining. For example,

    let make e = [|e|]
    let get a i = a.(i - 1) (* 1-origin get *)
    let () =
      let a = make 1. in
      print_float (get a 1)

Functions `make` and `get` are simple. The compiler inlines them to void costs of function call.
OCaml usually doesn't pass type information in run-time. But __generics__ array.get and array.set are exceptions.

Generic array operation need type information because of the optimization for float array.
OCaml compiler deal with float array as special. In general, the compiler force every value to fit in one word.
But only float array is flatten. This makes generics array opearation, which takes polymorphic array, need type information.

In this case, we don't need to pass any type information for `make` and `get` because we inline them and we know
`make` creates float array and `get` gets from float array. We can think the compiler can optimize the bodies of inlined functions.
Because every expression is represented as untyped intermediate languages when inlining,
the original compiler cannot avoid run-time type checks.

I extend the intermediate languages, `lambda` and `clambda`, to represent how polymorphic functions are specialized by types.

    type type_kind = I | F | P | Kvar of int

    and array_kind =
      Pgenarray | Paddrarray | Pintarray | Pfloatarray
    | Ptvar of int (* I add here *)

    type lambda =
        Lvar of Ident.t
      | Lspecialized of lambda * kind_map list * Types.type_expr * Env.t
        ...

    type ulambda =
        Uvar of Ident.t
      | Uconst of uconstant
      | Uspecialized of ulambda * Lambda.kind_map list * Types.type_expr * Env.t
        ...

Benchmark
---------
Ubuntu 14.04
Intel(R) Core(TM) i7-2677M CPU @ 1.80GHz
DDR3 4GiB

I just tried one following micro-benchmark program with option `-unsafe -inline 10000`.
Compared the latest OCaml 4.02 in github with modified one.

a.ml

    let get a i = a.(i - 1)

    let sum a =
      get a 1 +. get a 2 +. get a 3 +.
      get a 4 +. get a 5 +. get a 6 +.
      get a 7 +. get a 8 +. get a 9

    let arr () =
      Array.init 20000000 (fun i -> Array.make 9 (float_of_int i))

b.ml

    let () =
      let r = ref 0. in
      let arr = A.arr () in
      for i = 1 to Array.length arr do
        r := !r +. A.sum (A.get arr i)
      done

An average of 100 executions of `time -f "%e %U %S" ./a.out`

    vanilla   5.038400 4.783000 0.240800
    modified  4.981300 4.737200 0.229600


Representation of clambda
----------------------
I modified the compiler to print `array_kind` explicitly and compile `b.ml` with `-dclambda`.

original one:

    (seq
      (let
        (match/1011
           (let (r/1008 "camlB__1"=0. arr/1009 (apply* camlA__arr_1013  0a))
             (for i/1010 1 to (array.length[Paddr] arr/1009)
               (assign r/1008
                 (+. r/1008
                   (let (a/1012 (array.unsafe_get[Pgen] arr/1009 (- i/1010 1)))
                     (+.
                       (+.
                         (+.
                           (+.
                             (+.
                               (+.
                                 (+.
                                   (+. (array.unsafe_get[Pgen] a/1012 0)
                                     (array.unsafe_get[Pgen] a/1012 1))
                                   (array.unsafe_get[Pgen] a/1012 2))
                                 (array.unsafe_get[Pgen] a/1012 3))
                               (array.unsafe_get[Pgen] a/1012 4))
                             (array.unsafe_get[Pgen] a/1012 5))
                           (array.unsafe_get[Pgen] a/1012 6))
                         (array.unsafe_get[Pgen] a/1012 7))
                       (array.unsafe_get[Pgen] a/1012 8))))))))
        0a)
      0a)


modified one:

    (seq
      (let
        (match/1011
           (let (r/1008 "camlB__1"=0. arr/1009 (apply* camlA__arr_1013  0a))
             (for i/1010 1 to pr(array.length arr/1009)
               (assign r/1008
                 pr(+. r/1008
                   (let
                     (a/1012 pr(array.unsafe_get[Paddr] arr/1009 pr(- i/1010 1)))
                     pr(+.
                       pr(+.
                         pr(+.
                           pr(+.
                             pr(+.
                               pr(+.
                                 pr(+.
                                   pr(+. pr(array.unsafe_get[Pfloat] a/1012 0)
                                     pr(array.unsafe_get[Pfloat] a/1012 1))
                                   pr(array.unsafe_get[Pfloat] a/1012 2))
                                 pr(array.unsafe_get[Pfloat] a/1012 3))
                               pr(array.unsafe_get[Pfloat] a/1012 4))
                             pr(array.unsafe_get[Pfloat] a/1012 5))
                           pr(array.unsafe_get[Pfloat] a/1012 6))
                         pr(array.unsafe_get[Pfloat] a/1012 7))
                       pr(array.unsafe_get[Pfloat] a/1012 8))))))))
        0a)
      0a)
