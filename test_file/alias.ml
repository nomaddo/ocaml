type 'a t = 'a list
let list : int t = [2;3;4]
let map f l = List.map f l

let _ = map ((+) 1) list
