open Unix
let get a i = a.(i - 1)

let gather t =
  t.Unix.tms_utime +. t.tms_stime +. t.tms_cutime +. t.tms_cstime

let arr =
  Random.self_init ();
  Array.init 100000000 (fun _ -> Random.bool ())

let a = [|1|]
let b = [|1.0|]

let () =
  let t1 = Unix.times () in
  for i = 1 to Array.length arr do
    if get arr i then ignore (get a 1) else ignore (get b 1)
  done;
  let t2 = Unix.times () in
  gather t2 -. gather t1
  |> Format.printf "%f\n"
