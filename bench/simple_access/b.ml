open Unix

let gather t =
  t.Unix.tms_utime +. t.tms_stime +. t.tms_cutime +. t.tms_cstime

let control = Gc.get ()
let () = Gc.set
    { control
      with Gc.minor_heap_size = 320000;
           Gc.space_overhead = 80 * 10 }

let () =
  let r = ref 0. in
  let arr = A.arr () in
  let t1 = Unix.times () in
  for i = 1 to Array.length arr do
    r := !r +. A.sum (A.get arr i)
  done;
  let t2 = Unix.times () in
  gather t2 -. gather t1
  |> Format.printf "%f\n"
