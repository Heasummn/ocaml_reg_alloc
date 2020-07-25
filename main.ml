open Interval;;
open Register;;
open Stdio;;

let intervals = [|
  make_interval "a" 1 10;
  make_interval "b" 1 4;
  make_interval "c" 1 3;
  make_interval "d" 2 8;
  make_interval "e" 3 6;
  make_interval "f" 3 10;
  make_interval "g" 4 8;
|]
let stack_location = ref 0

let expire_old_intervals active current pool =
  let expire j = (fst j).end_point > current.start_point in
  (* let sorted = List.sort compare_endpoint active in *)
  let (new_active, regs_to_free) = List.partition (expire) active in
  let () = List.iter (fun x -> printf "Expiring interval for %s\n" (fst x).name) (regs_to_free) in
  let rec add pool l = match l with
    | (head::tail) -> let () = print_endline ("Adding register " ^ (name (fst head).reg) ^ " to pool") in add (free_reg (fst head).reg pool) tail
    | [] ->  pool
  in
  new_active, add pool regs_to_free

let spill_interval (active: (interval * int) list) current pool =
  let current, i = current in
  let rev_sort = List.sort (fun a b -> -1 * (compare_endpoint (fst a) (fst b))) active in
  let spill, index = List.hd rev_sort in
  let () = print_endline ("Longest interval is " ^ spill.name) in
  let tail = List.tl rev_sort in
  let active, pool = match spill.end_point >= current.end_point with
    | true ->
      let new_interval = {current with reg = spill.reg} in
      let () = intervals.(index) <- {spill with reg = NoReg; location = !stack_location} in
      let () = intervals.(i) <- new_interval in
        (new_interval, index)::tail, pool
    | false ->
      let () = intervals.(i) <- {current with location = !stack_location} in
        active, pool
  in
  let () = stack_location := !stack_location + 4 in
  active, pool

let () =
  let glob_pool = ref [EAX; EBX; ECX] in
  let num_regs = List.length !glob_pool in
  let glob_active = ref [] in

  let length = Array.length intervals in
  for i = 0 to length - 1 do
    let current = intervals.(i) in
    let (active, pool) = expire_old_intervals !glob_active current !glob_pool in
    let () = List.iter (fun x -> print_endline (name x)) pool in
    let () = print_endline "" in
    let (active, pool) = match List.length active >= num_regs with
      | true -> let () = print_endline ("conflict at variable " ^ current.name) in spill_interval active (current, i) pool
      | false ->  let new_reg, pool = get_reg pool in
                  let new_interval = {current with reg = new_reg} in
                  let () = intervals.(i) <- new_interval in
                  (new_interval, i)::active, pool
    in
    let () = glob_active := active in
    glob_pool := pool
  done;;
  Array.iter (fun interval -> printf "Interval %s with reg %s at location %d \n" interval.name (name interval.reg) interval.location) intervals