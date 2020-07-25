type regs =
  | EAX
  | EBX
  | ECX
  | EDX
  | NoReg

let name reg =
  match reg with
    | EAX -> "eax"
    | EBX -> "ebx"
    | ECX -> "ecx"
    | EDX -> "edx"
    | NoReg -> "spilled"

exception Empty_Pool of string

let get_reg pool =
  match pool with
    | [] -> raise (Empty_Pool "Empty Register Pool!")
    | (h::tail) -> h, tail

let free_reg reg pool =
  match reg with
    | NoReg -> pool
    | reg ->
      match List.mem reg pool with
        | true -> pool
        | false -> reg :: pool