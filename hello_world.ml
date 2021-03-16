open Ast

module StringHash = Hashtbl.Make(struct
      type t = string (* type of keys *)
      let equal x y = x = y (* use structural comparison *)
      let hash = Hashtbl.hash (* generic hash function *)
      end)

let map  = StringHash.create 10

let builtin_print_s s = (print_endline(s); 0) in StringHash.add map "print" builtin_print_s

let rec eval = function
    StrLit -> 1
  | Call(v1, v2) -> let func = (StringHash.find map v1) and let s::t = v2 in (func s; 0)
 
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.tokenize lexbuf in
  let result = eval expr in
  print_endline (string_of_int result)