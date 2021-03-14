open Ast

module StringMap = Map.Make(String)

module StringHash = Hashtbl.Make(struct
      type t = string (* type of keys *)
      let equal x y = x = y (* use structural comparison *)
      let hash = Hashtbl.hash (* generic hash function *)
      end)
let map  = StringHash.create 10

let rec eval = function
    Lit(x)            -> x
  | Var(x)            -> StringHash.find map x
  | Binop(e1, op, e2) ->
      let v1  = eval e1 in
      let v2 = eval e2 in
      (match op with
	Add -> v1 + v2
      | Sub -> v1 - v2
      | Mul -> v1 * v2
      | Div -> v1 / v2
      | Seq -> v2)
  | Assign(v1, e2) -> let v2 = eval e2 in (StringHash.add map v1 v2; v2)
  | Call(v1, v2) -> let func = (StringHash.find map v1) in func v2
 
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.tokenize lexbuf in
  let result = eval expr in
  print_endline (string_of_int result)