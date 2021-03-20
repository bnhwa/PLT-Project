
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.program Scanner.tokenize lexbuf in
  expr

(* module StringHash = Hashtbl.Make(struct
      type t = string (* type of keys *)
      let equal x y = x = y (* use structural comparison *)
      let hash = Hashtbl.hash (* generic hash function *)
      end)

let map  = StringHash.create 10

let builtin_print s = s ;;

StringHash.add map "print" builtin_print 

let eval = function
    StrLit(x) -> 1
  | Call(v1, v2) -> let temp_f = (StringHash.find map v1) in let s = List.hd v2 in temp_f s ;; *)

(* v1 = "print"
v2 = ["helloworld"] *)
 
(* let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = parserAlt.expr scannerAlt.tokenize lexbuf in
  let result = eval expr in
  print_endline (string_of_int result) *)