

type expr =
    StrLit of string
  | Call of string * expr list

type program = expr

