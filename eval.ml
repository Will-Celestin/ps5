open Ast

(******************************************************************************)
(** types (see .mli) **********************************************************)
(******************************************************************************)

type value =
  | VUnit | VInt of int | VBool of bool | VString of string
  | VClosure of var * expr * environment
  | VVariant of constructor * value
  | VPair of value * value
  | VError of string
and environment = (var * value ref) list

(******************************************************************************)
(** (optional) helper functions ***********************************************)
(******************************************************************************)

(** you may find it helpful to implement these or other helper
 * functions, but they are not required. Feel free to implement them if you
 * need them, change their types or arguments, delete them, whatever.
 *)

 (**
  * try to match a value against a pattern. If the match succeeds, return an
  * environment containing all of the bindings. If it fails, return None.
  *)

let rec find_match (p : pattern) (v : value) : environment option =
  failwith "Hot tunnels alternated with cool tunnels."

(** apply the given operator to the given arguments *)
let rec eval_operator op v1 v2 =
  match v1 with
  | VUnit -> VUnit

  | VInt i -> ( match v2 with
                | VInt j -> if    (op=Plus) then VInt (i+j)
                          else if (op=Minus) then VInt (i-j)
                          else if (op=Times) then VInt (i*j)
                          else if (op=Gt) then VBool (i>j)
                          else if (op=Lt) then VBool (i<j)
                          else if (op=GtEq) then VBool (i>=j)
                          else if (op=LtEq) then VBool (i<=j)
                          else if (op=NotEq) then VBool (i!=j)
                          else VError "Invalid operator"
                | _ -> VError "Value mismatch")


  | VBool b -> (match v2 with
                | VBool c -> if (op=Eq) then VBool (b=c)
                             else if (op=NotEq) then VBool (b!=c)
                             else VError "Invalid operator"
                | _ -> VError "Value mismatch")


  | VString s -> (match v2 with
                  | VString t -> if (op=Concat) then VString (s^t)
                                 else if (op=Eq) then VBool (s=t)
                                 else if (op=NotEq) then VBool (s!=t)
                                 else VError "Invalid operator"
                  | _ -> VError "Value mismatch")

  | VPair (x,y) -> (match v2 with
                    | VPair (a,b) -> VPair (eval_operator op x a,eval_operator op y b)
                    | _ -> VError "Value mismatch")

  | _ -> VError "Invalid values"

(** Format a value for printing. *)
let rec format_value (f : Format.formatter) (v : value) : unit =
  (* You will probably want to call Format.fprint f f <format string> <args>.
   *
   * Format.fprintf f <format string> has a different type depeding on the format
   * string. For example, Format.fprintf f "%s" has type string -> unit, while
   * Format.fprintf f "%i" has type int -> unit.
   *
   * Format.fprintf f "%a" is also useful. It has type
   *   (Format.formatter -> 'a -> unit) -> 'a -> unit
   * which is useful for recursively formatting values.
   *
   * Format strings can contain multiple flags and also other things to be
   * printed. For example (Format.fprintf f "result: %i %s") has type
   * int -> string -> unit, so you can write
   *
   *  Format.fprintf f "result: %i %s" 3 "blind mice"
   *
   * to output "result: 3 blind mice"
   *
   * See the documentation of the OCaml Printf module for the list of % flags,
   * and see the printer.ml for some (complicated) examples. Printer, format_type is
   * a nice example.
   *)
  failwith "The light was frozen, dead, a ghost."

(** use format_value to print a value to the console *)
let print_value = Printer.make_printer format_value

(** use format_value to convert a value to a string *)
let string_of_value = Printer.make_string_of format_value

(******************************************************************************)
(** eval **********************************************************************)
(******************************************************************************)

let rec eval env e =
  match e with
  | Unit     -> VUnit
  | Int a    -> VInt a
  | Bool a   -> VBool a
  | String a -> VString a

  | BinOp (o,e1,e2) -> eval_operator o (eval env e1) (eval env e2)

  | If (e1,e2,e3) -> (match (eval env e1) with
                      | VBool a -> if (a=true) then eval env e2
                                   else if (a=false) then eval env e3
                                   else VError "Invalid evaluation of bool"
                      | _ -> VError "Invalid evaluation of bool")

  | Var a ->  (match env with
               | [] -> VError "Variable not found"
               | h::d ->  let (j,k) = h in
                          if (a=j) then (!k)
                          else eval d e)

  | Let (v,e1,e2) -> let x = eval env e1 in
                     eval ((v,ref x)::env) e2

  | LetRec (f,e1,e2) -> let x = eval env e1 in
                        eval ((f,ref x)::env) e2

  | App (e1,e2) -> let v = eval env e2 in
                   eval ((string_of_value v,ref v)::env) e1

  | Fun (v,e)       -> VClosure (v,e,env)

  | Pair (e1,e2)    -> VPair (eval env e1, eval env e2)

  | Variant (e1,e2) -> VVariant (e1, eval env e2)

  | Match (e0,li)   ->  let m p e =
                          let pm pat =
                            match pat with
                            | PUnit -> 0
                            | PInt a -> 1
                            | PBool a -> 2
                            | PString a -> 3
                            | PVar a -> 4
                            | PVariant (a,b) -> 5
                            | PPair (a,b) -> 6
                          in
                          let em exp =
                            match exp with
                            | Unit -> 0
                            | Int a -> 1
                            | Bool a -> 2
                            | String a -> 3
                            | Var a -> 4
                            | Variant (a,b) -> 5
                            | Pair (a,b) -> 6
                            | _ -> -1
                          in
                          match ((pm p)=(em e)) with
                          |true -> true
                          |false -> false
                        in
                        match li with
                       | []   -> VError "Match not found"
                       | h::d -> let (p,e) = h in
                                  if ((m p e0) = true) then eval env e
                                  else eval env (Match (e0,d))
