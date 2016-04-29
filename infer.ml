open Ast
open TypedAst

type equation = Eq of typ * typ

(******************************************************************************)
(** type substitution *********************************************************)
(******************************************************************************)

(**
 * These are useful functions for applying a substitution of a type for a type
 * variable
 *)

(** A substitution of a type for a type variable *)
type substitution = talpha * typ

(** apply a type substitution to a type *)
let rec subst_typ ((x,t'):substitution) (t:typ) =
  match t with
  | TAlpha y
      -> if y = x then t' else TAlpha y
  | TUnit | TInt | TBool | TString
      -> t
  | TArrow (t1,t2)
      -> TArrow (subst_typ (x,t') t1, subst_typ (x,t') t2)
  | TStar (t1,t2)
      -> TStar (subst_typ (x,t') t1, subst_typ (x,t') t2)
  | TVariant (ts, name)
      -> TVariant (List.map (subst_typ (x,t')) ts, name)

(** apply a type substitution to a list of equations *)
let subst_eqn (s : substitution) (eqns : equation list) : equation list =
  List.map (fun (Eq (t1,t2)) -> Eq(subst_typ s t1, subst_typ s t2)) eqns

(** apply a type substitution to an annotated expression 
    we deliberately violate the 80-column restriction here to make the
    parallelism in the definition clearer, hence easier to read *)
let rec subst_expr (s : substitution) (e : annotated_expr) : annotated_expr =
  match e with
  | AVar      (t,x)            -> AVar      (subst_typ s t, x)
  | AApp      (t,e1,e2)        -> AApp      (subst_typ s t, subst_expr s e1, subst_expr s e2)
  | AFun      (t,(x,tx),e)     -> AFun      (subst_typ s t, (x, subst_typ s tx), subst_expr s e)
  | ALet      (t,(x,tx),e1,e2) -> ALet      (subst_typ s t, (x, subst_typ s tx), subst_expr s e1, subst_expr s e2)
  | ALetRec   (t,(x,tx),e1,e2) -> ALetRec   (subst_typ s t, (x, subst_typ s tx), subst_expr s e1, subst_expr s e2)
  | AUnit     (t)              -> AUnit     (subst_typ s t)
  | AInt      (t,n)            -> AInt      (subst_typ s t, n)
  | ABool     (t,b)            -> ABool     (subst_typ s t, b)
  | AString   (t,k)            -> AString   (subst_typ s t, k)
  | AVariant  (t,c,e)          -> AVariant  (subst_typ s t, c, subst_expr s e)
  | APair     (t,e1,e2)        -> APair     (subst_typ s t, subst_expr s e1, subst_expr s e2)
  | ABinOp    (t,op,e1,e2)     -> ABinOp    (subst_typ s t, op, subst_expr s e1, subst_expr s e2)
  | AIf       (t,e1,e2,e3)     -> AIf       (subst_typ s t, subst_expr s e1, subst_expr s e2, subst_expr s e3)
  | AMatch    (t,e,ps)         -> AMatch    (subst_typ s t, subst_expr s e, List.map (subst_case s) ps)
and subst_case s (p,e) = subst_pat s p, subst_expr s e
and subst_pat  s = function
  | APUnit    (t)              -> APUnit    (subst_typ s t)
  | APInt     (t,n)            -> APInt     (subst_typ s t, n)
  | APBool    (t,b)            -> APBool    (subst_typ s t, b)
  | APString  (t,k)            -> APString  (subst_typ s t, k)
  | APVar     (t,x)            -> APVar     (subst_typ s t, x)
  | APVariant (t,c,p)          -> APVariant (subst_typ s t, c, subst_pat s p)
  | APPair    (t,p1,p2)        -> APPair    (subst_typ s t, subst_pat s p1, subst_pat s p2)

(******************************************************************************)
(** helper functions **********************************************************)
(******************************************************************************)

(* you may find it helpful to implement these or other helper
 * functions, but they are not required.  Feel free to implement them if you
 * need them, change their types or arguments, delete them, whatever.
 *)


(** Format a list of equations for printing. *)
let format_eqns (f : Format.formatter) (eqns : equation list) : unit =
  (* see the comment in Eval.format_value for guidance implementing hints *)
  failwith "One believes things because one has been conditioned to believe them."

(** use format_eqns to print a value to the console *)
let print_eqns     = Printer.make_printer format_eqns

(** use format_value to convert a value to a string *)
let string_of_eqns = Printer.make_string_of format_eqns




(** generate an unused type variable *)
let next_var  = ref 0
let newvar () : typ =next_var := 1 + !next_var;
                  TAlpha (Format.sprintf "a%02i" !next_var)
  



(* return the constraints for a binary operator *)
let collect_binop (t:typ) (op:operator) (tl:typ) (tr:typ) : equation list =
  match op with 
    | Plus | Minus | Times -> [Eq (t,TInt); Eq (tl, TInt); Eq (tr, TInt)]
    | Gt | Lt | GtEq | LtEq -> [Eq (t,TBool); Eq (tl, TInt); Eq (tr, TInt)]
    | Concat -> [Eq (TString, t); Eq (TString, tl); Eq (TString, tr)]
    | Eq | NotEq ->  [Eq (t, TBool); Eq (tr, tl)] 
    
    (** return the constraints for an expr
  * vars refers to a data structure that stores the types of each of the variables
  * that have been defined.
  * It is completely your decision what type of data structure you want to use for vars
  *)
let rec collect_expr (specs:variant_spec list) vars (e : annotated_expr)
                     : equation list =
  match e with 
      |AUnit t -> [Eq (t, TUnit)] 
      |ABool (t,a) -> [Eq (t, TBool)] 
      |AString (t,a) -> [Eq (t, TString)] 
      |AInt (t,a) -> [Eq (t, TInt)] 
      
      |ABinOp (t,o,a1,a2) -> let x = collect_expr specs vars a1 in 
                             let y = collect_expr specs vars a2 in 
                             let t1 = typeof a1 in 
                             let t2 = typeof a2 in 
                             collect_binop t o t1 t2 @ x @ y
                                        
      |AIf (t,a1,a2,a3) -> let x = collect_expr specs vars a1 in 
                           let y = collect_expr specs vars a2 in
                           let z = collect_expr specs vars a3 in 
                           let t1 = typeof a1 in 
                           let t2 = typeof a2 in 
                           let t3 = typeof a3 in 
                           [Eq (t,t2); Eq (t,t3); Eq (t1, TBool)] @ x @ y @ z
                           
      |APair (t,a1,a2) -> let x = collect_expr specs vars a1 in 
                          let y = collect_expr specs vars a2 in
                          let t1 = typeof a1 in 
                          let t2 = typeof a2 in
                          [Eq (TStar (t1,t2), t)] @ x @ y
                          
      |ALet (t,(v,typ), a1,a2) -> let x = collect_expr specs vars a1 in
                                 let newvars = [(v,typ)] @ vars in
                                 let y = collect_expr specs newvars a2 in
                                 let t1 = typeof a1 in 
                                 let t2 = typeof a2 in
                                 [Eq (t, t2); Eq (t1, typ)] @ x @ y
      
      |ALetRec (t,(v,typ),a1,a2) -> let newvars = [(v,typ)] @ vars  in 
                                   let x = collect_expr specs newvars a1 in
                                   let y = collect_expr specs newvars a2 in
                                   let t1 = typeof a1 in 
                                   let t2 = typeof a2 in
                                   [Eq (t, t2); Eq (t1, typ)] @ x @ y
                                   
      |AFun (t,(v,typ),a) -> let newvars = [(v,typ)] @ vars in 
                             let x = collect_expr specs newvars a in 
                             let t1 = typeof a in
                             let t2 = TArrow (typ, t1) in 
                             [Eq (t2, t)] @ x
                             
      |AVar (t,v) ->  (match vars with 
                      |[] -> failwith "No such variable"
                      |h::tail -> let (var, typ) = h in 
                      if var = v then [Eq (t,typ)]
                      else (collect_expr specs tail e))
      
      |AApp (t,a1,a2) -> (let x = collect_expr specs vars a1 in 
                         let y = collect_expr specs vars a2 in 
                         let t1 = typeof a1 in 
                         let t2 = typeof a2 in
                         match t1 with 
                           |TArrow (t3,t4) -> 
                             [Eq (t, t4); Eq (t2, t3)] @ x @ y
                           | _ -> failwith "cannot apply")
                           
      |AMatch (t,a,li) -> (let x = collect_expr specs vars a in 
                          let t1 = typeof a in
                          match li with 
                            |[] -> []
                            |h::tail -> let (p,e) = h in 
                            let pats = collect_expr specs vars e in 
                            let t2 = typeof e in 
                            let pat = collect_pat specs vars p in 
                            let t3 = typeof_pat p in 
                            [Eq (t,t2); Eq (t1,t3)] @ x @ pat @ pats @ (collect_expr specs vars (AMatch (t,a,tail))))
                            
       |AVariant (t,c,a) -> let x = collect_expr specs vars a in 
                            let t1 = typeof a in
                            match specs with
                              |[] -> failwith "unknown variant"
                              |h::tail -> let cons = h.constructors in 
                                  let rec check_cons con =
                                  match con with
                                    |[] -> collect_expr tail vars e 
                                    |y::z -> let (constr, typ) = y in 
                                    if constr = c then
                                    (let var = h.vars in 
                                    let rec alpha_to_v lis newlist= 
                                      match lis with
                                        |[] -> newlist
                                        |hed::tel -> let talph = TAlpha hed in 
                                        alpha_to_v tel (newlist @ [talph]) in
                                    [Eq (t, TVariant((alpha_to_v var []), h.name)); Eq (t1,typ)] @ x)
                                    else check_cons z in 
                                  check_cons cons
                            

(** return the constraints for a match cases
  * tconst refers to the type of the parameters of the specific constructors
  * tvariant refers to the type of the variant as a whole
  *)
and collect_case specs vs tconst tvariant ((p:annotated_pattern),(e:annotated_expr)) =
    failwith "unused content"

(** return the constraints and variables for a pattern *)
and collect_pat specs vs (p:annotated_pattern) =
  match p with
      | APUnit t->  [Eq (t, TUnit)] 
      | APInt (t,a) -> [Eq (t, TInt)] 
      | APBool (t,a) -> [Eq (t, TBool)] 
      | APString (t,a) -> [Eq (t, TString)] 
      | APVar (t,v) -> (match vs with 
                      |[] -> failwith "No such variable"
                      |h::tail -> let (var, typ) = h in 
                      if var = v then [Eq (t,typ)]
                      else (collect_pat specs tail p))
      | APVariant (t,c,a) -> (let x = collect_pat specs vs a in 
                            let t1 = typeof_pat a in
                            match specs with
                              |[] -> failwith "unknown variant"
                              |h::tail -> let cons = h.constructors in 
                                  let rec check_cons con =
                                  match con with
                                    |[] -> collect_pat tail vs p 
                                    |y::z -> let (constr, typ) = y in 
                                    if constr = c then
                                    (let var = h.vars in 
                                    let rec alpha_to_v lis newlist= 
                                      match lis with
                                        |[] -> newlist
                                        |hed::tel -> let talph = TAlpha hed in 
                                        alpha_to_v tel (newlist @ [talph]) in
                                    [Eq (t, TVariant((alpha_to_v var []), h.name)); Eq (t1,typ)] @ x)
                                    else check_cons z in 
                                  check_cons cons)
      | APPair (t,a1,a2) -> let x = collect_pat specs vs a1 in 
                          let y = collect_pat specs vs a2 in
                          let t1 = typeof_pat a1 in
                          let t2 = typeof_pat a2 in 
                          [Eq (TStar (t1,t2), t)] @ x @ y

(******************************************************************************)
(** constraint generation                                                    **)
(******************************************************************************)

(**
 * collect traverses an expression e and returns a list of equations that must
 * be satisfied for e to typecheck.
 *)
let collect specs e = collect_expr specs [] e

(******************************************************************************)
(** constraint solver (unification)                                          **)
(******************************************************************************)

let rec occurs_in x = function
  | TAlpha y
      -> x = y
  | TArrow (t1,t2) | TStar (t1,t2)
      -> occurs_in x t1 || occurs_in x t2
  | TVariant (ts,_)
      -> List.exists (occurs_in x) ts
  | TUnit | TInt | TBool | TString
      -> false

(**
 * unify solves a system of equations and returns a list of
 * definitions for the type variables.
 *)
let rec unify eqns = match eqns with
  | [] -> []

  | Eq (t1,t2)::tl when t1 = t2
     -> unify tl

  | Eq ((TAlpha x as t1), (t as t2))::tl
  | Eq ((t as t1), (TAlpha x as t2))::tl
     -> if occurs_in x t
        then failwith (Format.asprintf "circular type constraint %a = %a"
                                       Printer.format_type t1
                                       Printer.format_type t2)
        else (x,t)::(unify (subst_eqn (x,t) tl))

  | Eq (TArrow (t1,t1'), TArrow (t2,t2'))::tl
  | Eq (TStar  (t1,t1'), TStar  (t2,t2'))::tl
     -> unify ((Eq (t1,t2))::(Eq (t1',t2'))::tl)

  | Eq ((TVariant (t1s, n1) as t1), (TVariant (t2s, n2) as t2))::tl
     -> if n1 <> n2
        then failwith (Format.asprintf "can't unify %a and %a"
                                       Printer.format_type t1
                                       Printer.format_type t2)
        else unify ((List.map2 (fun t1 t2 -> Eq (t1,t2)) t1s t2s)
                    @ tl)

  | Eq (t1,t2)::tl
     -> failwith (Format.asprintf "can't unify %a and %a"
                                  Printer.format_type t1
                                  Printer.format_type t2)

(******************************************************************************)
(** inference                                                                **)
(******************************************************************************)

(**
 * rename the type variables so that the first is "a", the
 * second "b", and so on.  Example:
 *
 *  rename_vars ('t23 -> 't17 -> 't23 -> int)
 *  is          ('a   -> 'b   -> 'a   -> int)
 *)
let rec simplify e =
  let rec alpha_of_int i =
    let let_of_int i = String.make 1 (char_of_int (i - 1 + int_of_char 'a')) in
    if i <= 0 then "" else (alpha_of_int (i/26))^(let_of_int (i mod 26))
  in

  let next_var  = ref 0 in

  let newvar () =
    next_var := 1 + !next_var;
    TAlpha (alpha_of_int !next_var)
  in

  let rec subst vars = function
    | TAlpha x -> if List.mem_assoc x vars then vars else (x,newvar())::vars
    | TUnit | TInt | TBool | TString -> vars
    | TArrow (t1,t2) | TStar (t1,t2) -> let vars' = subst vars t1 in
                                        subst vars' t2
    | TVariant (ts,_) -> List.fold_left subst vars ts
  in

  subst [] e

(**
 * given an expression, return the type for that expression,
 * failing if it cannot be typed.
 *)
let infer defs e =
  let annotated = annotate e in
  let eqns      = collect defs annotated in
  let solution  = unify eqns in
  let newtype   = List.fold_left (fun e s -> subst_expr s e) annotated solution in
  let simplify  = simplify (typeof newtype) in
  List.fold_right subst_expr simplify newtype

