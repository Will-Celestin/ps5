open Ast
open Eval
open Assertions

(* TODO: write more unit tests for [eval]

        _,  .-.  .-.
     ()(_ _ |  \/  |
         (_|||\__/|||
            ||    |||_
           _||   _||
          `""`  `""`
*)

(*Integers*)
TEST_UNIT = eval [] (Int 42) === VInt 42
TEST_UNIT = eval [] (BinOp (Plus,Int 42,Int 42)) === VInt 84
TEST_UNIT = eval [] (BinOp (Minus,Int 42,Int 42)) === VInt 0
TEST_UNIT = eval [] (BinOp (Times,Int 4,Int 2)) === VInt 8
TEST_UNIT = eval [] (BinOp (Gt,Int 42,Int 42)) === VBool false
TEST_UNIT = eval [] (BinOp (Gt,Int 42,Int 4)) === VBool true
TEST_UNIT = eval [] (BinOp (Lt,Int 42,Int 42)) === VBool false
TEST_UNIT = eval [] (BinOp (Lt,Int 4,Int 42)) === VBool true
TEST_UNIT = eval [] (BinOp (GtEq,Int 42,Int 42)) === VBool true
TEST_UNIT = eval [] (BinOp (GtEq,Int 42,Int 4)) === VBool true
TEST_UNIT = eval [] (BinOp (GtEq,Int 4,Int 42)) === VBool false
TEST_UNIT = eval [] (BinOp (LtEq,Int 42,Int 42)) === VBool true
TEST_UNIT = eval [] (BinOp (LtEq,Int 4,Int 42)) === VBool true
TEST_UNIT = eval [] (BinOp (LtEq,Int 42,Int 4)) === VBool false
TEST_UNIT = eval [] (BinOp (Eq,Int 42,Int 42)) === VBool true
TEST_UNIT = eval [] (BinOp (Eq,Int 42,Int 4)) === VBool false
TEST_UNIT = eval [] (BinOp (NotEq,Int 42,Int 4)) === VBool true
TEST_UNIT = eval [] (BinOp (NotEq,Int 42,Int 42)) === VBool false

(*Bools*)
TEST_UNIT = eval [] (BinOp (Eq, Bool true, Bool false)) === VBool false
TEST_UNIT = eval [] (BinOp (Eq, Bool false, Bool true)) === VBool false
TEST_UNIT = eval [] (BinOp (Eq, Bool false, Bool false)) === VBool true
TEST_UNIT = eval [] (BinOp (Eq, Bool true, Bool true)) === VBool true
TEST_UNIT = eval [] (BinOp (NotEq, Bool true, Bool false)) === VBool true
TEST_UNIT = eval [] (BinOp (NotEq, Bool false, Bool true)) === VBool true
TEST_UNIT = eval [] (BinOp (NotEq, Bool false, Bool false)) === VBool false
TEST_UNIT = eval [] (BinOp (NotEq, Bool true, Bool true)) === VBool false

(*Unit*)
TEST_UNIT = eval [] (Unit) === VUnit
TEST_UNIT = eval [] (BinOp (Eq,Unit,Unit)) === VBool true
TEST_UNIT = eval [] (BinOp (NotEq,Unit,Unit)) === VBool false

(*Strings*)
TEST_UNIT = eval [] (String "A") === VString "A"
TEST_UNIT = eval [] (BinOp (Concat, String "A ", String "B")) === VString "A B"
TEST_UNIT = eval [] (BinOp (Eq, String "A ", String "B")) === VBool false
TEST_UNIT = eval [] (BinOp (Eq, String "A", String "A")) === VBool true
TEST_UNIT = eval [] (BinOp (NotEq, String "A ", String "B")) === VBool true
TEST_UNIT = eval [] (BinOp (NotEq, String "A", String "A")) === VBool false

(*Pairs*)
(*Strings*)
TEST_UNIT = eval [] (BinOp (Concat, Pair (String "A",String "B"), Pair (String "C",String "D"))) ===
VPair (VString "AC",VString "BD")

TEST_UNIT = eval [] (BinOp (Eq, Pair (String "A",String "B"), Pair (String "C",String "D"))) ===
VPair (VBool false,VBool false)

TEST_UNIT = eval [] (BinOp (Eq, Pair (String "A",String "B"), Pair (String "A",String "B"))) ===
VPair (VBool true,VBool true)

TEST_UNIT = eval [] (BinOp (NotEq, Pair (String "A",String "B"), Pair (String "C",String "D"))) ===
VPair (VBool true,VBool true)

TEST_UNIT = eval [] (BinOp (NotEq, Pair (String "A",String "B"), Pair (String "A",String "B"))) ===
VPair (VBool false,VBool false)
(*Unit*)
TEST_UNIT = eval [] (BinOp (Eq, Pair (Unit,Unit), Pair (Unit,Unit))) === VPair (VBool true,VBool true)
TEST_UNIT = eval [] (BinOp (NotEq, Pair (Unit,Unit), Pair (Unit,Unit))) === VPair (VBool false,VBool false)
(*Bools*)
TEST_UNIT = eval [] (BinOp (Eq, Pair (Bool true,Bool false), Pair (Bool true,Bool false))) ===
VPair (VBool true,VBool true)

TEST_UNIT = eval [] (BinOp (Eq, Pair (Bool false,Bool true), Pair (Bool true,Bool false))) ===
VPair (VBool false,VBool false)

TEST_UNIT = eval [] (BinOp (Eq, Pair (Bool true,Bool true), Pair (Bool true,Bool false))) ===
VPair (VBool true,VBool false)

TEST_UNIT = eval [] (BinOp (NotEq, Pair (Bool true,Bool false), Pair (Bool true,Bool false))) ===
VPair (VBool false,VBool false)

TEST_UNIT = eval [] (BinOp (NotEq, Pair (Bool false,Bool true), Pair (Bool true,Bool false))) ===
VPair (VBool true,VBool true)

TEST_UNIT = eval [] (BinOp (NotEq, Pair (Bool false,Bool false), Pair (Bool true,Bool false))) ===
VPair (VBool true,VBool false)
(*Integers*)
TEST_UNIT = eval [] (BinOp (Plus, Pair (Int 42,Int 42), Pair (Int 42,Int 42))) ===
VPair (VInt 84,VInt 84)

TEST_UNIT = eval [] (BinOp (Minus, Pair (Int 42,Int 42), Pair (Int 42,Int 42))) ===
VPair (VInt 0,VInt 0)

TEST_UNIT = eval [] (BinOp (Times, Pair (Int 4,Int 4), Pair (Int 2,Int 2))) ===
VPair (VInt 8,VInt 8)

TEST_UNIT = eval [] (BinOp (Eq, Pair (Int 4,Int 4), Pair (Int 2,Int 2))) ===
VPair (VBool false,VBool false)

TEST_UNIT = eval [] (BinOp (Eq, Pair (Int 4,Int 4), Pair (Int 4,Int 2))) ===
VPair (VBool true,VBool false)

TEST_UNIT = eval [] (BinOp (Eq, Pair (Int 4,Int 4), Pair (Int 4,Int 4))) ===
VPair (VBool true,VBool true)

TEST_UNIT = eval [] (BinOp (NotEq, Pair (Int 4,Int 2), Pair (Int 4,Int 2))) ===
VPair (VBool false,VBool false)

TEST_UNIT = eval [] (BinOp (NotEq, Pair (Int 4,Int 4), Pair (Int 2,Int 2))) ===
VPair (VBool true,VBool true)

TEST_UNIT = eval [] (BinOp (NotEq, Pair (Int 4,Int 2), Pair (Int 4,Int 4))) ===
VPair (VBool false,VBool true)

TEST_UNIT = eval [] (BinOp (Gt, Pair (Int 4,Int 4), Pair (Int 4,Int 2))) ===
VPair (VBool false,VBool true)

TEST_UNIT = eval [] (BinOp (GtEq, Pair (Int 4,Int 4), Pair (Int 4,Int 2))) ===
VPair (VBool true,VBool true)

TEST_UNIT = eval [] (BinOp (Lt, Pair (Int 4,Int 2), Pair (Int 4,Int 4))) ===
VPair (VBool false,VBool true)

TEST_UNIT = eval [] (BinOp (LtEq, Pair (Int 4,Int 2), Pair (Int 4,Int 4))) ===
VPair (VBool true,VBool true)

(*Vars*)
TEST_UNIT = eval [( "one" , ref (VInt 1))] (Var "one") === VInt 1
TEST_UNIT = eval [( "one" , ref (VBool true))] (Var "one") === VBool true
TEST_UNIT = eval [( "one" , ref (VBool false))] (Var "one") === VBool false
TEST_UNIT = eval [( "one" , ref (VUnit))] (Var "one") === VUnit
TEST_UNIT = eval [( "one" , ref (VString "one"))] (Var "one") === VString "one"
TEST_UNIT = eval [( "one" , ref (VPair (VString "A",VString "B")))] (Var "one") ===
VPair (VString "A",VString "B")

(*If*)
TEST_UNIT = eval [] (Parse.parse_expr "if false then 4 + 2 else 4 * 2") === VInt 8
TEST_UNIT = eval [] (Parse.parse_expr "if true then 4 + 2 else 4 * 2") === VInt 6

(*Let*)
TEST_UNIT = eval [] (Parse.parse_expr "let a = 4 + 2 in a * 2") === VInt 12
TEST_UNIT = eval [] (Parse.parse_expr "let a = 4 * 2 in a + a") === VInt 16

(*LetRec/App*)
(*Map*)
TEST_UNIT = eval [] (Parse.parse_expr "let rec map = fun f -> fun l -> match l with
                                | Nil ()       -> Nil ()
                                | Cons (hd,tl) -> Cons (f hd, map f tl)
                                in map") === VClosure ("f",                                                                                       Fun ("l",
    Match (Var "l",
     [(PVariant ("Nil", PUnit), Variant ("Nil", Unit));
      (PVariant ("Cons", PPair (PVar "hd", PVar "tl")),
       Variant ("Cons", Pair (App (Var "f", Var "hd"), App (App (Var "map", Var "f"), Var "tl"))))])),
   [])
(*Fold*)
TEST_UNIT = eval [] (Parse.parse_expr "let rec fold = fun f -> fun l -> fun a -> match l with
                                 | Nil () -> a
                                 | Cons (hd,tl) -> f hd (fold f tl a)
                               in fold") === VClosure ("f",                                                                                       Fun ("l",
  Fun ("a",
   Match (Var "l",
    [(PVariant ("Nil", PUnit), Var "a");
     (PVariant ("Cons", PPair (PVar "hd", PVar "tl")),
      App (App (Var "f", Var "hd"), App (App (App (Var "fold", Var "f"), Var "tl"), Var "a")))]))),
 [])

(*Fun*)
TEST_UNIT = eval [] (Parse.parse_expr "fun x -> 3 + x") ===
VClosure ("x", BinOp (Plus, Int 3, Var "x"), [])

TEST_UNIT = eval [] (Fun ("x", BinOp(Plus, Int 3, Var "x"))) ===
VClosure ("x", BinOp (Plus, Int 3, Var "x"), [])

(*App*)
TEST_UNIT = eval [] (App ((Parse.parse_expr "fun x -> 3 + x"),Int 3)) ===
VInt 6

TEST_UNIT = eval [] (App ((Parse.parse_expr "fun x -> 3 * x"),Int 3)) ===
VInt 9

(*Variant*)
TEST_UNIT = eval [] (Variant ("name", Parse.parse_expr "if false then 4 + 2 else 4 * 2")) ===
VVariant ("name", VInt 8)

TEST_UNIT = eval [] (Variant ("name", BinOp(Minus, Int 3,Int 3 ))) ===
VVariant ("name", VInt 0)

(*Match*)


let () = Pa_ounit_lib.Runtime.summarize()
