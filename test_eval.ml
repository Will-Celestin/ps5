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




let () = Pa_ounit_lib.Runtime.summarize()
