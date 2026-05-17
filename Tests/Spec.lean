import Velang

#guard Velang.run "(+ 1 2)" = some (String.intercalate "\n" [
  "\"builtin.module\"() ({",
  "  %v0 = \"arith.constant\"() <{\"value\" = 1 : i32}> : () -> i32",
  "  %v1 = \"arith.constant\"() <{\"value\" = 2 : i32}> : () -> i32",
  "  %v2 = \"arith.addi\"(%v0, %v1) : (i32, i32) -> i32",
  "  \"func.return\"(%v2) : (i32) -> ()",
  "}) : () -> ()"])

#guard (Velang.run "(+ 1)").isNone
#guard (Velang.run "(+ 1 2 3)").isNone
#guard (Velang.run "(unknown)").isNone
#guard (Velang.run "1 2").isNone

#guard match Velang.run "(* 5 (mat 1 2 3 4))" with
  | some s => (s.splitOn "arith.muli").length == 3
  | _ => false
