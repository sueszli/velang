namespace Velang

inductive E where
  | n : Int → E
  | m : Nat → Nat → List E → E
  | add : E → E → E
  | mul : E → E → E
  | mmul : E → E → E
  | ix : E → Nat → Nat → E

def tok (s : String) : List String :=
  let nc := String.intercalate "\n" <| s.splitOn "\n" |>.map fun l => (l.splitOn ";").headD ""
  let p := ((nc.replace "(" " ( ").replace ")" " ) ").replace "\n" " "
  p.splitOn " " |>.filter (· ≠ "")

mutual

partial def parse : List String → Option (E × List String)
  | "(" :: "+" :: ts => do
    let (a, ts) ← parse ts; let (b, ts) ← parse ts
    match ts with | ")" :: r => some (E.add a b, r) | _ => none
  | "(" :: "*" :: ts => do
    let (a, ts) ← parse ts; let (b, ts) ← parse ts
    match ts with | ")" :: r => some (E.mul a b, r) | _ => none
  | "(" :: "@" :: ts => do
    let (a, ts) ← parse ts; let (b, ts) ← parse ts
    match ts with | ")" :: r => some (E.mmul a b, r) | _ => none
  | "(" :: "ix" :: ts => do
    let (a, ts) ← parse ts
    match ts with
    | i :: j :: ")" :: r => do
      let i ← i.toNat?; let j ← j.toNat?; some (E.ix a i j, r)
    | _ => none
  | "(" :: "mat" :: r :: c :: ts => do
    let r ← r.toNat?; let c ← c.toNat?
    let (xs, ts) ← parseList ts
    some (E.m r c xs, ts)
  | t :: ts => do let i ← t.toInt?; some (E.n i, ts)
  | [] => none

partial def parseList : List String → Option (List E × List String)
  | ")" :: r => some ([], r)
  | ts => do
    let (e, ts) ← parse ts
    let (es, ts) ← parseList ts
    some (e :: es, ts)

end

abbrev M := StateM (Nat × Array String)

def kst (i : Int) : M String := do
  let v ← modifyGet fun (n, ls) => (s!"%v{n}", (n+1, ls))
  modify fun (n, ls) => (n, ls.push s!"  {v} = \"arith.constant\"() <\{\"value\" = {i} : i32}> : () -> i32")
  return v

def bop (op a b : String) : M String := do
  let v ← modifyGet fun (n, ls) => (s!"%v{n}", (n+1, ls))
  modify fun (n, ls) => (n, ls.push s!"  {v} = \"arith.{op}\"({a}, {b}) : (i32, i32) -> i32")
  return v

def bcast (ra ca rb cb : Nat) (va vb : Array String) :
    Nat × Nat × Array String × Array String :=
  if ra*ca == 1 then (rb, cb, Array.replicate (rb*cb) va[0]!, vb)
  else if rb*cb == 1 then (ra, ca, va, Array.replicate (ra*ca) vb[0]!)
  else (ra, ca, va, vb)

mutual

partial def binop (op : String) (a b : E) : M (Nat × Nat × Array String) := do
  let (ra,ca,va) ← comp a; let (rb,cb,vb) ← comp b
  let (r,c,va,vb) := bcast ra ca rb cb va vb
  let o ← (List.range (r*c)).toArray.mapM fun k => bop op va[k]! vb[k]!
  return (r, c, o)

partial def comp : E → M (Nat × Nat × Array String)
  | .n i => return (1, 1, #[← kst i])
  | .m r c xs => do
    return (r, c, ← xs.toArray.mapM fun x => do let (_,_,vs) ← comp x; return vs[0]!)
  | .add a b => binop "addi" a b
  | .mul a b => binop "muli" a b
  | .mmul a b => do
    let (ar,ac,va) ← comp a; let (_,bc,vb) ← comp b
    let o ← (List.range (ar*bc)).toArray.mapM fun ij => do
      let mut s ← kst 0
      for k in [0:ac] do
        s ← bop "addi" s (← bop "muli" va[(ij/bc)*ac+k]! vb[k*bc+ij%bc]!)
      return s
    return (ar, bc, o)
  | .ix e i j => do let (_,c,vs) ← comp e; return (1, 1, #[vs[i*c+j]!])

end

def compile (e : E) : String :=
  let ((_,_,vs), (_, ls)) := (comp e).run (0, #[])
  let body := String.intercalate "\n" ls.toList
  s!"\"builtin.module\"() (\{\n{body}\n  \"func.return\"({vs[0]!}) : (i32) -> ()\n}) : () -> ()"

def run (src : String) : Option String :=
  match parse (tok src) with
  | some (e, []) => some (compile e)
  | _ => none

end Velang

def main : List String → IO UInt32
  | [p] => do
    let src ← IO.FS.readFile p
    match Velang.run src with
    | some mlir => IO.println mlir; return 0
    | none => IO.eprintln s!"velang: parse error in {p}"; return 1
  | _ => do IO.eprintln "usage: velang <file.velang>"; return 1
