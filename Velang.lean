namespace Velang

inductive E where
  | n : Int → E
  | m : Nat → Nat → List E → E
  | add : E → E → E
  | mul : E → E → E
  | mmul : E → E → E
  | ix : E → Nat → Nat → E

def tok (s : String) : List String :=
  s.splitOn "\n" |>.map (·.splitOn ";" |>.headD "") |> String.intercalate " "
    |>.replace "(" " ( " |>.replace ")" " ) " |>.splitOn " " |>.filter (· ≠ "")

mutual

partial def bin (f : E → E → E) (ts : List String) : Option (E × List String) := do
  let (a, ts) ← parse ts
  let (b, ts) ← parse ts
  match ts with | ")" :: r => some (f a b, r) | _ => none

partial def parse : List String → Option (E × List String)
  | "(" :: "+"  :: ts => bin E.add ts
  | "(" :: "*"  :: ts => bin E.mul ts
  | "(" :: "@"  :: ts => bin E.mmul ts
  | "(" :: "ix" :: ts => do
    let (a, ts) ← parse ts
    match ts with
    | si :: sj :: ")" :: r => (λ i j => (E.ix a i j, r)) <$> si.toNat? <*> sj.toNat?
    | _ => none
  | "(" :: "mat" :: sr :: sc :: ts => do
    let (xs, ts) ← parseList ts
    (λ r c => (E.m r c xs, ts)) <$> sr.toNat? <*> sc.toNat?
  | t :: ts => (λ i => (E.n i, ts)) <$> t.toInt?
  | []      => none

partial def parseList : List String → Option (List E × List String)
  | ")" :: r => some ([], r)
  | ts       => do
    let (e,  ts) ← parse ts
    let (es, ts) ← parseList ts
    some (e :: es, ts)

end

abbrev M := StateM (Nat × Array String)

def fresh (rhs : String) : M String :=
  modifyGet λ (n, ls) => (s!"%v{n}", (n+1, ls.push s!"  %v{n} = {rhs}"))

def kst (i : Int)       : M String := fresh s!"\"arith.constant\"() <\{\"value\" = {i} : i32}> : () -> i32"
def bop (op a b : String) : M String := fresh s!"\"arith.{op}\"({a}, {b}) : (i32, i32) -> i32"

def bcast (ra ca rb cb : Nat) (va vb : Array String) : Nat × Nat × Array String × Array String :=
  if ra*ca == 1      then (rb, cb, Array.replicate (rb*cb) va[0]!, vb)
  else if rb*cb == 1 then (ra, ca, va, Array.replicate (ra*ca) vb[0]!)
  else                    (ra, ca, va, vb)

mutual

partial def binop (op : String) (a b : E) : M (Nat × Nat × Array String) := do
  let (ra,ca,va) ← comp a
  let (rb,cb,vb) ← comp b
  let (r,c,va,vb) := bcast ra ca rb cb va vb
  let o ← (List.range (r*c)).toArray.mapM λ k => bop op va[k]! vb[k]!
  pure (r, c, o)

partial def comp : E → M (Nat × Nat × Array String)
  | .n i      => (λ v => (1, 1, #[v])) <$> kst i
  | .m r c xs => (r, c, ·) <$> xs.toArray.mapM λ x => do let (_,_,a) ← comp x; pure a[0]!
  | .add a b  => binop "addi" a b
  | .mul a b  => binop "muli" a b
  | .mmul a b => do
    let (ar,ac,va) ← comp a
    let (_,bc,vb)  ← comp b
    let o ← (List.range (ar*bc)).toArray.mapM λ ij => do
      let i := ij / bc; let j := ij % bc
      let s₀ ← kst 0
      (List.range ac).foldlM (init := s₀) λ s k => do
        bop "addi" s (← bop "muli" va[i*ac+k]! vb[k*bc+j]!)
    pure (ar, bc, o)
  | .ix e i j => do let (_,c,vs) ← comp e; pure (1, 1, #[vs[i*c+j]!])

end

def compile (e : E) : String :=
  let ((_,_,vs), (_, ls)) := (comp e).run (0, #[])
  s!"\"builtin.module\"() (\{\n{String.intercalate "\n" ls.toList}\n  \"func.return\"({vs[0]!}) : (i32) -> ()\n}) : () -> ()"

def run (src : String) : Option String :=
  match parse (tok src) with
  | some (e, []) => some (compile e)
  | _            => none

end Velang

def main : List String → IO UInt32
  | [p] => do match Velang.run (← IO.FS.readFile p) with
    | some s => IO.println s        *> pure 0
    | none   => IO.eprintln s!"velang: parse error in {p}" *> pure 1
  | _   => IO.eprintln "usage: velang <file.velang>" *> pure 1
