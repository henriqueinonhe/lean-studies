inductive Formula where
  | P (letter : String)
  | Not (formula : Formula)
  | And (left : Formula) (right : Formula)
  | Or (left : Formula) (right : Formula)
  | Implies (left : Formula) (right : Formula)
  | Equivalent (left : Formula) (right : Formula)

def AndFormula := { f : Formula // ∃P Q, f = Formula.And P Q }

def andLeft (f: AndFormula) : Formula :=
  match f with
    | Formula.And P Q => P
