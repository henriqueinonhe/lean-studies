inductive Formula where
  | P (letter : String)
  | Not (formula : Formula)
  | And (left : Formula) (right : Formula)
  | Or (left : Formula) (right : Formula)
  | Implies (left : Formula) (right : Formula)
  | Equivalent (left : Formula) (right : Formula)
