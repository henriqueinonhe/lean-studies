inductive MyAnd (α : Type) (β : Type) where
  | mk (a : α) (b : β)

def AndELeft {α : Type} {β : Type} (x : MyAnd α β) : α :=
  match x with
    | MyAnd.mk a _ => a

def AndERight {α : Type} {β : Type} (x : MyAnd α β) : β :=
  match x with
    | MyAnd.mk _ b => b

def AndI {α : Type} {β : Type} (a : α) (b : β) : MyAnd α β :=
  MyAnd.mk a b

def MyAndComm (α : Type) (β : Type) : (MyAnd α β) -> (MyAnd β α) :=
  fun (x : MyAnd α β) =>
    AndI (AndERight x) (AndELeft x)

inductive MyNot (α : Type) where
  | mk (a : α)

def NotE {α : Type} (x : MyNot $ MyNot α) : α :=
  match x with
    | MyNot.mk y =>
      match y with
        | MyNot.mk a => a

def test {α : Type} {β : Type} {γ : Type}: MyAnd (MyAnd α $ MyNot $ MyNot β) γ -> β :=
  fun x : MyAnd (MyAnd α $ MyNot $ MyNot β) γ =>
    NotE (@AndERight α (MyNot $ MyNot β) (@AndELeft (MyAnd α $ MyNot $ MyNot β) γ x))
