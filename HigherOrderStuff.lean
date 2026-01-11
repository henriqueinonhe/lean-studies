universe u

def doTwice {α : Type u} (f : α -> α) (x : α) : α :=
  f $ f x

def withRepetition {α : Type u} (f : α -> α) (n : Nat) : (α -> α) :=
  match n with
    | Nat.zero => id
    | Nat.succ k => fun x => f $ (withRepetition f k) x

def doThrice {α : Type u} (f : α -> α) (x : α) : α :=
  withRepetition f 3 x

#eval doThrice (fun (x : Nat) => x * 2) 4
