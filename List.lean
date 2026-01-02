inductive MyList (α : Type) where
  | Nil
  | Cons  (value : α) (list : MyList α)

def MyList.size {α : Type} (list : MyList α) : Int :=
  match list with
    | Nil => 0
    | Cons _ xs => (MyList.size xs) + 1

def MyList.isEmpty {α : Type} (list : MyList α) : Bool :=
  list.size == 0

def MyList.prepend {α : Type} (self : MyList α) (element : α) : MyList α :=
  MyList.Cons element self

def MyList.append {α : Type} (self : MyList α) (element : α) : MyList α :=
  match self with
    | MyList.Nil => MyList.Cons element MyList.Nil
    | MyList.Cons first tail => MyList.Cons first (tail.append element)

def MyList.map {α β : Type} (self : MyList α) (f : α -> β) : MyList β :=
  match self with
    | MyList.Nil => MyList.Nil
    | MyList.Cons first tail => MyList.Cons (f first) (tail.map f)

def MyList.some {α : Type} (self : MyList α) (f : α -> Bool) : Bool :=
  match self with
    | MyList.Nil => false
    | MyList.Cons first tail => (f first) || tail.some f

def MyList.every {α : Type} (self : MyList α) (f : α -> Bool) : Bool :=
  match self with
    | MyList.Nil => true
    | MyList.Cons first tail => (f first) && tail.every f

def MyList.reduce {α β : Type} (self : MyList α) (f : β -> α -> β) (init : β) : β :=
  match self with
    | MyList.Nil => init
    | MyList.Cons first tail => tail.reduce f (f init first)

def MyList.reverse {α : Type} (self : MyList α) : MyList α :=
  match self with
    | MyList.Nil => MyList.Nil
    | MyList.Cons first tail => MyList.append (tail.reverse) first

def MyList.isNil {α : Type} (self : MyList α) : Bool :=
  match self with
    | MyList.Nil => true
    | _ => false

def MyList.first? {α : Type} (self : MyList α) : Option α :=
  match self with
    | MyList.Nil => Option.none
    | MyList.Cons first _ => first

def MyList.last? {α : Type} (self : MyList α) : Option α :=
  match self with
    | MyList.Nil => Option.none
    | MyList.Cons first tail =>
      if tail.isNil
        then first
        else tail.last?


-- def MyList.at {α : Type} (self : MyList α) (index : Nat) :=
--   match self with
--     | MyList.Nil => MyList.Nil
--     | MyList.Cons first tail =>
--       match index with
--         | 0 => first
--         | Nat.succ k => first



def list := MyList.Cons 1 (MyList.Cons 2 (MyList.Cons 3 MyList.Nil))

#eval (MyList.Nil : MyList Int).first?
