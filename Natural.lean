inductive Natural where
  | Zero : Natural
  | Succ (n : Natural) : Natural
  deriving BEq

inductive Maybe (α : Type) where
  | None
  | Some (value : α)
  deriving Nonempty

instance {α : Type} [ToString α] : ToString (Maybe α) where
  toString (maybe : Maybe α) :=
    match maybe with
      | Maybe.None => "None"
      | Maybe.Some value => toString value

def Maybe.andThen {α β: Type} (self : Maybe α) (f : α -> β) : Maybe β :=
  match self with
    | Maybe.None => Maybe.None
    | Maybe.Some value => Maybe.Some (f value)

def Natural.One := Natural.Succ Natural.Zero

theorem OneSuccZero : Natural.One = Natural.Succ Natural.Zero := rfl

def Natural.Two := Natural.Succ Natural.One

def Natural.Three := Natural.Succ Natural.Two

def Natural.sum (n : Natural) (m : Natural) : Natural :=
  match m with
    | Natural.Zero => n
    | Natural.Succ k => Natural.sum (Natural.Succ n) k

instance : Add Natural where
  add := Natural.sum

def Natural.mult (n : Natural) (m : Natural) : Natural :=
  match m with
    | Natural.Zero => Natural.Zero
    | Natural.Succ k =>
      if k == Natural.Zero
        then n
        else Natural.sum (Natural.mult n k) n

instance : Mul Natural where
  mul := Natural.mult

def Natural.pow (n : Natural) (m : Natural) : Natural :=
  match m with
    | Natural.Zero => Natural.One
    | Natural.Succ k =>
      if k == Natural.Zero
        then n
        else Natural.mult (Natural.pow n k) n

instance : Pow Natural Natural where
  pow := Natural.pow

def Natural.pred (n : Natural) : Natural :=
  match n with
    | Natural.Zero => Natural.Zero
    | Natural.Succ k => k

def Natural.sub (n : Natural) (m : Natural) : Natural :=
  match m with
   | Natural.Zero => n
   | Natural.Succ k => Natural.pred (Natural.sub n k)

def Natural.lt (n : Natural) (m : Natural) : Bool :=
  (Natural.sub m n) != Natural.Zero

def Natural.gt (n : Natural) (m : Natural) : Bool :=
  Natural.lt m n

partial def Natural.div (n : Natural) (m : Natural) : Maybe Natural :=
  if m == Natural.Zero
    then Maybe.None
    else
  if n.lt m
    then Maybe.Some Natural.Zero
    else (Natural.div (Natural.sub n m) m).andThen Natural.Succ

def Natural.toString (self : Natural) : String :=
  match self with
    | Natural.Zero => ""
    | Natural.Succ k => "i".append (Natural.toString k)

instance : ToString Natural where
  toString := Natural.toString

def Natural.remainder (n : Natural) (m : Natural) : Maybe Natural :=
  ((Natural.div n m).andThen (Natural.mult m)).andThen (Natural.sub n)

#eval Natural.remainder Natural.Two Natural.Zero
