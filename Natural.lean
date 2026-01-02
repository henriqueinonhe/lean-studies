inductive Natural where
  | Zero : Natural
  | Succ (n : Natural) : Natural
  deriving BEq

inductive Maybe (α : Type) where
  | Some (value : α)
  | None

def Natural.One := Natural.Succ Natural.Zero

theorem OneSuccZero : Natural.One = Natural.Succ Natural.Zero := rfl

def Natural.Two := Natural.Succ Natural.One

def Natural.Three := Natural.Succ Natural.Two

def Natural.sum (n : Natural) (m : Natural) : Natural :=
  match m with
    | Natural.Zero => n
    | Natural.Succ k => Natural.sum (Natural.Succ n) k

-- theorem SumCommutativeZero : ∀ n, Natural.sum n Natural.Zero = Natural.sum Natural.Zero n :=

-- theorem SumAssociative : ∀a b c, Natural.sum (Natural.sum a b) c = Natural.sum a (Natural.sum b c) :=

def Natural.mult (n : Natural) (m : Natural) : Natural :=
  match m with
    | Natural.Zero => Natural.Zero
    | Natural.Succ k =>
      if k == Natural.Zero
        then n
        else Natural.sum (Natural.mult n k) n

def Natural.pow (n : Natural) (m : Natural) : Natural :=
  match m with
    | Natural.Zero => Natural.One
    | Natural.Succ k =>
      if k == Natural.Zero
        then n
        else Natural.mult (Natural.pow n k) n

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
