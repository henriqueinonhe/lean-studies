class StringOrNat (α : Type) where
  marker : Unit

instance : StringOrNat String where
  marker := Unit.unit

instance : StringOrNat Nat where
  marker := Unit.unit

def strOrNat {α : Type} [StringOrNat α] (val : α) :=
  "Hehe"

#check strOrNat true
