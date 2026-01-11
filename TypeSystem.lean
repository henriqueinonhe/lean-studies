class RTTI (α : Type) where
  typeName : String

inductive Foo

inductive Bar

instance : RTTI Foo where
  typeName := "Foo"

instance : RTTI Bar where
  typeName := "Bar"

inductive Choice (α : Type) (β : Type) where
  | left (value : α)
  | right (value : β)

def duba (α : Type) [RTTI α] (β : Type) (γ : Type) (first : β) (second : γ) : Choice β γ :=
  if RTTI.typeName α == "Bar"
    then Choice.left first
    else Choice.right second

#eval duba Foo Nat Bool 1 true
