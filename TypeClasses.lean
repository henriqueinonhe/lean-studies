class Stringifiable (α : Type) where
  stringify : α -> String

inductive Foo where
  | create

inductive Bar where
  | create

instance : Stringifiable Foo where
  stringify := λ_ => "FOO"

instance : Stringifiable Bar where
  stringify := λ_ => "BAR"

open Stringifiable

#eval stringify (Foo.create)
#eval stringify (Bar.create)
