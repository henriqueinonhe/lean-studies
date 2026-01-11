inductive Formula where
  | Primitive : String -> Formula
  | And : Formula -> Formula -> Formula
  | Or : Formula -> Formula -> Formula
  | Implies : Formula -> Formula -> Formula
  | Equivalent : Formula -> Formula -> Formula
  | Not : Formula -> Formula

def Formula.toString (f : Formula) :=
  match f with
    | Formula.Primitive prop => prop
    | Formula.And left right => s!"({toString left} ∧ {toString right})"
    | Formula.Or left right => s!"({toString left} ∨ {toString right})"
    | Formula.Implies antecedent consequent => s!"({toString antecedent} -> {toString consequent})"
    | Formula.Equivalent left right => s!"{toString left} <-> {toString right}"
    | Formula.Not prop => s!"¬ {toString prop}"

instance : ToString Formula where
  toString := Formula.toString

inductive Maybe.{u} (α : Type u) where
  | None
  | Some : α -> Maybe α

def Maybe.andThen.{u} {α : Type u} (value : Maybe α) (f : α -> Maybe α) : Maybe α :=
  match value with
    | Maybe.None => Maybe.None
    | Maybe.Some x =>  f x

infixl:99 "~>" => Maybe.andThen

instance {α : Type u} [ToString α] : ToString $ Maybe α where
  toString (value : Maybe α) :=
    match value with
      | Maybe.None => "None"
      | Maybe.Some x => ToString.toString x

def AndELeft (f : Formula) : Maybe Formula :=
  match f with
    | Formula.And l _ => Maybe.Some l
    | _ => Maybe.None

def AndERight (f : Formula) : Maybe Formula :=
  match f with
    | Formula.And _ r => Maybe.Some r
    | _ => Maybe.None

open Formula

def P := Primitive "P"
def Q := Primitive "Q"

#eval (AndELeft (And P Q)) ~> AndERight
