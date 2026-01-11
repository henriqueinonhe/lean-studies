inductive Foo (n : Nat) where
  | mk

def extract {n : Nat} (_ : Foo n) : Nat :=
  n

def zero := @Foo.mk 0

def succ {n : Nat} (_ : Foo n) : Foo (n + 1) :=
  @Foo.mk (n + 1)

def bar {x : Type} (foo : x) :=
  foo

#eval extract $ @Foo.mk 2
#eval extract zero
#eval extract $ succ $ @Foo.mk 2
