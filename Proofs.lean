
theorem t1 (p : Prop) (q : Prop) : p → q → p :=
  fun hp : p =>
  fun _ : q =>
  hp
