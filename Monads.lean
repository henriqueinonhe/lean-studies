class Thenable (F : Type -> Type) where
  andThen {α β : Type} : F α -> (α -> F β) -> F β

inductive Maybe (α : Type) where
  | None
  | Some (value : α)

instance {α : Type} [ToString α] : ToString $ Maybe α where
  toString (wrapped : Maybe α) :=
    match wrapped with
      | Maybe.None => "None"
      | Maybe.Some value => toString value

def Maybe.andThen {α β: Type} (self : Maybe α) (f : α -> Maybe β) : Maybe β :=
  match self with
    | None => Maybe.None
    | Some value => f value

instance : Thenable Maybe where
  andThen := Maybe.andThen

inductive Result (α : Type) where
  | Ok (value : α)
  | Error (message : String)

instance : Thenable Result where
  andThen result f :=
    match result with
      | Result.Ok value => f value
      | Result.Error message => Result.Error message

instance {α : Type} [ToString α] : ToString $ Result α where
  toString result :=
    match result with
      | Result.Error message => message
      | Result.Ok value => toString value

def foo : Result Int := Result.Ok $ 1
def bar : Result Int := Result.Ok $ -1

infixl:99 "~>" => Thenable.andThen

def double (x : Int) : Int :=
  x * 2

#eval bar
  ~> (λx => Result.Ok $ double x)
  ~> (λx => if x == 0 then Result.Error "Cannot divide by zero!" else Result.Ok $ 2 / x)
#eval foo
  ~> (λx => Result.Ok $ double x)
  ~> (λx => if x == 0 then Result.Error "Cannot divide by zero!" else Result.Ok $ 2 / x)
