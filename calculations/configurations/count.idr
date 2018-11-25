module Configuration

import Data.Vect

data State = UnOccupied
           | Occupied

data Square : Nat -> Type where
     Create : Vect n (Vect n State) -> Square n

times : (n : Nat) -> a -> Vect n a
times Z x = []
times (S k) x = x :: times k x

emptySquare : (n : Nat) -> Vect n (Vect n State)
emptySquare n = times n (times n UnOccupied)

empty : (n : Nat) -> Square n
empty n = Create (emptySquare n)

