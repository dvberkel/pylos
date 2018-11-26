module Configuration

import Data.Vect
import Data.Fin

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

update : (Fin n, Fin n) -> State -> Vect n (Vect n State) -> Vect n (Vect n State)
update x y xs = xs


play : (Fin n, Fin n) -> Square n -> Square n
play (a, b) (Create xs) = let ys = update (a, b) Occupied xs in
     Create ys
