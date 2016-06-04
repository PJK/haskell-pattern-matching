module SimpleVar where

data MyTrinary = A | B | C
fun :: MyTrinary -> MyTrinary -> MyTrinary
fun x A = A
fun B B = A

