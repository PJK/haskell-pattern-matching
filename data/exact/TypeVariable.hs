module TypeVariable where

data MyTrinary = A | B | C
data Container a = ContainerC a | Nope

unpack :: Container a -> a
unpack (ContainerC x) = x
unpack (ContainerC x) = x