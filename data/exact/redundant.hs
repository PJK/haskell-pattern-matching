module Redundant where

-- Figure out a way to get the base data constructors
-- We may have to rename all our internal representations of names because of possible overlaps,
-- but I really don't want to have to go and look through import lists.
data MyBool
    = True
    | False

func :: MyBool -> Int
func False = 1
func False = 2
