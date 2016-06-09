and' :: Bool -> Bool -> Bool
and' True True = True
and' False _ = False

-- Evaluateness should be:
--
-- Evaluatedness
--  "and'"
--  [ ( [ WildcardPattern, WildcardPattern]
--    , [ [ConstructorPattern "True" [], ConstructorPattern "False" []]
--      , []
--      ]
--    )
--  , ( [ConstructorPattern "True", WildcardPattern]
--    , [ [ConstructorPattern "True" []]
--      , [ConstructorPattern "True [], ConstructorPattern "False" []]
--      ]
--  ]
--
--
--  The constraints are:
--  For the first clause:
--   and' x    y where x is bottom will diverge
--   and' True y where y is bottom will diverge
--  For the second clause: nothing
--
--
