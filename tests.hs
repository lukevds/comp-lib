import FiniteAutomata
import Data.Tree

{-
(o|f)+
alph=fo
states=1..2
init=1
accept={2}
tf=
1 o = 2
1 f = 2
2 o = 2
2 f = 2
characteristics: simple. translates directly to dfa
-}

{-
language: { foo, foobar, foobarbaz }
characteristics: pretty straight forward. no empty transitions. if a word is rejected the computation tree suddenly stops. computation trees are linear, each node has at most one child.
-}
fooBarBaz :: NFA Char Int
fooBarBaz =
  NFA ['f','o','b','a','r','z']
  [1..10]
  1
  (
    [ ('f', 1, 2)
    , ('o', 2, 3)
    , ('o', 3, 4)
    , ('b', 4, 5)
    , ('a', 5, 6)
    , ('r', 6, 7)
    , ('b', 7, 8)
    , ('a', 8, 9)
    , ('z', 9, 10)
    ],
    []
  )
  [4,7,10]

{-
language: { fo^n : n is even }
characteristics: pretty straight forward. no empty transitions. accepts an infinite number of words. computation trees are linear.
-}
foEven :: NFA Char Int
foEven =
  NFA ['f','o']
  [1..4]
  1
  (
    [ ('f', 1, 2)
    , ('o', 2, 3)
    , ('o', 3, 4)
    , ('o', 4, 3)
    ],
    []
  )
  [4]

{-
language: foo(bar)*
caracteristics: one empty transition, but could be modeled without it. accepts infinite number of words. computation trees are linear.
-}
fooMaybeBarRepeated :: NFA Char Int
fooMaybeBarRepeated =
  NFA ['f','o','b','a','r']
  [1..7]
  1
  (
    [ ('f', 1,  2)
    , ('o', 2,  3)
    , ('o', 3,  4)
    , ('b', 4,  5)
    , ('a', 5,  6)
    , ('r', 6,  7)
    ],
    [ (7, 4) ]
  )
  [4]

{-
language: foo(bar)+
caracteristics: points out a flaw in the current algorithm that tests for acceptance of a word. currently it accepts if there is a LEAF with no characters left in an accept state, but it should be any NODE with such characteristics.
-}
fooBarRepeated :: NFA Char Int
fooBarRepeated =
  NFA ['f','o','b','a','r']
  [1..7]
  1
  (
    [ ('f', 1,  2)
    , ('o', 2,  3)
    , ('o', 3,  4)
    , ('b', 4,  5)
    , ('a', 5,  6)
    , ('r', 6,  7)
    ],
    [ (7,4) ]
  )
  [7]


printComputationTree :: Ord c => Ord s => Show c => Show s => NFA c s -> [c] -> IO ()
printComputationTree nfa word = putStrLn (drawTree (fmap show (nfaComputationTree nfa word)))
