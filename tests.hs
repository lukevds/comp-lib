import FiniteAutomata
import Data.Tree
import Data.Maybe


--                     id      nfa      accept         reject
type TestInput  c s = (String, NFA c s, [[c]],         [[c]])
type TestOutput c s = (String, NFA c s, [([c], Bool)], [([c], Bool)])


testNfaWordsAccept :: (Ord c, Ord s) => TestInput c s -> TestOutput c s
testNfaWordsAccept (name, nfa, shouldAccept, shouldReject) =
  (
    name,
    nfa,
    [(word, isWordAccepted nfa word) | word <- shouldAccept],
    [(word, isWordAccepted nfa word) | word <- shouldReject]
  )

printWordAcceptTests :: (Ord c, Ord s) => [TestInput c s] -> IO ()
printWordAcceptTests tests = sequence_ output
  where
    results = [(test, testNfaWordsAccept test) | test <- tests]
    testAccept = \(_, x) -> x
    testReject = not . testAccept
    output = [putStrLn
              ("nfa: " <> name <>
               " acceptAll: " <> (show (all testAccept accept)) <>
               " rejectAll: " <> (show (all testReject reject)))
             | (_, (name, _, accept, reject)) <- results]


printComputationTree :: (Ord c, Ord s, Show c, Show s) => NFA c s -> [c] -> IO ()
printComputationTree nfa word = putStrLn (drawTree (fmap show (nfaComputationTree nfa word)))


{-
language: { foo, foobar, foobarbaz }
characteristics: pretty straight forward. no empty transitions. if a word is rejected the computation tree suddenly stops. computation trees are linear, each node has at most one child.
-}
fooBarBaz :: NFA Char Int
fooBarBaz =
  fromJust $ makeNfa ['f','o','b','a','r','z']
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

fooBarBazAcceptTests :: TestInput Char Int
fooBarBazAcceptTests =
  (
    "foo|foobar|foobarbaz",
    fooBarBaz,
    [ ['f','o','o']
    , ['f','o','o','b','a','r']
    , ['f','o','o','b','a','r','b','a','z']
    ],
    [ []
    , ['f','o','o','o']
    , ['f','f','o','o']
    , ['f','f','o','o','b','a','r','b','a','z']
    , ['f','o','o','b','a']
    , ['f','o','o','b','a','z']
    , ['f','o','o','b','a','z','b','a','r']
    ]
  )

{-
language: { fo^n : n is even }
characteristics: pretty straight forward. no empty transitions. accepts an infinite number of words. computation trees are linear.
-}
foEven :: NFA Char Int
foEven =
  fromJust $ makeNfa ['f','o']
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

foEvenAcceptTests :: TestInput Char Int
foEvenAcceptTests =
  (
    "{fo^n : n is even}",
    foEven,
    [ ['f','o','o']
    , ['f','o','o','o','o']
    , ['f','o','o','o','o','o','o']
    ],
    [ []
    , ['f','o']
    , ['f','f','o','o']
    , ['f','o','o','o']
    , ['o','o']
    ]
  )


{-
language: foo(bar)*
caracteristics: one empty transition, but could be modeled without it. accepts infinite number of words. computation trees are linear.
-}
fooMaybeBarRepeated :: NFA Char Int
fooMaybeBarRepeated =
  fromJust $ makeNfa ['f','o','b','a','r']
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

fooMaybeBarAcceptTests :: TestInput Char Int
fooMaybeBarAcceptTests =
  (
    "foo(bar)*",
    fooMaybeBarRepeated,
    [ ['f','o','o']
    , ['f','o','o','b','a','r']
    , ['f','o','o','b','a','r','b','a','r']
    , ['f','o','o','b','a','r','b','a','r','b','a','r']
    ],
    [ []
    , ['f','f','o','o','b','a','r']
    , ['f','o','o','b','a','z']
    , ['f','o','b','a','r']
    ]
  )

{-
language: foo(bar)+
caracteristics: one empty transition. accepts infinite number of words. computation trees are linear.
-}
fooBarRepeated :: NFA Char Int
fooBarRepeated =
  fromJust $ makeNfa ['f','o','b','a','r']
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

fooBarAcceptTests :: TestInput Char Int
fooBarAcceptTests =
  (
    "foo(bar)+",
    fooBarRepeated,
    [ ['f','o','o','b','a','r']
    , ['f','o','o','b','a','r','b','a','r']
    , ['f','o','o','b','a','r','b','a','r','b','a','r']
    ],
    [ []
    , ['f','f','o','o','b','a','r']
    , ['f','o','o']
    , ['f','o','b','a','r']
    , ['f','o','o','b','a','r','b','a','z']
    , ['f','o','o','b','a','z']
    ]
  )


{-
language: (o|f)+
characteristics: simple, translates directly to dfa
-}
letterOorFoneOrMoreTimes :: NFA Char Int
letterOorFoneOrMoreTimes =
  fromJust $ makeNfa ['o','f']
  [1..2]
  1
  (
    [ ('o', 1, 2)
    , ('f', 1, 2)
    , ('o', 2, 2)
    , ('f', 2, 2)
    ],
    []
  )
  [2]

letterOorFAcceptTests :: TestInput Char Int
letterOorFAcceptTests =
  (
    "(f|o)+",
    letterOorFoneOrMoreTimes,
    [ ['o','f']
    , ['f','o']
    , ['f','f']
    , ['o','o']
    , ['f']
    , ['o']
    , ['f','f','o','o']
    , ['f','f','o','o','o']
    , ['f','f','f','o','o']
    ],
    [ []
    , ['f','o','o','b']
    ]
  )

concatenated1 = (mapNfaStatesToInt (nfaConcatenate fooBarBaz fooBarBaz))

concatenated1AcceptTests :: TestInput Char Int
concatenated1AcceptTests =
  let
    (_,_,xaccept,xreject) = fooBarBazAcceptTests
  in
    (
      "{ww | w \\in foo|foobar|foobarbaz}",
      concatenated1,
      [w++w' | w <- xaccept, w' <- xaccept],
      xaccept ++ xreject
    )

united1 = mapNfaStatesToInt (nfaUnite fooBarBaz foEven)

united1AcceptTests :: TestInput Char Int
united1AcceptTests =
  let
    (_,_,xaccept,xreject) = fooBarBazAcceptTests
    (_,_,yaccept,yreject) = foEvenAcceptTests
  in
    (
      "{(foo|foobar|foobarbaz)|fo^n : n is even}",
      united1,
      xaccept ++ yaccept,
      xreject ++ yreject
    )


main :: IO ()
main = printWordAcceptTests
  [ fooBarBazAcceptTests
  , foEvenAcceptTests
  , fooMaybeBarAcceptTests
  , fooBarAcceptTests
  , letterOorFAcceptTests
  , concatenated1AcceptTests
  , united1AcceptTests ]
