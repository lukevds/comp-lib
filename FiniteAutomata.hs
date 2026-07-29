module FiniteAutomata where
import Data.Tree
import Data.Monoid


type NFATF c s = ([(c, s, s)], [(s, s)])

data NFA c s = NFA
  { nfaAlphabet :: [c]
  , nfaStates :: [s]
  , nfaInitialState :: s
  , nfaTransFunc :: NFATF c s
  , nfaAcceptStates :: [s] }


reachableByChar :: Ord c => Ord s => NFATF c s -> c -> s -> [[s]]
reachableByChar nfaTf char s = go nfaTf char [s]
  where
    go nfaTf                 char []      = []
    go (charTrans, empTrans) char current = case next of
      [] -> []
      sts -> sts : (go (charTrans, empTrans) char sts)
      where
        next = [nst | (c, st, nst) <- charTrans, c == char && elem st current]

treePaths :: Tree a -> [[a]]
treePaths Node { rootLabel=r, subForest=[] } = [[r]]
treePaths Node { rootLabel=r, subForest=s  } = [r : x | nxt <- s, x <- (treePaths nxt)]


nfaComputationTree :: Ord c => Ord s => NFA c s -> [c] -> Tree ([c], s)
nfaComputationTree nfa word = unfoldTree go (word, initSt)
  where
    -- go :: ([c], s) -> (([c], s), [([c], s)])
    go x@([],     st) =
      (x,
       [([], nst) | (st', nst) <- empTrans, st' == st])
    go x@((c:cs), st) =
      (x,
       [(cs, nst) | (c', st', nst) <- charTrans, c' == c && st' == st] ++
       [((c:cs), nst) | (st', nst) <- empTrans, st' == st])
    initSt = nfaInitialState nfa
    (charTrans, empTrans) = nfaTransFunc nfa

withoutCycles :: Ord c => Ord s => Tree ([c], s) -> Tree ([c], s)
withoutCycles tree = go [] tree
  where
    go _   (Node l             [])        = Node l []
    go sts (Node l@(chars, st) subForest) =
      if elem l sts then
        Node l []
      else
        Node l [go (if chars' == chars then (l:sts) else [])
                (Node (chars', st') sf) | (Node (chars', st') sf) <- subForest]

treeLeaves :: Tree a -> [a]
treeLeaves tree = foldTree (\x xs -> if null xs then [x] else (concat xs)) tree

isWordAccepted :: Ord c => Ord s => NFA c s -> [c] -> Bool
isWordAccepted nfa word =
  getAny (foldMap foldFn (withoutCycles (nfaComputationTree nfa word)))
  where
    -- foldFn :: ([c], s) -> Any
    foldFn ([], st) = Any (st `elem` acceptSts)
    foldFn _        = Any False
    acceptSts = nfaAcceptStates nfa

{-
todo:
- nfas examples for testing
- tests
- concat
- alternative
- star
- dfa
- nfa to dfa

functor instances:
alphabet
states
so it's easier to rename alph and states
maybe functor instance for words? you transform input or output? or both?

monoid instances:
concat
alternative
star
why not?
-}
