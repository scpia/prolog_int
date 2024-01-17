{-# LANGUAGE TemplateHaskell #-}

module Unification
  ( testUnification
  , -- unify
  ) where


import Data.Maybe
import Base.Type
import Test.QuickCheck
import Subst
import Vars

-- Properties


-- Does  a variable occur in a term?
occurs :: VarName -> Term -> Bool
occurs v t = v `elem` allVars t

-- The disagreement set of a term with itself is empty
prop_1 :: Term -> Bool
prop_1 t = isNothing (ds t t)

-- The disagreement set of two different terms is not empty
prop_2 :: Term -> Term -> Property
prop_2 t1 t2 = isJust (ds t1 t2) ==> t1 /= t2

-- If a variable v occurs in a term t (other than the variable itself),
-- then the unification of v and t should fail due to the occur check
prop_3 :: VarName -> Term -> Property
prop_3 v t = occurs v t && t /= Var v ==> isNothing (unify (Var v) t)

-- If two terms t1 and t2 are unifiable, then the disagreement set of the mgu
-- applied to t1 and t2 is empty
prop_4 :: Term -> Term -> Property
prop_4 t1 t2 =
  let mMgu = unify t1 t2
  in isJust mMgu ==> let mgu = fromJust mMgu
                     in isNothing (ds (apply mgu t1) (apply mgu t2))

-- Run all tests
testUnification :: IO Bool
testUnification = do
  putStrLn "Running Substitution Tests..."

  results <- sequence
    [ quickCheckResult prop_1,
      quickCheckResult prop_2,
      quickCheckResult prop_3,
      quickCheckResult prop_4
    ]

  let overallResult = all isSuccess results

  if overallResult
    then putStrLn "All tests passed!"
    else putStrLn "Some tests failed."

  return overallResult

unify :: Term -> Term -> Maybe Subst
unify t1 t2 = unify' empty t1 t2
  where
    unify' s t1 t2 = case ds (apply s t1) (apply s t2) of Nothing -> Just s
                                                          Just (Var v, t) -> let s' = single v t `compose` s in unify' s' t1 t2
                                                          _ -> Nothing

ds :: Term -> Term -> Maybe (Term, Term)
ds t1 t2       | t1 == t2   = Nothing
ds (Var v) t2               = Just (Var v, t2)
ds t1 (Var v)               = Just (Var v, t1)
ds (Comb f ts) (Comb g ss)  | f /= g || length ts /= length ss = Just (Comb f ts, Comb g ss)
                            | otherwise = case filter isJust (zipWith ds ts ss) of []  -> Nothing
                                                                                   s:_ -> s
