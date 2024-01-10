{-# LANGUAGE InstanceSigs #-}
module Vars
  (
      Vars (allVars),
      freshVars,
      testVars
  )
where


  import Data.List (nub)
  import Base.Type
  testVars :: IO Bool
  testVars = do
    let t = termVars == map allVars testTerms
    let r = ruleVars == map allVars testRules
    let p = progVars == allVars testProg
    let g = goalVars == allVars testGoal
    return (t && p && r && g)
    where
      termVars = [[VarName "A"],[],[VarName "A"],[VarName "Abbot",VarName "B"]]
      testTerms = [Var (VarName "A"), Comb "true" [], Comb "f" [Var (VarName "A"), Comb "true" []], Comb "f" [Comb "1" [], Comb "h" [Comb "g" [Var (VarName "Abbot"), Var (VarName "B")], Comb "[]" []]]]
      ruleVars = [[VarName "X"],[VarName "X"],[VarName "[]",VarName "Ys"],[VarName "X",VarName "Xs",VarName "Ys",VarName "Zs"]]
      testRules = [Rule (Comb "f" [Var (VarName "X"), Comb "true" []]) [], Rule (Comb "f" [Var (VarName "X"), Comb "true" []]) [Comb "g" [Var (VarName "X")], Comb "h" []], Rule (Comb "append" [Var (VarName "[]"), Var (VarName "Ys"), Var (VarName "Ys")]) [], Rule (Comb "append" [Comb "." [Var (VarName "X"), Var (VarName "Xs")], Var (VarName "Ys"), Comb "." [Var (VarName "X"), Var (VarName "Zs")]]) [Comb "append" [Var (VarName "Xs"), Var (VarName "Ys"), Var (VarName "Zs")]]]
      progVars = [VarName "X",VarName "[]",VarName "Ys",VarName "Xs",VarName "Zs"]
      testProg = Prog testRules
      goalVars = [VarName "A",VarName "Abbot",VarName "B"]
      testGoal = Goal testTerms


  class Vars a where
    allVars :: a -> [VarName]

  instance Vars Term where
    allVars :: Term -> [VarName]
    allVars (Var var) = [var]
    allVars (Comb _ ys) = nub (concatMap allVars ys)

  instance Vars Rule where
    allVars :: Rule -> [VarName]
    allVars (Rule term terms) = nub (concatMap allVars (term:terms))

  instance Vars Prog where
    allVars (Prog progs) = nub (concatMap allVars progs)

  instance Vars Goal where
    allVars (Goal goals) = nub (concatMap allVars goals)

  freshVars :: [VarName]
  freshVars = [VarName [x] | x <- ['A'..'Z']] ++ [VarName (nameBase : show index) | index <- [0..], nameBase <- ['A'..'Z']]
