module Kip.AST where

type Name = String

data Case =
    Nom -- ^ nominative case (yalın hal)
  | Acc -- ^ accusative case (-i hali)
  | Dat -- ^ dative case (-e hali)
  | Loc -- ^ locative case (-de hali)
  | Abl -- ^ ablative case (-den hali)
  | Gen -- ^ genitive case (-in eki)
  | Ins -- ^ instrumental case (ile, -le)
  | Cond -- ^ conditional case (-se, şart kipi)
  deriving (Show, Eq, Ord)

data Ty =
    TyString
  | Arr Ty Ty
  | Mu Name Ty
  | TyVar Name
  deriving (Show, Eq, Ord)

type CasedTy = (Ty, Case)
type Arg = (String, CasedTy)

data Exp =
    Var Name
  | App Exp [Exp]
  | Abs Arg Ty Exp
  | StrLit String
  -- | Let Name Ty Exp
  deriving (Show, Eq, Ord)

data Stmt =
    Defn Name Ty Exp
  | Function Name [(Name, CasedTy)] Exp
  | NewType Name [(Name, [(Name, CasedTy)])]
  | Print Exp
  | ExpStmt Exp
  deriving (Show, Eq, Ord)

isMultiWord :: String -> Bool
isMultiWord s = length (words s) /= 1

prettyExp :: Exp -> String
prettyExp (Var name) | isMultiWord name = "(" <> name <> ")"
                     | otherwise        = name
prettyExp (StrLit s) = show s
