module Kip.AST where

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

data Ty a =
    TyString { annTy :: a }
  | Arr      { annTy :: a , dom :: Ty a, img :: Ty a }
  | TyInd    { annTy :: a , indName :: String }
  deriving (Show, Eq, Ord)

data Exp a =
    Var    { annExp :: a , varName :: String }
  | App    { annExp :: a , fn :: Exp a , args :: [Exp a] }
  | StrLit { annExp :: a , lit :: String }
  | Let    { annExp :: a , varName :: String , body :: Exp a }
  deriving (Show, Eq)

type Arg = (String, Ty Case)
type Ctor = (String, [(String, Ty Case)])

data Stmt =
    Defn String (Ty Case) (Exp Case)
  | Function String [Arg] (Ty Case) (Exp Case)
  | NewType String [Ctor]
  | Print (Exp Case)
  | ExpStmt (Exp Case)
  deriving (Show, Eq)

isMultiWord :: String -> Bool
isMultiWord s = length (words s) /= 1

prettyExp :: Exp a -> String
prettyExp (Var _ name) | isMultiWord name = "(" <> name <> ")"
                       | otherwise        = name
prettyExp (StrLit _ s) = show s
