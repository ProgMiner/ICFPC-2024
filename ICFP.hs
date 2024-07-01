{-# LANGUAGE UndecidableInstances #-}

module ICFP where

import Control.Monad.Except
import Control.Monad.ST.Trans
import Control.Applicative
import Unsafe.Coerce

import Data.Char (chr, ord)
import Data.List (genericTake, genericDrop, union, delete)

import Prelude hiding (showString, showChar)


type Var = Integer

data UnaryOp
    = NegUnaryOp
    | NotUnaryOp
    | StrToIntUnaryOp
    | IntToStrUnaryOp
    deriving (Show, Eq)

data BinaryOp
    = AddBinaryOp
    | SubBinaryOp
    | MulBinaryOp
    | DivBinaryOp
    | RemBinaryOp
    | LtBinaryOp
    | GtBinaryOp
    | EqBinaryOp
    | OrBinaryOp
    | AndBinaryOp
    | CatBinaryOp
    | TakeBinaryOp
    | DropBinaryOp
    deriving (Show, Eq)

data Token
    = BoolToken Bool
    | IntToken Integer
    | StringToken String
    | UnaryToken UnaryOp
    | BinaryToken BinaryOp
    | AppToken
    | IfToken
    | LambdaToken Var
    | VarToken Var
    deriving (Show, Eq)

data Expr
    = BoolExpr Bool
    | IntExpr Integer
    | StringExpr String
    | UnaryExpr UnaryOp Expr
    | BinaryExpr BinaryOp Expr Expr
    | AppExpr Expr Expr
    | IfExpr Expr Expr Expr
    | LambdaExpr Var Expr
    | VarExpr Var
    | ThunkExpr Thunk
    deriving (Show, Eq)

data Thunk deriving (Show, Eq)

newtype Parser m t a = Parser { runParser :: [t] -> m (a, [t]) }


instance Monad m => Functor (Parser m t) where

    fmap = liftM

instance Monad m => Applicative (Parser m t) where

    pure x = Parser $ \toks -> pure (x, toks)
    (<*>) = ap

instance Monad m => Monad (Parser m t) where

    Parser p >>= k =
        let p' toks = do (x, toks') <- p toks
                         runParser (k x) toks'
        in Parser p'

instance MonadError e m => MonadError e (Parser m t) where

    throwError = Parser . const . throwError
    catchError (Parser p) f = Parser
        $ \toks -> catchError (p toks) (\err -> runParser (f err) toks)


parseInt :: MonadError String m => String -> m Integer
parseInt [] = throwError "empty integer"
parseInt xs = pure $ foldl (\s x -> s * base + (x - zeroValue)) 0 $ map (toInteger . ord) xs
    where zeroValue = toInteger (ord '!')
          base = 94

unparseInt :: MonadError String m => Integer -> m String
unparseInt 0 = pure "!"
unparseInt n | n < 0     = throwError "negative number"
             | otherwise = pure $ f n ""
    where f 0 acc = acc
          f n acc = f (n `quot` base) $ chr (zeroValue + (fromInteger $ n `rem` base)) : acc
          zeroValue = ord '!'
          base = 94

parseUnaryOp :: MonadError String m => String -> m UnaryOp
parseUnaryOp "-" = pure NegUnaryOp
parseUnaryOp "!" = pure NotUnaryOp
parseUnaryOp "#" = pure StrToIntUnaryOp
parseUnaryOp "$" = pure IntToStrUnaryOp
parseUnaryOp tok = throwError $ "unsupported unary operator: " ++ tok

parseBinaryOp :: MonadError String m => String -> m BinaryOp
parseBinaryOp "+" = pure AddBinaryOp
parseBinaryOp "-" = pure SubBinaryOp
parseBinaryOp "*" = pure MulBinaryOp
parseBinaryOp "/" = pure DivBinaryOp
parseBinaryOp "%" = pure RemBinaryOp
parseBinaryOp "<" = pure LtBinaryOp
parseBinaryOp ">" = pure GtBinaryOp
parseBinaryOp "=" = pure EqBinaryOp
parseBinaryOp "|" = pure OrBinaryOp
parseBinaryOp "&" = pure AndBinaryOp
parseBinaryOp "." = pure CatBinaryOp
parseBinaryOp "T" = pure TakeBinaryOp
parseBinaryOp "D" = pure DropBinaryOp
parseBinaryOp tok = throwError $ "unsupported binary operator: " ++ tok

parseToken :: MonadError String m => String -> m Token
parseToken [] = throwError "empty token"
parseToken "T" = pure $ BoolToken True
parseToken "F" = pure $ BoolToken False
parseToken ('I':tok) = IntToken <$> parseInt tok
parseToken ('S':tok) = pure $ StringToken tok
parseToken ('U':tok) = UnaryToken <$> parseUnaryOp tok
parseToken "B$" = pure AppToken
parseToken ('B':tok) = BinaryToken <$> parseBinaryOp tok
parseToken "?" = pure IfToken
parseToken ('L':tok) = LambdaToken <$> parseInt tok
parseToken ('v':tok) = VarToken <$> parseInt tok
parseToken tok = throwError $ "unsupported token " ++ tok

parseTokens :: MonadError String m => String -> m [Token]
parseTokens code = mapM parseToken $ words code

tokenParser :: MonadError String m => Parser m Token Token
tokenParser = Parser f where
    f [] = throwError "empty stream"
    f (t:toks) = pure (t, toks)

exprParser' :: MonadError String m => Token -> Parser m Token Expr
exprParser' (BoolToken v) = pure $ BoolExpr v
exprParser' (IntToken v) = pure $ IntExpr v
exprParser' (StringToken v) = pure $ StringExpr v
exprParser' (UnaryToken op) = UnaryExpr op <$> exprParser
exprParser' (BinaryToken op) = BinaryExpr op <$> exprParser <*> exprParser
exprParser' AppToken = AppExpr <$> exprParser <*> exprParser
exprParser' IfToken = IfExpr <$> exprParser <*> exprParser <*> exprParser
exprParser' (LambdaToken x) = LambdaExpr x <$> exprParser
exprParser' (VarToken x) = pure $ VarExpr x

exprParser :: MonadError String m => Parser m Token Expr
exprParser = tokenParser >>= exprParser'

freeVars :: Expr -> [Var]
freeVars (BoolExpr _) = []
freeVars (IntExpr _) = []
freeVars (StringExpr _) = []
freeVars (UnaryExpr _ e) = freeVars e
freeVars (BinaryExpr _ lhs rhs) = freeVars lhs `union` freeVars rhs
freeVars (AppExpr lhs rhs) = freeVars lhs `union` freeVars rhs
freeVars (IfExpr c t f) = freeVars c `union` freeVars t `union` freeVars f
freeVars (LambdaExpr v e) = delete v $ freeVars e
freeVars (VarExpr v) = [v]
freeVars (ThunkExpr _) = []

substVar :: Var -> Expr -> Expr -> Expr
substVar _ _ e@(BoolExpr _) = e
substVar _ _ e@(IntExpr _) = e
substVar _ _ e@(StringExpr _) = e
substVar v x (UnaryExpr op e) = UnaryExpr op $ substVar v x e
substVar v x (BinaryExpr op lhs rhs) = BinaryExpr op (substVar v x lhs) (substVar v x rhs)
substVar v x (AppExpr lhs rhs) = AppExpr (substVar v x lhs) (substVar v x rhs)
substVar v x (IfExpr c t f) = IfExpr (substVar v x c) (substVar v x t) (substVar v x f)
substVar v x (LambdaExpr v' b) | v == v' = LambdaExpr v' b
                               | v' `elem` freeVars x = error "ill-formed substitution"
                               | otherwise = LambdaExpr v' $ substVar v x b

substVar v x (VarExpr v') | v == v'   = x
                          | otherwise = VarExpr v'
substVar _ _ e@(ThunkExpr _) = e

evalUnaryOp :: MonadError String m => UnaryOp -> Expr -> m Expr
evalUnaryOp NegUnaryOp (IntExpr v) = pure $ IntExpr (-v)
evalUnaryOp NotUnaryOp (BoolExpr v) = pure $ BoolExpr $ not v
evalUnaryOp StrToIntUnaryOp (StringExpr v) = IntExpr <$> parseInt v
evalUnaryOp IntToStrUnaryOp (IntExpr v) = StringExpr <$> unparseInt v
evalUnaryOp op e = throwError $ "unsupported operand for unary operator "
                    ++ show op ++ ": " ++ show e

evalBinaryOp :: MonadError String m => BinaryOp -> Expr -> Expr -> m Expr
evalBinaryOp AddBinaryOp  (IntExpr    a) (IntExpr    b) = pure $ IntExpr  $ a + b
evalBinaryOp SubBinaryOp  (IntExpr    a) (IntExpr    b) = pure $ IntExpr  $ a - b
evalBinaryOp MulBinaryOp  (IntExpr    a) (IntExpr    b) = pure $ IntExpr  $ a * b
evalBinaryOp DivBinaryOp  (IntExpr    a) (IntExpr    b) = pure $ IntExpr  $ a `quot` b
evalBinaryOp RemBinaryOp  (IntExpr    a) (IntExpr    b) = pure $ IntExpr  $ a `rem` b
evalBinaryOp LtBinaryOp   (IntExpr    a) (IntExpr    b) = pure $ BoolExpr $ a < b
evalBinaryOp GtBinaryOp   (IntExpr    a) (IntExpr    b) = pure $ BoolExpr $ a > b
evalBinaryOp EqBinaryOp   (BoolExpr   a) (BoolExpr   b) = pure $ BoolExpr $ a == b
evalBinaryOp EqBinaryOp   (IntExpr    a) (IntExpr    b) = pure $ BoolExpr $ a == b
evalBinaryOp EqBinaryOp   (StringExpr a) (StringExpr b) = pure $ BoolExpr $ a == b
evalBinaryOp OrBinaryOp   (BoolExpr   a) (BoolExpr   b) = pure $ BoolExpr $ a || b
evalBinaryOp AndBinaryOp  (BoolExpr   a) (BoolExpr   b) = pure $ BoolExpr $ a && b
evalBinaryOp CatBinaryOp  (StringExpr a) (StringExpr b) = pure $ StringExpr $ a ++ b
evalBinaryOp TakeBinaryOp (IntExpr    n) (StringExpr s) = pure $ StringExpr $ genericTake n s
evalBinaryOp DropBinaryOp (IntExpr    n) (StringExpr s) = pure $ StringExpr $ genericDrop n s
evalBinaryOp op lhs rhs = throwError $ "unsupported operands for binary operator "
                            ++ show op ++ ": " ++ show lhs ++ ", " ++ show rhs

evalApp :: MonadError String m => Expr -> Expr -> STT s m Expr
evalApp x (LambdaExpr v b) = do th <- newSTRef $ Left x
                                eval' $ substVar v (ThunkExpr $ unsafeCoerce th) b

evalApp _ f = throwError $ "unable to apply non-functional value: " ++ show f

evalIf :: MonadError String m => Expr -> Expr -> Expr -> STT s m Expr
evalIf t f (BoolExpr v) = eval' $ if v then t else f
evalIf _ _ c = throwError $ "unsupported condition " ++ show c

evalThunk :: MonadError String m => STRef s (Either Expr Expr) -> STT s m Expr
evalThunk th = do x <- readSTRef th
                  case x of
                      Left x -> do x' <- eval' x
                                   writeSTRef th $ Right x'
                                   pure x'

                      Right x -> pure x

eval' :: MonadError String m => Expr -> STT s m Expr
eval' e@(BoolExpr _) = pure e
eval' e@(IntExpr _) = pure e
eval' e@(StringExpr _) = pure e
eval' (UnaryExpr op e) = eval' e >>= evalUnaryOp op
eval' (BinaryExpr op lhs rhs) = eval' lhs >>= \lhs' -> eval' rhs >>= evalBinaryOp op lhs'
eval' (AppExpr f x) = eval' f >>= evalApp x
eval' (IfExpr c t f) = eval' c >>= evalIf t f
eval' e@(LambdaExpr _ _) = pure e
eval' (ThunkExpr th') = evalThunk $ unsafeCoerce th'
eval' e = throwError $ "unsupported expr: " ++ show e

eval :: MonadError String m => Expr -> m Expr
eval e = runSTT (eval' e)

showString :: MonadError String m => String -> m String
showString = mapM showChar

showChar :: MonadError String m => Char -> m Char
showChar x | x' < 33 || x' > 126 = throwError $ "unknown character with code " ++ show x'
           | otherwise = pure $ table !! (x' - 33)
    where table = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"
          x' = ord x

showValue :: MonadError String m => Expr -> m String
showValue (BoolExpr v) = pure $ show v
showValue (IntExpr v) = pure $ show v
showValue (StringExpr v) = showString v
showValue e = throwError $ "not a value: " ++ show e

toHaskell :: Expr -> ShowS
toHaskell (BoolExpr v) = shows v
toHaskell (IntExpr v) = shows v
toHaskell (StringExpr v) = shows v
toHaskell (UnaryExpr NegUnaryOp e) = ("(- " ++) . toHaskell e . (")" ++)
toHaskell (UnaryExpr NotUnaryOp e) = ("(not " ++) . toHaskell e . (")" ++)
toHaskell (UnaryExpr StrToIntUnaryOp e) = undefined
toHaskell (UnaryExpr IntToStrUnaryOp e) = undefined
toHaskell (BinaryExpr AddBinaryOp lhs rhs) = ("(" ++) . toHaskell lhs . (" + " ++) . toHaskell rhs . (")" ++)
toHaskell (BinaryExpr SubBinaryOp lhs rhs) = ("(" ++) . toHaskell lhs . (" - " ++) . toHaskell rhs . (")" ++)
toHaskell (BinaryExpr MulBinaryOp lhs rhs) = ("(" ++) . toHaskell lhs . (" * " ++) . toHaskell rhs . (")" ++)
toHaskell (BinaryExpr DivBinaryOp lhs rhs) = ("(" ++) . toHaskell lhs . (" `quot` " ++) . toHaskell rhs . (")" ++)
toHaskell (BinaryExpr RemBinaryOp lhs rhs) = ("(" ++) . toHaskell lhs . (" `rem` " ++) . toHaskell rhs . (")" ++)
toHaskell (BinaryExpr LtBinaryOp lhs rhs) = ("(" ++) . toHaskell lhs . (" < " ++) . toHaskell rhs . (")" ++)
toHaskell (BinaryExpr GtBinaryOp lhs rhs) = ("(" ++) . toHaskell lhs . (" > " ++) . toHaskell rhs . (")" ++)
toHaskell (BinaryExpr EqBinaryOp lhs rhs) = ("(" ++) . toHaskell lhs . (" == " ++) . toHaskell rhs . (")" ++)
toHaskell (BinaryExpr OrBinaryOp lhs rhs) = ("(" ++) . toHaskell lhs . (" || " ++) . toHaskell rhs . (")" ++)
toHaskell (BinaryExpr AndBinaryOp lhs rhs) = ("(" ++) . toHaskell lhs . (" && " ++) . toHaskell rhs . (")" ++)
toHaskell (BinaryExpr CatBinaryOp lhs rhs) = ("(" ++) . toHaskell lhs . (" ++ " ++) . toHaskell rhs . (")" ++)
toHaskell (BinaryExpr TakeBinaryOp lhs rhs) = ("(" ++) . toHaskell lhs . (" `take` " ++) . toHaskell rhs . (")" ++)
toHaskell (BinaryExpr DropBinaryOp lhs rhs) = ("(" ++) . toHaskell lhs . (" `drop` " ++) . toHaskell rhs . (")" ++)
toHaskell (AppExpr lhs rhs) = ("(" ++) . toHaskell lhs . (" " ++) . toHaskell rhs . (")" ++)
toHaskell (IfExpr c t f) = ("(if " ++) . toHaskell c . (" then " ++) . toHaskell t . (" else " ++) . toHaskell f . (")" ++)
toHaskell (LambdaExpr v b) = ("(\\v" ++) . shows v . (" -> " ++) . toHaskell b . (")" ++)
toHaskell (VarExpr v) = ("v" ++) . shows v
