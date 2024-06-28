{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Monad.Except
import Control.Monad.Writer
import Control.Applicative

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
    | AppBinaryOp
    deriving (Show, Eq)

data Token
    = BoolToken Bool
    | IntToken Integer
    | StringToken String
    | UnaryToken UnaryOp
    | BinaryToken BinaryOp
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
    | IfExpr Expr Expr Expr
    | LambdaExpr Var Expr
    | VarExpr Var
    deriving (Show, Eq)

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
parseBinaryOp "$" = pure AppBinaryOp
parseBinaryOp tok = throwError $ "unsupported binary operator: " ++ tok

parseToken :: MonadError String m => String -> m Token
parseToken [] = throwError "empty token"
parseToken "T" = pure $ BoolToken True
parseToken "F" = pure $ BoolToken False
parseToken ('I':tok) = IntToken <$> parseInt tok
parseToken ('S':tok) = pure $ StringToken tok
parseToken ('U':tok) = UnaryToken <$> parseUnaryOp tok
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
exprParser' IfToken = IfExpr <$> exprParser <*> exprParser <*> exprParser
exprParser' (LambdaToken x) = LambdaExpr x <$> exprParser
exprParser' (VarToken x) = pure $ VarExpr x

exprParser :: MonadError String m => Parser m Token Expr
exprParser = tokenParser >>= exprParser'

isValue :: Expr -> Bool
isValue (BoolExpr _) = True
isValue (IntExpr _) = True
isValue (StringExpr _) = True
isValue _ = False

freeVars :: Expr -> [Var]
freeVars (BoolExpr _) = []
freeVars (IntExpr _) = []
freeVars (StringExpr _) = []
freeVars (UnaryExpr _ e) = freeVars e
freeVars (BinaryExpr _ lhs rhs) = freeVars lhs `union` freeVars rhs
freeVars (IfExpr c t f) = freeVars c `union` freeVars t `union` freeVars f
freeVars (LambdaExpr v e) = delete v $ freeVars e
freeVars (VarExpr v) = [v]

freshVar :: [Var] -> Var
freshVar xs = maximum xs + 100

substVar :: MonadError String m => Var -> Expr -> Expr -> m Expr
substVar _ _ e@(BoolExpr _) = pure e
substVar _ _ e@(IntExpr _) = pure e
substVar _ _ e@(StringExpr _) = pure e
substVar v x (UnaryExpr op e) = UnaryExpr op <$> substVar v x e
substVar v x (BinaryExpr op lhs rhs) = BinaryExpr op <$> substVar v x lhs <*> substVar v x rhs
substVar v x (IfExpr c t f) = IfExpr <$> substVar v x c <*> substVar v x t <*> substVar v x f
substVar v x (LambdaExpr v' b) | v == v' = pure $ LambdaExpr v' b
                               | v' `elem` fv'x =
    let v'' = freshVar $ fv'x ++ freeVars b in
    LambdaExpr v'' <$> (substVar v' (VarExpr v'') b >>= substVar v x)
                               | otherwise = LambdaExpr v' <$> substVar v x b
    where fv'x = freeVars x
substVar v x (VarExpr v') | v == v'   = pure x
                          | otherwise = pure $ VarExpr v'

evalUnaryOp :: MonadError String m => UnaryOp -> Expr -> Maybe (m Expr)
evalUnaryOp NegUnaryOp (IntExpr v) = Just $ pure $ IntExpr (-v)
evalUnaryOp NotUnaryOp (BoolExpr v) = Just $ pure $ BoolExpr $ not v
evalUnaryOp StrToIntUnaryOp (StringExpr v) = Just $ IntExpr <$> parseInt v
evalUnaryOp IntToStrUnaryOp (IntExpr v) = Just $ StringExpr <$> unparseInt v
evalUnaryOp op e | isValue e = Just $ throwError $ "unsupported operand for unary operator "
                                        ++ show op ++ ": " ++ show e
                 | otherwise = (UnaryExpr op <$>) <$> oneStep e

evalBinaryOp' :: MonadError String m => BinaryOp -> Expr -> Expr -> Maybe (m Expr)
evalBinaryOp' AddBinaryOp  (IntExpr    a) (IntExpr    b) = Just $ pure $ IntExpr  $ a + b
evalBinaryOp' SubBinaryOp  (IntExpr    a) (IntExpr    b) = Just $ pure $ IntExpr  $ a - b
evalBinaryOp' MulBinaryOp  (IntExpr    a) (IntExpr    b) = Just $ pure $ IntExpr  $ a * b
evalBinaryOp' DivBinaryOp  (IntExpr    a) (IntExpr    b) = Just $ pure $ IntExpr  $ a `quot` b
evalBinaryOp' RemBinaryOp  (IntExpr    a) (IntExpr    b) = Just $ pure $ IntExpr  $ a `rem` b
evalBinaryOp' LtBinaryOp   (IntExpr    a) (IntExpr    b) = Just $ pure $ BoolExpr $ a < b
evalBinaryOp' GtBinaryOp   (IntExpr    a) (IntExpr    b) = Just $ pure $ BoolExpr $ a > b
evalBinaryOp' EqBinaryOp   (BoolExpr   a) (BoolExpr   b) = Just $ pure $ BoolExpr $ a == b
evalBinaryOp' EqBinaryOp   (IntExpr    a) (IntExpr    b) = Just $ pure $ BoolExpr $ a == b
evalBinaryOp' EqBinaryOp   (StringExpr a) (StringExpr b) = Just $ pure $ BoolExpr $ a == b
evalBinaryOp' OrBinaryOp   (BoolExpr   a) (BoolExpr   b) = Just $ pure $ BoolExpr $ a || b
evalBinaryOp' AndBinaryOp  (BoolExpr   a) (BoolExpr   b) = Just $ pure $ BoolExpr $ a && b
evalBinaryOp' CatBinaryOp  (StringExpr a) (StringExpr b) = Just $ pure $ StringExpr $ a ++ b
evalBinaryOp' TakeBinaryOp (IntExpr    n) (StringExpr s) = Just $ pure $ StringExpr $ genericTake n s
evalBinaryOp' DropBinaryOp (IntExpr    n) (StringExpr s) = Just $ pure $ StringExpr $ genericDrop n s
evalBinaryOp' op lhs rhs | isValue'lhs && isValue rhs = Just
    $ throwError $ "unsupported operands for binary operator " ++ show op ++ ": "
        ++ show lhs ++ ", " ++ show rhs
                         | isValue'lhs = (BinaryExpr op lhs <$>) <$> oneStep rhs
                         | otherwise   = ((\lhs' -> BinaryExpr op lhs' rhs) <$>) <$> oneStep lhs
    where isValue'lhs = isValue lhs

evalBinaryOp :: MonadError String m => BinaryOp -> Expr -> Expr -> Maybe (m Expr)
evalBinaryOp AppBinaryOp (LambdaExpr v b) rhs = Just $ substVar v rhs b
evalBinaryOp AppBinaryOp lhs rhs | isValue lhs = Just
    $ throwError $ "unable to apply non-functional value: " ++ show lhs
evalBinaryOp AppBinaryOp lhs rhs = ((\lhs' -> BinaryExpr AppBinaryOp lhs' rhs) <$>) <$> oneStep lhs
evalBinaryOp op lhs rhs = evalBinaryOp' op lhs rhs

oneStep :: MonadError String m => Expr -> Maybe (m Expr)
oneStep (BoolExpr _) = Nothing
oneStep (IntExpr _) = Nothing
oneStep (StringExpr _) = Nothing
oneStep (UnaryExpr op e) = evalUnaryOp op e
oneStep (BinaryExpr op lhs rhs) = evalBinaryOp op lhs rhs
oneStep (IfExpr (BoolExpr v) t f) = Just $ pure $ if v then t else f
oneStep (IfExpr c t f) | isValue c = Just $ throwError $ "unsupported condition " ++ show c
                       | otherwise = ((\c' -> IfExpr c' t f) <$>) <$> oneStep c
oneStep e = Just $ throwError $ "unsupported expr " ++ show e

eval' :: (MonadError String m, MonadWriter [Expr] m) => Expr -> m Expr
eval' e = case oneStep e of
             Just res -> tell [e] >> res >>= eval'
             Nothing -> pure e

evalWriter :: Functor m => WriterT w m a -> m a
evalWriter = (fst <$>) . runWriterT

eval :: MonadError String m => Expr -> m Expr
eval = evalWriter . eval'

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

runErrorIO :: (forall m. MonadError String m => m a) -> IO a
runErrorIO (Right x)  = pure x
runErrorIO (Left err) = error err

evalParser :: MonadError String m => Parser m t a -> [t] -> m a
evalParser p toks = do (x, toks') <- runParser p toks
                       when (not $ null toks') $ throwError "not fully parsed"
                       pure x

main = do code <- getContents

          toks <- runErrorIO $ parseTokens code
          putStrLn "Parsed tokens: "
          mapM_ print toks
          putStrLn ""

          expr <- runErrorIO $ evalParser exprParser toks
          putStrLn $ "Parsed expr: " ++ show expr
          putStrLn ""

          (res, steps) <- runErrorIO $ runWriterT $ eval' expr

          putStrLn "Evaluation result:"
          runErrorIO (showValue res) >>= putStrLn
          putStrLn ""

          putStrLn "Steps:"
          mapM_ print steps
