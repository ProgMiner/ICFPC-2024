{-# LANGUAGE TemplateHaskell #-}

module Main where

import System.Environment
import Control.Monad.Trans.Maybe
import Control.Monad.Except
import Control.Monad.State
import Control.Applicative
import Control.Lens

import Data.Maybe (isJust, fromJust)

import qualified Data.Map as Map
import Data.Map(Map)

import qualified Data.Set as Set
import Data.Set(Set)


-- `<`, `>`, `^`, `v`, `+`, `-`, `*`, `/`, `%`, `@`, `=`, `#`, `S`, `A`, `B`
data Op
    = LOp
    | ROp
    | UOp
    | DOp
    | AddOp
    | SubOp
    | MulOp
    | DivOp
    | RemOp
    | WarpOp
    | EqOp
    | NeOp
    | SubmitOp
    deriving (Show, Eq)

data Atom
    = IntAtom Integer
    | OpAtom Op
    deriving (Show, Eq)

data EvalStep
    = NewEvalStep Field
    | WarpEvalStep Int (Map (Int, Int) Atom)
    | SubmitEvalStep Atom

data EvalContext = EvalContext
    { _evalContextCurrentField :: Field
    , _evalContextNextField :: Field
    , _evalContextPlaced :: Set (Int, Int)
    , _evalContextSubmit :: Maybe Atom
    , _evalContextWarp :: Maybe (Int, Map (Int, Int) Atom)
    }

type Field = Map (Int, Int) Atom

makeLenses ''EvalContext

parseAtom :: MonadError String m => (Integer, Integer) -> String -> m Atom
parseAtom _ "<" = pure $ OpAtom LOp
parseAtom _ ">" = pure $ OpAtom ROp
parseAtom _ "^" = pure $ OpAtom UOp
parseAtom _ "v" = pure $ OpAtom DOp
parseAtom _ "+" = pure $ OpAtom AddOp
parseAtom _ "-" = pure $ OpAtom SubOp
parseAtom _ "*" = pure $ OpAtom MulOp
parseAtom _ "/" = pure $ OpAtom DivOp
parseAtom _ "%" = pure $ OpAtom RemOp
parseAtom _ "@" = pure $ OpAtom WarpOp
parseAtom _ "=" = pure $ OpAtom EqOp
parseAtom _ "#" = pure $ OpAtom NeOp
parseAtom _ "S" = pure $ OpAtom SubmitOp
parseAtom (a, _) "A" = pure $ IntAtom a
parseAtom (_, b) "B" = pure $ IntAtom b
parseAtom _ tok | [(x, "")] <- reads tok = pure $ IntAtom x
parseAtom _ tok = throwError $ "unknown atom " ++ tok

parseAtom' :: (MonadError String m, MonadState Field m)
    => (Integer, Integer) -> (Int, Int) -> String -> m ()
parseAtom' _  _  "." = pure ()
parseAtom' ab xy tok = do atom <- parseAtom ab tok
                          modify $ Map.insert xy atom

parseRow' :: (MonadError String m, MonadState Field m)
    => (Integer, Integer) -> (Int, [String]) -> m ()
parseRow' ab (y, row) = mapM_ (\(x, tok) -> parseAtom' ab (x, y) tok) $ zip [1..] row

parseField' :: (MonadError String m, MonadState Field m)
    => (Integer, Integer) -> [[String]] -> m ()
parseField' ab field = mapM_ (parseRow' ab) $ zip [1..] field

parseField :: MonadError String m => (Integer, Integer) -> String -> m Field
parseField ab field =
    let field' = words <$> lines field in
    execStateT (parseField' ab field') Map.empty

renderAtom :: Atom -> String
renderAtom (IntAtom x) = show x
renderAtom (OpAtom LOp) = "<"
renderAtom (OpAtom ROp) = ">"
renderAtom (OpAtom UOp) = "^"
renderAtom (OpAtom DOp) = "v"
renderAtom (OpAtom AddOp) = "+"
renderAtom (OpAtom SubOp) = "-"
renderAtom (OpAtom MulOp) = "*"
renderAtom (OpAtom DivOp) = "/"
renderAtom (OpAtom RemOp) = "%"
renderAtom (OpAtom WarpOp) = "@"
renderAtom (OpAtom EqOp) = "="
renderAtom (OpAtom NeOp) = "#"
renderAtom (OpAtom SubmitOp) = "S"

renderAtom' :: (Int, Int) -> Field -> String
renderAtom' xy field = maybe "." renderAtom $ Map.lookup xy field

renderField' :: (Int, Int) -> (Int, Int) -> Field -> [[String]]
renderField' (x1, y1) (x2, y2) field = [[renderAtom' (x, y) field | x <- [x1..x2]] | y <- [y1..y2]]

renderField :: Field -> ((Int, Int), [[String]])
renderField field | Map.null field = ((0, 0), [])
                  | otherwise =
    let xys = Map.keys field in
    let xs = map fst xys in
    let ys = map snd xys in

    let x1 = minimum xs in
    let y1 = minimum ys in
    let x2 = maximum xs in
    let y2 = maximum ys in

    let xy1 = (x1, y1) in
    let xy2 = (x2, y2) in

    (xy1, renderField' xy1 xy2 field)

renderTable :: [[String]] -> String
renderTable tbl =
    let ws = getZipList $ foldl (liftA2 max) (pure 0) $ map (ZipList . map length) tbl in
    let f w s = take w $ replicate ((w - length s + 1) `div` 2) ' ' ++ s ++ repeat ' ' in
    let tbl' = map (zipWith f ws) tbl in
    unlines $ map unwords $ tbl'

printField :: Int -> Field -> IO ()
printField t field = do
    let ((x, y), tbl) = renderField field
    putStrLn $ "[t=" ++ show t ++ ", x=" ++ show x ++ ", y=" ++ show y ++ "]"
    putStr $ renderTable tbl

oneStepConsume :: MonadState EvalContext m => (Int, Int) -> m (Maybe Atom)
oneStepConsume xy = runMaybeT $ do
    atom <- MaybeT $ uses evalContextCurrentField $ Map.lookup xy

    placed <- uses evalContextPlaced $ Set.member xy
    unless placed $ evalContextNextField %= Map.delete xy

    pure atom

oneStepPlace :: (MonadError String m, MonadState EvalContext m) => (Int, Int) -> Atom -> m ()
oneStepPlace xy atom = do
    placed <- uses evalContextPlaced $ Set.member xy
    evalContextPlaced %= Set.insert xy

    curAtom <- uses evalContextCurrentField $ Map.lookup xy
    case curAtom of
        Just (OpAtom SubmitOp) -> evalContextSubmit .= Just atom
        _ -> pure ()

    if not placed
    then evalContextNextField %= Map.insert xy atom
    else do
        atom' <- uses evalContextNextField $ Map.lookup xy
        when (Just atom /= atom') $ throwError $ "conflicting place at " ++ show xy

handleNothing :: (MonadState EvalContext m) => m (Maybe a) -> m ()
handleNothing m = do st <- get

                     x <- m
                     maybe (put st) (\_ -> pure ()) x

oneStepMove :: (MonadError String m, MonadState EvalContext m) => (Int, Int) -> (Int, Int) -> m ()
oneStepMove src dst = handleNothing $ runMaybeT
    $ (MaybeT $ oneStepConsume src) >>= oneStepPlace dst

oneStepArith :: (MonadError String m, MonadState EvalContext m)
    => (Integer -> Integer -> Integer) -> (Int, Int) -> m ()
oneStepArith f (x, y) = handleNothing $ runMaybeT $ do
    IntAtom a <- MaybeT $ oneStepConsume (x - 1, y)
    IntAtom b <- MaybeT $ oneStepConsume (x, y - 1)

    let c = IntAtom $ f a b
    oneStepPlace (x + 1, y) c
    oneStepPlace (x, y + 1) c

oneStepWarp :: (MonadError String m, MonadState EvalContext m) => (Int, Int) -> m ()
oneStepWarp (x, y) = handleNothing $ runMaybeT $ do
    atom <- MaybeT $ oneStepConsume (x, y - 1)
    
    IntAtom dx <- MaybeT $ oneStepConsume (x - 1, y)
    IntAtom dy <- MaybeT $ oneStepConsume (x + 1, y)
    IntAtom dt <- MaybeT $ oneStepConsume (x, y + 1)

    let xy' = (x - fromInteger dx, y - fromInteger dy)

    curWarp <- use evalContextWarp
    case curWarp of
        Nothing -> evalContextWarp .= Just (fromInteger dt, Map.singleton xy' atom)
        Just (dt', chgs) -> do
            when (fromInteger dt /= dt') $ throwError "conflicting warp time deltas"

            let (oldAtom, chgs') = Map.insertLookupWithKey (\_ _ _ -> atom) xy' atom chgs

            when (oldAtom /= Nothing && oldAtom /= Just atom)
                $ throwError $ "conflicting warp place at " ++ show xy'

            evalContextWarp .= Just (dt', chgs')

oneStepEq :: (MonadError String m, MonadState EvalContext m)
    => (Atom -> Atom -> Bool) -> (Int, Int) -> m ()
oneStepEq p (x, y) = handleNothing $ runMaybeT $ do
    a <- MaybeT $ oneStepConsume (x - 1, y)
    b <- MaybeT $ oneStepConsume (x, y - 1)

    unless (p a b) empty

    oneStepPlace (x, y + 1) a
    oneStepPlace (x + 1, y) b

oneStep'' :: (MonadError String m, MonadState EvalContext m) => (Int, Int) -> Op -> m ()
oneStep'' (x, y) LOp = oneStepMove (x + 1, y) (x - 1, y)
oneStep'' (x, y) ROp = oneStepMove (x - 1, y) (x + 1, y)
oneStep'' (x, y) UOp = oneStepMove (x, y + 1) (x, y - 1)
oneStep'' (x, y) DOp = oneStepMove (x, y - 1) (x, y + 1)
oneStep'' xy AddOp = oneStepArith (+) xy
oneStep'' xy SubOp = oneStepArith (-) xy
oneStep'' xy MulOp = oneStepArith (*) xy
oneStep'' xy DivOp = oneStepArith quot xy
oneStep'' xy RemOp = oneStepArith rem xy
oneStep'' xy WarpOp = oneStepWarp xy
oneStep'' xy EqOp = oneStepEq (==) xy
oneStep'' xy NeOp = oneStepEq (/=) xy
oneStep'' _ SubmitOp = pure ()

oneStep' :: (MonadError String m, MonadState EvalContext m) => (Int, Int) -> Atom -> m ()
oneStep' xy (OpAtom op) = oneStep'' xy op
oneStep' _ _ = pure ()

oneStep :: MonadError String m => Field -> m EvalStep
oneStep field = do
    let m = mapM_ (uncurry oneStep') $ Map.toList field
    ctx <- execStateT m $ EvalContext { _evalContextCurrentField = field
                                      , _evalContextNextField = field
                                      , _evalContextPlaced = Set.empty
                                      , _evalContextSubmit = Nothing
                                      , _evalContextWarp = Nothing
                                      }

    maybe undefined pure $ foldr (<|>) Nothing
        [ SubmitEvalStep <$> ctx^.evalContextSubmit
        , uncurry WarpEvalStep <$> ctx^.evalContextWarp
        , pure $ NewEvalStep $ ctx^.evalContextNextField
        ]

runErrorIO :: (forall m. MonadError String m => m a) -> IO a
runErrorIO (Right x)  = pure x
runErrorIO (Left err) = error err

eval :: [Field] -> IO ()
eval hist = do
    let cur = head hist

    let t = length hist
    printField t cur

    next <- runErrorIO $ oneStep cur

    case next of
        NewEvalStep field -> eval $ field:hist
        WarpEvalStep n chgs -> do
            when (n < 1) $ error "illegal warp"

            let (next:hist') = drop n hist
            let next' = chgs `Map.union` next
            eval $ next':hist'

        SubmitEvalStep atom -> putStrLn $ "Result: " ++ renderAtom atom

main = do
    (paramA:paramB:_) <- (++ [0, 0]) <$> (read <$>) <$> getArgs

    init <- getContents
    init <- runErrorIO $ parseField (paramA, paramB) init

    eval [init]
