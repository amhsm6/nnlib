module Main where

import Control.Monad
import Control.Monad.State
import qualified Data.Map as M

type Action = StateT Network IO

data Network = Network { nodes :: M.Map NodeIndex Node
                       , ready :: Bool
                       }

data Node = Node { op :: NodeOp
                 , children :: [NodeIndex]
                 , value :: Double
                 , derivative :: Double
                 }

data NodeOp = NodeOp { eval :: [Double] -> Double
                     , grad :: [Double] -> [Double]
                     }

type NodeIndex = Int

compute :: Action a -> IO a
compute m = evalStateT m $ Network M.empty True

getValue :: NodeIndex -> Action Double
getValue i = do
    (Network nodes ready) <- get
    let node = maybe undefined id $ M.lookup i nodes
    if ready then
        pure $ value node
    else do
        value <- eval (op node) <$> mapM getValue (children node)
        put $ Network (M.adjust (\x -> x { value = value }) i nodes) True
        pure value

setOp :: NodeIndex -> NodeOp -> Action ()
setOp i op = get >>= \x -> put $ Network (M.adjust (\x -> x { op = op }) i $ nodes x) False

calculateDerivativesOf :: NodeIndex -> Action ()
calculateDerivativesOf i = init >> go [i]
    where init = get >>= put . M.adjust (\x -> x { derivative = 1 }) i nodes
          go prev = do
              (Network nodes _) <- get
              forM_ prev $ \i -> children $ maybe undefined id $ M.lookup i nodes

getDerivative :: NodeIndex -> Action Double
getDerivative i = get >>= pure . derivative . maybe undefined id . M.lookup i . nodes

node :: NodeOp -> [NodeIndex] -> Action NodeIndex
node op children = do
    (Network nodes ready) <- get
    value <- if ready then eval op <$> mapM getValue children else pure 0
    let new = Node op children value 0
        i = length nodes
    put $ Network (M.insert i new nodes) ready
    pure i

main :: IO ()
main = pure ()
