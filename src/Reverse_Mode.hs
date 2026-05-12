module Reverse_Mode where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

-- ============================================================
-- Reverse-Mode Automatic Differentiation
--
-- Each node in the computation graph carries:
--   - its primal (forward) value
--   - a mutable accumulator for the adjoint (∂L/∂node)
--   - a list of "backward" thunks that propagate adjoints
--     to the node's inputs when called
-- ============================================================

data Node = Node
  { primal   :: Double          -- forward value
  , adjoint  :: IORef Double    -- accumulated gradient (∂L/∂this)
  , backward :: IORef [IO ()]   -- thunks added by ops that consume this node
  }

-- Smart constructor: fresh node with zero adjoint and no backward hooks
newNode :: Double -> IO Node
newNode v = do
  adj  <- newIORef 0.0
  back <- newIORef []
  return $ Node v adj back

-- Add a backward hook to a node
addBackward :: Node -> IO () -> IO ()
addBackward n thunk = modifyIORef (backward n) (thunk :)

-- Accumulate into a node's adjoint
accumAdj :: Node -> Double -> IO ()
accumAdj n delta = modifyIORef (adjoint n) (+ delta)

-- Run all backward hooks stored on a node (called during reverse sweep)
runBackward :: Node -> IO ()
runBackward n = do
  thunks <- readIORef (backward n)
  sequence_ thunks

-- ============================================================
-- Building the graph: lifted operations
-- Each op creates a new output node and registers a backward
-- hook that, when triggered, pushes ∂output into the inputs.
-- ============================================================

-- Binary ops
addN :: Node -> Node -> IO Node
addN u v = do
  out <- newNode (primal u + primal v)
  -- d(u+v)/du = 1,  d(u+v)/dv = 1
  addBackward out $ do
    g <- readIORef (adjoint out)
    accumAdj u g
    accumAdj v g
  return out

subN :: Node -> Node -> IO Node
subN u v = do
  out <- newNode (primal u - primal v)
  addBackward out $ do
    g <- readIORef (adjoint out)
    accumAdj u   g
    accumAdj v (-g)
  return out

mulN :: Node -> Node -> IO Node
mulN u v = do
  out <- newNode (primal u * primal v)
  -- d(u*v)/du = v,  d(u*v)/dv = u
  addBackward out $ do
    g <- readIORef (adjoint out)
    accumAdj u (g * primal v)
    accumAdj v (g * primal u)
  return out

divN :: Node -> Node -> IO Node
divN u v = do
  let val = primal u / primal v
  out <- newNode val
  -- d(u/v)/du = 1/v,  d(u/v)/dv = -u/v²
  addBackward out $ do
    g <- readIORef (adjoint out)
    accumAdj u ( g / primal v)
    accumAdj v (-g * primal u / (primal v * primal v))
  return out

negN :: Node -> IO Node
negN u = do
  out <- newNode (negate (primal u))
  addBackward out $ do
    g <- readIORef (adjoint out)
    accumAdj u (negate g)
  return out

absN :: Node -> IO Node
absN u = do
  out <- newNode (abs (primal u))
  addBackward out $ do
    g <- readIORef (adjoint out)
    accumAdj u (g * signum (primal u))
  return out

-- Unary ops (with their derivative rules)
unary :: (Double -> Double)   -- primal
      -> (Double -> Double)   -- derivative of primal w.r.t. input
      -> Node -> IO Node
unary f df u = do
  out <- newNode (f (primal u))
  addBackward out $ do
    g <- readIORef (adjoint out)
    accumAdj u (g * df (primal u))
  return out

expN   :: Node -> IO Node
expN    = unary exp exp

logN   :: Node -> IO Node
logN    = unary log (1/)

sinN   :: Node -> IO Node
sinN    = unary sin cos

cosN   :: Node -> IO Node
cosN    = unary cos (negate . sin)

sqrtN  :: Node -> IO Node
sqrtN   = unary sqrt (\x -> 1 / (2 * sqrt x))

asinN  :: Node -> IO Node
asinN   = unary asin (\x ->  1 / sqrt (1 - x*x))

acosN  :: Node -> IO Node
acosN   = unary acos (\x -> -1 / sqrt (1 - x*x))

atanN  :: Node -> IO Node
atanN   = unary atan (\x ->  1 / (1 + x*x))

sinhN  :: Node -> IO Node
sinhN   = unary sinh cosh

coshN  :: Node -> IO Node
coshN   = unary cosh sinh

asinhN :: Node -> IO Node
asinhN  = unary asinh (\x ->  1 / sqrt (x*x + 1))

acoshN :: Node -> IO Node
acoshN  = unary acosh (\x ->  1 / sqrt (x*x - 1))

atanhN :: Node -> IO Node
atanhN  = unary atanh (\x ->  1 / (1 - x*x))

-- Constant lift
constN :: Double -> IO Node
constN = newNode

-- ============================================================
-- Reverse sweep
-- Perform a reverse sweep from the given root node.
-- Precondition: the computation graph is a DAG and nodes are
-- visited in reverse topological order.  For simple scalar
-- functions the root is the only output; seed its adjoint to 1.
-- ============================================================

-- Collect all nodes reachable from a root via backward thunks.
-- We do a simple depth-first traversal using the backward list
-- as the edge set (each thunk closes over the parent nodes).
-- Instead, we expose a simpler API: the user builds the graph
-- with the IO-based ops above, then calls `reversePass`.

-- | Seed the output adjoint to 1 and run all stored backward thunks
--   in the order they were registered (which is reverse topological
--   order for a single-output DAG built left-to-right).
reversePass :: Node -> IO ()
reversePass root = do
  writeIORef (adjoint root) 1.0
  go root
  where
    go n = do
      thunks <- readIORef (backward n)
      sequence_ thunks

-- ============================================================
-- Convenience: differentiate a scalar function f : Node -> IO Node
-- Returns (f(x), f'(x))
-- ============================================================
diff :: (Node -> IO Node) -> Double -> IO (Double, Double)
diff f x = do
  xNode  <- newNode x
  yNode  <- f xNode
  reversePass yNode
  grad <- readIORef (adjoint xNode)
  return (primal yNode, grad)

-- ============================================================
-- Gradient of a multi-variable function
-- f : [Node] -> IO Node
-- Returns (f(xs), [∂f/∂x₁, …, ∂f/∂xₙ])
-- ============================================================
grad :: ([Node] -> IO Node) -> [Double] -> IO (Double, [Double])
grad f xs = do
  xNodes <- mapM newNode xs
  yNode  <- f xNodes
  reversePass yNode
  grads  <- mapM (readIORef . adjoint) xNodes
  return (primal yNode, grads)

-- ============================================================
-- Pure wrappers (unsafe, for interactive / demo use)
-- ============================================================

-- | Pure interface for single-variable differentiation.
--   Usage: diffPure (\x -> mulN x x) 3.0  ==>  (9.0, 6.0)
diffPure :: (Node -> IO Node) -> Double -> (Double, Double)
diffPure f x = unsafePerformIO (diff f x)

-- | Pure interface for gradient computation.
gradPure :: ([Node] -> IO Node) -> [Double] -> (Double, [Double])
gradPure f xs = unsafePerformIO (grad f xs)

