{-# LANGUAGE PatternGuards #-}
-- | Functions for working with the ATerms that ROSE produces.
module ATerm.Utilities
( -- * Utility functions
  app             -- (ATermTable -> a) -> ATermTable -> Int -> a
, foldr           -- (ATermTable -> a -> a) -> a -> ATermTable -> a
, foldl           -- (a -> ATermTable -> a) -> a -> ATermTable -> a
, foldl'          -- (a -> ATermTable -> a) -> a -> ATermTable -> a
, foldM           -- (a -> ATermTable -> m a) -> a -> ATermTable -> m a
, mapM            -- (ATermTable -> m b) -> ATermTable -> m [b]
, mapM_           -- (ATermTable -> m b) -> ATermTable -> m ()
, map             -- (ATermTable -> a) -> ATermTable -> [a]
, concatMap       -- (ATermTable -> [a]) -> ATermTable -> [a]
-- * Check Monad
, CheckM
, appM            -- (ATermTable -> a) -> Int -> CheckM log state m a
, currentTerm     -- CheckM log state m ATermTable
, withCurrentTerm -- ATermTable -> CheckM log state m a -> CheckM log state m a
, childrenM       -- CheckM log state m [Int]
, satisfy         -- (ATermTable -> Bool) -> CheckM log st m a -> CheckM log st m (Maybe a)
, inSubtree       -- CheckM log state m a -> CheckM log state m [[a]]
, inSubtree_      -- CheckM log state m a -> CheckM log state m ()
, everywhere      -- CheckM log state m a -> CheckM log state m [a]
, everywhere_     -- CheckM log state m a -> CheckM log state m ()
-- * Extractions
, extractString   -- ATermTable -> Maybe String
, extractInteger  -- ATermTable -> Maybe Integer
, extractFileInfo -- ATermTable -> Maybe (String, Integer, Integer)
, isNamed         -- String -> ATermTable -> Bool
, showATerm       -- ATermTable -> String
, children        -- ATermTable -> [Int]
-- * Read and write
, readATerm
, writeSharedATerm
-- * Misc
, getATermFromTable
) where

import ATerm.ReadWrite
import ATerm.SimpPretty
import ATerm.AbstractSyntax

import Control.Monad.Trans.RWS.Strict
import Control.Monad ( liftM )
import qualified Control.Monad as M
import Data.Monoid

import qualified Data.List as L ( foldl' )
import Prelude hiding (foldr, foldl, map, mapM_, mapM, concatMap)
import qualified Prelude as P

---------------------------------------------------------------------
-- Working with ATerms (eg., ATermTable)
---------------------------------------------------------------------

-- | Turns a normal function of ATermTable into a function that
-- works on the hash value instead.
app :: (ATermTable -> a) -> ATermTable -> Int -> a
app f t i = f (getATermByIndex1 i t)

-- | Standard foldr, but for ATermTables
foldr :: (ATermTable -> a -> a) -> a -> ATermTable -> a
foldr k z at = go at
  where
  go t = t `k` P.foldr k' z (children t)
  k' i acc = app (foldr k acc) at i

-- | Standard foldl, but for ATermTables
foldl :: (a -> ATermTable -> a) -> a -> ATermTable -> a
foldl k z at = go at
  where
  go t = k (P.foldl k' z (children t)) t
  k' acc i = app (foldl k acc) at i

-- | Standard foldl', but for ATermTables
foldl' :: (a -> ATermTable -> a) -> a -> ATermTable -> a
foldl' k z at = go at
  where
  go t = let z' = L.foldl' k' z (children t) in z' `seq` k z' t
  k' acc i = app (foldl' k acc) at i

-- | Standard foldM, but for ATermTables
foldM :: (Monad m) => (a -> ATermTable -> m a) -> a -> ATermTable -> m a
foldM k z at = go at
  where
  go t = k z t >>= \a -> M.foldM k' a (children t)
  k' acc i = app (foldM k acc) at i

-- | Standard mapM, but for ATermTables
mapM :: (Monad m) => (ATermTable -> m b) -> ATermTable -> m [b]
mapM f = foldM action []
  where
  action acc x = do
    x' <- f x
    return (x' : acc)

-- | Standard mapM_, but for ATermTables
mapM_ :: (Monad m) => (ATermTable -> m b) -> ATermTable -> m ()
mapM_ f = foldM action ()
  where
  action _ x = do
    _ <- f x
    return ()

-- | Standard map, but for ATermTables
map :: (ATermTable -> a) -> ATermTable -> [a]
map f at = foldr ((:) . f) [] at

-- | Standard concatMap, but for ATermTables
concatMap :: (ATermTable -> [a]) -> ATermTable -> [a]
concatMap f at = foldr ((++) . f) [] at

---------------------------------------------------------------------
-- Checker Monad
---------------------------------------------------------------------

-- | The checker monad. For now the environment is the current ATerm, in the
-- future we may also store the path from the root to the current ATerm, so use
-- 'currentTerm' instead of ask to get the current ATerm
type CheckM log state m a = RWST ATermTable log state m a

-- | Like 'app' but lifts the result into the CheckM monad
appM :: (Monoid log, Monad m) => (ATermTable -> a) -> Int -> CheckM log state m a
appM f i = do
  t <- currentTerm
  return (f (getATermByIndex1 i t))

-- | Use this instead of 'ask' so that we can refactor
-- the environment later without impacting existing code.
currentTerm :: (Monoid log, Monad m) => CheckM log state m ATermTable
currentTerm = ask

-- | Use this instead of 'local' so that we can refactor
-- the environment later without impacting existing code.
withCurrentTerm :: (Monoid log, Monad m)
                => ATermTable -> CheckM log state m a -> CheckM log state m a
withCurrentTerm t = local (const t)

-- | Return the hashes of the current term's children into the monad
childrenM :: (Monad m, Monoid log) => CheckM log state m [Int]
childrenM = children `liftM` currentTerm

-- | Use this when the current node must satisfy a specific property.
-- Note: Using Maybe here is a bit of a hack. Refactor to support MonadPlus style guards?
satisfy :: (Monad m, Monoid log)
        => (ATermTable -> Bool) -> CheckM log st m a -> CheckM log st m (Maybe a)
satisfy p m = do
  t <- currentTerm
  case p t of
    True  -> Just `liftM` m
    False -> return Nothing

-- | Applies a traversal in a subtree of the current ATerm. Differs from 'everywhere'
-- in that it does not apply the traversal to the current term (only its children and
-- their children).
inSubtree :: (Monad m, Monoid log) => CheckM log state m a -> CheckM log state m [[a]]
inSubtree c = do
  ks <- childrenM
  at <- currentTerm
  let kids = P.map (`getATermByIndex1` at) ks
  P.mapM (`withCurrentTerm` (everywhere c)) kids

-- Like 'inSubtree_' but throws away the result.
inSubtree_ :: (Monad m, Monoid log) => CheckM log state m a -> CheckM log state m ()
inSubtree_ c = do
  ks <- childrenM
  at <- currentTerm
  let kids = P.map (`getATermByIndex1` at) ks
  P.mapM_ (`withCurrentTerm` (everywhere c)) kids

-- | Applies a traversal over the tree defined by the current node (including root node).
everywhere :: (Monad m, Monoid log) => CheckM log state m a -> CheckM log state m [a]
everywhere c = currentTerm >>= mapM (\at -> withCurrentTerm at c)

-- | Like 'everywhere' but throws away the result.
everywhere_ :: (Monad m, Monoid log) => CheckM log state m a -> CheckM log state m ()
everywhere_ c = currentTerm >>= mapM_ (\at -> withCurrentTerm at c)

{-
-- Left here as a lesson: This doesn't seem to do the intended thing
everywhere' :: (Monad m, Monoid log) => CheckM log state m a -> CheckM log state m [a]
everywhere' c = currentTerm >>= mapM (const c)
-}

---------------------------------------------------------------------
-- Turn ATerms into normal values, we make assumptions
-- about the format that ROSE emits ATerms in.
---------------------------------------------------------------------

-- | Extracts the label of Application nodes
extractString :: ATermTable -> Maybe String
extractString at =
  case getATerm at of
  ShAAppl s _ _ -> Just s
  _             -> Nothing

-- | Extract the integer of Int nodes
extractInteger :: ATermTable -> Maybe Integer
extractInteger at =
  case getATerm at of
  ShAInt i _ -> Just i
  _          -> Nothing

-- | Extracts the filename, line number, and colunm number from
-- file_info nodes.
extractFileInfo :: ATermTable -> Maybe (String, Integer, Integer)
extractFileInfo at =
  case getATerm at of
  ShAAppl s [fp,line,col] _
    | s == "file_info"
    , Just f <- extractString  (getATermByIndex1 fp   at)
    , Just l <- extractInteger (getATermByIndex1 line at)
    , Just c <- extractInteger (getATermByIndex1 col  at) -> Just (f,l,c)
  _                                                       -> Nothing

-- | Equality test on the label of an Application node
isNamed :: String -> ATermTable -> Bool
isNamed name t =
  case getATerm t of
  ShAAppl s _ _ -> s == name
  _             -> False 

-- | It's not acually pretty, but that's not our fault.
showATerm :: ATermTable -> String
showATerm = render . writeSharedATermSDoc

-- | This pattern comes up in most traversals.  Simply return the stable names
-- so we don't break sharing.
children :: ATermTable -> [Int]
children t =
  case getATerm t of
  ShAAppl _ l _ -> l
  ShAList   l _ -> l
  ShAInt  _   _ -> []

---------------------------------------------------------------------
-- Misc
---------------------------------------------------------------------
getATermFromTable :: ATermTable -> Int -> ATermTable
getATermFromTable = flip getATermByIndex1
