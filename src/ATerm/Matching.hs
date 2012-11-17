{-# LANGUAGE PatternGuards #-}
module ATerm.Matching
( -- * Types
  Binding(..)
-- * Combinators
, exactlyS
, exactlyI
, exactlyL
, exactlyA
, exactlyNamed
, contains
, containsL
, containsA
, containsChildren
, bindT
, bindL
, bindI
, bindS
, bindA
) where

import ATerm.AbstractSyntax
import qualified ATerm.Utilities as U
import Control.Monad -- For MonadPlus

-- | Binding type gives you back the parts of the ATerm that
-- that match.
data Binding = BoundTerm Int     -- ^ The index into the ATermTable of the matching ATerm
             | BoundList [Int]   -- ^ The list of indexes into the ATermTable that match
             | BoundInt  Integer -- ^ The matching Integer
             | BoundStr  String  -- ^ The matching String
             | BoundAppl Int     -- ^ The index into the ATermTable for the ShAAppl term
  deriving (Eq, Read, Show, Ord)

-- | Turns anything with an Eq instance into a
-- matcher by using (==). No binding is generated.
exactly :: (MonadPlus m, Eq a) => a -> a -> m ()
exactly a b = guard (a == b) >> return ()

-- * ATermMatcher Combinators

---------------------------------------------------------------------
-- ** Exact matchers, no binding generated when they match
---------------------------------------------------------------------

-- | Matches exactly the string 's' within the ATerm
exactlyS :: MonadPlus m => String -> ATermTable -> m ()
exactlyS s t = case getATerm t of
  ShAAppl s' [] _ -> exactly s s'
  _               -> mzero

-- | Matches exactly the integer 'i' within the ATerm
exactlyI :: MonadPlus m => Integer -> ATermTable -> m ()
exactlyI i t = case getATerm t of
  ShAInt i' _ -> exactly i i'
  _           -> mzero

-- | Matches exactly the list 'xs' within the ATerm
exactlyL :: MonadPlus m => [ATermTable -> m a] -> ATermTable -> m [a]
exactlyL ms t = case getATerm t of
  ShAList ls _ -> do
    guard (length ms == length ls)
    sequence (zipWith (\m i -> m (getATermByIndex1 i t)) ms ls)
  _ -> mzero

-- | Matches the string exactly
exactlyA :: MonadPlus m => String -> [ATermTable -> m a] -> ATermTable -> m [a]
exactlyA s ms t = case getATerm t of
  ShAAppl s' ls _ -> do
    exactly s s'
    guard (length ms == length ls)
    sequence (zipWith (\m i -> U.app m t i) ms ls)
  _ -> mzero

-- | Looks for an Appl with name 's' and any children
exactlyNamed :: MonadPlus m => String -> ATermTable -> m ()
exactlyNamed s t = case getATerm t of
  ShAAppl s' _ _ -> exactly s s'
  _              -> mzero

---------------------------------------------------------------------
-- ** Partial matchers, ie., they just specify part of the structure
---------------------------------------------------------------------

containsChildren :: Monad m
                 => m (ATermTable -> b) -> m Int -> ATermTable -> m b
containsChildren ms is t = do
  i <- is
  m <- ms
  return (m (getATermByIndex1 i t))

-- | Matches a partial specification against the children. The
-- matching is maximised so that if the pattern occurs more than
-- once it is matched each time it appears.
contains :: [ATermTable -> a] -> ATermTable -> [a]
contains ms t = case getATerm t of
  ShAAppl _ ls _ -> containsChildren ms ls t
  ShAList   ls _ -> containsChildren ms ls t
  _              -> mzero

-- | Matches a partial specification of a sub aterm List. The
-- matching is maximised so that if the pattern occurs more than
-- once it is matched each time it appears.
containsL :: [ATermTable -> a] -> ATermTable -> [a]
containsL ms t = case getATerm t of
  ShAList ls _ -> containsChildren ms ls t
  _            -> mzero

-- | Matches a partial specification of an Appl. The matching is
-- maximised so that if the pattern occurs more than once it is match
-- each time it appears.
containsA :: String -> [ATermTable -> a] -> ATermTable -> [a]
containsA s ams t = case getATerm t of
  ShAAppl s' ls _ -> do
    exactly s s'
    containsChildren ams ls t
  _               -> mzero

---------------------------------------------------------------------
-- ** Wildcard matchers, these create bindings when they match
---------------------------------------------------------------------

-- | Matches any ATerm and generates a binding to that term
bindT :: MonadPlus m => ATermTable -> m Binding
bindT t = return (BoundTerm (getTopIndex t))
  
-- | Matches any list and generates a binding to that list
bindL :: MonadPlus m => ATermTable -> m Binding
bindL t = case getATerm t of
  ShAList ls _ -> return (BoundList ls)
  _            -> mzero

-- | Matches any integer and generates a binding to that integer
bindI :: MonadPlus m => ATermTable -> m Binding
bindI t = case getATerm t of
  ShAInt i _ -> return (BoundInt i)
  _          -> mzero

-- | Matches any string and generates a binding to that string
-- Strings have the form (Appl somestring [] []).
-- Not to be confused with matching the the string part of an Appl.
bindS :: MonadPlus m => ATermTable -> m Binding
bindS t = case getATerm t of
  ShAAppl s [] [] -> return (BoundStr s)
  _               -> mzero

-- | Matches any Appl and generates a binding to that Appl
bindA :: MonadPlus m => ATermTable -> m Binding
bindA t = case getATerm t of
  ShAAppl _ _ _ -> return (BoundAppl (getTopIndex t))
  _             -> mzero
