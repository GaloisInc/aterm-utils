{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternGuards #-}
module ATerm.Generics where

import ATerm.Unshared hiding (fromATerm)
import GHC.Generics
import Control.Applicative
import Control.Monad.State

------------------------------------------------------------------------
-- Generic data type serialization
------------------------------------------------------------------------

class GToATerm f where
  gToATerm :: f a -> ATerm

instance GToATerm a => GToATerm (D1 c a) where
  gToATerm (M1 x) = gToATerm x

instance (GToATerm f, GToATerm g) => GToATerm (f :+: g) where
  gToATerm (L1 x) = gToATerm x
  gToATerm (R1 x) = gToATerm x

instance (Constructor c, GToATerms a) => GToATerm (C1 c a) where
  gToATerm m1 = AAppl (conName m1) (gToATerms (unM1 m1) []) []


------------------------------------------------------------------------
-- Generic constructor serialization
------------------------------------------------------------------------

class                                  GToATerms f         where gToATerms          :: f a -> [ATerm] -> [ATerm]
instance GToATerms f                => GToATerms (S1 i f)  where gToATerms (M1 x)    = gToATerms x
instance (GToATerms f, GToATerms g) => GToATerms (f :*: g) where gToATerms (f :*: g) = gToATerms f . gToATerms g
instance ToATerm a                  => GToATerms (Rec0 a)  where gToATerms (K1 x)    = (toATerm x:)
instance                               GToATerms U1        where gToATerms U1        = id

------------------------------------------------------------------------
-- Serialization
------------------------------------------------------------------------

class ToATerm a where
  toATerm :: a -> ATerm
  default toATerm :: (Generic a, GToATerm (Rep a)) => a -> ATerm
  toATerm x = gToATerm (from x)

  toATermList :: [a] -> ATerm
  default toATermList :: Generic a => [a] -> ATerm
  toATermList = listToATerm

-- Automatically derived instances
instance                           ToATerm Bool
instance                           ToATerm Float
instance                           ToATerm Double
instance                           ToATerm ()
instance ToATerm a              => ToATerm (Maybe a)
instance (ToATerm a, ToATerm b) => ToATerm (Either a b)

instance (ToATerm a, ToATerm b) => ToATerm (a,b)   where toATerm     = tupleToATerm
instance                           ToATerm Char    where toATerm     = showToATerm
                                                         toATermList = stringToATerm
instance                           ToATerm Int     where toATerm     = integralToATerm
instance                           ToATerm Integer where toATerm     = integralToATerm
                                                         toATermList = listToATerm
instance ToATerm a              => ToATerm [a]     where toATerm     = toATermList

instance (ToATerm a, ToATerm b, ToATerm c) => ToATerm (a,b,c)   where toATerm     = tripleToATerm

-- Base type implementations
integralToATerm :: Integral a => a -> ATerm
integralToATerm x = AInt (toInteger x) []

showToATerm :: Show a => a -> ATerm
showToATerm x = AAppl (show x) [] []

listToATerm :: ToATerm a => [a] -> ATerm
listToATerm xs = AList (map toATerm xs) []

stringToATerm :: String -> ATerm
stringToATerm s = AAppl (show s) [] []

tupleToATerm :: (ToATerm a, ToATerm b) => (a,b) -> ATerm
tupleToATerm (a,b) = AAppl [] [toATerm a, toATerm b] []

tripleToATerm :: (ToATerm a, ToATerm b, ToATerm c) => (a,b,c) -> ATerm
tripleToATerm (a,b,c) = AAppl [] [toATerm a, toATerm b, toATerm c] []

------------------------------------------------------------------------
-- Deserialization
------------------------------------------------------------------------

class FromATerm a where
  fromATerm :: ATerm -> Maybe a

  default fromATerm :: (Generic a, GFromATerm (Rep a)) => ATerm -> Maybe a
  fromATerm a = to <$> gFromATerm a

  fromATermList :: ATerm -> Maybe [a]

  default fromATermList :: ATerm -> Maybe [a]
  fromATermList = atermToList

-- Automatically derived instances
instance FromATerm                                  ()
instance FromATerm                                  Bool
instance FromATerm                                  Float
instance FromATerm                                  Double
instance (FromATerm a, FromATerm b)              => FromATerm (Either a b)
instance FromATerm a                             => FromATerm (Maybe a)

instance (FromATerm a, FromATerm b)              => FromATerm (a,b)     where fromATerm = atermToTuple
instance (FromATerm a, FromATerm b, FromATerm c) => FromATerm (a, b, c) where fromATerm = atermToTriple

instance                FromATerm Int     where fromATerm     = atermToIntegral
instance                FromATerm Integer where fromATerm     = atermToIntegral
instance                FromATerm Char    where fromATerm     = atermToRead
                                                fromATermList = atermToString
instance FromATerm a => FromATerm [a]     where fromATerm     = fromATermList

-- Base type implementations
atermToIntegral :: Integral a => ATerm -> Maybe a
atermToIntegral (AInt x _) = Just (fromIntegral x)
atermToIntegral _          = Nothing

atermToRead :: Read a => ATerm -> Maybe a
atermToRead (AAppl x [] _) | [(z,"")] <- reads x = Just z
atermToRead _                                    = Nothing

atermToString :: ATerm -> Maybe String
atermToString (AAppl ('"':x) [] _) | null x        = Nothing
                                   | last x == '"' = Just (init x)
atermToString _                                    = Nothing

atermToList :: FromATerm a => ATerm -> Maybe [a]
atermToList (AList as _) = mapM fromATerm as
atermToList _            = Nothing

atermToTuple :: (FromATerm a, FromATerm b) => ATerm -> Maybe (a,b)
atermToTuple (AAppl "" [a,b] []) = do
  a' <- fromATerm a
  b' <- fromATerm b
  return (a',b')
atermToTuple _                   = Nothing

atermToTriple :: (FromATerm a, FromATerm b, FromATerm c) => ATerm -> Maybe (a,b,c)
atermToTriple (AAppl "" [a,b,c] []) = do
  a' <- fromATerm a
  b' <- fromATerm b
  c' <- fromATerm c
  return (a',b',c')
atermToTriple _                     = Nothing
------------------------------------------------------------------------
-- Generic data type deserialization
------------------------------------------------------------------------

class GFromATerm f where
  gFromATerm :: ATerm -> Maybe (f a)

instance GFromATerm a => GFromATerm (D1 c a) where
  gFromATerm a = M1 <$> gFromATerm a

instance (GFromATerm f, GFromATerm g) => GFromATerm (f :+: g) where
  gFromATerm a = L1 <$> gFromATerm a -- try to deserialize as the left side
             <|> R1 <$> gFromATerm a -- fail over to deserializing on the right

instance (Constructor c, GFromATerms a) => GFromATerm (C1 c a) where
  gFromATerm (AAppl str xs _) =
    -- Lambda used to get a monomorphic binding
    -- conName does not evaluate its argument
    (\result@(~(Just x)) -> if conName x == str then result else Nothing)
    (M1 <$> gFromATerms' xs)

  gFromATerm _ = Nothing

------------------------------------------------------------------------
-- Generic constructor deserialization
------------------------------------------------------------------------

-- | Convert all the 'ATerm' elements into the requested structure
gFromATerms' :: GFromATerms f => [ATerm] -> Maybe (f a)
gFromATerms' = evalStateT $ do
  res <- gFromATerms
  []  <- get -- check that all aterms are consumed
  return res

-- | Convert the next 'ATerm' to the next needed field type
next :: FromATerm a => StateT [ATerm] Maybe a
next = do
  x:xs <- get -- pattern failure happens in Maybe
  put xs
  lift (fromATerm x)

class                                      GFromATerms f         where gFromATerms :: StateT [ATerm] Maybe (f a)
instance GFromATerms f                  => GFromATerms (S1 i f)  where gFromATerms = M1    <$> gFromATerms
instance (GFromATerms f, GFromATerms g) => GFromATerms (f :*: g) where gFromATerms = (:*:) <$> gFromATerms <*> gFromATerms
instance FromATerm a                    => GFromATerms (Rec0 a)  where gFromATerms = K1    <$> next
instance                                   GFromATerms U1        where gFromATerms = pure U1

-- example:   fromATerm (toATerm ('a', True)) :: Maybe (Char, Bool)
