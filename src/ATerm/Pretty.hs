module ATerm.Pretty
(
  ppATerm
)
where

import Prelude hiding ((<$>))
import Text.PrettyPrint.Leijen
import ATerm.AbstractSyntax

ppATerm :: ATermTable -> Doc
ppATerm t = case getATerm t of
  ShAAppl s [] [] -> text s -- this will already have double quotes in this case
  ShAAppl s is _  -> nestAppl t s is
  ShAList   is _  -> nestList t is
  ShAInt  i    _  -> integer i

ppAList :: ATermTable -> [Int] -> [Doc]
ppAList t is = map (\i -> ppATerm (getATermByIndex1 i t)) is

nestList :: ATermTable -> [Int] -> Doc
nestList t is = children ds
  where
  children []  = lbracket <> rbracket
  children [d] = lbracket <> d <> rbracket
  children _   =
    lbracket                                                          <$>
      indent 2 (align (cat (zipWith (<>) (empty : repeat comma) ds))) <$>
    rbracket
  ds = ppAList t is


nestAppl :: ATermTable -> String -> [Int] -> Doc
nestAppl t s is = children ds
  where
  children []  = text s <> lparen <> rparen
  children [d] = text s <> lparen <> d <> rparen
  children _   = text s <>
    lparen                                                               <$>
      indent 2 (align (cat (zipWith (<>) (text " " : repeat comma) ds))) <$>
    (indent 1 rparen)
  ds = ppAList t is
