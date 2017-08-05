{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
module Text.XML.Hexml.Lens
  ( -- * Nodes
    _children
  , _contents
  , ChildNode(..)
  , TextContents(..)
  -- * Attributes
  , Attributes(..)
  -- * Parsing
  , AsXML(..)
  ) where

import Control.Lens hiding (children)
import Data.ByteString as Strict (ByteString)
import qualified Data.ByteString.Internal as Strict
import Data.ByteString.Lens
import Data.String
import qualified Data.Text as Strict
import qualified Data.Text.Encoding as Strict
import Data.Text.Lens
import qualified Foundation as F
import qualified Foundation.String as F
import qualified Foundation.Array.Internal as F
import Text.XML.Hexml

-- | Fold over the element children
_children :: Fold Node Node
_children = folding children

-- | Fold over all the children (text and element)
_contents :: Fold Node (Either ByteString Node)
_contents = folding contents

-- ---------------------------------------------------------------------------------
-- | Folds for element nodes
class ChildNode s where
  node :: s -> Fold Node Node

-- | A fold for accessing named children nodes
--   This is a more efficient version of
--
-- > node foo = _children . filtered (\n -> name n == foo)
instance ChildNode String where
  node name_ = folding $ flip childrenBy ( name_ ^. strictUtf8)

-- | A fold for accessing named children nodes
--   This is a more efficient version of
--
-- > node foo = _children . filtered (\n -> name n == foo)
instance ChildNode F.String where
  node name_ = folding $ flip childrenBy ( F.toList name_ ^. strictUtf8)

-- | A fold for accessing named children nodes
--   This is a more efficient version of
--
-- > node foo = _children . filtered (\n -> name n == foo)
instance ChildNode ByteString where
  node name_ = folding $ flip childrenBy name_

-- | A fold for accessing named children nodes
--   This is a more efficient version of
--
-- > node foo = _children . filtered (\n -> name n == foo)
instance ChildNode Strict.Text where
  node name_ = folding $ flip childrenBy ( name_ ^. strictTextUtf8 )

-- | A fold for accessing a child node by its index
instance ChildNode Int where
  node n = folding $ take 1 . drop n . children

-- ---------------------------------------------------------------------------------

-- | Fold for accessing the text contents of a node
class TextContents s where
  textContents :: Fold Node s

instance TextContents ByteString where
  textContents = folding contents . _Left

instance TextContents String where
  textContents = textContents @ByteString . from strictUtf8

instance TextContents Strict.Text where
  textContents = textContents . from strictTextUtf8

instance TextContents F.String where
  textContents = textContents . foundation F.UTF8

-- ---------------------------------------------------------------------------------

-- | Optics for accessing attributes
class Attributes s where
  -- | Fold for accessing attributes by name.
  _Attribute  :: s -> Getter Node (Maybe s)
  -- | Name-Indexed fold over the attribute values
  iattributes :: IndexedFold String Node s

instance Attributes ByteString where
  _Attribute n = pre $ to (`attributeBy` n) . folded . to attributeValue
  iattributes  = ifolding (map (\ (Attribute n v) -> (n^.from strictUtf8, v)) . attributes )

instance Attributes String where
  _Attribute n = pre $ to (`attributeBy` (n ^. packedChars)) . folded . to attributeValue . from strictUtf8
  iattributes  = iattributes @ ByteString . unpackedChars

instance Attributes Strict.Text where
  _Attribute n = pre $ to (`attributeBy` (n ^. strictTextUtf8)) . folded . to attributeValue . from strictTextUtf8
  iattributes  = iattributes . packed

instance Attributes F.String where
  _Attribute n = pre $ to (`attributeBy` (F.toList n ^. packedChars)) . folded . to attributeValue . foundation F.UTF8
  iattributes  = iattributes . to fromString

-- ---------------------------------------------------------------------------------

class AsXML s where
  -- | A prism for parsing and unparsing XML.
  --
  -- unparsing is provided by 'outer'.
  --
  -- >>> "<?xml version=\"1.0\"?><foo/>" ^? _XML
  -- Just Node "<?xml version=\"1.0\"?><foo/>"
  --
  -- Nameless nodes are inserted for trees with >1 root.
  --
  -- >>> "<?xml version=\"1.0\"?><foo/>" ^? _XML.to name
  -- Just ""
  --
  -- >>> "<?xml version=\"1.0\"?><foo/>" ^? _XML.node(0::Int)
  -- Just Node "<?xml version=\"1.0\"?>"
  --
  -- >>> "<?xml version=\"1.0\"?><foo/>" ^? _XML.node(1::Int)
  -- Just Node "<foo/>"
  --
  -- If the tree has only 1 root, no nameless nodes are inserted.
  --
  -- >>> "<foo/>" ^? _XML.re(_XML @String)._XML.to name
  -- Just "foo"
  --
  --   The law @x ^? re _XML . _XML == x@ doesn't hold for the nameless nodes
  --   injected by 'parse'.
  --
  -- >>> parse "<foo/>" ^? _Right.to name
  -- Just ""
  -- >>> parse "<foo/>" ^? _Right.re(_XML @String)._XML.to name
  -- Just "foo"

  _XML :: Prism' s Node

instance AsXML ByteString where
  _XML = prism' outer doParse where
    doParse x =
      case parse x of
        Right n -> Just $ case children n of [y] -> y ; _ -> n
        Left  _ -> Nothing

instance AsXML String where
  _XML = strictUtf8 . _XML @ ByteString

instance AsXML Strict.Text where
  _XML = strictTextUtf8 . _XML

-- ---------------------------------------------------------------------------------

strictTextUtf8 :: Iso' Strict.Text Strict.ByteString
strictTextUtf8 = iso Strict.encodeUtf8 Strict.decodeUtf8

strictUtf8 :: Iso' String Strict.ByteString
strictUtf8 = packed . strictTextUtf8

foundation :: F.Encoding -> Fold Strict.ByteString F.String
foundation encoding = to (F.fromBytes encoding . fromByteString) . filtered (hasn't (_2.folded)) . _1
  where
    fromByteString = F.fromForeignPtr . Strict.toForeignPtr

-- Test setup
-- ---------------------------------------------------------------------------------
-- $setup
-- >>> import Test.QuickCheck
-- >>> :set -XTypeApplications
-- >>> :set -XOverloadedStrings
