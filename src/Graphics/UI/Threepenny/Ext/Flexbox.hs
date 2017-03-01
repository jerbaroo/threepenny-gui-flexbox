module Graphics.UI.Threepenny.Ext.Flexbox where

import           Clay.Display                (Display)
import qualified Clay.Display                as CD
import           Clay.Flexbox                (AlignContentValue,
                                              AlignItemsValue, FlexDirection,
                                              FlexWrap, JustifyContentValue)
import qualified Clay.Flexbox                as CF
import           Clay.Property               (unKeys, unPlain, unValue)
import           Clay.Stylesheet             (Rule (Property), runS)
import           Data.Text                   (unpack)
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core

-- This library was inspired by the guide to Flexbox at:
-- https://css-tricks.com/snippets/css/a-guide-to-flexbox

class ToStyle a where
  toStyle :: a -> [(String, String)]
  
-- |Convert a Clay Property to style.
instance ToStyle Rule where
  toStyle (Property k v) =
    [(unpack $ unPlain $ unKeys k, unpack $ unPlain $ unValue v)]

-- |Properties for the parent.
data Parent = Parent {
    pDisplay        :: Display
  , pFlexDirection  :: FlexDirection
  , pFlexWrap       :: FlexWrap
  , pJustifyContent :: JustifyContentValue
  , pAlignItems     :: AlignItemsValue
  , pAlignContent   :: AlignContentValue
}

instance ToStyle Parent where
  toStyle p = concatMap toStyle $ concatMap runS [
        CD.display        $ pDisplay        p
      , CF.flexDirection  $ pFlexDirection  p
      , CF.flexWrap       $ pFlexWrap       p
      , CF.justifyContent $ pJustifyContent p
      , CF.alignItems     $ pAlignItems     p
      , CF.alignContent   $ pAlignContent   p
    ]

-- |Default properties for the parent.
parent :: Parent
parent = Parent {
    pDisplay        = CD.flex
  , pFlexDirection  = CF.row
  , pFlexWrap       = CF.nowrap
  , pJustifyContent = CF.flexStart
  , pAlignItems     = CF.stretch
  , pAlignContent   = CF.stretch
}
