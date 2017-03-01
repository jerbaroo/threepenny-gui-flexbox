module Graphics.UI.Threepenny.Ext.Flexbox (
  -- Core functions.
  ChildProps (..), ParentProps (..), defaultParentProps, defaultChildProps,
  flexbox,

  -- Helper functions.
  column, row
  ) where

import qualified Clay.Common                 as CC
import           Clay.Display                (Display)
import qualified Clay.Display                as CD
import           Clay.Flexbox                (AlignContentValue,
                                              AlignItemsValue, AlignSelfValue,
                                              FlexDirection, FlexWrap,
                                              JustifyContentValue)
import qualified Clay.Flexbox                as CF
import           Clay.Property               (unKeys, unPlain, unValue)
import           Clay.Size                   (LengthUnit, Size)
import           Clay.Stylesheet             (Rule (Property), runS)
import           Data.Text                   (unpack)
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core hiding (column, row)

-- |Convert to Threepenny style.
class ToStyle a where
  toStyle :: a -> [(String, String)]

-- |Convert a Clay Property to Threepnny style.
instance ToStyle Rule where
  toStyle (Property k v) =
    [(unpack $ unPlain $ unKeys k, unpack $ unPlain $ unValue v)]

-- |Properties for a parent.
data ParentProps = ParentProps {
    pDisplay        :: Display
  , pFlexDirection  :: FlexDirection
  , pFlexWrap       :: FlexWrap
  , pJustifyContent :: JustifyContentValue
  , pAlignItems     :: AlignItemsValue
  , pAlignContent   :: AlignContentValue
}

-- |Convert parent properties to Threepenny style.
instance ToStyle ParentProps where
  toStyle p = concatMap toStyle $ concatMap runS [
        CD.display        $ pDisplay        p
      , CF.flexDirection  $ pFlexDirection  p
      , CF.flexWrap       $ pFlexWrap       p
      , CF.justifyContent $ pJustifyContent p
      , CF.alignItems     $ pAlignItems     p
      , CF.alignContent   $ pAlignContent   p
    ]

-- |Default properties for a parent.
defaultParentProps :: ParentProps
defaultParentProps = ParentProps {
    pDisplay        = CD.flex
  , pFlexDirection  = CF.row
  , pFlexWrap       = CF.nowrap
  , pJustifyContent = CF.flexStart
  , pAlignItems     = CF.stretch
  , pAlignContent   = CF.stretch
}

-- |Properties for a child.
data ChildProps = ChildProps {
    cOrder      :: Int
  , cFlexGrow   :: Int
  , cFlexShrink :: Int
  , cFlexBasis  :: Size LengthUnit
  , cAlignSelf  :: AlignSelfValue
}

-- |Convert child properties to Threepenny style.
instance ToStyle ChildProps where
  toStyle c = concatMap toStyle $ concatMap runS [
        CF.order      $ cOrder      c
      , CF.flexGrow   $ cFlexGrow   c
      , CF.flexShrink $ cFlexShrink c
      , CF.flexBasis  $ cFlexBasis  c
      , CF.alignSelf  $ cAlignSelf  c
    ]

-- |Default properties for a child.
defaultChildProps :: ChildProps
defaultChildProps = ChildProps {
    cOrder      = 1
  , cFlexGrow   = 0
  , cFlexShrink = 1
  , cFlexBasis  = CC.auto
  , cAlignSelf  = CC.auto
}

-- |Set Flexbox properties on an element.
setProps :: ToStyle a => UI Element -> a -> UI Element
setProps el props = el # set UI.style (toStyle props)

-- |Attach elements to a parent element, with given Flexbox properties applied.
flexbox ::
     UI Element -> ParentProps  -- Parent and its Flexbox properties
  -> [(UI Element, ChildProps)] -- Children and respective Flexbox properties
  -> UI Element                 -- Parent with attached children
flexbox p pProps cs = do
  p'  <- setProps p pProps
  cs' <- mapM (uncurry setProps) cs
  element p' #+ map element cs'

-- Helper functions ------------------------------------------------------------

-- |Attach elements to a parent element with flex-direction column.
column :: UI Element -> [UI Element] -> UI Element
column p cs = flexbox p defaultParentProps { pFlexDirection = CF.column } $
  zip cs $ repeat defaultChildProps

-- |Attach elements to a parent element with default Flexbox properties.
row :: UI Element -> [UI Element] -> UI Element
row p cs = flexbox p defaultParentProps $ zip cs $ repeat defaultChildProps

