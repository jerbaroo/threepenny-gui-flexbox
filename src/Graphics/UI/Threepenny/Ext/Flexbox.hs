module Graphics.UI.Threepenny.Ext.Flexbox (
  -- * Parent Properties
  ParentProps (..), parentProps,

  -- ** Parent Property Helpers
  display, flexDirection, flexWrap, justifyContent, alignItems, aligContent,
 
  -- * Child Properties
  ChildProps (..), childProps,

  -- ** Child Property Helpers
  order, flexGrow, flexShrink, flexBasis, alignSelf,

  -- * Core Functions
  ToStyle (..), setProps, flex, flex_p, flex_c, flex_pc 
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
parentProps :: ParentProps
parentProps = ParentProps {
    pDisplay        = CD.flex
  , pFlexDirection  = CF.row
  , pFlexWrap       = CF.nowrap
  , pJustifyContent = CF.flexStart
  , pAlignItems     = CF.stretch
  , pAlignContent   = CF.stretch
}

display        x = parentProps { pDisplay        = x }
flexDirection  x = parentProps { pFlexDirection  = x }
flexWrap       x = parentProps { pFlexWrap       = x }
justifyContent x = parentProps { pJustifyContent = x }
alignItems     x = parentProps { pAlignItems     = x }
aligContent    x = parentProps { pAlignContent   = x }

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
childProps :: ChildProps
childProps = ChildProps {
    cOrder      = 1
  , cFlexGrow   = 0
  , cFlexShrink = 1
  , cFlexBasis  = CC.auto
  , cAlignSelf  = CC.auto
}

order      x = childProps { cOrder      = x }
flexGrow   x = childProps { cFlexGrow   = x }
flexShrink x = childProps { cFlexShrink = x }
flexBasis  x = childProps { cFlexBasis  = x }
alignSelf  x = childProps { cAlignSelf  = x }

-- |Set Flexbox properties on an element.
setProps :: ToStyle a => a -> UI Element -> UI Element
setProps props el = el # set UI.style (toStyle props)

-- |Attach elements to a parent element, applying given Flexbox properties.
flex ::
     UI Element                 -- ^ Parent
  -> ParentProps                -- ^ Parent Flexbox properties
  -> [(UI Element, ChildProps)] -- ^ Children and respective Flexbox properties
  -> UI Element                 -- ^ Parent with attached children
flex p pProps cs = do
  p'  <- p # setProps pProps
  cs' <- mapM (\(c, cProps) -> c # setProps cProps) cs
  element p' #+ map element cs'

-- |Like 'flex' but apply default properties to the parent.
flex_p ::
     UI Element                 -- ^ Parent
  -> [(UI Element, ChildProps)] -- ^ Children and respective Flexbox properties
  -> UI Element                 -- ^ Parent with attached children
flex_p p = flex p parentProps

-- |Like 'flex' but apply default properties to the children.
flex_c ::
     UI Element   -- ^ Parent
  -> ParentProps  -- ^ Parent Flexbox properties
  -> [UI Element] -- ^ Children
  -> UI Element   -- ^ Parent with attached children
flex_c p pProps cs = flex p pProps $ zip cs $ repeat childProps

-- |Like 'flex' but apply default properties to the parent and children.
flex_pc ::
     UI Element   -- ^ Parent
  -> [UI Element] -- ^ Children
  -> UI Element   -- ^ Parent with attached children
flex_pc p = flex_c p parentProps

