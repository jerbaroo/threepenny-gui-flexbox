module Graphics.UI.Threepenny.Ext.Flexbox (
  -- * Parent Properties
  ParentProps (..),

  -- ** Parent Property Constructors
  parentProps,

  display, flexDirection, flexWrap, justifyContent, alignItems, aligContent,

  -- * Child Properties
  ChildProps (..),

  -- ** Child Property Constructors
  childProps,

  order, flexGrow, flexShrink, flexBasis, alignSelf,

  -- * Core Functions
  ToStyle (..), FlexProps(..), setFlex, modifyFlex,

  -- * Useful Abstractions
  flex, flex_p, flex_c, flex_pc
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
import           Data.Monoid                 (Last(..))
import           Control.Monad               ((<=<))
import           Data.Maybe                  (mapMaybe)
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core hiding (column, row)

-- |Convert to Threepenny style.
class ToStyle a where
  toStyle :: a -> [(String, String)]

-- | A class for bundles of Flexbox properties suitable for application
--   to 'Elements'.
class (ToStyle a, Semigroup a) => FlexProps a where
  defaultFlexProps :: a

-- |Convert a Clay Property to Threepnny style.
instance ToStyle Rule where
  toStyle (Property [] k v) =
    [(unpack $ unPlain $ unKeys k, unpack $ unPlain $ unValue v)]

-- |Properties for a parent.
data ParentProps = ParentProps {
    pDisplay        :: Last Display
  , pFlexDirection  :: Last FlexDirection
  , pFlexWrap       :: Last FlexWrap
  , pJustifyContent :: Last JustifyContentValue
  , pAlignItems     :: Last AlignItemsValue
  , pAlignContent   :: Last AlignContentValue
}

instance Semigroup ParentProps where
  p1 <> p2 = ParentProps {
      pDisplay        = pDisplay p1        <> pDisplay p2
    , pFlexDirection  = pFlexDirection p1  <> pFlexDirection p2
    , pFlexWrap       = pFlexWrap p1       <> pFlexWrap p2
    , pJustifyContent = pJustifyContent p1 <> pJustifyContent p2
    , pAlignItems     = pAlignItems p1     <> pAlignItems p2
    , pAlignContent   = pAlignContent p1   <> pAlignContent p2
  }

instance Monoid ParentProps where
  mempty = ParentProps {
      pDisplay        = mempty
    , pFlexDirection  = mempty
    , pFlexWrap       = mempty
    , pJustifyContent = mempty
    , pAlignItems     = mempty
    , pAlignContent   = mempty
  }

-- |Convert parent properties to Threepenny style.
instance ToStyle ParentProps where
  toStyle p = concatMap (toStyle <=< runS) $ mapMaybe getLast $ [
        CD.display        <$> pDisplay        p
      , CF.flexDirection  <$> pFlexDirection  p
      , CF.flexWrap       <$> pFlexWrap       p
      , CF.justifyContent <$> pJustifyContent p
      , CF.alignItems     <$> pAlignItems     p
      , CF.alignContent   <$> pAlignContent   p
    ]

-- |Default properties for a parent.
parentProps :: ParentProps
parentProps = ParentProps {
    pDisplay        = pure CD.flex
  , pFlexDirection  = pure CF.row
  , pFlexWrap       = pure CF.nowrap
  , pJustifyContent = pure CF.flexStart
  , pAlignItems     = pure CF.stretch
  , pAlignContent   = pure CF.stretch
}

instance FlexProps ParentProps where
  defaultFlexProps = parentProps

display        x = mempty { pDisplay        = pure x }
flexDirection  x = mempty { pFlexDirection  = pure x }
flexWrap       x = mempty { pFlexWrap       = pure x }
justifyContent x = mempty { pJustifyContent = pure x }
alignItems     x = mempty { pAlignItems     = pure x }
aligContent    x = mempty { pAlignContent   = pure x }

-- |Properties for a child.
data ChildProps = ChildProps {
    cOrder      :: Last Int
  , cFlexGrow   :: Last Int
  , cFlexShrink :: Last Int
  , cFlexBasis  :: Last (Size LengthUnit)
  , cAlignSelf  :: Last AlignSelfValue
}

instance Semigroup ChildProps where
  c1 <> c2 = ChildProps {
      cOrder      = cOrder c1      <> cOrder c2
    , cFlexGrow   = cFlexGrow c1   <> cFlexGrow c2
    , cFlexShrink = cFlexShrink c1 <> cFlexShrink c2
    , cFlexBasis  = cFlexBasis c1  <> cFlexBasis c2
    , cAlignSelf  = cAlignSelf c1  <> cAlignSelf c2
  }

instance Monoid ChildProps where
  mempty = ChildProps {
      cOrder      = mempty
    , cFlexGrow   = mempty
    , cFlexShrink = mempty
    , cFlexBasis  = mempty
    , cAlignSelf  = mempty
  }

-- |Convert child properties to Threepenny style.
instance ToStyle ChildProps where
  toStyle c = concatMap (toStyle <=< runS) $ mapMaybe getLast $ [
        CF.order      <$> cOrder      c
      , CF.flexGrow   <$> cFlexGrow   c
      , CF.flexShrink <$> cFlexShrink c
      , CF.flexBasis  <$> cFlexBasis  c
      , CF.alignSelf  <$> cAlignSelf  c
    ]

-- |Default properties for a child.
childProps :: ChildProps
childProps = ChildProps {
    cOrder      = pure 1
  , cFlexGrow   = pure 0
  , cFlexShrink = pure 1
  , cFlexBasis  = pure CC.auto
  , cAlignSelf  = pure CC.auto
}

instance FlexProps ChildProps where
  defaultFlexProps = childProps

order      x = mempty { cOrder      = pure x }
flexGrow   x = mempty { cFlexGrow   = pure x }
flexShrink x = mempty { cFlexShrink = pure x }
flexBasis  x = mempty { cFlexBasis  = pure x }
alignSelf  x = mempty { cAlignSelf  = pure x }

-- | Set Flexbox properties on an element. Unspecified properties are
--   set to the corresponding 'defaultFlexProps' field values.
setFlex :: FlexProps a => a -> UI Element -> UI Element
setFlex props = modifyFlex (defaultFlexProps <> props)

-- | Modify Flexbox properties on an element. Unspecified properties
--   are left unchanged.
modifyFlex :: FlexProps a => a -> UI Element -> UI Element
modifyFlex props el = el # set UI.style (toStyle props)

-- |Attach elements to a parent element, applying given Flexbox properties.
flex ::
     UI Element                 -- ^ Parent
  -> ParentProps                -- ^ Parent Flexbox properties
  -> [(UI Element, ChildProps)] -- ^ Children and respective Flexbox properties
  -> UI Element                 -- ^ Parent with attached children
flex p pProps cs = do
  p'  <- p # setFlex pProps
  cs' <- mapM (\(c, cProps) -> c # setFlex cProps) cs
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

