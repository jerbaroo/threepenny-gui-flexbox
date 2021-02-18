module Graphics.UI.Threepenny.Ext.Flexbox (
  -- * Parent Properties
  ParentProps (..),

  parentProps,

  -- ** Parent Property Constructors
  --
  -- $clay-values
  display, flexDirection, flexWrap, justifyContent, alignItems, aligContent,

  -- * Child Properties
  ChildProps (..),

  childProps,

  -- ** Child Property Constructors
  --
  -- $clay-values
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

-- $clay-values
-- The available values for the Clay argument types can be found in
-- their documentation by drilling down into their instance lists and
-- looking for the methods therein.

-- | Bundles of flexbox properties that can be converted to Threepenny
-- style property-value pairs.
class ToStyle a where
  toStyle :: a -> [(String, String)]

-- | Bundles of flexbox properties that can be applied to an 'Element'
-- through 'setFlex' and 'modifyFlex'.
class (ToStyle a, Semigroup a) => FlexProps a where
  -- | Defaults to use when setting properties with unspecified values.
  defaultFlexProps :: a

-- | Note that 'toStyle' for this instance is only defined for the
-- 'Property' constructor of 'Rule'.
instance ToStyle Rule where
  toStyle (Property [] k v) =
    [(unpack $ unPlain $ unKeys k, unpack $ unPlain $ unValue v)]
  toStyle _                 = error
    "Graphics.UI.Threepenny.Ext.Flexbox.toStyle @Rule: not a Property"

-- | Properties for a flexbox parent.
data ParentProps = ParentProps {
    pDisplay        :: Last Display
  , pFlexDirection  :: Last FlexDirection
  , pFlexWrap       :: Last FlexWrap
  , pJustifyContent :: Last JustifyContentValue
  , pAlignItems     :: Last AlignItemsValue
  , pAlignContent   :: Last AlignContentValue
}

-- | 'Data.Semigroup.<>' combines properties through the 'Last' monoid:
-- if a property is specified by both arguments, choose the value from
-- the second one.
instance Semigroup ParentProps where
  p1 <> p2 = ParentProps {
      pDisplay        = pDisplay p1        <> pDisplay p2
    , pFlexDirection  = pFlexDirection p1  <> pFlexDirection p2
    , pFlexWrap       = pFlexWrap p1       <> pFlexWrap p2
    , pJustifyContent = pJustifyContent p1 <> pJustifyContent p2
    , pAlignItems     = pAlignItems p1     <> pAlignItems p2
    , pAlignContent   = pAlignContent p1   <> pAlignContent p2
  }

-- | 'mempty' specifies no properties.
instance Monoid ParentProps where
  mempty = ParentProps {
      pDisplay        = mempty
    , pFlexDirection  = mempty
    , pFlexWrap       = mempty
    , pJustifyContent = mempty
    , pAlignItems     = mempty
    , pAlignContent   = mempty
  }

instance ToStyle ParentProps where
  toStyle p = concatMap (toStyle <=< runS) $ mapMaybe getLast $ [
        CD.display        <$> pDisplay        p
      , CF.flexDirection  <$> pFlexDirection  p
      , CF.flexWrap       <$> pFlexWrap       p
      , CF.justifyContent <$> pJustifyContent p
      , CF.alignItems     <$> pAlignItems     p
      , CF.alignContent   <$> pAlignContent   p
    ]

-- | Default flexbox properties for a parent.
parentProps :: ParentProps
parentProps = ParentProps {
    pDisplay        = pure CD.flex
  , pFlexDirection  = pure CF.row
  , pFlexWrap       = pure CF.nowrap
  , pJustifyContent = pure CF.flexStart
  , pAlignItems     = pure CF.stretch
  , pAlignContent   = pure CF.stretch
}

-- | Gets defaults from 'parentProps'.
instance FlexProps ParentProps where
  defaultFlexProps = parentProps

display :: Display -> ParentProps
display x = mempty { pDisplay = pure x }

flexDirection :: FlexDirection -> ParentProps
flexDirection  x = mempty { pFlexDirection = pure x }

flexWrap :: FlexWrap -> ParentProps
flexWrap x = mempty { pFlexWrap = pure x }

justifyContent :: JustifyContentValue -> ParentProps
justifyContent x = mempty { pJustifyContent = pure x }

alignItems :: AlignItemsValue -> ParentProps
alignItems x = mempty { pAlignItems = pure x }

aligContent :: AlignContentValue -> ParentProps
aligContent x = mempty { pAlignContent = pure x }

-- | Properties for a flexbox child.
data ChildProps = ChildProps {
    cOrder      :: Last Int
  , cFlexGrow   :: Last Int
  , cFlexShrink :: Last Int
  , cFlexBasis  :: Last (Size LengthUnit)
  , cAlignSelf  :: Last AlignSelfValue
}

-- | 'Data.Semigroup.<>' combines properties through the 'Last' monoid:
-- if a property is specified by both arguments, choose the value from
-- the second one.
instance Semigroup ChildProps where
  c1 <> c2 = ChildProps {
      cOrder      = cOrder c1      <> cOrder c2
    , cFlexGrow   = cFlexGrow c1   <> cFlexGrow c2
    , cFlexShrink = cFlexShrink c1 <> cFlexShrink c2
    , cFlexBasis  = cFlexBasis c1  <> cFlexBasis c2
    , cAlignSelf  = cAlignSelf c1  <> cAlignSelf c2
  }

-- | 'mempty' specifies no properties.
instance Monoid ChildProps where
  mempty = ChildProps {
      cOrder      = mempty
    , cFlexGrow   = mempty
    , cFlexShrink = mempty
    , cFlexBasis  = mempty
    , cAlignSelf  = mempty
  }

instance ToStyle ChildProps where
  toStyle c = concatMap (toStyle <=< runS) $ mapMaybe getLast $ [
        CF.order      <$> cOrder      c
      , CF.flexGrow   <$> cFlexGrow   c
      , CF.flexShrink <$> cFlexShrink c
      , CF.flexBasis  <$> cFlexBasis  c
      , CF.alignSelf  <$> cAlignSelf  c
    ]

-- | Default flexbox properties for a child.
childProps :: ChildProps
childProps = ChildProps {
    cOrder      = pure 1
  , cFlexGrow   = pure 0
  , cFlexShrink = pure 1
  , cFlexBasis  = pure CC.auto
  , cAlignSelf  = pure CC.auto
}

-- | Gets defaults from 'childProps'.
instance FlexProps ChildProps where
  defaultFlexProps = childProps

order :: Int -> ChildProps
order x = mempty { cOrder = pure x }

flexGrow :: Int -> ChildProps
flexGrow x = mempty { cFlexGrow = pure x }

flexShrink :: Int -> ChildProps
flexShrink x = mempty { cFlexShrink = pure x }

flexBasis :: Size LengthUnit -> ChildProps
flexBasis x = mempty { cFlexBasis = pure x }

alignSelf :: AlignSelfValue -> ChildProps
alignSelf x = mempty { cAlignSelf = pure x }

-- | Sets flexbox properties on an 'Element'. Unspecified properties are
-- set to the corresponding 'defaultFlexProps' field values.
setFlex :: FlexProps a => a -> UI Element -> UI Element
setFlex props = modifyFlex (defaultFlexProps <> props)

-- | Modifies flexbox properties on an 'Element'. Unspecified properties
-- are left unchanged.
modifyFlex :: FlexProps a => a -> UI Element -> UI Element
modifyFlex props el = el # set UI.style (toStyle props)

-- | Attach 'Element's to a parent element, applying given flexbox properties.
flex ::
     UI Element                 -- ^ Parent
  -> ParentProps                -- ^ Parent Flexbox properties
  -> [(UI Element, ChildProps)] -- ^ Children and respective Flexbox properties
  -> UI Element                 -- ^ Parent with attached children
flex p pProps cs = do
  p'  <- p # setFlex pProps
  cs' <- mapM (\(c, cProps) -> c # setFlex cProps) cs
  element p' #+ map element cs'

-- | Like 'flex', but applies default properties to the parent.
flex_p ::
     UI Element                 -- ^ Parent
  -> [(UI Element, ChildProps)] -- ^ Children and respective flexbox properties
  -> UI Element                 -- ^ Parent with attached children
flex_p p = flex p parentProps

-- | Like 'flex', but applies default properties to the children.
flex_c ::
     UI Element   -- ^ Parent
  -> ParentProps  -- ^ Parent flexbox properties
  -> [UI Element] -- ^ Children
  -> UI Element   -- ^ Parent with attached children
flex_c p pProps cs = flex p pProps $ zip cs $ repeat childProps

-- | Like 'flex', but applies default properties to the parent and
-- children.
flex_pc ::
     UI Element   -- ^ Parent
  -> [UI Element] -- ^ Children
  -> UI Element   -- ^ Parent with attached children
flex_pc p = flex_c p parentProps

