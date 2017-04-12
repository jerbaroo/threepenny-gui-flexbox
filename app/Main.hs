module Main where

import           Control.Monad                      (void)
import qualified Graphics.UI.Threepenny             as UI
import           Graphics.UI.Threepenny.Core        hiding (row)
import           Graphics.UI.Threepenny.Ext.Flexbox

main :: IO ()
main = startGUI defaultConfig example'

-- |Example without 'flex_p'.
example :: Window -> UI ()
example w = void $
  getBody w # setFlex parentProps #+ [
      foo # setFlex (flexGrow 1)
    , foo # setFlex (flexGrow 2)
    , foo # setFlex (flexGrow 1)
    ]

-- |Example with 'flex_p'.
example' :: Window -> UI ()
example' w = void $
  flex_p (getBody w) [
      (foo, flexGrow 1)
    , (foo, flexGrow 2)
    , (foo, flexGrow 1)
    ]

-- | Simple coloured 'div'.
foo = UI.div # set UI.text "foo"
             # set UI.style [("background-color", "#F89406"),
                             ("margin", "8px")]
