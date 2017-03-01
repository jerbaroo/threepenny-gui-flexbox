module Main where

import           Control.Monad                      (void)
import qualified Graphics.UI.Threepenny             as UI
import           Graphics.UI.Threepenny.Core        hiding (row)
import           Graphics.UI.Threepenny.Ext.Flexbox (ChildProps, attach,
                                                     cFlexGrow,
                                                     defaultChildProps,
                                                     defaultParentProps)

main :: IO ()
main = startGUI defaultConfig example

-- |Example of three divs using a flex-grow ratio of 1:2:1
example :: Window -> UI ()
example w = void $
    attach (getBody w) defaultParentProps $ [grow 1, grow 2, grow 1]

-- |Example "foo" div and given flex-grow value.
grow :: Int -> (UI Element, ChildProps)
grow n = (foo, defaultChildProps { cFlexGrow = n })

-- |Example "foo" div.
foo :: UI Element
foo = UI.div # set UI.text "foo"
              # set UI.style [("background-color", "red"), ("margin", "8px")]
