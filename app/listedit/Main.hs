{--------------------------------------
        Minimalistic List Editor
---------------------------------------}
module Main ( main ) where

import Base.Interface ( application )
import ListEdit.App ( app )

main :: IO ()
main = application app