{--------------------------------------
        Minimalistic List Editor
---------------------------------------}
module Main ( main ) where

import Base.Lib ( application )
import ListEdit.App ( app )

main :: IO ()
main = application app