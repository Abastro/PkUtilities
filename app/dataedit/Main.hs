{-------------------------------------
        Simplistic Data Editor
--------------------------------------}
module Main (main) where

import Base.Lib ( application )
import DataEdit.App ( app )

main :: IO ()
main = application app
