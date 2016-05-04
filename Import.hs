module Import
    ( module Import
    ) where

import Foundation            as Import
import Import.NoFoundation   as Import

checkMap :: (Monad m, RenderMessage (HandlerSite m) msg) =>
    (a -> Either msg b) -> (b -> a) -> Field m a -> Field m b
checkMap f = checkMMap (return . f)
