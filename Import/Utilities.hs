module Import.Utilities where

import ClassyPrelude

showT :: (Show a) => a -> Text
showT = pack . show

read :: (Read c, MonoFoldable a, Element a ~ Char) => a -> c
read = fromMaybe (error "If you're seeing this error, you should have used\
    \ 'readMay'") . readMay

filterMap :: (Foldable t) => (a -> Bool) -> (a -> b) -> t a -> t b
filterMap = error "undefined"

-- | Map over each element, and then apply the predicate filter to the new
-- element, removing it if it fails to satisfy the predicate.
--
-- > ((mapFilter (*2) odd) :: [Int] -> [Int]) [1..1000]
-- []
-- > ((mapFilter (*2) even) :: [Int] -> [Int]) [1..25]
-- [2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50]
mapFilter ::
    forall c b.
    (MonoFoldable c, SemiSequence b, Monoid b) =>
       (Element c -> Element b) -- ^ mapping function
    -> (Element b -> Bool) -- ^ predicate filter
    -> c
    -> b
mapFilter mp filt = foldr (\a rest ->
    let b = mp a in
    if filt b
        then b `cons` rest
        else rest) mempty
