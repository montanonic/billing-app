import Import

imp

commonTerms ::
    Set Text ->
    [(id, Set Text)] ->
    [(id, Set Text)]
commonTerms inputTerms tuples = let
    -- intersects the input terms with the terms part of a tuple
    getIntersection = second $ intersect inputTerms
    in
        foldr (\a b -> case getIntersection a of
                -- when the intersection of terms is empty, we exclude the
                -- result from our output list
                (_, s) | null s == True -> b
                -- since the above case catches all empty matches, any other
                -- tuples have at least one search-term in common, and thus we
                -- prepend them to the resulting list
                tuple -> tuple : b)
            []
            tuples

commonTerms' ::
    Set Text ->
    [(id, Set Text)] ->
    [(id, Set Text)]
commonTerms' inputTerms tuples =
    mapFilter
        (second $ intersect inputTerms)
        (not . null)
        tuples

commonTermsTest ::
    [Text] ->
    [(Int, [Text])] ->
    [(Int, Set Text)]
commonTermsTest terms tuples = commonTerms'
    (setFromList terms) (second setFromList <$> tuples)

-- | Input: ["a","b","c","d"] [ (1, ["b","e","r"]) , (2, ["r", "c"])
-- , (3, []) , (4, ["d", "b"]) (5, ["f", "g", "h", "i", "j"]) ]
--
-- Result: [(1,fromList ["b"]),(2,fromList ["c"]),(4,fromList ["b","d"])]
test :: [(Int, Set Text)]
test = commonTermsTest
    ["a","b","c","d"]
    [ (1, ["b","e","r"])
    , (2, ["r", "c"])
    , (3, [])
    , (4, ["d", "b"])
    , (5, ["f", "g", "h", "i", "j"])
    ]
