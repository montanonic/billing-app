module Form.Field.SearchTerms
    ( searchTermsField
    ) where

import Import
import Data.Text (strip)

_textListField :: Field Handler [Text]
_textListField = convertField
    (map strip . (splitElem ','))
    (intercalate ", ")
    textField

_textSetField :: Field Handler (Set Text)
_textSetField = convertField
    setFromList
    setToList
    _textListField

type InvoiceProfileDisplayName = Text

-- | TODO: Write a proper parser instead of relying upon text functions.
-- "splitElem ','" only works on comma-separated inputs, which is pretty
-- restrictive.

-- | A field for InvoiceProfileSearchTerms. Gives an error whenever terms in the
-- field overlap with any other searchTerms from a user's other InvoiceProfile
-- entitites, explaining which fields are in common, and linking to the
-- conflicting InvoiceProfiles respectively.
searchTermsField :: UserId -> Field Handler (Set Text)
searchTermsField uid = checkMMap
    -- turns "a, B,    C,d" into fromList ["a", "b", "c", "d"]. In the future,
    -- will probably use a proper parser.
    (doNotConflict . setFromList . map (toLower . strip) . (splitElem ','))
    -- converts back from Set into a comma-separated Text
    (intercalate ", " . toList)
    textField
    where
        -- make sure the search terms don't conflict with other InvoiceProfile
        -- calendar search term entries
        doNotConflict terms = do
            iprofiles <- runDB $ selectList [ InvoiceProfileUserId ==. uid ] []

            let conflicts :: [(InvoiceProfileDisplayName, Set Text)]
                conflicts = commonTerms terms iprofiles

            case conflicts of
                [] -> return $ Right terms
                xs -> return $ Left $ msg xs

        msg :: [(InvoiceProfileDisplayName, Set Text)] -> Text
        msg xs =
            "Conflicting terms. "
            ++ (concatMap (\(name, termSet) ->
                -- name of profile with terms in conflict
                "From " ++ name ++ ": "
                -- a list of the conflicting terms
                ++ (intercalate ", " $ setToList termSet) ++ ". ") xs)

-- | See "commonTerms" below. This function is a more generic implementation,
-- but it's not really useful generically; keeping it generic makes it easier
-- to read though.
commonTerms' ::
    Set Text ->
    [(a, Set Text)] ->
    [(a, Set Text)]
commonTerms' inputTerms tuples =
    mapFilter
        -- intersect the input set with the other sets, to reveal any
        -- conflicting terms. The result will be an empty Set when there are no
        -- terms in conflict.
        (second $ intersect inputTerms)
        -- only keep tuples with conflicting entitites
        (not . null . snd)
        tuples

-- | Takes a Set of search terms and a list of InvoiceProfile entities, and
-- returns a list containing all of the InvoiceProfileDisplayNames corresponding
-- to entitites containing overlapping terms, in a Tuple with a Set of those
-- conflicting terms.
commonTerms ::
    Set Text ->
    [Entity InvoiceProfile] ->
    [(InvoiceProfileDisplayName, Set Text)]
commonTerms terms iprofiles = let
    toTuples (Entity _ ip) = ( invoiceProfileDisplayName ip
                             , invoiceProfileSearchTerms ip)
    in commonTerms' terms (toTuples <$> iprofiles)

{-
Refined implementation. Uses a fold to guarantee stream-fusion and make the
algorithm single-pass. Latest implementation uses the same technique, but

commonTerms' ::
    Set Text ->
    [(id, Set Text)] ->
    [(id, Set Text)]
commonTerms' inputTerms tuples = let
    -- intersects the input terms with the terms part of a tuple
    getIntersection = second $ intersect inputTerms
    in
        foldr (\a b -> case getIntersection a of
                -- when the intersection of terms is empty, we exclude the
                -- result from our output list
                (_, s) | null s -> b
                -- since the above case catches all empty matches, any other
                -- tuples have at least one search-term in common, and thus we
                -- prepend them to the resulting list
                tuple -> tuple : b)
            []
            tuples

Older implementation. Uses easier to read filter and map, whereas newer
version merges both functionalities into a single-pass fold.

commonTerms' ::
    Set Text ->
    [(id, Set Text)] ->
    [(id, Set Text)]
commonTerms' inputTerms tuples = let
    getIntersection (key, existingTerms) =
        (key,) $ inputTerms `intersect` existingTerms
    in filter ((/=) S.empty . snd) (getIntersection <$> tuples)
-}

-- | Simpler types and automatic conversions for easier testing.
_commonTermsTest ::
    [Text] ->
    [(Int, [Text])] ->
    [(Int, Set Text)]
_commonTermsTest terms tuples = commonTerms'
    (setFromList terms) (second setFromList <$> tuples)

-- | Input: ["a","b","c","d"] [ (1, ["b","e","r"]) , (2, ["r", "c"])
-- , (3, []) , (4, ["d", "b"]) (5, ["f", "g", "h", "i", "j"]) ]
--
-- Result: [(1,fromList ["b"]),(2,fromList ["c"]),(4,fromList ["b","d"])]
_test :: [(Int, Set Text)]
_test = _commonTermsTest
    ["a","b","c","d"]
    [ (1, ["b","e","r"])
    , (2, ["r", "c"])
    , (3, [])
    , (4, ["d", "b"])
    , (5, ["f", "g", "h", "i", "j"])
    ]
