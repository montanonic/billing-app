module Form.InvoiceProfile where

import Import

import Form.Field.Currency
import Form.Field.SearchTerms
import Text.Blaze.Renderer.Text (renderMarkup)

{-
personForm :: Html -> MForm Handler (FormResult Person, Widget)
personForm extra = do
    (nameRes, nameView) <- mreq textField "this is not used" Nothing
    (ageRes, ageView) <- mreq intField "neither is this" Nothing
    let personRes = Person <$> nameRes <*> ageRes
    let widget = do
            toWidget
                [lucius|
                    ##{fvId ageView} {
                        width: 3em;
                    }
                |]
            [whamlet|
                #{extra}
                <p>
                    Hello, my name is #
                    ^{fvInput nameView}
                    \ and I am #
                    ^{fvInput ageView}
                    \ years old. #
                    <input type=submit value="Introduce myself">
            |]
    return (personRes, widget)
-}

-- | Checks to see if the user has any other InvoiceProfile entity with the
-- same InvoiceProfileName. We don't match against InvoiceProfileDisplayName
-- because it is case-sensitive (for display), whereas InvoiceProfileName is
-- used for Uniqueness matching, and only uses lower-case values.
profileNameField :: UserId -> Field Handler Text
profileNameField uid = (`checkM` textField) $ \res -> do
    mprofile <- runDB $ selectFirst
        [ InvoiceProfileName ==. (toLower res)
        , InvoiceProfileUserId ==. uid ] []
    case mprofile of
        Nothing -> return $ Right res
        Just (Entity _ val) -> return . Left $
            "A profile with the name \"" ++ invoiceProfileDisplayName val
            ++ "\" already exists. Please use a different name."

-- | Uses the result of profileNameField in both the InvoiceProfileDisplayName
-- and InvoiceProfileName field, making the latter the lower-cased version of
-- the result.
invoiceProfileForm :: UserId -> Form InvoiceProfile
invoiceProfileForm uid = renderDivs $ formToAForm $ do
    (pnR, pnV) <- mreq (profileNameField uid) "Profile Name" Nothing
    (stR, stV) <- mreq (searchTermsField uid) "Calendar Search Terms\
        \ (case-insensitive)" Nothing
    (hlR, hlV) <- mopt intField "Hour Limit" Nothing
    (hrR, hrV) <- mopt currencyField "Hourly Rate" Nothing
    (mrR, mrV) <- mopt currencyField "Milage Rate" Nothing
    (adR, adV) <- mopt intField "All-Day Hours" Nothing
    (btR, btV) <- mopt textField "Bill To" Nothing
    let invoiceProfileRes = InvoiceProfile
            <$> pure uid
            <*> pnR -- name to display
            <*> (toLower <$> pnR) -- lower-case name to check for Uniqueness
            <*> stR
            <*> hlR
            <*> hrR
            <*> mrR
            <*> adR
            <*> btR
        views = [pnV, stV, hlV, hrV, mrV, adV, btV]
    $(logDebug) (showT ( renderMarkup $ fvLabel stV
                  , renderMarkup <$> fvTooltip stV
                  , renderMarkup <$> fvErrors stV))
    return (invoiceProfileRes, views)

{-
invoiceProfileForm :: UserId -> Form InvoiceProfile
invoiceProfileForm uid = renderDivs $ InvoiceProfile
    <$> pure uid
    <*> areq ciTextField "The name of this Profile" Nothing
    <*> areq searchTermsField "Terms to search your calendar entries by" Nothing
    <*> aopt intField "The maximum number of hours you can bill this client per\
            \ billing period" Nothing
    <*> aopt currencyField "Hourly rate" Nothing
    <*> aopt currencyField "Milage rate" Nothing
    <*> aopt doubleField "All day hours" Nothing -- TODO: validate this field
    <*> aopt textField "Bill To" Nothing

-}
