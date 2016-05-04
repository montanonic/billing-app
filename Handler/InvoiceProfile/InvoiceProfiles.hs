module Handler.InvoiceProfile.InvoiceProfiles where

import Import

getInvoiceProfilesR :: Handler Html
getInvoiceProfilesR = do
    uid <- requireAuthId
    profiles <- runDB $ selectList
        [ InvoiceProfileUserId ==. uid ]
        [ Asc InvoiceProfileName ]
    defaultLayout $(widgetFile "invoice-profile/invoice-profiles")
