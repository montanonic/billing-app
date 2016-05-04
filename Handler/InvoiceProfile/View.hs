module Handler.InvoiceProfile.View where

import Import

-- The current code is a pretty ugly implementation for the sake of having
-- textual ids. We should just make all links have userIds in them. Making the
-- urls nice can come later.

getViewInvoiceProfileR :: Text -> Handler Html
getViewInvoiceProfileR _ = do
    mtipid <- lookupSession "INVOICE_PROFILE_ID"
    deleteSession "INVOICE_PROFILE_ID"
    case mtipid of
        Nothing -> error "This feature requires cookies."
        Just tipid -> do
            let ipid = read tipid
            InvoiceProfile{..} <- runDB $ get404 ipid
            defaultLayout $(widgetFile "invoice-profile/view")
