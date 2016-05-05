module Handler.InvoiceProfile.View where

import Import

-- The current code is a pretty ugly implementation for the sake of having
-- textual ids. We should just make all links have userIds in them. Making the
-- urls nice can come later.

getViewInvoiceProfileR :: InvoiceProfileId -> Handler Html
getViewInvoiceProfileR ipid = do
    InvoiceProfile{..} <- runDB $ get404 ipid
    defaultLayout $(widgetFile "invoice-profile/view")
