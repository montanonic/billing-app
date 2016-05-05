module Handler.InvoiceProfile.View where

import Import

import Model.InvoiceProfile

-- The current code is a pretty ugly implementation for the sake of having
-- textual ids. We should just make all links have userIds in them. Making the
-- urls nice can come later.

getViewInvoiceProfileR :: InvoiceProfileId -> Handler Html
getViewInvoiceProfileR ipid = do
    InvoiceProfile{..} <- runDB $ getInvoiceProfile404 ipid
    defaultLayout $(widgetFile "invoice-profile/view")

{-
Previous code for handling the Nothing case. Since this *really* shouldn't ever
happen, a 404 page should suffice.

do
    $(logError) "This same function is run in Authorization code,\
        \ which should have only led here if the value existed,\
        \ yet it does not, which is a bug."
    addMessage "error" "There was a problem in getting the last\
        \ Invoice Profile. If this reoccurs, please open an issue,\
        \ and we'll deal with it as soon as possible."
    redirect InvoiceProfilesR
-}
