module Handler.InvoiceProfile.Create where

import Import

import Form.InvoiceProfile

getCreateInvoiceProfileR :: Handler Html
getCreateInvoiceProfileR = do
    uid <- requireAuthId
    ((_, widget), enctype) <- runFormPost $ invoiceProfileForm uid
    defaultLayout [whamlet|
<form enctype=#{enctype} method=post action=@{CreateInvoiceProfileR}>
    ^{widget}
    <input type=submit value="Create Invoice Profile">
|]

postCreateInvoiceProfileR :: Handler Html
postCreateInvoiceProfileR = do
    uid <- requireAuthId
    ((res, _), _) <- runFormPost $ invoiceProfileForm uid
    case res of
        FormSuccess ip -> do
            ipid <- runDB $ insert ip
            addMessage "success" [shamlet|Profile successfully created.|]
            redirect (ViewInvoiceProfileR ipid)

        _ -> getCreateInvoiceProfileR
{-
        FormSuccess invoiceProfile -> do
            eres <- runDB $ insertBy invoiceProfile
            case rightMay eres of
                Just ipid -> redirect (ViewInvoiceProfileR ipid)
                _ -> return ()
            defaultLayout [whamlet|
$newline never
$maybe (Entity key ip) <- maybeLeft eres
    <p>A profile with the name #{invoiceProfileDisplayName ip} #
        <a href=@{ViewInvoiceProfileR key}>already exists
        . Please use a different name.

    <form enctype=#{enctype} method=post action=@{CreateInvoiceProfileR}>
        ^{widget}
        <input type=submit value="Create Invoice Profile">
|]
        _ -> getCreateInvoiceProfileR
-}
