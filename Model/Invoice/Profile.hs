module Model.Invoice.Profile where

import Import

{-
createInvoiceProfile ::
    InvoiceProfile ->
    DB (Either (Entity InvoiceProfile) (Key
createInvoiceProfile = insertBy ip
-}

{-
    -- maybe get an InvoiceProfile with the same name
    mconflictingEntity <- getBy $
        UniqueInvoiceProfile uid (invoiceProfileName ip)
    case mconflictingEntity of
        -- if there's a conflict, return the conflicting entity
        Just ip2 -> return $ Just ip2
        -- otherwise, create a new InvoiceProfile, and return Nothing
        Nothing -> do
            insert_ ip
            return Nothing
-}
