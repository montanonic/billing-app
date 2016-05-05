module Model.InvoiceProfile where

import Import.NoFoundation

newtype CachedMaybeInvoiceProfile
    = CachedMaybeInvoiceProfile
        { unCachedMaybeInvoiceProfile :: Maybe InvoiceProfile }
    deriving Typeable

maybeInvoiceProfile :: InvoiceProfileId -> DB (Maybe InvoiceProfile)
maybeInvoiceProfile ipid
    = map unCachedMaybeInvoiceProfile
    . cached
    . map CachedMaybeInvoiceProfile
    $ get ipid

getInvoiceProfile404 :: InvoiceProfileId -> DB InvoiceProfile
getInvoiceProfile404 ipid = maybeInvoiceProfile ipid >>= maybe notFound return

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
