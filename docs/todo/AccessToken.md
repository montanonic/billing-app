AccessTokens are not permanent, and so we need to have a seamless process to
update a user's access token to allow the actions that they want to perform in
the event that their current AccessToken has expired. The token may have expired
because they revoked permission, and no later decided they wanted to grant it
again. There *needs* to be a process for this, although basic functionality
takes priority.
