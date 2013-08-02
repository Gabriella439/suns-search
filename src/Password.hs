{-| Utilities to securely read in user passwords from standard input.

    All the programs in the Suns suite do not accept any passwords as command
    line arguments because process listings like @ps@ would leak passwords
    passed this way.

    To securely automate these programs, store the password a single line in a
    file with appropriate permissions and feed the file to the program's
    standard input like this:

> $ suns-server < password.txt

    The only way to obtain a value of type 'Password' is to use the 'password'
    function.  This allows functions to demand an argument of type 'Password'
    if they want to ensure that their argument safely originates from the
    'password' function.
-}

module Password (Password, Password.getPassword, password) where

import Control.Monad (when)
import Control.Exception (throwIO)
import Control.Monad.Trans.Class (lift)
import qualified Data.Text as T
import System.Console.Haskeline (
    getPassword, runInputT, defaultSettings, haveTerminalUI )

-- | User password
newtype Password = Password
    { getPassword :: T.Text
      -- ^ Extract the password as 'T.Text'
    }

{-| Retrieve a password from standard input, displaying a prompt if connected to
    an interactive terminal
-}
password :: IO Password
password = runInputT defaultSettings $ do
    interactive <- haveTerminalUI
    when interactive $ lift $ putStrLn "Enter server password:"
    mPassword <- System.Console.Haskeline.getPassword Nothing ""
    case mPassword of
        Nothing -> lift $ throwIO $
            userError "Failed to read password due to premature end of input"
        Just password -> return $ Password (T.pack password)
