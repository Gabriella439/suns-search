{- Copyright 2013 Gabriel Gonzalez

   This file is part of the Suns Search Engine

   The Suns Search Engine is free software: you can redistribute it and/or
   modify it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 2 of the License, or (at your
   option) any later version.

   The Suns Search Engine is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   You should have received a copy of the GNU General Public License along with
   the Suns Search Engine.  If not, see <http://www.gnu.org/licenses/>.
-}

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
import Data.Text (Text, pack)
import System.Console.Haskeline (
    getPassword, runInputT, defaultSettings, haveTerminalUI )

-- | User password
newtype Password = Password
    { getPassword :: Text
      -- ^ Extract the password as 'Text'
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
        Nothing  -> lift $ throwIO $
            userError "Failed to read password due to premature end of input"
        Just pwd -> return $ Password (pack pwd)
