{-# LANGUAGE OverloadedStrings #-}

import qualified AMQP.Error as AE
import AMQP.Types (HostName, QueueName)
import Control.Applicative ((<$>), (<*>))
import Control.Exception (bracket)
import Control.Error (scriptIO, runScript, (!?))
import qualified Data.Map as M
import Data.Monoid (mconcat, (<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Log (initLog)
import qualified Network.AMQP as A
import qualified Network.AMQP.Types as A
import qualified Options.Applicative as O
import Password (Password, getPassword, password)
import System.Environment (getArgs, getProgName)

data Options = Options { hostName :: HostName , queue :: QueueName }

options :: O.Parser Options
options = Options
    <$> (O.strOption $ mconcat [
        O.short 'n',
        O.long "node",
        O.metavar "NODE",
        O.value "127.0.0.1",
        O.showDefaultWith id,
        O.completer (O.bashCompleter "hostname"),
        O.help "AMQP node to connect to" ] )
    <*> (fmap T.pack $ O.strOption $ mconcat [
        O.short 'q',
        O.long "queue",
        O.metavar "QUEUE",
        O.value "1.0.0",
        O.showDefaultWith id,
        O.help "Queue to initialize" ] )

parserInfo :: O.ParserInfo Options
parserInfo = O.info (O.helper <*> options) $ mconcat
    [ O.fullDesc
    , O.header "The Suns setup utility"
    , O.progDesc "Sets up an AMQP node"
    , O.footer "Report bugs to Gabriel439@gmail.com"
    ]

main = runScript $ do
    Options hostName queue <- scriptIO $ O.execParser parserInfo

    scriptIO initLog

    pwd <- scriptIO password

    scriptIO $ bracket
        (A.openConnection hostName "suns-vhost" "suns-admin" (getPassword pwd))
        A.closeConnection
        $ \connection -> do
            channel <- A.openChannel connection
        
            let xName1 = "suns-exchange-requests"
            AE.declareExchange channel $ A.ExchangeOpts
                xName1   -- exchangeName
                "direct" -- exchangeType
                False    -- exchangePassive
                True     -- exchangeDurable
                False    -- exchangeAutoDelete
                False    -- exchangeInternal
        
            let xName2 = "suns-exchange-responses"
            AE.declareExchange channel $ A.ExchangeOpts
                xName2   -- exchangeName
                "direct" -- exchangeType
                False    -- exchangePassive
                True     -- exchangeDurable
                False    -- exchangeAutoDelete
                False    -- exchangeInternal
        
            let qName = "suns-queue-" <> queue
            AE.declareQueue channel $ A.QueueOpts
                qName                  -- queueName
                False                  -- queuePassive
                True                   -- queueDurable
                False                  -- queueExclusive
                False                  -- queueAutoDelete
                (A.FieldTable M.empty) -- queueHeaders
        
            AE.bindQueue channel qName xName1 queue
