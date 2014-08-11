{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module : Web.PubSub.Terminal
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Web.PubSub.Terminal where

--------------------------------------------------------------------------------
import Prelude hiding (map, takeWhile)
import Control.Applicative
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.IORef
import Data.Monoid ((<>))
import System.IO

--------------------------------------------------------------------------------
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.Text             as T
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO          as T
import           Network.HTTP.Types
import           Network.URL
import           Pipes
import           Pipes.Prelude (fold, map, stdoutLn)

--------------------------------------------------------------------------------
import Web.PubSub.Command
import Web.PubSub.EventStore
import Web.PubSub.Hub
import Web.PubSub.Param
import Web.PubSub.Subscription
import Web.PubSub.Type
import Web.PubSub.Verification

--------------------------------------------------------------------------------
data CMD
    = SUBSCRIBE
    | VERIFY Callback Topic
    | LIST Callback Topic
    | QUIT
    | UNKNOWN B.ByteString

--------------------------------------------------------------------------------
cmdParser :: Parser CMD
cmdParser = parseSubscribe <|>
            parseVerify    <|>
            parseList      <|>
            parseQuit      <|>
            parseUnknown

--------------------------------------------------------------------------------
parseSubscribe :: Parser CMD
parseSubscribe = SUBSCRIBE <$ string "subscribe"

--------------------------------------------------------------------------------
parseVerify :: Parser CMD
parseVerify
    = do _ <- string "verify"
         spaces
         cb <- fmap decodeUtf8 word
         spaces
         tp <- fmap decodeUtf8  word
         let res = (,) <$> validateCallback cb <*> validateTopic tp

         case runValidation res of
             Left es
                 -> let err = foldedSubErrorText es in fail $ T.unpack err
             Right (callback, topic)
                 -> return $ VERIFY callback topic

--------------------------------------------------------------------------------
parseList :: Parser CMD
parseList
    = do _ <- string "list"
         spaces
         cb <- fmap decodeUtf8 word
         spaces
         tp <- fmap decodeUtf8  word
         let res = (,) <$> validateCallback cb <*> validateTopic tp

         case runValidation res of
             Left es
                 -> let err = foldedSubErrorText es in fail $ T.unpack err
             Right (callback, topic)
                 -> return $ LIST callback topic

--------------------------------------------------------------------------------
parseQuit :: Parser CMD
parseQuit = QUIT <$ string "quit"

--------------------------------------------------------------------------------
parseUnknown :: Parser CMD
parseUnknown = fmap UNKNOWN word

--------------------------------------------------------------------------------
getCMD :: IO CMD
getCMD
    = do line <- B.getLine
         either fail return $ parseOnly cmdParser line

--------------------------------------------------------------------------------
terminalHub :: IO (Hub IO)
terminalHub
    = do hSetBuffering stdout NoBuffering
         return Hub
                { hubParamGet     = terminalParamGet
                , hubVerification = terminalVerification
                }

--------------------------------------------------------------------------------
terminalApp :: EventStore IO -> Hub IO -> IO ()
terminalApp store hub = loop where
  loop = do T.putStr "pubsubhub > "
            cmd <- getCMD
            case cmd of
                SUBSCRIBE
                    -> subscribing store hub >> loop

                VERIFY cb tp
                    -> do mSub <- unverifiedSub store cb tp
                          traverse_ (verifying store hub) mSub
                          loop

                LIST cb tp
                    -> do listing store cb tp
                          loop

                QUIT
                    -> return ()

                UNKNOWN x
                    | B.null x  -> loop
                    | otherwise -> reportUnknownCmd x >> loop

--------------------------------------------------------------------------------
reportUnknownCmd :: B.ByteString -> IO ()
reportUnknownCmd cmd
    = B.putStrLn ("Unknown '" <> cmd <> "' command" )

--------------------------------------------------------------------------------
subscribing :: EventStore IO -> Hub IO -> IO ()
subscribing store h
    = do r <- execute h Subscribing
         case r of
             Left (SubError es)
                 -> T.putStrLn $ foldedSubErrorText es
             Right (SubEvent sub)
                 -> storeAddEvent store
                    (subCallback sub)
                    (subTopic sub)
                    (SubEvent sub)

--------------------------------------------------------------------------------
verifying :: EventStore IO -> Hub IO -> Sub -> IO ()
verifying store hub sub
    = do let challenge = "challenge"
             url       = makeVerificationURL challenge sub

         verified <- verifySubscription ver challenge url
         if verified
             then storeAddEvent store
                  (subCallback sub)
                  (subTopic sub)
                  SubVerified
             else storeAddEvent store
                  (subCallback sub)
                  (subTopic sub)
                  SubVerifyFailed
  where
    ver = hubVerification hub

--------------------------------------------------------------------------------
listing :: EventStore IO -> Callback -> Topic -> IO ()
listing store cb tp = runEffect (src >-> map toString >-> stdoutLn)
  where
    src = storeSource store cb tp

    toString :: Slot -> String
    toString (Slot _ _ ev _)
        = case ev of
              SubEvent sub
                  | Subscribe <- subMode sub -> "SUBSCRIBE"
                  | otherwise                -> "UNSUBSCRIBE"
              SubVerified     -> "VERIFY"
              SubVerifyFailed -> "VERIFY_FAILED"

--------------------------------------------------------------------------------
terminalParamGet :: ParamGet IO
terminalParamGet
    = ParamGet
      { getRequiredParam = _requireParam
      , getOptionalParam = _optionalParam
      }
  where
    _requireParam key
        = do T.putStr (key <> ": ")
             T.getLine

    _optionalParam key
        = do T.putStr (key <> " (optional): ")
             v <- T.getLine
             return $
                 if T.length v == 0
                 then Nothing
                 else Just v

--------------------------------------------------------------------------------
terminalVerification :: Verification IO
terminalVerification = Verification go where
  go _ url
      = do let urlTxt = T.pack $ exportURL url
               mode   = lookup "hub.mode" $ url_params url

           case mode of
               Nothing
                   -> do T.putStrLn ("Error: hub.mode param missing")
                         return False
               Just m
                   -> let msg = "Validate " <>
                                urlTxt      <>
                                " in "      <>
                                T.pack m    <>
                                " mode ? (yes|no)"

                          loop = do T.putStr msg
                                    v <- T.getLine
                                    case v of
                                        "yes" -> return True
                                        "no"  -> return False
                                        _     -> loop in
                      loop


--------------------------------------------------------------------------------
terminalChallenge :: IORef Int -> IO String
terminalChallenge ref
    = do i <- readIORef ref
         writeIORef ref (i+1)
         return $ show i

--------------------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------------------
spaces :: Parser ()
spaces = void $ many1 space

--------------------------------------------------------------------------------
word :: Parser B.ByteString
word = takeWhile (not . isSpace)
