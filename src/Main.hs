{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
import Text.Trim
import           Control.Exception           (displayException, try)
import qualified Data.ByteString.Lazy.Char8  as L8
import           Network.HTTP.Client.Conduit (HttpException (..),
                                              HttpExceptionContent (..),
                                              parseUrlThrow, responseTimeout,
                                              responseTimeoutNone)
import           Network.HTTP.Conduit        (HttpException)
import           Network.HTTP.Simple         (Response, getResponseBody,
                                              httpLBS)
import WebURL(URL)
import Types


main :: IO ()
main = execScript $ TryOuts (Start 1) (Max 10)

getWebsiteHTML :: TryOuts -> URL -> IO ()
getWebsiteHTML (TryOuts (Start n) (Max m)) = do
    request' <- parseUrlThrow https://www.w3schools.com/tags/tag_body.asp
    let request = request' { responseTimeout = responseTimeoutNone }
    eresponse <- (try $ httpLBS request :: IO ( Either HttpException (Response L8.ByteString) ) )

    if
        n < m
    then
        case eresponse of

            Left (HttpExceptionRequest _ (StatusCodeException response _)) -> do
                print . statusException $ response
                execScript . TryOuts (Start $ n + 1) . Max $ m

            Left exception                                                 -> do
                print . descriptionException $ exception
                execScript . TryOuts (Start $ n + 1) . Max $ m

            Right response                                                 -> do
                print . getResponseBody $ response
                print . success . Attempts $ n
    else
        print . failure . Attempts $ m

success :: Attempts -> String
success attm =
    "Script was successfully executed !! ( " ++ attempts attm ++ " ) "

failure :: Attempts -> String
failure attm =
    "All execution attempts failed !! ( " ++ attempts attm ++ " ) "

attempts :: Attempts -> String
attempts (Attempts attm) =
    "Attempts :: " ++ show attm

descriptionException :: HttpException -> String
descriptionException excpt =
    "Exception Occured ( " ++ description excpt ++ " ) "

description :: HttpException -> String
description excpt =
    "Description :: " ++ displayException excpt

statusException :: Response a -> String
statusException resp =
    "Exception Occured ( " ++ status resp ++ " ) "

statusResponse :: Response a -> String
statusResponse resp =
    "Responded With ( " ++ status resp ++ " ) "

status :: Response a -> String
status resp =
    "StatusCode :: " ++ show (getResponseStatusCode resp)
    
match :: KindOfMatch -> ToMatch -> ToMatchAgainst -> a
match kindOfMatch toMatch toMatchAgainst = 
    case kindOfMatch of
        let
            result = toMatchAgainst =~ toMatch
        in
        FirstMatch                 -> 
            if trimSpaces (result :: String) /= ""
            then result :: String
            else noMatch
        MatchedAtAll               -> 
            if result :: Bool
            then result :: Bool
            else noMatch
        MatchAndTextBeforeAndAfter -> 
            if (result :: (String, String, String)) == ("", "", "")
            then result :: (String, String, String)
            else noMatch
        AllMatches                 -> 
            if (result :: [String]) == []
            then result ::  [String]
            else noMatch

noMatch :: String
noMatch =
    "No match was found !"


