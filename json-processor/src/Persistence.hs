module Persistence (loadState, updateState) where

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import System.Directory (doesFileExist, createDirectoryIfMissing)
import Data.Time.Clock (UTCTime, getCurrentTime)

stateFilePath :: FilePath
stateFilePath = "data/state.json"

loadState :: IO Value
loadState = do
  createDirectoryIfMissing True "data"
  exists <- doesFileExist stateFilePath
  if exists 
    then do
      content <- BL.readFile stateFilePath
      case eitherDecode content of
        Right state -> return state
        Left _      -> return defaultState
    else return defaultState
  where
    defaultState = object []

saveState :: Value -> IO ()
saveState state = BL.writeFile stateFilePath (encode state)

updateState :: String -> Maybe Value -> Value -> IO Value
updateState op output currentState = do
  now <- getCurrentTime
  let newState = object
        [ "operation" .= op
        , "timestamp" .= show now
        , "output" .= output
        ]
  saveState newState
  return newState