{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module StaDaClient where

import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client
import System.Environment (getEnv)

type Auth = Header "Authorization" String

type StaDaApi = "stations" :> Auth :> QueryParam "offset" Integer :> QueryParam "limit" Integer :> Get '[JSON] QueryResult

staDaApi :: Proxy StaDaApi
staDaApi = Proxy

stations :: Maybe String -> Maybe Integer -> Maybe Integer -> Manager -> BaseUrl -> ClientM QueryResult
stations = client staDaApi

-- test run
run' :: IO ()
run' = do
  manager <- newManager tlsManagerSettings
  token <- getEnv "STADA_TOKEN"
  res <- runExceptT (stations (Just ("Bearer " ++ token)) (Just 50) (Just 50) manager (BaseUrl Https "api.deutschebahn.com" 443 "/stada/v2"))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right d -> do
      print d

-- types

data QueryResult = QueryResult { offset :: Integer
                               , limit :: Integer
                               , total :: Integer
                               , result :: [Station]
                               } deriving (Eq, Show, Generic)

instance FromJSON QueryResult


data MailingAddress = MailingAddress { city :: String
                                     , zipcode :: String
                                     , street :: String
                                     } deriving (Eq, Show, Generic)

instance FromJSON MailingAddress

data Station = Station { number :: Integer
                       , name :: String
                       , mailingAddress :: MailingAddress
                       , category :: Integer
                       , hasParking :: Bool
                       , hasBicycleParking :: Bool
                       , hasLocalPublicTransport :: Bool
                       , hasPublicFacilities :: Bool
                       , hasLockerSystem :: Bool
                       , hasTaxiRank :: Bool
                       , hasTravelNecessities :: Bool
                       , hasSteplessAccess :: String
                       , hasMobilityService :: String
                       , federalState :: String
                       } deriving (Eq, Show, Generic)

instance FromJSON Station

-- {
--   "number": 1,
--   "name": "Aachen Hbf",
--   "mailingAddress": {
--     "city": "Aachen",
--     "zipcode": "52064",
--     "street": "Bahnhofplatz 2a"
--   },
--   "category": 2,
--   "hasParking": true,
--   "hasBicycleParking": true,
--   "hasLocalPublicTransport": true,
--   "hasPublicFacilities": true,
--   "hasLockerSystem": true,
--   "hasTaxiRank": true,
--   "hasTravelNecessities": true,
--   "hasSteplessAccess": "yes",
--   "hasMobilityService": "Ja, um Voranmeldung unter 01806 512 512 wird gebeten",
--   "federalState": "Nordrhein-Westfalen",
--   "regionalbereich": {
--     "number": 4,
--     "name": "RB West",
--     "shortName": "RB W"
--   },
--   "aufgabentraeger": {
--     "shortName": "Zweckverband Nahverkehr Rheinland GmbH",
--     "name": "NVR"
--   },
--   "localServiceStaff": {
--     "availability": {
--       "monday": {
--         "fromTime": "06:00",
--         "toTime": "22:30"
--       },
--       "tuesday": {
--         "fromTime": "06:00",
--         "toTime": "22:30"
--       },
--       "wednesday": {
--         "fromTime": "06:00",
--         "toTime": "22:30"
--       },
--       "thursday": {
--         "fromTime": "06:00",
--         "toTime": "22:30"
--       },
--       "friday": {
--         "fromTime": "06:00",
--         "toTime": "22:30"
--       },
--       "saturday": {
--         "fromTime": "06:00",
--         "toTime": "22:30"
--       },
--       "sunday": {
--         "fromTime": "06:00",
--         "toTime": "22:30"
--       },
--       "holiday": {
--         "fromTime": "06:00",
--         "toTime": "22:30"
--       }
--     }
--   },
--   "timeTableOffice": {
--     "email": "DBS.Fahrplan.NordrheinWestfalen@deutschebahn.com",
--     "name": "Bahnhofsmanagement Köln"
--   },
--   "szentrale": {
--     "number": 15,
--     "publicPhoneNumber": "0203/30171055",
--     "name": "Duisburg Hbf"
--   },
--   "stationManagement": {
--     "number": 45,
--     "name": "Düsseldorf"
--   },
--   "evaNumbers": [
--     {
--       "number": 8000001,
--       "geographicCoordinates": {
--         "type": "Point",
--         "coordinates": [
--           6.091499,
--           50.7678
--         ]
--       },
--       "isMain": true
--     }
--   ],
--   "ril100Identifiers": [
--     {
--       "rilIdentifier": "KA",
--       "isMain": true,
--       "hasSteamPermission": true,
--       "geographicCoordinates": {
--         "type": "Point",
--         "coordinates": [
--           6.091201396,
--           50.767558188
--         ]
--       }
--     }
--   ]
-- }
