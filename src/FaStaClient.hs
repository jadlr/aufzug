{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module FaStaClient where

import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson
import Data.Aeson.Types
import Data.Proxy
import Data.Time.LocalTime
import GHC.Generics
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client
import System.Environment (getEnv)

type Auth = Header "Authorization" String

-- TODO missing equipmentnumbers and area on /facilites..probably need an instance of `ToHttpApiData`
type FaStaApi = "stations"    :> Auth :> Capture "stationnumber" Integer :> Get '[JSON]  Station
           :<|> "disruptions" :> Auth :> QueryParam "type" String :> QueryParam "equipmentnumber" Integer :> QueryParam "stationnumber" Integer :> Get '[JSON] [Disruption]
           :<|> "disruptions" :> Auth :> Capture "disruptionnumber" Integer :> Get '[JSON]  Disruption
           :<|> "facilities"  :> Auth :> QueryParam "type" String :> QueryParam "state" String :> QueryParam "stationnumber" Integer :> Get '[JSON] [Facility]
           :<|> "facilities"  :> Auth :> Capture "equipmentnumber" Integer :> Get '[JSON]  Facility

faStaApi :: Proxy FaStaApi
faStaApi = Proxy

stations    :: Maybe String -> Integer -> Manager -> BaseUrl -> ClientM  Station
disruptions :: Maybe String -> Maybe String -> Maybe Integer -> Maybe Integer -> Manager -> BaseUrl -> ClientM [Disruption]
disruption  :: Maybe String -> Integer -> Manager -> BaseUrl -> ClientM  Disruption
facilities  :: Maybe String -> Maybe String -> Maybe String -> Maybe Integer -> Manager -> BaseUrl -> ClientM [Facility]
facility    :: Maybe String -> Integer -> Manager -> BaseUrl -> ClientM  Facility
stations :<|> disruptions :<|> disruption :<|> facilities :<|> facility = client faStaApi

-- test run
run :: IO ()
run = do
  manager <- newManager tlsManagerSettings
  token <- getEnv "FASTA_TOKEN"
  res <- runExceptT (facility (Just ("Bearer " ++ token)) 10502143 manager (BaseUrl Https "api.deutschebahn.com" 443 "/fasta/v1"))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right d -> do
      print d

coordinatePrefix :: String -> String
coordinatePrefix "coordinateType" = "type"
coordinatePrefix s = s

data GeographicCoordinates =
  GeographicCoordinates { coordinateType :: String
                        , coordinates :: (Float, Float)
                        } deriving (Show, Generic)

instance FromJSON GeographicCoordinates where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = coordinatePrefix })

disruptionPrefix :: String -> String
disruptionPrefix "disruptionType" = "type"
disruptionPrefix "disruptionState" = "state"
disruptionPrefix "disruptionStationNumber" = "stationnumber"
disruptionPrefix "disruptionEquipmentNumber" = "equipmentnumber"
disruptionPrefix s = s

data Disruption =
  Disruption { disruptionEquipmentNumber :: Integer
             , geographicCoordinates :: GeographicCoordinates
             , disruptionnumber :: Integer
             , description :: Maybe String
             , outOfServiceTo :: Maybe ZonedTime
             , disruptionType :: String
             , outOfServiceReason :: Maybe String
             , outOfServiceOn :: Maybe ZonedTime
             , furtherDescription :: Maybe String
             , plannedCompletion :: Maybe ZonedTime
             , lastUpdate :: ZonedTime
             , disruptionStationNumber :: Integer
             , disruptionState :: String
             } deriving (Show, Generic)

instance FromJSON Disruption where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = disruptionPrefix })

stationPrefix :: String -> String
stationPrefix "stationName" = "name"
stationPrefix "stationNumber" = "stationnumber"
stationPrefix "stationFacilities" = "facilities"
stationPrefix s = s

data Station =
  Station { stationName :: String
          , stationNumber :: Integer
          , stationFacilities :: [Facility]
          } deriving (Show, Generic)

instance FromJSON Station

facilityPrefix :: String -> String
facilityPrefix "facilityEquipmentNumber" = "equipmentnumber"
facilityPrefix "facilityDescription" = "description"
facilityPrefix "facilityStationNumber" = "stationnumber"
facilityPrefix "facilityState" = "state"
facilityPrefix "facilityType" = "type"
facilityPrefix s = s

data Facility =
  Facility { facilityEquipmentNumber :: Integer
           , geocoordY :: Maybe Float
           , geocoordX :: Maybe Float
           , facilityDescription :: Maybe String
           , facilityStationNumber :: Integer
           , facilityState :: String
           , facilityType :: String
           } deriving (Show, Generic)

instance FromJSON Facility where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = facilityPrefix })
