{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Types where

import Data.Aeson
import Data.Aeson.TH
import GHC.Generics (Generic)

data Scientist = Scientist
  {
    sNames     :: [String]
  , sPhotoUrl  :: String
  } deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''Scientist)

scientists :: [Scientist]
scientists = [ Scientist ["Isaac", "Newton"] "https://upload.wikimedia.org/wikipedia/commons/3/39/GodfreyKneller-IsaacNewton-1689.jpg"
             , Scientist ["Albert", "Einstein"] "https://upload.wikimedia.org/wikipedia/commons/d/d3/Albert_Einstein_Head.jpg"
             , Scientist ["Gottfried", "Wilhelm", "Leibniz"] "http://apusepress.booktype.pro/early-readings-in-the-philosophy-of-science/gottfried-wilhelm-leibniz-1646-1716/static/Leibniz.jpg"
             , Scientist ["Stephen", "Hawking"] "https://upload.wikimedia.org/wikipedia/commons/e/eb/Stephen_Hawking.StarChild.jpg"
             , Scientist ["Pythagoras"] "http://www.thefamouspeople.com/profiles/images/pythagoras-4.jpg"
             , Scientist ["Wernher", "von", "Braun"] "https://upload.wikimedia.org/wikipedia/commons/5/56/Wernher_von_Braun_1960.jpg"
             ]
