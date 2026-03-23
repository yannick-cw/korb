module Storage where

import Cli
import Data.Aeson
import Data.Text.IO qualified as TIO
import Errors
import GHC.Generics
import System.Directory (XdgDirectory (XdgConfig), createDirectoryIfMissing, getXdgDirectory)
import System.FilePath ((</>))

configDir :: IO FilePath
configDir = getXdgDirectory XdgConfig "korb"

data CurrentStore = CurrentStore {wwIdent :: WwIdent, zipCode :: ZipCode}
  deriving stock (Generic, Show)
instance ToJSON CurrentStore
instance FromJSON CurrentStore

writeSettings :: WwIdent -> ZipCode -> IOE FileError CurrentStore
writeSettings (WwIdent wwIdent) (ZipCode zipCode) =
  liftIOE FileError $
    do
      dir <- configDir
      createDirectoryIfMissing True dir
      TIO.writeFile (dir </> "selected_store") wwIdent
      TIO.writeFile (dir </> "selected_zip") zipCode
      pure $ CurrentStore (WwIdent wwIdent) (ZipCode zipCode)

readSettings :: IOE FileError CurrentStore
readSettings =
  liftIOE FileReadError $
    do
      dir <- configDir
      wwIdent <- WwIdent <$> TIO.readFile (dir </> "selected_store")
      zipCode <- ZipCode <$> TIO.readFile (dir </> "selected_zip")
      pure $ CurrentStore wwIdent zipCode
