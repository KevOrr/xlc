module XLam
  ( evaluateWorkbook
  , writeWorkbook
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Search as BSL (replace)
import Data.Functor ((<&>))
import Data.String.Conversions (cs)
import Data.Text (Text, strip)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (canonicalizePath)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import qualified Codec.Archive.Zip as Zip
import Data.Maybe (fromJust)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (nominalDiffTimeToSeconds)

evaluateWorkbook :: FilePath -> IO (Either Text Text)
evaluateWorkbook fp = do
  fpAbs <- canonicalizePath fp
  readProcessWithExitCode "osascript" ["invoke.scpt", fpAbs] "" <&> \case
    (ExitFailure _, _, stderr) -> Left $ cs stderr
    (ExitSuccess, stdout, _) -> Right . strip $ cs stdout

fixupSheet :: BS.ByteString -> BS.ByteString -> BS.ByteString
fixupSheet formula =
  cs . BSL.replace valueElement ("" :: BSL.ByteString) . cs . BSL.replace sentinel (escape formula)
  where
    escape = BSL.replace "<" ("&lt;" :: BSL.ByteString) . cs . BSL.replace "&" ("&amp;" :: BSL.ByteString)
    sentinel = "_xlfn.LAMBDA(_xlpm.x, _xlpm.x)(45613894717945)"
    valueElement = "<v>45613894717945</v>"

sheetPath :: FilePath
sheetPath = "xl/worksheets/sheet1.xml"

writeWorkbook :: FilePath -> FilePath -> Text -> IO ()
writeWorkbook fpTemplate fpOut formula = do
  ar <- Zip.toArchive <$> BSL.readFile fpTemplate
  let
      sheet = Zip.fromEntry . fromJust $ Zip.findEntryByPath sheetPath ar
      encodedFormula = encodeUtf8 formula
      sheet' = fixupSheet encodedFormula (cs sheet)
  time <- floor . nominalDiffTimeToSeconds <$> getPOSIXTime
  let sheetEntry = Zip.toEntry sheetPath time (cs sheet')
      ar' = Zip.addEntryToArchive sheetEntry ar
  BSL.writeFile fpOut $ Zip.fromArchive ar'
