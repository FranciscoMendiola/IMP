module Main (main) where

import Test.Hspec
import Control.Monad (forM_)
import Data.List (sort)
import System.Directory (listDirectory, doesFileExist, createDirectoryIfMissing)
import System.FilePath  ((</>), takeBaseName, takeExtension)
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Char8 as BSC

import Com.Syrion.Models.Lenguajes.ImpFast (scanIMP)
import Com.Syrion.Models.Lexer.Lexer (Token)

samplesDir, expectedDir :: FilePath
samplesDir  = "samples/imp"
expectedDir = "samples/expected"

main :: IO ()
main = hspec $ beforeAll_ (createDirectoryIfMissing True expectedDir) $ do
  describe "Golden tests del lexer" $ do
    files <- runIO (listDirectory samplesDir)
    let imps = sort $ filter (hasExt ".imp") files
    forM_ imps $ \f ->
      it (takeBaseName f) $
        checkOne f

checkOne :: FilePath -> Expectation
checkOne f = do
  let base    = takeBaseName f
      srcPath = samplesDir  </> f
      expPath = expectedDir </> (base ++ ".golden")

  srcExists <- doesFileExist srcPath
  srcExists `shouldBe` True

  input <- T.readFile srcPath
  case scanIMP (T.unpack input) of
    Left err   -> expectationFailure ("Lexer error en " ++ f ++ ": " ++ err)
    Right toks -> do
      let actual = BSC.pack . unlines $ map show toks
      expExists <- doesFileExist expPath
      if expExists
        then do expected <- BS.readFile expPath
                actual `shouldBe` expected
        else expectationFailure $
             "Falta golden: " ++ expPath
             ++ "\nEjecuta ./scripts/create_golden.sh para generarlo."

hasExt :: String -> FilePath -> Bool
hasExt ext fp = takeExtension fp == ext
