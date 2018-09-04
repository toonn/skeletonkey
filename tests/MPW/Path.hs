{-# LANGUAGE CPP #-}
module MPW.Path where

import System.FilePath (replaceFileName)

mpwTestsXml = replaceFileName __FILE__ "mpw_tests.xml"
