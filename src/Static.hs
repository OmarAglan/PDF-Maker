{-# LANGUAGE TemplateHaskell #-}

-- | latex and config files embedded into the binary application .
module Static where

import Data.ByteString
import Data.ByteString.UTF8
import Data.FileEmbed
import System.Directory
import System.FilePath

-- {-| webImage -}

-- mainImage :: [(FilePath, ByteString)] mainImage = $(embedFile
-- "src/mainImage.png")

-- | needed LaTeX Headers
headerFiles :: [(FilePath, ByteString)]
headerFiles = $(embedDir "./document/headers")

-- | language configuration file of mediawiki2latex
babelFiles :: [(FilePath, ByteString)]
babelFiles = $(embedDir "./src/babel")

-- | test cases for self test of mediawiki2latex
testFiles :: [(FilePath, ByteString)]
testFiles = $(embedDir "./test")

-- | function to create the necessary directories for the LaTeX tree in the
-- temporary directory. The pathname of the temporary directory has to be
-- supplied as first parameter
createDirectories :: String -> IO [()]
createDirectories pathname =
  mapM
    ( \s ->
        (createDirectoryIfMissing True (pathname ++ "/document/" ++ s))
    )
    ["images", "headers", "main", "formulas"]

-- | function to write files to an already created directory in the temporary
-- directory. The first parameter is the pathname of the temporary directory.
-- The second parameter is a list tuples. The first element of each tuple is the
-- relative pathname of the file to be created. The second element of each tuple
-- is the binary data to be written into the file
writeFiles ::
  String -> [(FilePath, Data.ByteString.ByteString)] -> IO ()
writeFiles pathname filelist =
  do
    _ <-
      ( mapM
          ( \(name, content) ->
              Data.ByteString.writeFile (pathname </> name) content
          )
          filelist
        )
    return ()

-- | the header of the main latex file
latexHeader :: ByteString
latexHeader = $(embedFile "./latex/my-head.tex")

-- | the footer of the main latex file
latexFooter :: ByteString
latexFooter = $(embedFile "./latex/my-tail.tex")

-- | the preamble of the list of figures in the main latex file
latexPreamble :: ByteString
latexPreamble = $(embedFile "./latex/preamble.tex")

-- | the header for a latex file which contains only a single comlumn of a
-- table. This header is used when precompiling latex table columns in order to
-- calculate optimized column widths for each table automaticaly
latexTableHeader :: ByteString
latexTableHeader = $(embedFile "./latex/my-tabhead.tex")

-- | the header for a latex file which contains only a single comlumn of a
-- table. This header is used when precompiling latex table columns in order to
-- calculate optimized column widths for each table automaticaly
latexTableFooter :: ByteString
latexTableFooter = $(embedFile "./latex/my-tabtail.tex")

-- | the templates.user file. This file contains a mapping of mediawikis
-- templates to latex commands. This is the default file. It is used when
-- suppliing the -i command line option. The user may override it with the -t
-- command line option. This file contains a list of lists in Haskell notaion.
-- Each sublists contains strings. The first string in each sublist is the name
-- of the template as it is used in the wiki notation, the second string is the
-- name of the latex command to be called. The following strings are the names
-- of the parameters in wiki notation which should be passed to the latex
-- command in the order they are denoted.
userTemplates :: String
userTemplates =
  toString $(embedFile "./latex/templates.user")

-- | a function to write all files that are embedded into the binary application
-- which are needed for the LaTeX tree in the temporary directory to disk. The
-- first parameter is the name of the temporary directory in which the latex
-- tree which the necessary files shall be created.
extract :: String -> IO ()
extract pathname =
  do
    _ <- createDirectories pathname
    writeFiles (pathname ++ "/document/headers/") headerFiles
