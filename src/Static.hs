{-# LANGUAGE TemplateHaskell #-}
{-DHUN| latex and config files embedded into the binary application . DHUN-}
module Static where
import Data.FileEmbed
import Data.ByteString
import Data.ByteString.UTF8
import System.Directory
import System.FilePath

--{-DHUN| webImage DHUN-}
 
--mainImage :: [(FilePath, ByteString)]
--mainImage = $(embedFile "src/mainImage.png")

 
{-DHUN| needed LaTeX Headers DHUN-}
 
headerFiles :: [(FilePath, ByteString)]
headerFiles = $(embedDir "./document/headers")
 
{-DHUN| language configuration file of mediawiki2latex DHUN-}
 
babelFiles :: [(FilePath, ByteString)]
babelFiles = $(embedDir "./src/babel")
 
{-DHUN| function to create the necessary directories for the LaTeX tree in the temporary directory. The pathname of the temporary directory has to be supplied as first parameter DHUN-}
 
createDirectories :: String -> IO [()]
createDirectories pathname
  = mapM
      (\ s ->
         (createDirectoryIfMissing True (pathname ++ "/document/" ++ s)))
      ["images", "headers", "main", "formulas"]
 
{-DHUN| function to write files to an already created directory in the temporary directory. The first parameter is the pathname of the temporary directory. The second parameter is a list tuples. The first element of each tuple is the relative pathname of the file to be created. The second element of each tuple is the binary data to be written into the file DHUN-}
 
writeFiles ::
           String -> [(FilePath, Data.ByteString.ByteString)] -> IO ()
writeFiles pathname filelist
  = do _ <- (mapM
               (\ (name, content) ->
                  Data.ByteString.writeFile (pathname </> name) content)
               filelist)
       return ()
 
{-DHUN| the header of the main latex file DHUN-}
 
latexHeader :: ByteString
latexHeader = $(embedFile "./latex/my-head.tex")
 
{-DHUN| the footer of the main latex file DHUN-}
 
latexFooter :: ByteString
latexFooter = $(embedFile "./latex/my-tail.tex")
 
{-DHUN| the preamble of the list of figures in the main latex file DHUN-}
 
latexPreamble :: ByteString
latexPreamble = $(embedFile "./latex/preamble.tex")
 
{-DHUN| the header for a latex file which contains only a single comlumn of a table. This header is used when precompiling latex table columns in order to calculate optimized column widths for each table automaticaly DHUN-}
 
latexTableHeader :: ByteString
latexTableHeader = $(embedFile "./latex/my-tabhead.tex")
 
{-DHUN| the header for a latex file which contains only a single comlumn of a table. This header is used when precompiling latex table columns in order to calculate optimized column widths for each table automaticaly DHUN-}
 
latexTableFooter :: ByteString
latexTableFooter = $(embedFile "./latex/my-tabtail.tex")
 
{-DHUN| the templates.user file. This file contains a mapping of mediawikis templates to latex commands. This is the default file. It is used when suppliing the -i command line option. The user may override it with the -t command line option. This file contains a list of lists in Haskell notaion. Each sublists contains strings. The first string in each sublist is the name of the template as it is used in the wiki notation, the second string is the name of the latex command to be called. The following strings are the names of the parameters in wiki notation which should be passed to the latex command in the order they are denoted. DHUN-}
 
userTemplates :: String
userTemplates
  = toString $(embedFile "./latex/templates.user")
 
{-DHUN| a function to write all files that are embedded into the binary application which are needed for the LaTeX tree in the temporary directory to disk. The first parameter is the name of the temporary directory in which the latex tree which the necessary files shall be created.  DHUN-}
 
extract :: String -> IO ()
extract pathname
  = do _ <- createDirectories pathname
       writeFiles (pathname ++ "/document/headers/") headerFiles

