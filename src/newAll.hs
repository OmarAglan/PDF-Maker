
inputUrl <- getInputUrl -- IO
runMode <- getRunMode
inputText <- loadUrl inputUrl -- IO 
lemmata <- getLemmataToLoad runMode inputText
urlsToLoad <- (concat . map) (getPossibleUrls inputUrl) lemmata
[(url,text)] <- map_parallel loadUrl urlsToLoad -- Parallel IO
parseTree <- generateParseTree [(url,text)] runmode inputText inputUrl
[imagename] <- getImagenames parseTree
latexBodyMVar -> process_fork getLaTeXBody parseTree -- Process Fork
Map imagename [imageUrlToLoad] <- fromList (map (getPossibleImageUrls inputUrl))
textAuthorUrls <- textAuthorUrls [url] runmode inputUrl
[(url2,text2)] <- map_parallel loadUrl imageUrlsToLoad -- Parallel IO
Map imagename (url,imagedata) <- getImageData [(url2,text2)] (Map imagename [imageUrlToLoad])
imageAuthorUrls <- getPossibleImageAuthorUrls (Map imagename (url,imagedata))
[(url3,text3)] <- map_parallel loadUrl (textAuthorUrls ++ imageAuthorUrls) -- Parallel IO
textAuthors <- getTextAuthors parseTree [(url3,text3)] inputUrl:[url]
imageAuthors <- getImageAuthors parseTree [(url3,text3)] (Map imagename (url,imagedata))
latexBody <- join latexBodyMVar  -- Process Join
result <- getResult textAuthors latexBody imageAuthors
