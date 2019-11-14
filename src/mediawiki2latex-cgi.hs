import Network.CGI

cgiMain :: CGI CGIResult
cgiMain = output "Hello World!"

main :: IO ()
main = runCGI (handleErrors cgiMain)

module Main (main) where
