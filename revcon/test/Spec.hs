import Lib

main :: IO ()
main = listScripts All >>= runSuites . getSuites >>= evaluate
