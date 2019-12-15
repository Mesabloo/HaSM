import System.Exit
import System.Info

main :: IO ()
main = do
    print (arch <> " is not supported.")
    exitFailure