import Control.Monad
import System.IO

main :: IO ()
main = withFile "chapter1.ppm" WriteMode $ \h -> do
        hPutStrLn h "P3"
        let nx = 200
            ny = 100
            lim = 255.99
        hPrint h (floor nx)
        hPrint h (floor ny)
        hPrint h 255
        forM_ [(y, x) | y <- [ny-1, ny - 2..0], x <- [0..nx-1]] $ \(y, x) ->
            let r = x / nx
                g = y / ny 
                b = 0.2
                vals = show . floor <$> [r*lim, g*lim, b*lim]
            in hPutStrLn h $ unwords vals
