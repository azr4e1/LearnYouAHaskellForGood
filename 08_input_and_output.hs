-- main = putStrLn "Hello, World"
import System.IO
import System.Random
-- withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
-- withFile' path mode f = do
--     handle <- openFile path mode
--     result <- f handle
--     hClose handle
--     return result

main = do
    withFile "./auxiliary/girlfriend.txt" ReadMode (\handle -> do
        hSetBuffering handle $ BlockBuffering (Just 2048)
        contents <- hGetContents handle
        putStr contents)


a = random (mkStdGen 100) :: (Int, StdGen)
b = random (mkStdGen 101) :: (Float, StdGen)
c = random (mkStdGen 102) :: (Bool, StdGen)
d = random (mkStdGen 103) :: (Integer, StdGen)

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen = 
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, newGen'') = random newGen'
    in (firstCoin, secondCoin, thirdCoin)


randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen =
    let (value, newGen) = random gen in value:randoms' newGen

finiteRandoms :: (RandomGen g, Random a, Integral n) => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
    let (value, newGen) = random gen
        (restOfList, finalGen) = finiteRandoms (n-1) newGen
    in (value:restOfList, finalGen)

e = randomR (1, 6) (mkStdGen 3849383) :: (Int, StdGen)
