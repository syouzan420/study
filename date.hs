import Data.Time(utctDay, getCurrentTime)
import Data.Time.Calendar.OrdinalDate(showOrdinalDate)
import Data.Time.LocalTime(utc,TimeZone(..),getCurrentTimeZone,getZonedTime,utcToLocalTime,ZonedTime(..),LocalTime(..))
import Data.Time.Format(formatTime,defaultTimeLocale)

day :: IO (Int, Int, Int) 
day = do
  da <- show <$> localDay <$> zonedTimeToLocalTime <$> getZonedTime
  return (read (take 4 da), read (take 2 (drop 5 da)), read (drop 8 da))

day' :: IO String
day' = showOrdinalDate <$> utctDay <$> getCurrentTime

day'' :: IO String
day'' = do
  d <- show <$> utctDay <$> getCurrentTime
  let da = drop 5 d
  let mo = take 2 da
  let dy = drop 3 da
  return ("month: "++mo++" day: "++dy)

wake :: String -> (Int, Int, Int)
wake a = (read (take 4 a), read (take 2 (drop 4 a)), read (drop 6 a))

daylist :: [Int]
daylist = [31,28,31,30,31,30,31,31,30,31,30,31]

nin :: String -> Int
nin a = let (y,m,d) = wake a
         in if(m==1) then d
                     else sum (take (m-1) daylist) + d

uru :: Int -> Bool
uru y = let r1 = mod y 4 == 0
            r2 = mod y 100 == 0
            r3 = mod y 400 == 0
         in r3 || (r1 && not r2)
