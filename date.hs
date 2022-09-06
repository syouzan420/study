import Data.Time.LocalTime(getZonedTime,ZonedTime(zonedTimeToLocalTime),LocalTime(localDay))

day :: IO String 
day = do
  da <- show <$> localDay <$> zonedTimeToLocalTime <$> getZonedTime
  return ((take 4 da) ++ (take 2 (drop 5 da)) ++ (drop 8 da))

wake :: String -> (Int, Int, Int)
wake a = (read (take 4 a), read (take 2 (drop 4 a)), read (drop 6 a))

daylist :: [Int]
daylist = [31,28,31,30,31,30,31,31,30,31,30,31]

uru :: Int -> Bool
uru y = let r1 = mod y 4 == 0
            r2 = mod y 100 == 0
            r3 = mod y 400 == 0
         in r3 || (r1 && not r2)

nen :: String -> Int
nen a = let (y,m,d) = wake a
            iuru = uru y
         in if(m==1) then d
                     else sum (take (m-1) daylist) + d 
                          + (if (m>2 && iuru) then 1 else 0)

daynow :: String -> IO Int
daynow a = do
  nday <- day
  let (y,m,d) = wake a
      (yn,_,_) = wake nday
      fyday = 365 - nen a + (if (uru y) then 1 else 0)
      lsday = nen nday 
  return (fyday + mdday (y+1) (yn-1) + lsday)

mdday :: Int -> Int -> Int
mdday c t
  | c==t = if (uru t) then 366 else 365
  | otherwise = (if(uru c) then 366 else 365) + (mdday (c+1) t)

daybf :: Int -> IO String
daybf i = do
  nday <- day
  let ds = nen nday 
      (yn,_,_) = wake nday
  if (i<ds) then return ((show yn)++(monthday (uru yn) (ds-i))) 
             else return (daybfy (yn-1) (i-ds))

daybfy :: Int -> Int -> String
daybfy y i = let yl = if (uru y) then 366 else 365
              in if (i<yl) then (show y)++(monthday (uru y) (yl-i)) 
                           else daybfy (y-1) (i-yl)

monthday :: Bool -> Int -> String
monthday u ds = md 1 u ds
  where  md m u d = let dy = daylist!!(m-1) + (if(m==2 && u) then 1 else 0)
                    in if (d > dy) then md (m+1) u (d-dy) 
                                   else (show m)++(show d) 
