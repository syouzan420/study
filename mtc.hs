import System.IO(IOMode(..), openFile, hClose, hGetContents, hSetEncoding, utf8, hPutStr)
import Data.Char(toLower)

txPass :: FilePath
txPass = "in.txt"

tgPass :: FilePath
tgPass = "out.txt"

convert :: String -> String
convert con = let swd = sortWords$getWords con 
                  sct = sortCounts$countWords swd []
                  fls = map showCounts sct
               in unwords fls 

showCounts :: Cwd -> String
showCounts (wd,co) = "["++ wd ++ " :" ++ (show co) ++ "]"

type Lwd = (String, Int)
type Cwd = (String, Int)

getWords :: String -> [Lwd]
getWords doc = let lns = lines doc
                   wls = makeWordList (init lns) 0
                in wls

makeWordList :: [String] -> Int -> [Lwd]
makeWordList [] _ = []
makeWordList (l:ls) i = (zip awords (repeat i))++(makeWordList ls (i+1))
  where awords = map ajustWord (ajustSentence$words l)

ajustSentence :: [String] -> [String]
ajustSentence (x:xs)
  | x=="I" || elem x spNouns = x:xs
  | otherwise = ((toLower (head x)):(tail x)):xs

spNouns :: [String]
spNouns = ["Rei","Brown","Lucy","Hana","Ema","Makiko","Wakaba","Japanese"]

ajustWord :: String -> String
ajustWord wd = let lsc = last wd
                   hdc = head wd
                in if elem lsc lastMarks then 
                          if elem hdc headMarks then tail$init wd else init wd
                                         else
                          if elem hdc headMarks then tail wd else wd 

lastMarks :: [Char]
lastMarks = ".?,!\"\'"

headMarks :: [Char]
headMarks = "\"\'"

sortWords :: [Lwd] -> [Lwd]
sortWords [] = []
sortWords ((wd,ls):xs) = smaller ++ [(wd,ls)] ++ larger 
  where smaller = sortWords [(w,l)| (w,l)<-xs, w<=wd] 
        larger = sortWords [(w,l)| (w,l)<-xs, w>wd]

countWords :: [Lwd] -> [Cwd] -> [Cwd]
countWords [] acc  = acc 
countWords ((wd,_):xs) acc
  | acc == [] = countWords xs [(wd,1)]
  | (fst$head acc) == wd = countWords xs ((wd,(snd$head acc)+1):(tail acc))
  | otherwise = countWords xs ((wd,1):acc)

sortCounts :: [Cwd] -> [Cwd]
sortCounts [] = []
sortCounts ((wd,co):xs) = larger ++ [(wd,co)] ++ smaller
  where smaller = sortCounts [(w,c)| (w,c)<-xs, c<=co]
        larger = sortCounts [(w,c)| (w,c)<-xs, c>co]

makeLists :: [Cwd] -> [String] -> [String]
makeLists [] acc = acc 
makeLists ((wd,_):xs) acc
  | acc == [] = makeLists xs [wd]
  | (head acc) == wd = makeLists xs acc
  | otherwise = makeLists xs (wd:(tail acc))


main :: IO ()
main = do
  hin <- openFile txPass ReadMode
  hout <- openFile tgPass WriteMode
  hSetEncoding hin utf8
  con <- hGetContents hin 
  let res = convert con
  hSetEncoding hout utf8
  hPutStr hout res
  mapM_ putStrLn $ lines res
  hClose hin
  hClose hout
