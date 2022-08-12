import Data.Char
import Data.Ratio
import System.IO (hSetBuffering,hFlush,stdout,BufferMode(NoBuffering))

data Typ = Kazu | Mozi
              deriving (Show,Eq)

-- separate a kou into Kazu && Mozi
kou :: String -> [(Typ,String)]
kou = kouz []

kou2 :: String -> [String]
kou2 = kouz2 []

kouz :: [(Typ,String)] -> String -> [(Typ,String)]
kouz acc [] = acc 
kouz acc (x:xs)
  | acc==[] = if(isDigit x) then kouz [(Kazu,[x])] xs else kouz [(Mozi,[x])] xs
  | length acc==1 && (fst$head acc)==Kazu
        = if(isDigit x) then kouz [(Kazu,(snd$head acc)++[x])] xs
                        else kouz (acc++[(Mozi,[x])]) xs
  | otherwise = if(isDigit x) then []
                              else kouz (acc++[(Mozi,[x])]) xs

kouz2 :: [String] -> String -> [String]
kouz2 acc [] = acc
kouz2 acc (x:xs)
  | acc==[] = if(isDigit x) then kouz2 [[x]] xs else kouz2 ["1",[x]] xs
  | length acc==1 = if(isDigit x) then kouz2 [head acc++[x]] xs
                                  else kouz2 (acc++[[x]]) xs
  | otherwise = if(isDigit x) then []
                              else kouz2 (acc++[[x]]) xs

-- eliminate parenthesis and replace it the mark @
elipa :: String -> [String]
elipa = elipaz False 0 [] [] 

elipaz :: Bool -> Int -> String -> String -> String -> [String]
elipaz _ _ ex _ [] = [ex]
elipaz False _ ex acc (x:xs) = if(x=='(') then elipaz True 1 ex [] xs
                                         else elipaz False 0 (ex++[x]) [] xs
elipaz True i ex acc (x:xs)
  | x=='(' = elipaz True (i+1) ex (acc++[x]) xs
  | x==')' = if(i==1) then acc:(elipaz False 0 (ex++"@") [] xs) 
                      else elipaz True (i-1) ex (acc++[x]) xs
  | otherwise = elipaz True i ex (acc++[x]) xs

-- separate operator (+,-) and kou
ankou :: [String] -> [(Char,String)]
ankou = map (\inp -> case (head inp) of
                       '+' -> ('+',tail inp)
                       '-' -> ('-',tail inp)
                       _   -> ('+',inp))

testkous :: [String]
testkous =["8","-3","2a","-3a","12b","15","11a","-4b"]

opnkou :: [String] -> [(Char,String)]
opnkou [] = []
opnkou (x:xs)
  | (head x=='+')||(head x=='-') = (head x,tail x):opnkou xs
  | otherwise = ('+',x):opnkou xs 

--kous :: [[(Typ,String)]] -> [[String]]
--kous = map (map snd)

kous :: [String] -> [[String]]
kous ls = map (map snd)$map (kou.snd) (opnkou ls) 

kous2 :: [String] -> [[String]]
kous2 ls = map (kou2.snd) (opnkou ls)

idnln :: [[String]] -> [(Int,Int)]
idnln = foldl (\acc x->acc++
          [(if(null acc) then 0 else (fst$last acc)+1,length x)]) []

sml :: [(Int,Int)] -> [[Int]]
sml [] = []
sml ((id,lng):xs) = [id:(map fst (filter (\(i,l)->lng==l) xs))]
                      ++(sml (filter (\(i,l)-> l/=lng) xs))

smlkou :: [String] -> [[Int]]
smlkou = sml.idnln.kous2

strs :: [[Int]] -> [String] -> [[String]]
strs [] _ = []
strs (x:xs) ls = (map ((!!) ls) x):(strs xs ls)

-------------------------------------------------------



-- remove spaces
rmsp :: String -> String
rmsp [] = []
rmsp (x:xs) = if(x==' ') then rmsp xs else x:rmsp xs

expr :: [String] -> String
expr [] = "0"
expr ls = if((head$head ls)=='+') then concat ((tail$head ls):tail ls)
                                  else concat ls

showk :: Int -> String
showk n
  | n==0 = "zero"
  | n==1 = ""
  | otherwise = show n

toNum :: String -> Int
toNum [] = 0
toNum (x:xs) = if(x=='+') then read xs else read (x:xs)

-- remove the variable from the kou
rmvar :: Char -> String -> String
rmvar v kou =
  let ki = foldl (\acc c -> if(c==v) then acc else acc++[c]) [] kou
   in if(ki=="-" || ki=="+" || ki=="") then ki++"1" else ki

--interchange the first operator + -
chdir :: String -> String
chdir [] = []
chdir inp 
  | head inp=='+' = '-':(tail inp)
  | head inp=='-' = '+':(tail inp)
  | otherwise = '-':inp

--separates whether kous include the variable
vrnel :: Char -> [String] -> ([String],[String])
vrnel = vrnelz [] [] 

vrnelz :: [String] -> [String] -> Char -> [String] -> ([String],[String])
vrnelz ags els _ [] = (ags,els)
vrnelz ags els ch (x:xs)
  = if(elem ch x) then vrnelz (ags++[x]) els ch xs
                  else vrnelz ags (els++[x]) ch xs

-- separate operators (+,-)
sepop :: String -> [String]
sepop [] = []
sepop (x:xs) = 
  if(x=='-' || x=='+') then let rs = sepopz [] xs
                             in (x:(head rs)):(tail rs)
                       else sepopz [] (x:xs)

sepopz :: String -> String -> [String]
sepopz acc [] = [rmsp acc]
sepopz acc (x:xs)
  | x=='+' || x=='-' = (rmsp acc):(sepopz [x] xs) 
  | otherwise = sepopz (acc++[x]) xs

sepop2 :: String -> [String]
sepop2 [] = []
sepop2 (x:xs)
  | x=='+' || x=='-' = []:(x:hs):ts
  | otherwise = if(xs==[]) then [[x]] else (x:hs):ts
  where (hs:ts)=map rmsp (sepop2 xs)

-- separate left expression and right expression with '='
sepeq :: String -> (String,String)
sepeq = sepeqz []

sepeqz :: String -> String -> (String,String)
sepeqz acc [] = ([],[])
sepeqz acc (x:xs) = if(x=='=') then (acc,xs)
                               else sepeqz (acc++[x]) xs 

sepeq2 :: String -> (String,String)
sepeq2 inp = let (lf,rt) = foldl (\(l,r) x -> if(l/=[] && last l=='=') 
                      then (l,r++[x]) else (l++[x],r)) ([],[]) inp
              in (if(lf==[]) then lf else init lf ,rt)

sepeq3 :: String -> (String,String)
sepeq3 [] = ([],[])
sepeq3 inp = if(lp=='=') then (ip,[]) else (nf,ns++[lp])
     where ip=init inp; lp=last inp
           (nf,ns)=sepeq3 ip

sepeq4 :: String -> (String,String)
sepeq4 [] = ([],[])
sepeq4 (x:xs) = if(x=='=') then ([],xs) else (x:nf,ns)
  where (nf,ns) = sepeq4 xs

-- change the equation so that the variable get together
gtv :: String -> Char -> ([String],[String]) 
gtv inp arg = 
  let (le,re) = sepeq2 inp 
      (les,res) = (sepop le,sepop re)
      (la,lb) = vrnel arg les
      (ra,rb) = vrnel arg res
      la' = la++(map chdir ra) 
      rb' = rb++(map chdir lb)
   in (la',rb')

showeq :: ([String],[String]) -> String
showeq (lgv,rgv) = expr lgv++" = "++expr rgv

rpa :: String -> String
rpa = foldr (\c acc -> case c of '%'->'/':acc; ' '-> acc; _ -> c:acc) []

showr :: Ratio Int -> String
showr rsl = let rslm = show rsl
                lng = length rslm - 3
                ls3 = drop lng rslm
             in if(ls3=="% 1") then take lng rslm else rpa rslm 

finalDivision :: Int -> Int -> Char -> IO ()
finalDivision crv crgv v = do
  let skcr = showk crv
      scrgv = show crgv
      rsl = crgv % crv
      srsl = showr rsl
  putStr ("Finally, divide by "++skcr) >> getLine
  putStrLn ([v]++" = "++scrgv++"/"++skcr) >> getLine
  putStrLn "Answer:"
  putStrLn ([v]++" = "++srsl)

calculateNumbers :: [String] -> [String] -> Char -> IO ()
calculateNumbers rv rgv v = do
  putStr ("Then, calculate numbers") >> getLine
  let crv = sum$map toNum rv
      crgv = sum$map toNum rgv
      skcr = showk crv
  if(skcr=="zero") 
     then putStrLn "This can't solve!!" 
     else do
       if(skcr=="") then do
          putStr "Answer: "
          putStr (skcr++[v]++" = "++show crgv)
                    else do
          putStr (skcr++[v]++" = "++show crgv) >> getLine
          finalDivision crv crgv v 

separateVariables :: [String] -> [String] -> Char -> IO [String]
separateVariables lgv rgv v = do
  let rv = map (rmvar v) lgv
  if (length lgv>1) then do
    putStr ("Next, separate "++[v]++" and others") >> getLine
    putStr ("("++expr rv++")"++[v]++" = "++expr rgv) >> getLine >> return ()
                    else return ()
  return rv

gatherKous :: String -> Char -> IO ([String],[String])
gatherKous e v = do
  putStr ("first, gather terms which include "++[v]) >> getLine
  let (lgv,rgv) = gtv e v
  putStr (showeq (lgv,rgv)) >> getLine
  return (lgv,rgv)
  
enterEquation :: IO (String,Char)
enterEquation = do
  e <- putStr "Enter the equation: " >> getLine
  v <- putStr "Enter the variable: " >> getChar
  putStrLn ("solving "++e++" for "++[v]) >> getLine 
  return (e,v)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  (e,v)     <- enterEquation
  (lgv,rgv) <- gatherKous e v
  rv        <- separateVariables lgv rgv v 
  calculateNumbers rv rgv v

