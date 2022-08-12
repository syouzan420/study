getIndex :: Char -> String -> Int
getIndex _ [] = (-1) 
getIndex ch (x:xs) =
  if(ch==x) then 0 else if(xs==[]) then (-1)
                                   else (getIndex ch xs)+1
