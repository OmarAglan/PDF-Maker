import Data.List.Split
import Data.Map.Strict hiding (map,foldr)
import Data.List hiding (lookup)
import Data.Char
import Data.List.HT (dropWhileRev)
main = do d<-readFile "brett"
          print (toBoard d)          

strip :: (Eq a) => [a] -> [a] -> [a]
strip l = dropWhileRev isBad . dropWhile isBad
  where isBad x = x `elem` l

letters::Map Integer Char
letters =fromList (zip [0..] ['a'..'h'])

bf::String->Maybe String
bf (x:'d':[]) | pre x = Just [toLower x]
bf (x:'l':[]) | pre x = Just [toUpper x]
bf ('x':'x':[]) = Nothing
bf _ = Just []
pre x = x `elem` "rnbqkp"

fun::(Integer,[(Integer,Maybe String)])->[String]->[String]
fun (r,x) acu = acu++(foldr (fun2 r) [] (reverse x))

fun2::Integer->(Integer,Maybe String)->[String]->[String]
fun2 r (c,Nothing) acu  = acu++
                            case Data.Map.lookup c letters of
                              Just z -> [[z]++(show (8-r))] 
                              _-> []
fun2 _ _ acu = acu

fun3::(Integer,[(Integer,Maybe String)])->[String]->[String]
fun3 (r,x) acu = acu++[a++(if i>0 then (show i) else "")]
  where (a,i)=(foldr (fun4 r) ([],0) (reverse x))
fun4::Integer->(Integer,Maybe String)->(String,Integer)->(String,Integer)
fun4 r (c,Just "") (acu,i) = (acu,i+1)
fun4 r (c,Just x) (acu,i) = (acu++(if i>0 then (show i) else "")++x,0)
fun4 _ _ (acu,i) = (acu,i+1)


toBoard::String->(String,String)
toBoard x = ((intercalate "/" (foldr fun3 [] (reverse l)))++" b - - 0 1" ,intercalate "," ((foldr fun [] (reverse l))::[String]))
 where
  sp=splitOn "|=" x 
  l = zip [0..] (map (\x->zip [0..] (map (bf.(strip "\n\r\t ")) ((drop 1) ((splitOn "|") x)))) (take 8 (drop 2 sp)))
  









