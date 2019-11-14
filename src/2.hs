
import NomenFull
import Data.List
import Data.Char
import Grammatik
import Fun0
import Data.Maybe
import GHC.Int          
import Data.Set (fromList,toList,member)
import Data.Time.Clock.POSIX
import qualified Data.HashTable.IO as Map
import qualified Data.Map as M
worte = [("die",Artikel Definit PluralA Nominativ),("die",Artikel Definit PluralA Akkusativ),("Katzen",Nomen "Katze" Feminin Nominativ Plural),("Katzen",Nomen "Katze" Feminin Akkusativ Plural),("Hunde",Nomen "Hund" Maskulin Nominativ Plural),("Hunde",Nomen "Hund" Maskulin Akkusativ Plural),("essen", Verb "essen" Grundform) ]



s=concat (replicate 1000000 "Hunde essen Katen.")

-- Darum will ich mein Korn und meinen Most wieder nehmen zu seiner Zeit Haupsatz
-- ich Nomen Subjekt Nominativ
-- will Hilfsverb erste person singular 
-- nehmen Verb grundform
-- mein Korn Objekt Akkusativ
-- meinen Most Objekt Akkusativ
-- und ihr meine Wolle und meinen Flachs entziehen Nebensatz; Subjekt vom Hauptsatz übernommen
-- und trenner zwischen Hauptsatz und nebensatz
-- meine Wolle Objekt Akkusativ
-- meinen Flachs Objekt Akkusativ
-- entziehen verb grundform
-- , damit sie ihre Blöße bedeckt Relativsatz ?
-- sie Subjekt Nominativ
-- bedekt Verb
-- Blöße Akkusativ Objekt


tokenizetext [] = []
tokenizetext t = sentence:(tokenizetext rest)
  where 
    sentence=takeWhile (\x->not (x `elem` ":.=")) t 
    rest=  let x=(dropWhile (\x->not (x `elem` ":.=")) t) in if x/=[] then tail x else []

letters = "abcdefghijklmnopqrstuvwxyz"++
          "ABCDEFGHIJKLMNOPQRSTUVWXYZ"++
          "öäüÄÖÜß1234567890-"

ss=[("die",[[Artikel Definit PluralA Nominativ],[Artikel Definit PluralA Akkusativ]]),("Hunde",[[Nomen "Hund" Maskulin Nominativ Plural],[Nomen "Hund" Maskulin Akkusativ Plural]]),("essen",[[Verb "essen" Grundform]]),("die",[[Artikel Definit PluralA Nominativ],[Artikel Definit PluralA Akkusativ]]),("Katzen",[[Nomen "Katze" Feminin Nominativ Plural],[Nomen "Katze" Feminin Akkusativ Plural]])]


tokenizesentence [] = [] 
tokenizesentence s = word:(tokenizesentence rest) 
  where 
    word=takeWhile (`elem` letters) s 
    rest=dropWhile (not.(`elem` letters)) (dropWhile (`elem` letters) s)
alles = []

go::[(String,Wort)]->String->[Wort]
go ((wa,wb):ws) x | wa == x = wb:(go ws x)
go (_:ws) x = (go ws x)
go [] _ = []
tokenized::[String]
tokenized = concat [concat (map (hh.(dropWhile (== [])).tokenizesentence) (tokenizetext s))]
tokenized3::String->[String]
tokenized3 y= concat [concat (map (hh.(dropWhile (== [])).tokenizesentence) (tokenizetext y))]

tokenized2::[[String]]
tokenized2 = (map (hh.(dropWhile (== [])).tokenizesentence) (tokenizetext s))

hh::[String]->[String]
hh (x:xs) = case x of
              (y:ys)->((toLower y):ys):xs
              ys->ys:xs
hh xs = xs 

pr (Nomen s g c p) = "Nomen: "++(show c)++" "++(show p)++" von "++s++" ("++(map toLower (show g))++")"
pr x = show x
main3 :: [[Wort]]
main3 = (map (go worte) tokenized)
main4 = putStrLn (body (tab (zip tokenized main3)))
body::String->String
body x = "<html><head><meta charset=\"utf-8\"><title>test</title><style>table.sourceCode, tr.sourceCode, td.lineNumbers, td.sourceCode {  margin: 0; padding: 0; vertical-align: baseline; border: none; } table.sourceCode { width: 100%; line-height: 100%; } td.lineNumbers { text-align: right; padding-right: 4px; padding-left: 4px; } td.sourceCode { padding-left: 5px; } code > span.kw { font-weight: bold; } code > span.dt { text-decoration: underline; } code > span.co { font-style: italic; } code > span.al { font-weight: bold; } code > span.er { font-weight: bold; } </style></head><body>"++x++"</body></html>"
tab::[(String,[Wort])]->String
tab t = "<table frame=\"box\" rules=\"all\"><tr><th>Wort</th><th>grammatikalische Form(en)</th></tr>"++(concat (map row t))++"</table>"
row::(String,[Wort])->String
row (x,y) = "<tr><td>"++x++"</td><td>"++(ins y)++"</td></tr>"
ins::[Wort]->String
ins y = "<ul><li>"++(intercalate "</li><li>" (map (pr) y)) ++"</li></ul>"


tab2::[(String,SatzTeil)]->String
tab2 t = "<table frame=\"box\" rules=\"all\"><tr><th>Wort</th><th>grammatikalische Form(en)</th></tr>"++(concat (map row2 t))++"</table>"
row2::(String,SatzTeil)->String
row2 (x,y) = "<tr><td>"++x++"</td><td>"++(ins2 y)++"</td></tr>"
ins2::SatzTeil->String
ins2 y = pr2 y
pr2 (Subjekt (Nomen s g c p)) = "Subjekt: Nomen: "++(show c)++" "++(show p)++" von "++s++" ("++(map toLower (show g))++")"
pr2 (Objekt (Nomen s g c p)) = "Objekt: Nomen: "++(show c)++" "++(show p)++" von "++s++" ("++(map toLower (show g))++")"
pr2 (VerbS (Verb s v)) = "Prädikat: Verb: "++s++" "++(show v)
pr2 x = show x

mark x = x
{-
mark x | x `elem` verben = "V"++x++"V" 
mark x | x `elem` "ich":nomen = "N"++x++"N" 
mark x | x `elem` adjektive = "A"++x++"A"
mark x | x `elem` "will":hilfsverben = "H"++x++"H"

mark x = x 
-}

verbindpred (VerbS _) = True
verbindpred _ = False
objindpred (Objekt _) = True
objindpred _ = False
subjindpred (Subjekt _) = True
subjindpred _ = False
satzstellpred l = (s < v) && (v < o)
  where
    v=length (takeWhile (not.verbindpred) l)
    o=length (takeWhile (not.objindpred) l)
    s=length (takeWhile (not.subjindpred) l)

inse::[(String,Wort)]->(Map.BasicHashTable String [Wort])->IO ()
inse ((s,w):xs) m = do tt<-do Map.lookup m s
                       case tt of 
                                Just ww -> do Map.insert m s (w:ww)
                                              return () 
                                Nothing -> Map.insert m s [w]
                       inse xs m
                       return ()
inse [] m = return ()
{-
main = do w<-readFile "bibel.txt"
          t<-getPOSIXTime
          print t
          (inse ( nomenfull) m)))))

  where
    words=(toList (fromList (map fst nomenfull)))
    m =Map.fromList (zip words (repeat []))
-}
{-
main = mapM print (filter (\y->(toLower (head y)/=(head y))) (filter (\x-> ((x/="")&&(not (member x words)))) tokenized))
  where
    words=( (fromList (map fst nomenfull)))
-}
gok::String->String->[String]
gok (x:xs) acu = [(acu++[x])]++(gok xs (acu++[x])) 
gok [] acu = []

eat (x:xs) (y:ys) | x ==y =eat xs ys
eat x _ = x

wordes::IO (Map.BasicHashTable String [Wort])
wordes= Map.fromList ((map (\x->(fst x,[])) nomenfull))
    

data Node = Full (M.Map Char Node) | Empty (M.Map Char Node) | Leaf
 deriving Show
inser::String->Node->Node
inser (x:xs) (Empty m) = case M.lookup x m of 
                           Just n -> Empty (M.insert x (inser xs n) m)
                           Nothing -> Empty (M.insert x (inser xs (Empty M.empty)) m) 
inser (x:xs)(Full m) = case M.lookup x m of 
                           Just n -> Full (M.insert x (inser xs n) m)
                           Nothing -> Full (M.insert x (inser xs (Empty M.empty)) m) 
inser (x:xs) Leaf = Full (M.singleton x (inser xs (Empty M.empty)))
inser [] (Empty m) = Full m
inser [] Leaf = Leaf
inser [] (Full m)  = Full m

shwg::Bool->String->M.Map Char Node->String
shwg b j m = concat (map go vvv)++"fun"++j++" [] = "++(if b then "True" else "False")++"\nfun"++j++" _ = "++("False")++"\n\n"++(concat (map gog vvv))
  where
    vvv = (zip [1..] (M.toList m))
    go (i,(k,v)) | k `elem` letters = "fun"++j++" ('"++[k]++"':xs) = fun"++j++"x"++(show i)++" xs\n"
    go _ = []
    gog (i,(k,v)) | k `elem` letters =shw (j++"x"++(show i)) v
    gog _ = []
shw:: String->Node->String
shw s (Full m) = shwg True s m 
shw s (Empty m) = shwg False s m 
shw s Leaf = shwg True s M.empty  


fg x=reverse( tail(reverse x))
main = do w<-readFile "bibel.txt"
          wrds<-wordes
          t<-getPOSIXTime
          mm<-mapM (Map.lookup wrds) (tokenized3 w) 
          print $ length (filter isJust mm)
          tt<-getPOSIXTime
          print $  ((read (fg(show tt)))::Double)- (read (fg(show t))::Double)
          print $ length (filter id (map fun (tokenized3 w)))
          ttt<-getPOSIXTime
          print $  ((read (fg(show ttt)))::Double)- (read (fg(show tt))::Double)
          return ()
          

gogo ::(Map.BasicHashTable String [Wort])->String->IO Bool
gogo words w =  do uuu<-uu
                   ff <- (if uuu==[] then return [] else mapM (gogo words) (map snd uuu) )
                   kkk<-zz 
                   if kkk==[] then return False else if [] `elem` kkk then return True else return (or ff)
  where
    uu::IO [(Int,String)]
    uu = do zzz<-zz
            return $ sort (map vv zzz)
    vv::String->(Int,String)
    vv x = (length x, x)
    zz::IO [String]
    zz = do mm<-mapM (\j->if (length j>1)||(j=="S")||(j=="-") then (if j=="-" then return (Just []) else (Map.lookup words j)) else return Nothing) xx 
            let kk=map (\g->case g of
                              Just gg->True
                              _ -> False
                         ) mm
            return $ map (upe.(eat w).snd) (filter fst (zip kk xx))
    xx = (gok w [])
    upe (x:xs)=(toUpper x):xs
    upe [] = []
--main = putStrLn (body (tab2 (zip tokenized (head(grammerize main3))))) 
--grammerize :: [[Wort]]->[[SatzTeil]]
grammerize l = do s<-subj xx [] subjpred Subjekt
                  o<-subj s [] objpred Objekt
                  filter satzstellpred (subj o [] verbpred VerbS)
  where 
    xx=(map Unbekannt l)

subjpred::[Wort]->Maybe Wort
subjpred ((Nomen s g Nominativ p):xs) = Just (Nomen s g Nominativ p) 
subjpred (x:xs) = subjpred xs 
subjpred [] = Nothing
objpred::[Wort]->Maybe Wort
objpred ((Nomen s g Akkusativ p):xs) = Just (Nomen s g Akkusativ p) 
objpred (x:xs) = objpred xs 
objpred [] = Nothing
verbpred::[Wort]->Maybe Wort
verbpred ((Verb s (Grundform)):xs) = Just (Verb s Grundform) 
verbpred (x:xs) = verbpred xs 
verbpred [] = Nothing


subj::[SatzTeil]->[SatzTeil]->([Wort]->Maybe Wort)->(Wort->SatzTeil)->[[SatzTeil]]
subj ((Unbekannt x):xs) ys p e = case p x of 
                   Just xx -> [ys++[e xx]++xs]++(subj xs (ys++[Unbekannt x]) p e)
                   Nothing -> subj xs (ys++[Unbekannt x]) p e
subj (x:xs) ys p e = subj xs (ys++[x]) p e
subj [] ys _ _ = []











