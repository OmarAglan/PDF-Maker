import Data.Tuple
import BaseFont
import Data.Maybe
import Control.Monad
import Data.Array
import Data.Char
import System.Process



findfont::[Font]->[Font]->Font
findfont (x:xs) f= if x `elem` f then x else findfont f xs
findfont [] f= GnuUnifont


getArray::Font->IO (Array Int Bool)
getArray f = do print name
                _<-system ("python3 /home/dirk/Downloads/usr/bin/names.py "++(show f)++"> "++name)
                text<-readFile name
                return $ array (0,(length text) -1) (zip [0..((length text) -1)] (zt text))
  where
    zt::[Char]->[Bool]
    zt text=(map fun (zip [0..] text))
    fun::(Int,Char)->Bool 
    fun (_,'T') = True
    fun (i,_) = (chr i) `elem` "\n\r\t " 
    name=takeWhile (/='.') filename
    ext =dropWhile (/='.') filename
    filename = reverse ((takeWhile (/='/')) (reverse (getttf f)))
 



itbf s= [FontStyle{stylebase=s,bold=False,italic=False},FontStyle{stylebase=s,bold=True,italic=False},FontStyle{stylebase=s,bold=False,italic=True},FontStyle{stylebase=s,bold=True,italic=True}]

stylebasenext::FontStyleBase->FontStyleBase
stylebasenext Normal = Mono
stylebasenext Mono = Smallcaps
stylebasenext Smallcaps = Normal

styles =(itbf Normal)++(itbf Mono)++(itbf Smallcaps)

myfun as i fs = [f|(f,b)<-zip fonts (map (! i) as),b&&(fs==getstyle f)]

getdata::[Array Int Bool]->Int-> [([Font],FontStyle)]
getdata as i = zip (map (myfun as i) styles) styles


main = jjj

jjj = do arrs<-mapM getArray fonts
         let chrs=array (0,2^16-1) (zip [0..2^16-1] (map (getdata arrs) [0..2^16-1]))  
         print [(s, map (fromFontToChar.f) [0..2^16-1])|(s,f)<-zip styles (map  (getFont chrs) styles)] 




getFont::Array Int [([Font],FontStyle)]->FontStyle->Int->Font
getFont chars fontStyle i = if (mi<=i) && (i<=mx) then getf else def 
  where
    (mi,mx) = bounds chars
    def = GnuUnifont
    ch= chars!i
    swp = map swap ch
    ff sty = (lookup sty swp)
    gg sty = (ff sty) `mplus` (ff sty{italic= not (italic sty)}) `mplus` (ff sty{bold= not (bold sty)}) `mplus` (ff sty{italic= not (italic sty),bold= not (bold sty)})
    hh sty = (gg sty) `mplus` (gg sty{stylebase= stylebasenext .stylebase $ sty}) `mplus` (gg sty{stylebase= stylebasenext.stylebasenext.stylebase$ sty})
    getf = fromMaybe def ((hh fontStyle)>>=return.(findfont fonts) )


