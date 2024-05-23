{-# LANGUAGE DefaultSignatures, DeriveAnyClass, DeriveGeneric #-}
{-DHUN| general Fonts configuration modules DHUN-}
module BaseFont where
import Data.Char
import Data.Array
import Data.Tuple
import Data.Serialize
import GHC.Generics
{-DHUN| Basic Fontstyle, may be either normal of monospaced or small caps DHUN-}

data FontStyleBase = Normal
                   | Mono
                   | Smallcaps
                   deriving (Eq, Ord, Show, Read, Serialize, Generic)

{-DHUN| Full discription of style of a font. Consists of Basic Fontstyle plus boolean for bold and/or italic DHUN-}

data FontStyle = FontStyle{stylebase :: FontStyleBase,
                           bold :: Bool, italic :: Bool}
               deriving (Eq, Ord, Show, Read, Serialize, Generic)

{-DHUN| Font, a list of ttf font file currently used by mediawiki2latex DHUN-}

data Font = GnuUnifont
          | WenQuanYiZenHei
          | FreeMono
          | FreeMonoOblique
          | FreeMonoBold
          | FreeMonoBoldOblique
          | FreeSerif
          | FreeSerifBold
          | FreeSerifBoldItalic
          | FreeSerifItalic
          | ComputerModernTypeWriter
          | ComputerModernTypeWriterBold
          | ComputerModernTypeWriterItalic
          | ComputerModernTypeWriterBoldItalic
          | ComputerModernRoman
          | ComputerModernRomanBold
          | ComputerModernRomanItalic
          | ComputerModernRomanBoldItalic
          deriving (Eq, Ord, Show, Ix, Read, Serialize, Generic)

{-DHUN| list of Fonts. The first element in the list is the fonts that is preferred to be used. If it can not be used for some reason the next font in the list is used. Note that the list is denoted in reverse order and put into the right order by the reverse function in the way it is just written at this point of the source file DHUN-}

fonts :: [Font]
fonts
  = reverse
      [GnuUnifont, WenQuanYiZenHei, FreeMono, FreeMonoOblique,
       FreeMonoBold, FreeMonoBoldOblique, FreeSerif, FreeSerifBold,
       FreeSerifBoldItalic, FreeSerifItalic, ComputerModernTypeWriter,
       ComputerModernTypeWriterBold, ComputerModernTypeWriterItalic,
       ComputerModernTypeWriterBoldItalic, ComputerModernRoman,
       ComputerModernRomanBold, ComputerModernRomanItalic,
       ComputerModernRomanBoldItalic]

{-DHUN| enumation of Fonts. To store the Fonts database efficiently on disc and to read it to memory quickly interger indices are used. The Integers are low in magnitude and thus stored as chars on disc. DHUN-}

fontList :: [(Font, Int)]
fontList = zip fonts [(ord 'A') ..]

{-DHUN| converts font to char. See also fontList in this source file DHUN-}

fromFontToChar :: Font -> Char
fromFontToChar f
  = chr
      ((array (GnuUnifont, ComputerModernRomanBoldItalic) fontList) ! f)

{-DHUN| converts from char to font. See also fontList in this source file DHUN-}

fromCharToFont :: Char -> Font
fromCharToFont c
  = (array (ord ('A'), ord ('A') + (length fontList) - 1)
       (map swap fontList))
      ! (ord c)

{-DHUN| converts from Fonts to path of ttf file on disc. DHUN-}

getttf :: Font -> [Char]
getttf ComputerModernTypeWriter
  = "/usr/share/fonts/truetype/cmu/cmuntt.ttf"
getttf ComputerModernTypeWriterBoldItalic
  = "/usr/share/fonts/truetype/cmu/cmuntx.ttf"
getttf ComputerModernTypeWriterItalic
  = "/usr/share/fonts/truetype/cmu/cmunit.ttf"
getttf ComputerModernTypeWriterBold
  = "/usr/share/fonts/truetype/cmu/cmuntb.ttf"
getttf ComputerModernRoman
  = "/usr/share/fonts/truetype/cmu/cmunrm.ttf"
getttf ComputerModernRomanBold
  = "/usr/share/fonts/truetype/cmu/cmunbx.ttf"
getttf ComputerModernRomanItalic
  = "/usr/share/fonts/truetype/cmu/cmunti.ttf"
getttf ComputerModernRomanBoldItalic
  = "/usr/share/fonts/truetype/cmu/cmunbi.ttf"
getttf FreeMono = "/usr/share/fonts/truetype/freefont/FreeMono.ttf"
getttf FreeMonoOblique
  = "/usr/share/fonts/truetype/freefont/FreeMonoOblique.ttf"
getttf FreeMonoBold
  = "/usr/share/fonts/truetype/freefont/FreeMonoBold.ttf"
getttf FreeMonoBoldOblique
  = "/usr/share/fonts/truetype/freefont/FreeMonoBoldOblique.ttf"
getttf FreeSerif
  = "/usr/share/fonts/truetype/freefont/FreeSerif.ttf"
getttf FreeSerifBold
  = "/usr/share/fonts/truetype/freefont/FreeSerifBold.ttf"
getttf FreeSerifBoldItalic
  = "/usr/share/fonts/truetype/freefont/FreeSerifBoldItalic.ttf"
getttf FreeSerifItalic
  = "/usr/share/fonts/truetype/freefont/FreeSerifItalic.ttf"
getttf GnuUnifont = "/usr/share/fonts/opentype/unifont/unifont.otf"
getttf WenQuanYiZenHei
  = "/usr/share/fonts/truetype/wqy/wqy-zenhei.ttc"

{-DHUN| defines the FontStyle for each ttf font file. See also FontStyle in this source file. DHUN-}

getstyle :: Font -> FontStyle
getstyle GnuUnifont
  = FontStyle{stylebase = Normal, bold = False, italic = False}
getstyle WenQuanYiZenHei
  = FontStyle{stylebase = Normal, bold = False, italic = False}
getstyle FreeMono
  = FontStyle{stylebase = Mono, bold = False, italic = False}
getstyle FreeMonoOblique
  = FontStyle{stylebase = Mono, bold = False, italic = True}
getstyle FreeMonoBold
  = FontStyle{stylebase = Mono, bold = True, italic = False}
getstyle FreeMonoBoldOblique
  = FontStyle{stylebase = Mono, bold = True, italic = True}
getstyle FreeSerif
  = FontStyle{stylebase = Normal, bold = False, italic = False}
getstyle FreeSerifBold
  = FontStyle{stylebase = Normal, bold = True, italic = False}
getstyle FreeSerifBoldItalic
  = FontStyle{stylebase = Normal, bold = True, italic = True}
getstyle FreeSerifItalic
  = FontStyle{stylebase = Normal, bold = False, italic = True}
getstyle ComputerModernTypeWriter
  = FontStyle{stylebase = Mono, bold = False, italic = False}
getstyle ComputerModernTypeWriterBold
  = FontStyle{stylebase = Mono, bold = True, italic = False}
getstyle ComputerModernTypeWriterItalic
  = FontStyle{stylebase = Mono, bold = False, italic = True}
getstyle ComputerModernTypeWriterBoldItalic
  = FontStyle{stylebase = Mono, bold = True, italic = True}
getstyle ComputerModernRoman
  = FontStyle{stylebase = Normal, bold = False, italic = False}
getstyle ComputerModernRomanBold
  = FontStyle{stylebase = Normal, bold = True, italic = False}
getstyle ComputerModernRomanItalic
  = FontStyle{stylebase = Normal, bold = False, italic = True}
getstyle ComputerModernRomanBoldItalic
  = FontStyle{stylebase = Normal, bold = True, italic = True}
