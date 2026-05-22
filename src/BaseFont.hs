{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | general Fonts configuration module
module BaseFont where

import Data.Array
import Data.Char
import Data.Serialize
import Data.Tuple
import GHC.Generics

-- | Basic fontstyle, may be either normal of monospaced or small caps
data FontStyleBase
  = -- | Normal font
    Normal
  | -- | Mono spaced font
    Mono
  | -- | Small caps font
    Smallcaps
  deriving (Eq, Ord, Show, Read, Serialize, Generic)

-- | Full description of style of a font. Consists of Basic fontstyle
-- (`FontStyleBase`) plus boolean for bold and/or italic
data FontStyle = FontStyle
  { -- | basic fontstyle
    stylebase :: FontStyleBase,
    -- | bold font
    bold :: Bool,
    -- | italic font
    italic :: Bool
  }
  deriving (Eq, Ord, Show, Read, Serialize, Generic)

-- | Font, a list of ttf font file currently used by mediawiki2latex
data Font
  = -- | GNU Unifont (fallback if nothing else has the glyph)
    GnuUnifont
  | -- | GNU Unifont "upper" companion see Supplementary Multilingual Plane (SMP)
    GnuUnifontUpper
  | -- | WenQuanYi Zen Hei (Chinese glyphs)
    WenQuanYiZenHei
  | -- | GNU FreeFont Monospaced
    FreeMono
  | -- | GNU FreeFont Monospaced italic
    FreeMonoOblique
  | -- | GNU FreeFont Monospaced bold
    FreeMonoBold
  | -- | GNU FreeFont Monospaced bold italic
    FreeMonoBoldOblique
  | -- | GNU FreeFont Serif
    FreeSerif
  | -- | GNU FreeFont Serif bold
    FreeSerifBold
  | -- | GNU FreeFont Serif bold italic
    FreeSerifBoldItalic
  | -- | GNU FreeFont Serif italic
    FreeSerifItalic
  | -- | Computer Modern Typewriter
    ComputerModernTypeWriter
  | -- | Computer Modern Typewriter bold
    ComputerModernTypeWriterBold
  | -- | Computer Modern Typewriter italic
    ComputerModernTypeWriterItalic
  | -- | Computer Modern Typewriter bold italic
    ComputerModernTypeWriterBoldItalic
  | -- | Computer Modern Roman
    ComputerModernRoman
  | -- | Computer Modern Roman bold
    ComputerModernRomanBold
  | -- | Computer Modern Roman italic
    ComputerModernRomanItalic
  | -- | Computer Modern Roman bold italic
    ComputerModernRomanBoldItalic
  deriving (Eq, Ord, Show, Ix, Read, Serialize, Generic)

-- | list of Fonts. The first element in the list is the fonts that is preferred
-- to be used. If it can not be used for some reason the next font in the list
-- is used. Note that the list is denoted in reverse order and put into the
-- right order by the reverse function in the way it is just written at this
-- point of the source file.
fonts ::
  -- | List of fonts in the order of priority for use.
  [Font]
fonts =
  reverse
    [ GnuUnifontUpper,
      GnuUnifont,
      WenQuanYiZenHei,
      FreeMono,
      FreeMonoOblique,
      FreeMonoBold,
      FreeMonoBoldOblique,
      FreeSerif,
      FreeSerifBold,
      FreeSerifBoldItalic,
      FreeSerifItalic,
      ComputerModernTypeWriter,
      ComputerModernTypeWriterBold,
      ComputerModernTypeWriterItalic,
      ComputerModernTypeWriterBoldItalic,
      ComputerModernRoman,
      ComputerModernRomanBold,
      ComputerModernRomanItalic,
      ComputerModernRomanBoldItalic
    ]

-- | Enumeration of Fonts. To store the fonts database efficiently on disk and
-- to read it to memory quickly integer indices are used. The Integers are low
-- in magnitude and thus stored as chars on disk.
fontList ::
  -- | List of tuples, first element of the tuple is the font as instance of
  -- type `Font`. The second element of the tuple is the integer index where it
  -- is store in the fonts database on disk. Please note that the Char is just
  -- an index in the fonts database on disk and has nothing to do with the chars
  -- available inside the ttf and otf files.
  [(Font, Int)]
fontList = zip fonts [(ord 'A') ..]

-- | converts font to char. See also `fontList` in this source file
fromFontToChar ::
  -- | The font as a instance of type `Font` to be looked up.
  Font ->
  -- | The Char index under which it is stored on disk in the fonts database.
  Char
fromFontToChar f =
  chr
    ((array (GnuUnifont, ComputerModernRomanBoldItalic) fontList) ! f)

-- | converts from char to font. See also `fontList` in this source file
fromCharToFont ::
  -- | The Char index under which the Font is stored on disk.
  Char ->
  -- | An instance of the `Font` type representing the requested font.
  Font
fromCharToFont c =
  ( array
      (ord ('A'), ord ('A') + (length fontList) - 1)
      (map swap fontList)
  )
    ! (ord c)

-- | converts from Fonts to path of ttf or otf file on disk.
getttf ::
  -- | An instance of the type `Font` representing the font for which the ttf or
  -- otf file on disk shall be returned.
  Font ->
  -- | The filename of the ttf or otf file for the requested `Font`.
  String
getttf ComputerModernTypeWriter =
  "/usr/share/fonts/truetype/cmu/cmuntt.ttf"
getttf ComputerModernTypeWriterBoldItalic =
  "/usr/share/fonts/truetype/cmu/cmuntx.ttf"
getttf ComputerModernTypeWriterItalic =
  "/usr/share/fonts/truetype/cmu/cmunit.ttf"
getttf ComputerModernTypeWriterBold =
  "/usr/share/fonts/truetype/cmu/cmuntb.ttf"
getttf ComputerModernRoman =
  "/usr/share/fonts/truetype/cmu/cmunrm.ttf"
getttf ComputerModernRomanBold =
  "/usr/share/fonts/truetype/cmu/cmunbx.ttf"
getttf ComputerModernRomanItalic =
  "/usr/share/fonts/truetype/cmu/cmunti.ttf"
getttf ComputerModernRomanBoldItalic =
  "/usr/share/fonts/truetype/cmu/cmunbi.ttf"
getttf FreeMono = "/usr/share/fonts/truetype/freefont/FreeMono.ttf"
getttf FreeMonoOblique =
  "/usr/share/fonts/truetype/freefont/FreeMonoOblique.ttf"
getttf FreeMonoBold =
  "/usr/share/fonts/truetype/freefont/FreeMonoBold.ttf"
getttf FreeMonoBoldOblique =
  "/usr/share/fonts/truetype/freefont/FreeMonoBoldOblique.ttf"
getttf FreeSerif =
  "/usr/share/fonts/truetype/freefont/FreeSerif.ttf"
getttf FreeSerifBold =
  "/usr/share/fonts/truetype/freefont/FreeSerifBold.ttf"
getttf FreeSerifBoldItalic =
  "/usr/share/fonts/truetype/freefont/FreeSerifBoldItalic.ttf"
getttf FreeSerifItalic =
  "/usr/share/fonts/truetype/freefont/FreeSerifItalic.ttf"
getttf GnuUnifont = "/usr/share/fonts/opentype/unifont/unifont.otf"
getttf GnuUnifontUpper = "/usr/share/fonts/opentype/unifont/unifont_upper.otf"
getttf WenQuanYiZenHei =
  "/usr/share/fonts/truetype/wqy/wqy-zenhei.ttc"

-- | defines the FontStyle for each ttf font file. See also FontStyle in this
-- source file.
getstyle ::
  -- | an instance of the `Font` type for which the `FontStyle` is requested.
  Font ->
  -- | The `FontStyle` of the requested `Font`
  FontStyle
getstyle GnuUnifont =
  FontStyle {stylebase = Normal, bold = False, italic = False}
getstyle GnuUnifontUpper =
  FontStyle {stylebase = Normal, bold = False, italic = False}
getstyle WenQuanYiZenHei =
  FontStyle {stylebase = Normal, bold = False, italic = False}
getstyle FreeMono =
  FontStyle {stylebase = Mono, bold = False, italic = False}
getstyle FreeMonoOblique =
  FontStyle {stylebase = Mono, bold = False, italic = True}
getstyle FreeMonoBold =
  FontStyle {stylebase = Mono, bold = True, italic = False}
getstyle FreeMonoBoldOblique =
  FontStyle {stylebase = Mono, bold = True, italic = True}
getstyle FreeSerif =
  FontStyle {stylebase = Normal, bold = False, italic = False}
getstyle FreeSerifBold =
  FontStyle {stylebase = Normal, bold = True, italic = False}
getstyle FreeSerifBoldItalic =
  FontStyle {stylebase = Normal, bold = True, italic = True}
getstyle FreeSerifItalic =
  FontStyle {stylebase = Normal, bold = False, italic = True}
getstyle ComputerModernTypeWriter =
  FontStyle {stylebase = Mono, bold = False, italic = False}
getstyle ComputerModernTypeWriterBold =
  FontStyle {stylebase = Mono, bold = True, italic = False}
getstyle ComputerModernTypeWriterItalic =
  FontStyle {stylebase = Mono, bold = False, italic = True}
getstyle ComputerModernTypeWriterBoldItalic =
  FontStyle {stylebase = Mono, bold = True, italic = True}
getstyle ComputerModernRoman =
  FontStyle {stylebase = Normal, bold = False, italic = False}
getstyle ComputerModernRomanBold =
  FontStyle {stylebase = Normal, bold = True, italic = False}
getstyle ComputerModernRomanItalic =
  FontStyle {stylebase = Normal, bold = False, italic = True}
getstyle ComputerModernRomanBoldItalic =
  FontStyle {stylebase = Normal, bold = True, italic = True}
