-- | module to switch between different ttf fonts during latex code generation
module FontTool where

import BaseFont
import Data.Array
import Data.Char
import Data.Map.Strict hiding ((!))
import Data.Maybe
import MegaFont
import System.Info

-- | a map taking a FontStyle as key. (see FontStyle in the BaseFont module).
-- The Values of a maps is and array. The array has got an integer index
-- covering 16 bit. The elements of the arrows are chars. These chars can be
-- converted to the paths of ttf files on disc. (see functions fromCharToFont
-- and getttf in the module BaseFont on how to converts the char to a path to a
-- ttf file on disc). The idea is that you got a 16 Bit unicode charter with
-- certain font style properties. It put in this information in the this map and
-- the array you get from it you get the idea ttf file to print that character
-- in that fontstyle in a latex document.
megafont2 :: Map FontStyle (Array Int Char)
megafont2 =
  Data.Map.Strict.fromList
    [ ( s,
        array
          ((0, (2 :: Int) ^ (16 :: Int) - 1) :: (Int, Int))
          (zip ([0 .. (2 :: Int) ^ (16 :: Int) - 1] :: [Int]) f)
      )
      | (s, f) <- megafont
    ]

-- | takes a fontstyle and a 16 bis unicode charater and give you the ideal font
-- to print this character in a LaTeX document. See also megafont2 in this
-- module
getFont ::
  -- | The `FontStyle` for which to look the font for.
  FontStyle ->
  -- | The character for which to look the font for.
  Char ->
  -- | The ideal font to use for the given `FontStyle` and character.
  Font
getFont fontStyle c =
  fromMaybe
    GnuUnifont
    ( do
        d <- Data.Map.Strict.lookup fontStyle megafont2
        (if (ord c) > 65535 then Just GnuUnifontUpper else Just ((fromCharToFont (d ! ord (c)))))
    )

-- | Takes a font and returns the LaTeX Command to switch to this particular
-- font in xelatex as well as lulatex.
fontsetter ::
  -- | The font to switch to
  Font ->
  -- | The LaTeX command to switch to the given font.
  [Char]
fontsetter f =
  "\\allowbreak{}\\setmainfont" ++ inner ++ "\\setmonofont" ++ innermono
  where
    filename =
      reverse ((takeWhile (/= '/')) (reverse (getttf f)))
    pathname = reverse ((dropWhile (/= '/')) (reverse (getttf f)))
    inner =
      "{"
        ++ filename
        ++ "}"
        ++ "[RawFeature={fallback=myfallback},"
        ++ (if os == "linux" then "Path=" ++ pathname else "")
        ++ (mid f)
        ++ "]"
    innermono =
      "{"
        ++ filename
        ++ "}"
        ++ "[RawFeature={fallback=myfallback},"
        ++ (if os == "linux" then "Path=" ++ pathname else "")
        ++ (midmono f)
        ++ "]"
    mid i
      | i
          `elem` [ ComputerModernRoman,
                   ComputerModernRomanBold,
                   ComputerModernRomanItalic,
                   ComputerModernRomanBoldItalic,
                   ComputerModernTypeWriter,
                   ComputerModernTypeWriterBold,
                   ComputerModernTypeWriterItalic,
                   ComputerModernTypeWriterBoldItalic
                 ] =
          ",UprightFont=cmunrm,BoldFont=cmunbx"
            ++ ",ItalicFont=cmunti,BoldItalicFont=cmunbi"
    mid i
      | i
          `elem` [ FreeSerif,
                   FreeSerifBold,
                   FreeSerifBoldItalic,
                   FreeSerifItalic,
                   FreeMono,
                   FreeMonoOblique,
                   FreeMonoBold,
                   FreeMonoBoldOblique
                 ] =
          ",UprightFont=FreeSerif,BoldFont=FreeSerifBold,"
            ++ "ItalicFont=FreeSerifItalic,BoldItalicFont=FreeSerifBoldItalic"
    mid _ = ""
    midmono i
      | i
          `elem` [ ComputerModernRoman,
                   ComputerModernRomanBold,
                   ComputerModernRomanItalic,
                   ComputerModernRomanBoldItalic,
                   ComputerModernTypeWriter,
                   ComputerModernTypeWriterBold,
                   ComputerModernTypeWriterItalic,
                   ComputerModernTypeWriterBoldItalic
                 ] =
          ",UprightFont=cmuntt,BoldFont=cmuntb"
            ++ ",ItalicFont=cmunit,BoldItalicFont=cmuntx"
    midmono i
      | i
          `elem` [ FreeSerif,
                   FreeSerifBold,
                   FreeSerifBoldItalic,
                   FreeSerifItalic,
                   FreeMono,
                   FreeMonoOblique,
                   FreeMonoBold,
                   FreeMonoBoldOblique
                 ] =
          ",UprightFont=FreeMono,BoldFont=FreeMonoBold,"
            ++ "ItalicFont=FreeMonoOblique,BoldItalicFont=FreeMonoBoldOblique"
    midmono _ = ""

-- | Takes a FontStyle and returns the LaTeX Command to switch to that style.
fontstyler ::
  -- | the `FontStyle` to switch to.
  FontStyle ->
  -- | the LaTeX command to switch to the given `FontStyle`
  [Char]
fontstyler s =
  (if (stylebase s) == Mono then "\\ttfamily " else "")
    ++ (if (bold s) == True then "\\bfseries " else "")
    ++ (if (italic s) == True then "\\itshape " else "")
