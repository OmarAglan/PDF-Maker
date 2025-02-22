{-DHUN| This module provides string constants DHUN-}
module MagicStrings where
import Data.Char
import Network.URI
import Data.List.Split
import qualified Data.Map as Map

{-DHUN| Wikimedia project prefixes so 'de' from de.wikipedia.org DHUN-}

foreignPrefixes :: [String]
foreignPrefixes
  = ["af", "als", "an", "roa-rup", "ast", "gn", "av", "ay", "az",
     "id", "ms", "bm", "zh-min-nan", "jv", "map-bms", "su", "bug", "bi",
     "bar", "bs", "br", "ca", "cbk-zam", "ch", "cs", "ny", "sn", "tum",
     "ve", "co", "za", "cy", "da", "pdc", "de", "nv", "na", "lad", "et",
     "ang", "en", "es", "eo", "ext", "eu", "to", "fo", "fr", "frp",
     "fy", "ff", "fur", "ga", "gv", "sm", "gd", "gl", "got", "hak",
     "haw", "hsb", "hr", "io", "ilo", "ig", "ia", "ie", "ik", "xh",
     "zu", "is", "it", "mh", "kl", "pam", "csb", "kw", "kg", "ki", "rw",
     "ky", "rn", "sw", "ht", "ku", "ksh", "la", "lv", "lb", "lt", "lij",
     "li", "ln", "jbo", "lg", "lmo", "hu", "mg", "mt", "mi", "cdo",
     "my", "nah", "fj", "nl", "cr", "ne", "nap", "pih", "no", "nn",
     "nrm", "oc", "om", "ng", "pag", "pi", "pap", "pms", "nds", "pl",
     "pt", "ty", "ro", "rmy", "rm", "qu", "se", "sg", "sc", "sco", "st",
     "tn", "sq", "scn", "simple", "ceb", "ss", "sk", "sl", "so", "sh",
     "fi", "sv", "tl", "tt", "tet", "vi", "tpi", "chy", "tr", "tk",
     "tw", "vec", "vo", "fiu-vro", "wa", "vls", "war", "wo", "ts", "yo",
     "bat-smg", "el", "ab", "ba", "be", "bg", "bxr", "cu", "os", "kk",
     "kv", "mk", "mn", "ce", "ru", "sr", "tg", "udm", "uk", "uz", "xal",
     "cv", "hy", "ka", "he", "yi", "ar", "fa", "ha", "ps", "sd", "ur",
     "ug", "arc", "dv", "as", "bh", "bn", "bo", "bpy", "dz", "gu", "hi",
     "kn", "ks", "ml", "mr", "ne", "new", "or", "pa", "sa", "si", "ta",
     "te", "km", "lo", "th", "am", "ti", "iu", "chr", "ko", "ja", "zh",
     "wuu", "lzh", "yue"]

{-DHUN| Wikimedia projects for interwiki links [[w:Foobar]] means en.wikipedia.org/wiki/Foobar DHUN-}

multilangwikis :: [(String, String)]
multilangwikis
  = ([("w", "wikipedia"), ("wikipedia", "wikipedia"),
      ("wikt", "wiktionary"), ("wiktionary", "wiktionary")]
       ++
       [("n", "wikinews"), ("wikinews", "wikinews"), ("b", "wikibooks"),
        ("wikibooks", "wikibooks"), ("q", "wikiquote")]
         ++
         [("wikiquote", "wikiquote"), ("s", "wikisource"),
          ("wikisource", "wikisource"), ("species", "wikispecies")]
           ++
           [("wikispecies", "wikispecies"), ("v", "wikiversity"),
            ("wikiversity", "wikiversity")])

{-DHUN| Wikimedia projects for interwiki links to wikis which only have got a single language version DHUN-}

singlelangwikis :: [(String, String)]
singlelangwikis
  = [("wikimedia", "wikimediafoundation"),
     ("foundation", "wikimediafoundation"),
     ("wmf", "wikimediafoundation"), ("mw", "mediawiki")]

{-DHUN| Wikimedia projects for interwiki links to wikis which only have got a single language version DHUN-}

wikimediasingellangwikis :: [(String, String)]
wikimediasingellangwikis
  = [("commons", "commons"), ("metawikipedia", "meta"),
     ("meta", "meta"), ("m", "meta"), ("incubator", "incubator"),
     ("strategy", "strategy")]

{-DHUN| All Wikis DHUN-}

allwikis :: [(String, String)]
allwikis
  = multilangwikis ++ singlelangwikis ++ wikimediasingellangwikis

{-DHUN| Prefixes for including images in wikis DHUN-}

imgtags :: [[Char]]
imgtags
  = [map toLower x |
     x <- ["\1589\1608\1585\1577", "Billede", "Im\225gen", "Image",
           "Immagine", "Bild", "Afbeelding", "Imagine", "\3619\3641\3611",
           "\22270\20687", "\1605\1604\1601", "Fil", "Archivo", "File",
           "Datei", "Bestand", "Fisier", "\3652\3615\3621\3660",
           "\25991\20214", "Fichier", "\1605\1604\1601", "Archiv",
           "\1060\1072\1081\1083", "\2458\2495\2468\2509\2480", "Datoteka",
           "Fitxer", "Image", "So", "Delwedd", "Fil", "Datei",
           "\913\961\967\949\943\959", "Dosiero", "Archivo", "Pilt",
           "\1662\1585\1608\1606\1583\1607", "Tiedosto", "Fichier",
           "\205omh\225", "\1511\1493\1489\1509", "Datoteka", ": ", "Berkas",
           "Video", "\12501\12449\12452\12523", "Gambar",
           "\4324\4304\4312\4314\4312", "\6063\6016\6047\6070\6042",
           "\54028\51068", "Fascic", "Fichier", "Plaetje", "\3758\3769\3738",
           "Vaizdas", "Att\275ls", "Image", "Sary",
           "\1055\1086\1076\1072\1090\1086\1090\1077\1082\1072", "Fi\351ier",
           "Fail", "Bestand", "Fil", "Fil", "Fichi\232r", "Plik", "Imagem",
           "Fi\351ier", ": ", "S\250bor", "Slika",
           "\1057\1083\1080\1082\1072", "Bild", "\3652\3615\3621\3660",
           "Talaksan", "Dosya", "\1496\1506\1511\1506", "\22294\20687",
           "\22294\20687"]]

{-DHUN| lower Greek letter for HTML entity to latex so &delta; to \\delta DHUN-}

lowergreek :: [[Char]]
lowergreek
  = ["alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta",
     "theta", "iota", "kappa", "lambda", "mu", "nu", "xi", "pi", "rho",
     "sigma", "tau", "upsilon", "phi", "chi", "psi", "omega"]

{-DHUN| not Greek but to be processed like the Greeks above DHUN-}

notsogreek :: [[Char]]
notsogreek = ["cap", "cup", "sim"]

{-DHUN| Full list of characters with Greek like processing explained above DHUN-}

greek :: [[Char]]
greek
  = concat
      (do g <- lowergreek
          case g of
              (x : xs) -> return [(toUpper x) : xs]
              [] -> return [])
      ++ lowergreek ++ notsogreek

{-DHUN| HTML entities to latex DHUN-}

htmlchars :: [([Char], [Char])]
htmlchars
  = [("thetasym", "{\\mbox{$\\vartheta$}}"),
     ("Upsilon", "{\\mbox{$\\Upsilon$}}"),
     ("epsilon", "{\\mbox{$\\varepsilon$}}"),
     ("upsilon", "{\\mbox{$\\upsilon$}}"),
     ("Epsilon", "{\\mbox{$\\Epsilon$}}"),
     ("Omicron", "{\\mbox{$\\text{O}$}}"),
     ("omicron", "{\\mbox{$\\text{o}$}}"),
     ("forall", "{\\mbox{$\\forall$}}"),
     ("lowast", "{\\mbox{$\\ast$}}"),
     ("there4", "{\\mbox{$\\therefore$}}"),
     ("otimes", "{\\mbox{$\\otimes$}}"),
     ("Lambda", "{\\mbox{$\\Lambda$}}"),
     ("lambda", "{\\mbox{$\\lambda$}}"),
     ("sigmaf", "{\\mbox{$\\varsigma$}}"),
     ("dagger", "{\\mbox{$\\dagger$}}"),
     ("Dagger", "{\\mbox{$\\ddagger$}}"),
     ("hellip", "{\\mbox{$\\ldots$}}"),
     ("lfloor", "{\\mbox{$\\lfloor$}}"),
     ("rfloor", "{\\mbox{$\\rfloor$}}"),
     ("spades", "{\\mbox{$\\spadesuit$}}"),
     ("hearts", "{\\mbox{$\\varheartsuit$}}"),
     ("plusmn", "{\\mbox{$\\pm$}}"), ("divide", "{\\mbox{$\\div$}}"),
     ("Scaron", "{\\mbox{$\\text{\\v{S}}$}}"),
     ("scaron", "{\\mbox{$\\text{\\v{s}}$}}"),
     ("thinsp", "{\\mbox{$\\,$}}"),
     ("permil", "{\\mbox{$\\text{\\textperthousand}$}}"),
     ("lsaquo", "{\\mbox{$\\text{\\guilsinglleft}$}}"),
     ("rsaquo", "{\\mbox{$\\text{\\guilsinglright}$}}"),
     ("curren", "{\\mbox{$\\text{\\currency}$}}"),
     ("brvbar", "{\\mbox{$\\text{\\textbrokenbar}$}}"),
     ("middot", "{\\mbox{$\\cdot$}}"),
     ("frac14", "{\\mbox{$\\frac{1}{4}$}}"),
     ("frac12", "{\\mbox{$\\frac{1}{2}$}}"),
     ("frac34", "{\\mbox{$\\frac{3}{4}$}}"),
     ("iquest", "{\\mbox{$\\text{\\textquestiondown}$}}"),
     ("Agrave", "{\\mbox{$\\text{\\`A}$}}"),
     ("Aacute", "{\\mbox{$\\text{\\'A}$}}"),
     ("Atilde", "{\\mbox{$\\text{\\~A}$}}"),
     ("Ccedil", "{\\mbox{$\\text{\\c{C}}$}}"),
     ("Egrave", "{\\mbox{$\\text{\\`E}$}}"),
     ("Eacute", "{\\mbox{$\\text{\\'E}$}}"),
     ("Igrave", "{\\mbox{$\\text{\\`I}$}}"),
     ("Iacute", "{\\mbox{$\\text{\\'I}$}}"),
     ("Ntilde", "{\\mbox{$\\text{\\~N}$}}"),
     ("Ograve", "{\\mbox{$\\text{\\`O}$}}"),
     ("Oacute", "{\\mbox{$\\text{\\'O}$}}"),
     ("Otilde", "{\\mbox{$\\text{\\~O}$}}"),
     ("Oslash", "{\\mbox{$\\O$}}"),
     ("Ugrave", "{\\mbox{$\\text{\\`U}$}}"),
     ("Uacute", "{\\mbox{$\\text{\\'U}$}}"),
     ("Yacute", "{\\mbox{$\\text{\\'Y}$}}"),
     ("agrave", "{\\mbox{$\\text{\\`a}$}}"),
     ("aacute", "{\\mbox{$\\text{\\'a}$}}"),
     ("atilde", "{\\mbox{$\\text{\\~a}$}}"),
     ("ccedil", "{\\mbox{$\\text{\\c{c}}$}}"),
     ("egrave", "{\\mbox{$\\text{\\`e}$}}"),
     ("eacute", "{\\mbox{$\\text{\\'e}$}}"),
     ("igrave", "{\\mbox{$\\text{\\`i}$}}"),
     ("iacute", "{\\mbox{$\\text{\\'i}$}}"),
     ("ntilde", "{\\mbox{$\\text{\\~n}$}}"),
     ("ograve", "{\\mbox{$\\text{\\`o}$}}"),
     ("oacute", "{\\mbox{$\\text{\\'o}$}}"),
     ("otilde", "{\\mbox{$\\text{\\~o}$}}"),
     ("oslash", "{\\mbox{$\\o$}}"),
     ("ugrave", "{\\mbox{$\\text{\\`u}$}}"),
     ("uacute", "{\\mbox{$\\text{\\'u}$}}"),
     ("yacute", "{\\mbox{$\\text{\\'y}$}}"), ("#x202f", "{\\,}"),
     ("#8704", "{\\mbox{$\\forall$}}"),
     ("#8706", "{\\mbox{$\\partial$}}"),
     ("#8707", "{\\mbox{$\\exists$}}"),
     ("exist", "{\\mbox{$\\exists$}}"),
     ("#8709", "{\\mbox{$\\varnothing$}}"),
     ("empty", "{\\mbox{$\\varnothing$}}"),
     ("#8711", "{\\mbox{$\\nabla$}}"), ("nabla", "{\\mbox{$\\nabla$}}"),
     ("#8712", "{\\mbox{$\\in$}}"), ("#8713", "{\\mbox{$\\notin$}}"),
     ("notin", "{\\mbox{$\\notin$}}"), ("#8715", "{\\mbox{$\\ni$}}"),
     ("#8719", "{\\mbox{$\\prod$}}"), ("#8721", "{\\mbox{$\\sum$}}"),
     ("#8722", "{\\mbox{$-$}}"), ("minus", "{\\mbox{$-$}}"),
     ("#8727", "{\\mbox{$\\ast$}}"), ("#8730", "{\\mbox{$\\sqrt$}}"),
     ("radic", "{\\mbox{$\\sqrt$}}"), ("#8733", "{\\mbox{$\\propto$}}"),
     ("#8734", "{\\mbox{$\\infty$}}"), ("infin", "{\\mbox{$\\infty$}}"),
     ("#8736", "{\\mbox{$\\angle$}}"), ("#8743", "{\\mbox{$\\wedge$}}"),
     ("#8744", "{\\mbox{$\\vee$}}"), ("#8745", "{\\mbox{$\\cap$}}"),
     ("#8746", "{\\mbox{$\\cup$}}"), ("#8747", "{\\mbox{$\\int$}}"),
     ("#8756", "{\\mbox{$\\therefore$}}"),
     ("#8764", "{\\mbox{$\\sim$}}"), ("#8773", "{\\mbox{$\\cong$}}"),
     ("#8776", "{\\mbox{$\\approx$}}"),
     ("asymp", "{\\mbox{$\\approx$}}"), ("#8800", "{\\mbox{$\\neq$}}"),
     ("#8801", "{\\mbox{$\\equiv$}}"), ("equiv", "{\\mbox{$\\equiv$}}"),
     ("#8804", "{\\mbox{$\\leq$}}"), ("#8805", "{\\mbox{$\\geq$}}"),
     ("#8834", "{\\mbox{$\\subset$}}"),
     ("#8835", "{\\mbox{$\\supset$}}"),
     ("#8836", "{\\mbox{$\\nsubset$}}"),
     ("#8838", "{\\mbox{$\\subseteq$}}"),
     ("#8839", "{\\mbox{$\\supseteq$}}"),
     ("#8853", "{\\mbox{$\\oplus$}}"), ("oplus", "{\\mbox{$\\oplus$}}"),
     ("#8855", "{\\mbox{$\\otimes$}}"), ("#8869", "{\\mbox{$\\bot$}}"),
     ("#8901", "{\\mbox{$\\cdot$}}"), ("Gamma", "{\\mbox{$\\Gamma$}}"),
     ("Delta", "{\\mbox{$\\Delta$}}"), ("Theta", "{\\mbox{$\\Theta$}}"),
     ("Sigma", "{\\mbox{$\\Sigma$}}"), ("Omega", "{\\mbox{$\\Omega$}}"),
     ("alpha", "{\\mbox{$\\alpha$}}"), ("gamma", "{\\mbox{$\\gamma$}}"),
     ("delta", "{\\mbox{$\\delta$}}"), ("theta", "{\\mbox{$\\theta$}}"),
     ("kappa", "{\\mbox{$\\kappa$}}"), ("sigma", "{\\mbox{$\\sigma$}}"),
     ("omega", "{\\mbox{$\\omega$}}"),
     ("#8224", "{\\mbox{$\\dagger$}}"),
     ("#8225", "{\\mbox{$\\ddagger$}}"),
     ("#8230", "{\\mbox{$\\ldots$}}"), ("#8242", "{\\mbox{$\\prime$}}"),
     ("prime", "{\\mbox{$\\prime$}}"),
     ("#8243", "{\\mbox{$\\second$}}"),
     ("Prime", "{\\mbox{$\\second$}}"),
     ("#8592", "{\\mbox{$\\leftarrow$}}"),
     ("#8593", "{\\mbox{$\\uparrow$}}"),
     ("#8594", "{\\mbox{$\\rightarrow$}}"),
     ("#8595", "{\\mbox{$\\downarrow$}}"),
     ("#8596", "{\\mbox{$\\leftrightarrow$}}"),
     ("#8968", "{\\mbox{$\\lceil$}}"), ("lceil", "{\\mbox{$\\lceil$}}"),
     ("#8969", "{\\mbox{$\\rceil$}}"), ("rceil", "{\\mbox{$\\rceil$}}"),
     ("#8970", "{\\mbox{$\\lfloor$}}"),
     ("#8971", "{\\mbox{$\\rfloor$}}"),
     ("#9674", "{\\mbox{$\\lozenge$}}"),
     ("#9824", "{\\mbox{$\\spadesuit$}}"),
     ("#9827", "{\\mbox{$\\clubsuit$}}"),
     ("clubs", "{\\mbox{$\\clubsuit$}}"),
     ("#9829", "{\\mbox{$\\varheartsuit$}}"),
     ("#9830", "{\\mbox{$\\vardiamondsuit$}}"),
     ("diams", "{\\mbox{$\\vardiamondsuit$}}"),
     ("pound", "{\\mbox{$\\pounds$}}"),
     ("micro", "{\\mbox{$\\Micro$}}"), ("times", "{\\mbox{$\\times$}}"),
     ("Alpha", "{\\mbox{$\\Alpha$}}"), ("Kappa", "{\\mbox{$\\Kappa$}}"),
     ("upsih", "{\\mbox{$\\Upsilon$}}"),
     ("OElig", "{\\mbox{$\\text{\\OE}$}}"),
     ("oelig", "{\\mbox{$\\text{\\oe}$}}"),
     ("tilde", "{\\mbox{$\\text{\\~{}}$}}"),
     ("#8194", "{\\mbox{$\\text{ }$}}"),
     ("#8195", "{\\mbox{$\\text{\\hspace*{1em}}$}}"),
     ("#8201", "{\\mbox{$\\,$}}"), ("#8211", "{\\mbox{$-$}}"),
     ("ndash", "{\\mbox{$-$}}"), ("#8212", "{\\mbox{$\\text{---}$}}"),
     ("mdash", "{\\mbox{$\\text{---}$}}"),
     ("#8216", "{\\mbox{$\\text{\\textquoteleft}$}}"),
     ("lsquo", "{\\mbox{$\\text{\\textquoteleft}$}}"),
     ("#8217", "{\\mbox{$\\text{\\textquoteright}$}}"),
     ("rsquo", "{\\mbox{$\\text{\\textquoteright}$}}"),
     ("#8218", "{\\mbox{$\\text{\\quotesinglbase}$}}"),
     ("sbquo", "{\\mbox{$\\text{\\quotesinglbase}$}}"),
     ("#8220", "{\\mbox{$\\text{\\textquotedblleft}$}}"),
     ("ldquo", "{\\mbox{$\\text{\\textquotedblleft}$}}"),
     ("#8221", "{\\mbox{$\\text{\\textquotedblright}$}}"),
     ("rdquo", "{\\mbox{$\\text{\\textquotedblright}$}}"),
     ("#8222", "{\\mbox{$\\text{\\quotedblbase}$}}"),
     ("bdquo", "{\\mbox{$\\text{\\quotedblbase}$}}"),
     ("#8226", "{\\mbox{$\\bullet$}}"),
     ("#8240", "{\\mbox{$\\text{\\textperthousand}$}}"),
     ("#8249", "{\\mbox{$\\text{\\guilsinglleft}$}}"),
     ("#8250", "{\\mbox{$\\text{\\guilsinglright}$}}"),
     ("#8254", "{\\mbox{$\\text{\\textasciimacron}$}}"),
     ("oline", "{\\mbox{$\\text{\\textasciimacron}$}}"),
     ("#8364", "{\\mbox{$\\text{\\EUR}$}}"),
     ("#8482", "{\\mbox{$\\text{\\texttrademark}$}}"),
     ("trade", "{\\mbox{$\\text{\\texttrademark}$}}"),
     ("#8629", "{\\mbox{$\\hookleftarrow$}}"),
     ("crarr", "{\\mbox{$\\hookleftarrow$}}"),
     ("iexcl", "{\\mbox{$\\text{\\textexclamdown}$}}"),
     ("laquo", "{\\mbox{$\\text{\\guillemotleft}$}}"),
     ("acute", "{\\mbox{$\\text{\\'{}}$}}"),
     ("cedil", "{\\mbox{$\\c{}$}}"),
     ("raquo", "{\\mbox{$\\text{\\guillemotright}$}}"),
     ("Acirc", "{\\mbox{$\\text{\\^A}$}}"),
     ("Aring", "{\\mbox{$\\text{\\r{A}}$}}"),
     ("AElig", "{\\mbox{$\\text{\\AE}$}}"),
     ("Ecirc", "{\\mbox{$\\text{\\^E}$}}"),
     ("Icirc", "{\\mbox{$\\text{\\^I}$}}"),
     ("Ocirc", "{\\mbox{$\\text{\\^O}$}}"),
     ("Ucirc", "{\\mbox{$\\text{\\^U}$}}"),
     ("THORN", "{\\mbox{$\\text{\\Thorn}$}}"),
     ("szlig", "{\\mbox{$\\text{\\ss}$}}"),
     ("acirc", "{\\mbox{$\\text{\\^a}$}}"),
     ("aring", "{\\mbox{$\\text{\\r{a}}$}}"),
     ("aelig", "{\\mbox{$\\text{\\ae}$}}"),
     ("ecirc", "{\\mbox{$\\text{\\^e}$}}"),
     ("icirc", "{\\mbox{$\\text{\\^i}$}}"),
     ("ocirc", "{\\mbox{$\\text{\\^o}$}}"),
     ("ucirc", "{\\mbox{$\\text{\\^u}$}}"),
     ("thorn", "{\\mbox{$\\text{\\thorn}$}}"),
     ("part", "{\\mbox{$\\partial$}}"), ("isin", "{\\mbox{$\\in$}}"),
     ("prod", "{\\mbox{$\\prod$}}"), ("prop", "{\\mbox{$\\propto$}}"),
     ("cong", "{\\mbox{$\\cong$}}"), ("nsub", "{\\mbox{$\\nsubset$}}"),
     ("sube", "{\\mbox{$\\subseteq$}}"),
     ("supe", "{\\mbox{$\\supseteq$}}"), ("perp", "{\\mbox{$\\bot$}}"),
     ("sdot", "{\\mbox{$\\cdot$}}"), ("#915", "{\\mbox{$\\Gamma$}}"),
     ("#916", "{\\mbox{$\\Delta$}}"), ("#920", "{\\mbox{$\\Theta$}}"),
     ("#923", "{\\mbox{$\\Lambda$}}"), ("#926", "{\\mbox{$\\Xi$}}"),
     ("#928", "{\\mbox{$\\Pi$}}"), ("#931", "{\\mbox{$\\Sigma$}}"),
     ("#933", "{\\mbox{$\\Upsilon$}}"), ("#934", "{\\mbox{$\\Phi$}}"),
     ("#936", "{\\mbox{$\\Psi$}}"), ("#937", "{\\mbox{$\\Omega$}}"),
     ("#945", "{\\mbox{$\\alpha$}}"), ("#946", "{\\mbox{$\\beta$}}"),
     ("beta", "{\\mbox{$\\beta$}}"), ("#947", "{\\mbox{$\\gamma$}}"),
     ("#948", "{\\mbox{$\\delta$}}"),
     ("#949", "{\\mbox{$\\varepsilon$}}"),
     ("#950", "{\\mbox{$\\zeta$}}"), ("zeta", "{\\mbox{$\\zeta$}}"),
     ("#951", "{\\mbox{$\\eta$}}"), ("#952", "{\\mbox{$\\theta$}}"),
     ("#953", "{\\mbox{$\\iota$}}"), ("iota", "{\\mbox{$\\iota$}}"),
     ("#954", "{\\mbox{$\\kappa$}}"), ("#955", "{\\mbox{$\\lambda$}}"),
     ("#956", "{\\mbox{$\\mu$}}"), ("#957", "{\\mbox{$\\nu$}}"),
     ("#958", "{\\mbox{$\\xi$}}"), ("#960", "{\\mbox{$\\pi$}}"),
     ("#961", "{\\mbox{$\\rho$}}"), ("#962", "{\\mbox{$\\varsigma$}}"),
     ("#963", "{\\mbox{$\\sigma$}}"), ("#964", "{\\mbox{$\\tau$}}"),
     ("#965", "{\\mbox{$\\upsilon$}}"),
     ("#966", "{\\mbox{$\\varphi$}}"), ("#967", "{\\mbox{$\\chi$}}"),
     ("#968", "{\\mbox{$\\psi$}}"), ("#969", "{\\mbox{$\\omega$}}"),
     ("#977", "{\\mbox{$\\vartheta$}}"),
     ("#982", "{\\mbox{$\\varpi$}}"), ("#402", "{\\mbox{$f$}}"),
     ("fnof", "{\\mbox{$f$}}"), ("larr", "{\\mbox{$\\leftarrow$}}"),
     ("uarr", "{\\mbox{$\\uparrow$}}"),
     ("rarr", "{\\mbox{$\\rightarrow$}}"),
     ("darr", "{\\mbox{$\\downarrow$}}"),
     ("harr", "{\\mbox{$\\leftrightarrow$}}"),
     ("varr", "{\\mbox{$\\updownarrow$}}"),
     
     ("8658", "{\\mbox{$\\Leftarrow$}}"),
     ("8657", "{\\mbox{$\\Uparrow$}}"),
     ("8656", "{\\mbox{$\\Rightarrow$}}"),
     ("8659", "{\\mbox{$\\Downarrow$}}"),
     ("8660", "{\\mbox{$\\Leftrightarrow$}}"),
     ("8661", "{\\mbox{$\\Updownarrow$}}"),
     
     ("8592", "{\\mbox{$\\leftarrow$}}"),
     ("8593", "{\\mbox{$\\uparrow$}}"),
     ("8594", "{\\mbox{$\\rightarrow$}}"),
     ("8595", "{\\mbox{$\\downarrow$}}"),
     ("8596", "{\\mbox{$\\leftrightarrow$}}"),
     ("8597", "{\\mbox{$\\updownarrow$}}"),
     
     
     ("lArr", "{\\mbox{$\\Leftarrow$}}"),
     ("uArr", "{\\mbox{$\\Uparrow$}}"),
     ("rArr", "{\\mbox{$\\Rightarrow$}}"),
     ("dArr", "{\\mbox{$\\Downarrow$}}"),
     ("hArr", "{\\mbox{$\\Leftrightarrow$}}"),
     ("vArr", "{\\mbox{$\\Updownarrow$}}"),
     
     
     ("#160", "{\\mbox{$~$}}"), ("nbsp", "{\\mbox{$~$}}"),
     ("#162", "{\\mbox{$\\cent$}}"), ("cent", "{\\mbox{$\\cent$}}"),
     ("#163", "{\\mbox{$\\pounds$}}"), ("#165", "{\\mbox{$\\yen$}}"),
     ("#168", "{\\mbox{$\\spddot$}}"), ("#172", "{\\mbox{$\\neg$}}"),
     ("#174", "{\\mbox{$\\circledR$}}"), ("#177", "{\\mbox{$\\pm$}}"),
     ("#181", "{\\mbox{$\\Micro$}}"), ("#215", "{\\mbox{$\\times$}}"),
     ("#247", "{\\mbox{$\\div$}}"), ("#240", "{\\mbox{$\\eth$}}"),
     ("#913", "{\\mbox{$\\Alpha$}}"), ("#914", "{\\mbox{$\\Beta$}}"),
     ("Beta", "{\\mbox{$\\Beta$}}"), ("#917", "{\\mbox{$\\Epsilon$}}"),
     ("#918", "{\\mbox{$\\Zeta$}}"), ("Zeta", "{\\mbox{$\\Zeta$}}"),
     ("#919", "{\\mbox{$\\Eta$}}"), ("#921", "{\\mbox{$\\Iota$}}"),
     ("Iota", "{\\mbox{$\\Iota$}}"), ("#922", "{\\mbox{$\\Kappa$}}"),
     ("#924", "{\\mbox{$\\Mu$}}"), ("#925", "{\\mbox{$\\Nu$}}"),
     ("#927", "{\\mbox{$\\text{O}$}}"), ("#929", "{\\mbox{$\\Rho$}}"),
     ("#932", "{\\mbox{$\\Tau$}}"), ("#935", "{\\mbox{$\\Chi$}}"),
     ("#959", "{\\mbox{$\\text{o}$}}"),
     ("#978", "{\\mbox{$\\Upsilon$}}"),
     ("#338", "{\\mbox{$\\text{\\OE}$}}"),
     ("#339", "{\\mbox{$\\text{\\oe}$}}"),
     ("#352", "{\\mbox{$\\text{\\v{S}}$}}"),
     ("#353", "{\\mbox{$\\text{\\v{s}}$}}"),
     ("#376", "{\\mbox{$\\text{\\\"Y}$}}"),
     ("Yuml", "{\\mbox{$\\text{\\\"Y}$}}"),
     ("#710", "{\\mbox{$\\text{\\^{}}$}}"),
     ("circ", "{\\mbox{$\\text{\\^{}}$}}"),
     ("#732", "{\\mbox{$\\text{\\~{}}$}}"),
     ("ensp", "{\\mbox{$\\text{ }$}}"),
     ("emsp", "{\\mbox{$\\text{\\hspace*{1em}}$}}"),
     ("bull", "{\\mbox{$\\bullet$}}"),
     ("euro", "{\\mbox{$\\text{\\EUR}$}}"),
     ("quot", "{\\mbox{$\\text{\\symbol{34}}$}}"),
     ("#161", "{\\mbox{$\\text{\\textexclamdown}$}}"),
     ("#164", "{\\mbox{$\\text{\\currency}$}}"),
     ("#166", "{\\mbox{$\\text{\\textbrokenbar}$}}"),
     ("#167", "{\\mbox{$\\text{\\S}$}}"),
     ("sect", "{\\mbox{$\\text{\\S}$}}"),
     ("#169", "{\\mbox{$\\copyright$}}"),
     ("copy", "{\\mbox{$\\copyright$}}"),
     ("#170", "{\\mbox{$\\text{\\textordfeminine{}}$}}"),
     ("ordf", "{\\mbox{$\\text{\\textordfeminine{}}$}}"),
     ("#171", "{\\mbox{$\\text{\\guillemotleft}$}}"),
     ("#173", "{\\mbox{$\\text{\\-}$}}"),
     ("#175", "{\\mbox{$\\text{\\textasciimacron}$}}"),
     ("macr", "{\\mbox{$\\text{\\textasciimacron}$}}"),
     ("#176", "{\\mbox{$^\\circ$}}"), ("#178", "{\\mbox{${}^2$}}"),
     ("sup2", "{\\mbox{${}^2$}}"), ("#179", "{\\mbox{${}^3$}}"),
     ("sup3", "{\\mbox{${}^3$}}"),
     ("#180", "{\\mbox{$\\text{\\'{}}$}}"),
     ("#182", "{\\mbox{$\\text{\\textparagraph}$}}"),
     ("para", "{\\mbox{$\\text{\\textparagraph}$}}"),
     ("#183", "{\\mbox{$\\cdot$}}"), ("#184", "{\\mbox{$\\c{}$}}"),
     ("#185", "{\\mbox{${}^1$}}"), ("sup1", "{\\mbox{${}^1$}}"),
     ("#186", "{\\mbox{$\\text{\\textordmasculine{}}$}}"),
     ("ordm", "{\\mbox{$\\text{\\textordmasculine{}}$}}"),
     ("#187", "{\\mbox{$\\text{\\guillemotright}$}}"),
     ("#188", "{\\mbox{$\\frac{1}{4}$}}"),
     ("#189", "{\\mbox{$\\frac{1}{2}$}}"),
     ("#190", "{\\mbox{$\\frac{3}{4}$}}"),
     ("#191", "{\\mbox{$\\text{\\textquestiondown}$}}"),
     ("#192", "{\\mbox{$\\text{\\`A}$}}"),
     ("#193", "{\\mbox{$\\text{\\'A}$}}"),
     ("#194", "{\\mbox{$\\text{\\^A}$}}"),
     ("#195", "{\\mbox{$\\text{\\~A}$}}"),
     ("#196", "{\\mbox{$\\text{\\\"A}$}}"),
     ("Auml", "{\\mbox{$\\text{\\\"A}$}}"),
     ("#197", "{\\mbox{$\\text{\\r{A}}$}}"),
     ("#198", "{\\mbox{$\\text{\\AE}$}}"),
     ("#199", "{\\mbox{$\\text{\\c{C}}$}}"),
     ("#200", "{\\mbox{$\\text{\\`E}$}}"),
     ("#201", "{\\mbox{$\\text{\\'E}$}}"),
     ("#202", "{\\mbox{$\\text{\\^E}$}}"),
     ("#203", "{\\mbox{$\\text{\\\"E}$}}"),
     ("Euml", "{\\mbox{$\\text{\\\"E}$}}"),
     ("#204", "{\\mbox{$\\text{\\`I}$}}"),
     ("#205", "{\\mbox{$\\text{\\'I}$}}"),
     ("#206", "{\\mbox{$\\text{\\^I}$}}"),
     ("#207", "{\\mbox{$\\text{\\\"I}$}}"),
     ("Iuml", "{\\mbox{$\\text{\\\"I}$}}"),
     ("#208", "{\\mbox{$\\DH$}}"), ("#209", "{\\mbox{$\\text{\\~N}$}}"),
     ("#210", "{\\mbox{$\\text{\\`O}$}}"),
     ("#211", "{\\mbox{$\\text{\\'O}$}}"),
     ("#212", "{\\mbox{$\\text{\\^O}$}}"),
     ("#213", "{\\mbox{$\\text{\\~O}$}}"),
     ("#214", "{\\mbox{$\\text{\\\"O}$}}"),
     ("Ouml", "{\\mbox{$\\text{\\\"O}$}}"), ("#216", "{\\mbox{$\\O$}}"),
     ("#217", "{\\mbox{$\\text{\\`U}$}}"),
     ("#218", "{\\mbox{$\\text{\\'U}$}}"),
     ("#219", "{\\mbox{$\\text{\\^U}$}}"),
     ("#220", "{\\mbox{$\\text{\\\"U}$}}"),
     ("Uuml", "{\\mbox{$\\text{\\\"U}$}}"),
     ("#221", "{\\mbox{$\\text{\\'Y}$}}"),
     ("#222", "{\\mbox{$\\text{\\Thorn}$}}"),
     ("#223", "{\\mbox{$\\text{\\ss}$}}"),
     ("#224", "{\\mbox{$\\text{\\`a}$}}"),
     ("#225", "{\\mbox{$\\text{\\'a}$}}"),
     ("#226", "{\\mbox{$\\text{\\^a}$}}"),
     ("#227", "{\\mbox{$\\text{\\~a}$}}"),
     ("#228", "{\\mbox{$\\text{\\\"a}$}}"),
     ("auml", "{\\mbox{$\\text{\\\"a}$}}"),
     ("#229", "{\\mbox{$\\text{\\r{a}}$}}"),
     ("#230", "{\\mbox{$\\text{\\ae}$}}"),
     ("#231", "{\\mbox{$\\text{\\c{c}}$}}"),
     ("#232", "{\\mbox{$\\text{\\`e}$}}"),
     ("#233", "{\\mbox{$\\text{\\'e}$}}"),
     ("#234", "{\\mbox{$\\text{\\^e}$}}"),
     ("#235", "{\\mbox{$\\text{\\\"e}$}}"),
     ("euml", "{\\mbox{$\\text{\\\"e}$}}"),
     ("#236", "{\\mbox{$\\text{\\`i}$}}"),
     ("#237", "{\\mbox{$\\text{\\'i}$}}"),
     ("#238", "{\\mbox{$\\text{\\^i}$}}"),
     ("#239", "{\\mbox{$\\text{\\\"i}$}}"),
     ("iuml", "{\\mbox{$\\text{\\\"i}$}}"),
     ("#241", "{\\mbox{$\\text{\\~n}$}}"),
     ("#242", "{\\mbox{$\\text{\\`o}$}}"),
     ("#243", "{\\mbox{$\\text{\\'o}$}}"),
     ("#244", "{\\mbox{$\\text{\\^o}$}}"),
     ("#245", "{\\mbox{$\\text{\\~o}$}}"),
     ("#246", "{\\mbox{$\\text{\\\"o}$}}"),
     ("ouml", "{\\mbox{$\\text{\\\"o}$}}"), ("#248", "{\\mbox{$\\o$}}"),
     ("#249", "{\\mbox{$\\text{\\`u}$}}"),
     ("#250", "{\\mbox{$\\text{\\'u}$}}"),
     ("#251", "{\\mbox{$\\text{\\^u}$}}"),
     ("#252", "{\\mbox{$\\text{\\\"u}$}}"),
     ("uuml", "{\\mbox{$\\text{\\\"u}$}}"),
     ("#253", "{\\mbox{$\\text{\\'y}$}}"),
     ("#254", "{\\mbox{$\\text{\\thorn}$}}"),
     ("#255", "{\\mbox{$\\text{\\\"y}$}}"),
     ("yuml", "{\\mbox{$\\text{\\\"y}$}}"),
     ("#10", "{\\newline}"),
     ("#39", "'"),
     ("sum", "{\\mbox{$\\sum$}}"), ("ang", "{\\mbox{$\\angle$}}"),
     ("and", "{\\mbox{$\\wedge$}}"), ("cap", "{\\mbox{$\\cap$}}"),
     ("cup", "{\\mbox{$\\cup$}}"), ("int", "{\\mbox{$\\int$}}"),
     ("sim", "{\\mbox{$\\sim$}}"), ("sub", "{\\mbox{$\\subset$}}"),
     ("sup", "{\\mbox{$\\supset$}}"), ("Phi", "{\\mbox{$\\Phi$}}"),
     ("Psi", "{\\mbox{$\\Psi$}}"), ("eta", "{\\mbox{$\\eta$}}"),
     ("rho", "{\\mbox{$\\rho$}}"), ("tau", "{\\mbox{$\\tau$}}"),
     ("phi", "{\\mbox{$\\varphi$}}"), ("chi", "{\\mbox{$\\chi$}}"),
     ("psi", "{\\mbox{$\\psi$}}"), ("piv", "{\\mbox{$\\varpi$}}"),
     ("loz", "{\\mbox{$\\lozenge$}}"), ("#38", "{\\mbox{$\\&$}}"),
     ("amp", "{\\mbox{$\\&$}}"), ("#60", "{\\mbox{$<$}}"),
     ("#62", "{\\mbox{$>$}}"), ("yen", "{\\mbox{$\\yen$}}"),
     ("uml", "{\\mbox{$\\spddot$}}"), ("not", "{\\mbox{$\\neg$}}"),
     ("reg", "{\\mbox{$\\circledR$}}"), ("eth", "{\\mbox{$\\eth$}}"),
     ("Eta", "{\\mbox{$\\Eta$}}"), ("Rho", "{\\mbox{$\\Rho$}}"),
     ("Tau", "{\\mbox{$\\Tau$}}"), ("Chi", "{\\mbox{$\\Chi$}}"),
     ("#34", "{\\mbox{$\\text{\\symbol{34}}$}}"),
     ("shy", "{\\mbox{$\\text{\\-}$}}"), ("deg", "{\\mbox{$^\\circ$}}"),
     ("ETH", "{\\mbox{$\\DH$}}"), ("ni", "{\\mbox{$\\ni$}}"),
     ("or", "{\\mbox{$\\vee$}}"), ("ne", "{\\mbox{$\\neq$}}"),
     ("le", "{\\mbox{$\\leq$}}"), ("ge", "{\\mbox{$\\geq$}}"),
     ("Xi", "{\\mbox{$\\Xi$}}"), ("Pi", "{\\mbox{$\\Pi$}}"),
     ("mu", "{\\mbox{$\\mu$}}"), ("nu", "{\\mbox{$\\nu$}}"),
     ("xi", "{\\mbox{$\\xi$}}"), ("pi", "{\\mbox{$\\pi$}}"),
     ("lt", "{\\mbox{$<$}}"), ("weierp", "{\\mbox{$\\wp$}}"),
     ("image", "{\\mbox{$\\Im$}}"), ("real", "{\\mbox{$\\Re$}}"),
     ("alefsym", "{\\mbox{$\\aleph$}}"),
     ("uArr", "{\\mbox{$\\Uparrow$}}"),
     ("rArr", "{\\mbox{$\\Rightarrow$}}"),
     ("dArr", "{\\mbox{$\\Downarrow$}}"),
     ("hArr", "{\\mbox{$\\Leftrightarrow$}}"),
     ("lArr", "{\\mbox{$\\Leftarrow$}}"),
     ("rang", "{\\mbox{$\\rangle$}}"), ("lang", "{\\mbox{$\\langle$}}"),
     ("zwnj", "{}"), ("zwj", ""), ("lrm", ""), ("rlm", ""),
     ("gt", "{\\mbox{$>$}}"), ("Mu", "{\\mbox{$\\Mu$}}"),
     ("#151", "{--}"), ("Nu", "{\\mbox{$\\Nu$}}"), ("frasl", "\8260")]

{-DHUN| get latex representation of HTML entity like &amp; DHUN-}

getHtmlChar :: String -> String
getHtmlChar x = Map.findWithDefault [] x (Map.fromList htmlchars)

{-DHUN| a function to remove the taling print version string from urls (usually on wikibooks). This function is needed so the name of the book without the tailing print version string will be printed on the titlepage of the book DHUN-}

removePrintVersion :: [Char] -> [Char]
removePrintVersion lem
  = fun
      ["/Druckversion", "/ Druckversion", "/Print version",
       "/Complete Wikibook", "/All Chapters", "/Print Version",
       "/print version", "/Printable version", "/The Whole Book",
       "/print", ": Druckversion"]
      lem
  where fun (y : ys) x
          = case splitOn y x of
                (z : _) -> fun ys z
                _ -> []
        fun [] x = x

{-DHUN| Nearly all HTML tags DHUN-}

goodtags1 :: [[Char]]
goodtags1
  = ["templatestyles", "includeonly", "references", "blockquote", "noinclude", "figcaption",
     "noframes", "frameset", "colgroup", "fieldset", "basefont",
     "!DOCTYPE", "noscript", "address", "acronym", "caption", "strong",
     "applet", "script", "button", "select", "section", "legend", "footer", "figure","header",
     "strike", "object", "input", "center", "legend", "iframe", "small",
     "video", "audio", "style", "input", "label", "tbody", "thead",
     "title", "track" ,"frame", "param", "base", "area", "font", "code", "span", "main",
     "abbr", "body", "link", "menu", "math", "meta", "samp", "cite",
     "head", "html", "form", "cite", "ref", "div", "pre", "sub", "nav",
     "sup", "big", "del", "map", "bdo", "var", "dfn", "kbd", "col", "wbr",
     "ins", "bdi", "dir", "img", "h1", "h2", "h3", "h4", "h5", "h6",
     "li", "ul", "ol", "tt", "dd", "dl", "dt", "hr", "em", "b", "i",
     "s", "u", "p", "q", "a"]

{-DHUN| HTML tags for tables rows in tables and so on, only lower case DHUN-}

tabletags :: [[Char]]
tabletags = ["table", "td", "th", "tr"]

{-DHUN| HTML tags for tables rows in tables and so on, lower case as well as upper case  DHUN-}

listOfTableTags :: [[Char]]
listOfTableTags = tabletags ++ (map (map toUpper) tabletags)

{-DHUN| All HTML tags DHUN-}

listOfTags :: [[Char]]
listOfTags = goodtags1 ++ (map (map toUpper) goodtags1)

{-DHUN| Character escaping from Unicode to latex DHUN-}

chartrans :: Char -> String
chartrans '•' = "\\allowbreak{}•"
chartrans '|' = "\\allowbreak{}|"
chartrans '\'' = "\\textquotesingle{}"
chartrans '[' = "{$\\text{[}$}"
chartrans ']' = "{$\\text{]}$}"
chartrans '&' = "\\&"
chartrans '%' = "\\%"
chartrans '{' = "\\{"
chartrans '}' = "\\}"
chartrans '_' = "\\_"
chartrans '$' = "\\${}"
chartrans '#' = "\\#"
chartrans '~' = "\\~{}"
chartrans '^' = "\\^{}"
chartrans '"' = "\\symbol{34}"
chartrans '\\' = "\\textbackslash{}"
chartrans '<' = "<{}"
chartrans '>' = ">{}"
chartrans '-' = "-{}"
chartrans '\8239' = "\\,"
chartrans c = c : []

{-DHUN| Character escaping from Unicode to web links inside latex with the URL package DHUN-}

chartransforlink :: Char -> String
chartransforlink '&' = "\\&"
chartransforlink '%' = "\\%"
chartransforlink '{' = "\\{"
chartransforlink '}' = "\\}"
chartransforlink '#' = "\\#"
chartransforlink '$' = "\\${}"
chartransforlink '\\' = "\\textbackslash{}"
chartransforlink '<' = "<{}"
chartransforlink '>' = ">{}"
chartransforlink '^' = "\\%5E"
chartransforlink c
  | 127 < ord c =
    concat (map chartransforlink $ escapeURIString (const False) [c])
chartransforlink c = c : []
