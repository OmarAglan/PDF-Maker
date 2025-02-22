{-DHUN| A parser for the medaiiwki grammar DHUN-}
module MediaWikiParser where
import Text.ParserCombinators.Parsec
import qualified Data.List as List
import qualified Data.Map as Map
import MediaWikiParseTree
import MagicStrings
import Control.Monad
import Data.String.HT (trim)
import Data.List.Split (splitOn)
import Data.List (isInfixOf)
import Tools
import Codec.Binary.UTF8.String
import Data.Maybe
import Network.URI
import WikiHelper

{-DHUN| flattens out the HTML 'a' tags. That is it replaces each 'a' element with its content. That is everything that is between its opening and its closing tag. The only parameter of this function is the parse tree to be processed. This function returns the parse tree with the 'a' HTML elements flattened |DHUN-}

reducea :: [Anything Char] -> [Anything Char]
reducea ll = concat (map go ll)
  where go :: Anything Char -> [Anything Char]
        go (Environment Tag (TagAttr "a" _) l) = l
        go (Environment x y l) = [Environment x y (reducea l)]
        go x = [x]

{-DHUN| flattens out the HTML 'div' tags, which have got a 'class' attributes present with the value 'noresize'. That is it replaces each 'div' element with the properties mentioned above by its content. That is everything that is between its opening and its closing tag. The only parameter of this function is the parse tree to be processed. This function returns the parse tree with the 'div' HTML elements with the properties given above flattened |DHUN-}

reducediv :: [Anything Char] -> [Anything Char]
reducediv ll = concat (map go ll)
  where go :: Anything Char -> [Anything Char]
        go (Environment Tag (TagAttr "div" m) l)
          | (Map.lookup "class" m) == (Just "noresize") = l
        go (Environment x y l) = [Environment x y (reducea l)]
        go x = [x]

{-DHUN| a function to get HTML elements out of a parse tree. The first parameter is name of the tag to be looked for. The second parameter is a key in the attributes of that element that has to be present for the element to be considered. The third parameter is a value that has to be found under the given key in the attributes of the element, in order for the element to be part of the returned output list. The fourth parameter is the parse tree. |DHUN-}

deepGet ::
        [Char] -> String -> [Char] -> [Anything a] -> [Anything a]
deepGet t k v ll = concat $ map go ll
  where go (Environment Tag (TagAttr tag m) l)
          | (tag == t) && ((Map.lookup k m) == (Just v)) =
            [Environment Tag (TagAttr tag m) l]
        go (Environment _ _ l) = (deepGet t k v l)
        go _ = []

deepGetFigCaption ::
        [Anything a] -> [Anything a]
deepGetFigCaption ll = concat $ map go ll
  where go (Environment Tag (TagAttr "figcaption" m) l) =[Environment Tag (TagAttr "figcaption" m) l]
        go (Environment _ _ l) = (deepGetFigCaption l)
        go _ = []

deepGetFigRes ::
        [Anything Char] -> [Anything Char]
deepGetFigRes ll = concat $ map go ll
  where go (Environment Tag (TagAttr "img" m) _) = case Map.lookup "resource" m of
                                                     Just x -> map C (replace2 ("File"++ (dropWhile (/= ':') x)) "%2B" "+")
                                                     _ -> []
        go (Environment _ _ l) = (deepGetFigRes l)
        go _ = []

{-DHUN| flattens a part of the parse tree, that is takes the characters found in the tree and turns them into a string dropping all other information in the tree DHUN-}

deepFlatten :: [Anything t] -> [Anything t]
deepFlatten ll = concat $ map go ll
  where go (Environment HtmlChar s l) = [Environment HtmlChar s l]
        go (Environment _ _ l) = (deepFlatten l)
        go x = [x]

{-DHUN| converts a wiki source document to a parse tree to be converted to LaTeX be treeToLaTeX3. The first parameter is that list of parsers. That is the list of environments to be recognized by the parser. This is usually either only plain HTML, or HTML mixed with mediawiki markup. The second parameter is the source code to be parsed. This function returns a parse tree |DHUN-}

parseit :: [MyParser Char] -> String -> [Anything Char]
parseit pp x
  = (parseit2
       (decon2 (remake pp)
          (parseAnything2
             [MyStackFrame{endparser = pzero, startdata = Str "",
                           environment = Root, badparser = \ _ -> pzero, parsernumber = 0,
                           nestingdepth = 0}]
             (remake pp)
             []))
       ('\n' : x))

{-DHUN| helper function of parseit, not to be called directly. This function takes the parser for the grammar, in the sense of a parser of the parsec library, (so that is the final combined parser) as first argument. It takes the source code to be parsed (usually HTML of mediawiki markup mixed with HTML) as second and runs the parser on the source code. It returns the resulting parse tree. |DHUN-}

parseit2 :: Parser [Anything Char] -> String -> [Anything Char]
parseit2 p input
  = case (parse p "" input) of
        Left _ -> []
        Right x -> x

{-DHUN| A parser for one particular element of the mediawiki grammar DHUN-}

data MyParser tok = MyParser{bad ::
                             [Anything tok] -> GenParser tok () (),
                             start :: MyStack tok -> GenParser tok () StartData,
                             end :: StartData -> GenParser tok () (), allowed :: [EnvType],
                             self :: EnvType,
                             modify :: StartData -> [Anything tok] -> [Anything tok],
                             reenv :: EnvType -> EnvType}

{-DHUN| the stack of the parser. See documentation on MyStackFrame in this module for details. DHUN-}

type MyStack tok = [MyStackFrame tok]

{-DHUN| A stack frame on the parsers stack. A stack frame represents an environment that was opened. So something like an opening HTML tag. The value endparser. Is a parser that should match exactly the closing bracket of the environment. The value startdata is the return value of the start parser of the enviroment. This stack frame is created immediately after the startparser of an environment has matches and is given the return value of that startparser as startdata. The value environment is the environment this stack frame belongs to in case of an HTML tag this would be Tag. See the type EnvType in the module MediaWikiParseTree for a full list of possible environments. In the parse tree that is finally generated each node with arbitrarily nested children has got an EnvType associated with it. The badparser is a parser that is repeatedly tries while processing the current environment, if it matches the current environment is considered to be invalid. Backtracking occurs an the characters currently under consideration are parser (possible very) different manner. So with badparser you can signal that an environment is invalid if a creating parser (the badparser) matches within the environment. The parserenumber is just a number that uniquely identifies each parsers in the list of parsers active for the whole parsing process. These numbers are usually generated by the remake function. The nestingdepth is a bit of a misnomer. It is a unique number for each stack frame. So each stack frame that is newly created gets a different number. DHUN-}

data MyStackFrame tok = MyStackFrame{endparser ::
                                     GenParser tok () (),
                                     startdata :: StartData, environment :: EnvType,
                                     badparser :: [Anything tok] -> GenParser tok () (),
                                     parsernumber :: Int, nestingdepth :: Int}

{-DHUN| takes a result returned by parseAnything3 and converts it into a parse tree for further processing. The only purpose of this function is to convert the notation of bracket. The bracket a denoted by the Open and Close parse tree elements of the type Anything be the function parseAnything3. The need to be converted to environments, that is node with children in the parse tree. The environments will be denoted by the 'Environment' data constructors of the type Anything|DHUN-}

decon2 ::
         (Monad m) =>
         [(a1, MyParser a)] -> m (t, [Anything a]) -> m [Anything a]
decon2 l x
  = do (_, s) <- x
       return (findMatchingBrackets l (reverse s))

{-DHUN| Usually bracket can be close in an order different from the reverse one in which they were opened. But for certain environments this is not allowed, and the order has to be strictly followed. This value is the list of those environments. DHUN-}

preserving :: [EnvType]
preserving
  = [Math, Source, Comment, Gallery, NoWiki, NoInclude, BigMath,
     Preformat, TableCap, TableRowSep, TableColSep, TableHeadColSep,
     TemplateInside, Wikitable, TableTag]

{-DHUN| Helper function for parseAnyClosingBracket. Should not be called directly. The only parameter is the current parser stack. Returns the depth on the stack of the stack frame whose closing bracket matched. DHUN-}

parseAnyClosingBracket2 ::
                          (Show tok, Eq tok) =>
                          MyStack tok -> GenParser tok () Integer
parseAnyClosingBracket2
  = (parseAnyClosingBracket3 0) . (List.map (\ x -> endparser x))

{-DHUN| Helper function for parseAnyClosingBracket. Should not be called directly. The this will take stack frame by stack frame of the stack. The first parameter is an integer and indicates how many stack frames have allready been take of the stack. Returns the depth on the stack of the stack frame whose closing bracket matched. DHUN-}

parseAnyClosingBracket3 ::
                        Integer -> [GenParser tok () ()] -> GenParser tok () Integer
parseAnyClosingBracket3 i (x : xs)
  = try
      (do x
          return i)
      <|> (parseAnyClosingBracket3 (i + 1) xs)
parseAnyClosingBracket3 _ [] = pzero

{-DHUN| Remove the n'th elements from a list. n is an integer an given is first parameter. The list to be processed is given as second parameter DHUN-}

myremove :: Integer -> [a] -> [a]
myremove _ [] = []
myremove 0 (_ : xs) = myremove (-1) xs
myremove i (x : xs) = x : (myremove (i - 1) xs)

{-DHUN| Enumerates a list of parsers. needed to prepare a list of parsers for use with parseAnything2 DHUN-}

remake :: [a] -> [(Int, a)]
remake x = zip (iterate (+ 1) 0) x

{-DHUN| predicate to test whether the current stack-frame-index is in a stack. The first parameter is the stack-frame-index the second parameter is the stack. Returns true if it could be found DHUN-}

isin ::
       (Show tok, Eq tok) => Int -> (MyStack tok) -> Bool
isin i s = i `elem` (List.map nestingdepth s)

{-DHUN| tries to parse exactly one specific opening bracket. The parameters are identical to the ones of parseAnyOpeningBracket, which the exception of the second parameter. The second parameter is that parser for the bracket currently under consideration. This function 'catch' the BBad 'exception' 'thrown' by parseAnything. In this case it returns pzero, causing the parser to backtrack. DHUN-}

parseSpecificOpeningBracket ::
                              (Show tok, Eq tok) =>
                              Int ->
                                (Int, MyParser tok) ->
                                  (MyStack tok) ->
                                    [(Int, MyParser tok)] ->
                                      [Anything tok] ->
                                        GenParser tok () (Either2 (MyStack tok, [Anything tok]))
parseSpecificOpeningBracket v (n, x) s l i
  = do r <- do sd <- try (start x s)
               parseAnything (v + 1)
                 (MyStackFrame{endparser = (end x) sd, startdata = sd,
                               environment = self x, badparser = bad x, parsernumber = n,
                               nestingdepth = v}
                    : s)
                 l
                 ((Open (length s) (self x) sd n) : i)
       case r of
           BBad (ss, y) -> if isin v ss then pzero else return (BBad (ss, y))
           _ -> return r

{-DHUN| tried to parse any of the opening brackets given by the parsers passed as the third parameter. The first parameter is the stack number (see documentation of the parseAnything function for more details on that). The second parameter is the current parser stack (see documentation of the parseAnything2 function for more details on that). The forth parameter is the list of parsers to b taken into account by the general parsing process. In contrast the third parameter contains only a list of parsers that are allowed to match in the current step of the parsing process. The fifth parameter is the current parser output stream. That is the information returned by the parser up to the current step. It is kind of an accumulator for parser results. DHUN-}

parseAnyOpeningBracket ::
                         (Show tok, Eq tok) =>
                         Int ->
                           MyStack tok ->
                             [(Int, MyParser tok)] ->
                               [(Int, MyParser tok)] ->
                                 [Anything tok] ->
                                   GenParser tok () (Either2 (MyStack tok, [Anything tok]))
parseAnyOpeningBracket _ _ [] _ _ = pzero
parseAnyOpeningBracket v s (x : xs) l i
  = try (parseSpecificOpeningBracket v x s l i) <|>
      parseAnyOpeningBracket v s xs l i

{-DHUN| insert a list of closing brackets into the parser output stream. Later on matching opening and closing brackets will be found and parse tree will be generated this way. The first parameter is an integer it is the number of brackets which should be close. The second parameter is the parser stack. It says which kind of brackets should be closed. It returns a parser output stream just containing the opening brackets DHUN-}

generateClosingBrackets ::
                          (Num a, Eq a, Show tok, Eq tok) =>
                          a -> MyStack tok -> [Anything tok]
generateClosingBrackets 0 (s : xs)
  = [Close (length xs) (environment s)]
generateClosingBrackets mi (s : xs)
  = (Close (length xs) (environment s)) :
      (generateClosingBrackets (mi - 1) xs)
generateClosingBrackets _ _ = []

{-DHUN| insert a list of opening brackets into the parser output stream. Later on matching opening and closing brackets will be found and parse tree will be generated this way. The first parameter is an integer it is the number of brackets which should be opened. The second parameter is the parser stack. It says which kind of brackets should be opened. It returns a parser output stream just containing the opening brackets DHUN-}

generateOpeningBrackets ::
                          (Num a, Eq a, Show tok, Eq tok) =>
                          a -> MyStack tok -> [Anything tok]
generateOpeningBrackets 0 _ = []
generateOpeningBrackets mi (s : xs)
  = (Open (length xs) (environment s) (startdata s) (parsernumber s))
      : (generateOpeningBrackets (mi - 1) xs)
generateOpeningBrackets _ _ = []

{-DHUN| a version of either with the difference that the left and right types are the same. RRight stands for sucessful parse of a token. BBad stands for parse failure in which the next possiblity is tried. DHUN-}

data Either2 b = RRight b
               | BBad b

{-DHUN| tries to match any of the currently possible closing brackets. Brackets closed in a order different from the reverse to the one in which they were opened are usually possible. And exception are the so called preserving elements, they can only be closed in the correct order. In the general case of this kind of crossbracketing it is necessary to add some opening and closing brackets to the output stream and to take the right stack frame of the stack keeping all others on it in the right order. DHUN-}

parseAnyClosingBracket ::
                         (Show tok, Eq tok) =>
                         Int ->
                           MyStack tok ->
                             [(Int, MyParser tok)] ->
                               [Anything tok] ->
                                 GenParser tok () (Either2 (MyStack tok, [Anything tok]))
parseAnyClosingBracket v s l i
  = do mi <- try
               (do mmi <- parseAnyClosingBracket2 s
                   guard
                     (case s of
                          (g : _) -> (mmi == 0) ||
                                       ((environment g) == Link2) ||
                                         ((not ((environment g) `elem` preserving)) &&
                                            (not
                                               ((environment (s !! (fromIntegral mmi))) `elem`
                                                  preserving)))
                          [] -> False)
                   return mmi)
       let ss = myremove mi s
       parseAnything v ss l
         ((reverse
             ((generateClosingBrackets mi s) ++
                (reverse (generateOpeningBrackets mi ss))))
            ++ i)

{-DHUN| this function tries to match the bad parser of the current environment. If it matches it returns BBAD, otherwise it returns RRight. See also comment of the parserAnything function. DHUN-}

trybadparser ::
               (Show tok, Eq tok) =>
               MyStack tok ->
                 [Anything tok] -> GenParser tok () (Either2 (MyStack tok, [a2]))
trybadparser s i
  = do x <- case s of
                (g : _) -> (do _ <- (badparser g) i
                               return True)
                             <|> return False
                [] -> return False
       if x == True then return (BBad (s, [])) else
         return (RRight (s, []))

{-DHUN| this is the main function of the parser which calls itself recursively. To run the parser you should not call this function directly but rather use parseAnything2. The parameter are the same as the parameters to the parameters to the function parseAnything2. So look at the documentation for their meaning. But there is one additional parameter namely the first one. This is the stack frame number, it is increase for every stack frame and never decreased this way each stack frame has got a unique identifier this way. An other difference is the return type this function returns the always same type as the function parseAnything2, but wrapped in the Either2 monad. The Either2 monad has an additional bit to signal whether the parse was good or bad. The bad bit signals so called bad parser of the current environment has matched signaling that the environment is to be considered invalid, and we have to backtrack. But what we do here is just stop paring and return a successful parse, but return the bad flag as set in the return type. This will propagate through to the parser that was trying to open the environment that caused the current problem. If that recognizes the problem it can flag the environment as failed by returning pzero. So again here we just return BBad. So we kind of throw an exception. And in parseSpecificOpeningBracket we will catch BBad and signal the actual problem by returning pzero and that way kick of backtracking. DHUN-}

parseAnything ::
                (Show tok, Eq tok) =>
                Int ->
                  MyStack tok ->
                    [(Int, MyParser tok)] ->
                      [Anything tok] ->
                        GenParser tok () (Either2 (MyStack tok, [Anything tok]))
parseAnything v s l i
  = (do eof
        return $ RRight (s, i ++ (generateClosingBrackets (length s) s)))
      <|>
      do nb <- trybadparser s i
         case nb of
             RRight _ -> try (parseAnyClosingBracket v s l i) <|>
                           case s of
                               (g : _) -> do try
                                               (parseAnyOpeningBracket v s
                                                  [x | x <- l,
                                                   (environment g) `elem` (allowed (snd x))]
                                                  l
                                                  i)
                               [] -> return (RRight ([], i))
                           <|>
                           do c <- anyToken
                              parseAnything v s l ((C c) : i)
             BBad _ -> case s of
                           (g : _) -> try
                                        (parseAnyOpeningBracket v s
                                           [x | x <- l, (environment g) `elem` (allowed (snd x))]
                                           l
                                           i)
                                        <|> return (BBad (s, i))
                           [] -> pzero

{-DHUN| This is the main entry point of the parse. So the function you need to call when you want to convert the source into the parse tree. The first parameter is the stack. I usually should contain only and exactly the root stack frame. The second parameter is an enumerated list of parsers. You usually take a list like the list parsers from this module and enumerate it by running remake on it. So thats the list of environments the parser is able to recognize. The third parameter is the parse results that have been created so far. Since we are just starting the parse this has to be the empty list. The function returns a parser. See the documentation of the parse module for more details on the type GenParser. Roughly is means that this parser takes an input list whose items are of type tok and that the parsers does not have state (hence the void type '()') and return a tuple. The first elements of that tuple is a stack. Where a new stack frame is added to the stack for each new environment that is found to open by the parser, like an opening HTML tag. And the second elements of the tuple is a parse tree, that is a list of parse tree elements, where each parse tree element may contain sublists of parse tree element. This way it is a real tree. DHUN-}

parseAnything2 ::
                 (Show tok, Eq tok) =>
                 MyStack tok ->
                   [(Int, MyParser tok)] ->
                     [Anything tok] -> GenParser tok () (MyStack tok, [Anything tok])
parseAnything2 s l i
  = do x <- parseAnything 0 s l i
       case x of
           BBad (_, b) -> return (s, b)
           RRight b -> return b

{-DHUN| this find the matching closing bracket for an opening bracket. It returns a tuple. Its first element is the environment created form the given opening bracket together with its closing bracket and the content between opening and closing bracket. Its second elements is the remaining list of parsed elements, after the closing bracket. This list does still contain the Open and Close parser tree elements for opening and closing bracket, and those are not yet converted to environments. This function takes the list of parse tree elements after the opening bracket as first input parameter. It takes the index of the parser that created the opening bracket as second input parameter. That is the index created by the remake function in this module. It takes the size of the stack at the time when the opening bracket was found as third input parameter. It takes the EnvType of the environment of the opening bracket as fourth input parameter. It takes the StartData parse result associated with the opening bracket as fifth parameter. The sixth parameter is the accumulator an should be the empty list when calling this function externally. The seventh parameter is the remaining parse tree after the opening bracket without the opening and closing brackets converted to environments DHUN-}

findMatchingClosingBracket ::
                           [(a1, MyParser a)] ->
                             Int ->
                               Int ->
                                 EnvType ->
                                   StartData ->
                                     [Anything a] -> [Anything a] -> (Anything a, [Anything a])
findMatchingClosingBracket l n i e s b ((Close i2 e2) : xs)
  = if (i, e) == (i2, e2) then
      (Environment ((reenv (snd (l !! n))) e) s
         ((modify (snd (l !! n))) s (findMatchingBrackets l (reverse b))),
       xs)
      else findMatchingClosingBracket l n i e s ((Close i2 e2) : b) xs
findMatchingClosingBracket l n i e s b (x : xs)
  = findMatchingClosingBracket l n i e s (x : b) xs
findMatchingClosingBracket l n _ e s b []
  = (Environment ((reenv (snd (l !! n))) e) s
       ((modify (snd (l !! n))) s (findMatchingBrackets l (reverse b))),
     [])

{-DHUN| run findMatchingBrackets on the inner part environment given as second parameter. This function takes the enumerated list of parsers created by remake as first input parameter DHUN-}

findMatchingBrackets2 ::
                      [(a1, MyParser a)] -> Anything a -> Anything a
findMatchingBrackets2 l (Environment e s b)
  = Environment e s (findMatchingBrackets l b)
findMatchingBrackets2 l xs
  = Environment Root (Str "") (findMatchingBrackets l [xs])

{-DHUN| the parser (Anything3) creates a list of parser elements, which is not a tree. The environments which will form the nodes with children in the final tree are denoted as opening and closing brackets in this list. This function takes that list as second input parameter, finds matching pairs of opening and closing brackets and converts the to environments. The opening an closing brackets are already balanced because of the way Anything3 works, that means there is exactly one matching closing bracket for each opening one and they open and close in to proper order. This function takes the enumerated list of parsers as first input parameter, that is the same list also given to the function Anything. DHUN-}

findMatchingBrackets ::
                     [(a1, MyParser a)] -> [Anything a] -> [Anything a]
findMatchingBrackets l ((Open i e s n) : xs)
  = let (t, xxs) = findMatchingClosingBracket l n i e s [] xs in
      (findMatchingBrackets2 l t) : (findMatchingBrackets l xxs)
findMatchingBrackets l (x : xs) = x : (findMatchingBrackets l xs)
findMatchingBrackets _ [] = []

{-DHUN| a list of environments. Most parsers use this list as their 'allowed' variable. Meaning that the parser is only allowed to match within the environments given in the 'allowed' list DHUN-}

everywhere :: [EnvType]
everywhere = [Wikitable] ++ everywheretbl

{-DHUN| list containing the Italic and Bold environments, see documentation on the list 'everywhere' in this module DHUN-}

bi :: [EnvType]
bi = [Italic, Bold]

{-DHUN| list containing the same environments as the list 'everywhere' except the Wikitable environment, see documentation on the list 'everywhere' in this module DHUN-}

everywheretbl :: [EnvType]
everywheretbl = bi ++ everywherebi

{-DHUN| list containing the same environments as the list 'everywhere' except the environment Wikitable, Bold and Italic, see documentation on the list 'everywhere' in this module DHUN-}

everywherebi :: [EnvType]
everywherebi = basicwhere ++ [Wikilink]

{-DHUN| list containing the same environments as the list 'everywhere' except the environment Wikitable, Bold, Italic and Wikilink see documentation on the list 'everywhere' in this module DHUN-}

basicwhere :: [EnvType]
basicwhere = [Link] ++ verybasicwhere

{-DHUN| list containing the same environments as the list 'everywhere' except the environment Wikitable, Bold, Italic, Wikilink and Link see documentation on the list 'everywhere' in this module DHUN-}

verybasicwhere :: [EnvType]
verybasicwhere
  = [Itemgroup, Root, Wikiheading, TableCap, Chapter, Tag, TableTag,
     TemplateInside, IncludeOnly]

{-DHUN| list containing the environments where the parser linkp is allowed to match. Currently this seems to be everywhere. So this possibly can go away  DHUN-}

everywherel :: [EnvType]
everywherel = basicwhere ++ bi ++ [Wikitable, Wikilink]

{-DHUN| list containing the same environments as the list 'everywhere' except the Link environment see documentation on the list 'everywhere' in this module DHUN-}

everywherel2 :: [EnvType]
everywherel2 = verybasicwhere ++ bi ++ [Wikitable, Wikilink]

{-DHUN| list containing the TableColSep and TableHeadColSep environments, see documentation on the list 'everywhere' in this module. the environments mean table header column separator and table column separator DHUN-}

wikilinkwhere :: [EnvType]
wikilinkwhere = [TableColSep, TableHeadColSep]

{-DHUN| the list of parsers needed for processing the HTML output created by MediaWiki DHUN-}

minparsers :: [MyParser Char]
minparsers
  = [doctagparser, metatagparser, supp, subp, dhunurlp, itagparser,
     pagebreakp, htmlcharp, p302p, attrp, greekp, brparser, mytablep,
     mytrsepp2, mytcolsepp2, mytcapp2, mythcolsepp2, annop, tagparser,
     tagparserp, tagparser2, tagparser2p, tagparsert, tagparsert,
     tagparser2t, ttagparsers, tagparsers,ttagparsers2, tagparsers2, stagparser, commentp, numhtmlp,
     rtagparser]

{-DHUN| the list of parsers for parsing contributor information for images on MediaWiki websites DHUN-}

htmlminparsers :: [MyParser Char]
htmlminparsers
  = [doctagparser, metatagparser, supp, subp, dhunurlp, itagparser,
     pagebreakp, htmlcharp, p302p, attrp, greekp, brparser, htmytablep,
     htmytrsepp, htmytcolsepp, htmytcapp, htmythcolsepp, tagparser,
     tagparserp, tagparser2, tagparser2p, tagparsert, tagparsert,
     tagparser2t, tagparsers, stagparser, commentp, numhtmlp,
     rtagparser]

{-DHUN| the list of parsers needed for processing the image title description so that is the content a html attibutes DHUN-}

imgparsers :: [MyParser Char]
imgparsers = [supp, subp, htmlcharp, p302p, greekp, numhtmlp]

{-DHUN| the list of parsers needed for parsing source code in the MediaWiki markup language DHUN-}

parsers :: [MyParser Char]
parsers
  = [doctagparser, metatagparser, supp, subp, dhunurlp, itagparser,
     chapterp, prep, pagebreakp, htmlcharp, p302p, attrp, greekp,
     brparser, wikilinkp, wikitablep, mytablep, wikiheadingp,
     itempgrouppt, itempgroupp, itemlinep, boldp, italicp, tablecapp,
     tablecapp2, tablecapp3, rowsepp, mytrsepp, colsepp, colsepp2,
     mytcolsepp, mytcapp, headcolsepp, headcolsepp2, mythcolsepp,
     galleryp, imagemapp, nowikip, noincludep, mathp, annop, imagemapp,
     ttagparser, ttagparser2, ttagparsert, ttagparser2t, ttagparsers,
     tagparser, tagparser2, tagparsert, tagparser2t, tagparsers,
     stagparser, commentp, reservedp, templatewikilinkp, wikiparamp,
     wikitemplatep, templateinsideverbatimp, templateinsidep,
     gallerywlp, imagemapwlp, hdevlinep, linkp, linkp2, poemp, presectionp,
     presectionpt, numhtmlp, rtagparser]

{-DHUN| the parser record, with some fields initialized with default values DHUN-}

baseParser :: MyParser tok
baseParser
  = MyParser{bad = \ _ -> pzero, start = undefined,
             end = \ _ -> return (), allowed = everywhere, self = undefined,
             modify = \ _ x -> x, reenv = id}

{-DHUN| this function takes a string and returns a parser that matches any of the given strings DHUN-}

oneOfTheStrings :: [String] -> Parser String
oneOfTheStrings (x : xs) = try (string x) <|> (oneOfTheStrings xs)
oneOfTheStrings [] = pzero

{-DHUN| parses a HTML entity, that is a character escaped with the ampersand notation DHUN-}

htmlcharp :: MyParser Char
htmlcharp
  = baseParser{start =
                 \ _ ->
                   do _ <- char '&'
                      s <- (oneOfTheStrings [fst x | x <- htmlchars])
                      _ <- char ';'
                      return (Str (s)),
               allowed = Preformat : SpaceIndent : NoWiki : everywhere,
               self = HtmlChar}

{-DHUN| parses a HTML entity, escaped with numeric ampersand notation DHUN-}

numhtmlp :: MyParser Char
numhtmlp
  = baseParser{start =
                 \ _ ->
                   do _ <- string "&#"
                      s <- try (many1 digit) <|>
                             do ss <- try (string "x") <|> try (string "X")
                                sss <- try (many1 hexDigit)
                                return $ ss ++ sss
                      _ <- char ';'
                      return (Str (s)),
               allowed = Preformat : SpaceIndent : NoWiki : everywhere,
               self = NumHtml}

{-DHUN| parses a HTML #302 character. Special parser needed since it acts on the receding character DHUN-}

p302p :: MyParser Char
p302p
  = baseParser{start =
                 \ _ ->
                   do c <- anyChar
                      _ <- string "&#x302;"
                      return (Str (c : [])),
               self = P302}

{-DHUN| parses a HTML &sub entity. DHUN-}

subp :: MyParser Char
subp
  = baseParser{start =
                 \ _ ->
                   do _ <- string "&sub"
                      c <- anyChar
                      _ <- string ";"
                      return (Str (c : [])),
               self = Sub}

{-DHUN| parses a HTML &sup entity. DHUN-}

supp :: MyParser Char
supp
  = baseParser{start =
                 \ _ ->
                   do _ <- string "&sup"
                      c <- anyChar
                      _ <- string ";"
                      return (Str (c : [])),
               self = Sup}

{-DHUN| parses the start of a new URL. That is the place where a page begin that was downloaded from an URL different from the previous one DHUN-}

dhunurlp :: MyParser Char
dhunurlp
  = baseParser{start =
                 \ _ ->
                   do _ <- string "\ndhunparserurl "
                      return (Str ""),
               end = \ _ -> string "\n" >> return (), self = DhunUrl,
               allowed = [Root, Tag]}

{-DHUN| parses a Greek HTML entity. So a Greek letter or something similar DHUN-}

greekp :: MyParser Char
greekp
  = baseParser{start =
                 \ _ ->
                   do _ <- char '&'
                      s <- (oneOfTheStrings greek)
                      _ <- char ';'
                      return (Str (s)),
               self = Greek}

{-DHUN| parses the mediawiki poem tag. That is a little bit like verbatim but allowes some inner tags DHUN-}


poemp :: MyParser Char
poemp
  = (maketagparser ["poem"]){allowed =
                               SpaceIndent : everywhere ++ wikilinkwhere,
                             self = SpaceIndent}

{-DHUN| parses the mediawiki math tag. That is a latex formula in the wiki DHUN-}

mathp :: MyParser Char
mathp
  = (maketagparser ["math"]){allowed =
                               SpaceIndent : everywhere ++ wikilinkwhere,
                             self = Math}

annop :: MyParser Char
annop
  = (maketagparser ["annotation"]){allowed =
                                     SpaceIndent : everywhere ++ wikilinkwhere,
                                   self = Math, reenv = const Tag}

{-DHUN| parses a new chapter heading DHUN-}

chapterp :: MyParser Char
chapterp
  = baseParser{start =
                 \ _ ->
                   do _ <- try
                             (do _ <- string "\n"
                                 many (char ' '))
                      string "dhunincludechaper" >> return (Str ""),
               end = \ _ -> string "/dhunincludechaper" >> return (),
               self = Chapter}

{-DHUN| parses a horizontal dividing line DHUN-}

hdevlinep :: MyParser Char
hdevlinep
  = baseParser{start =
                 \ _ ->
                   do _ <- string "----"
                      skipMany (string "-")
                      return (Str ""),
               allowed = [Root], self = HDevLine}

{-DHUN| parses the mediawiki 'nowiki' tag DHUN-}

nowikip :: MyParser Char
nowikip
  = baseParser{start = \ _ -> string "<nowiki>" >> return (Str ""),
               end = \ _ -> string "</nowiki>" >> return (),
               allowed = everywhere ++ wikilinkwhere ++ [SpaceIndent],
               self = NoWiki}

{-DHUN| parses the mediawiki 'noinclude' tag DHUN-}

noincludep :: MyParser Char
noincludep
  = baseParser{start =
                 \ _ -> string "<noinclude>" >> return (Str ""),
               end =
                 \ _ ->
                   try (string "</noinclude>" >> return ()) <|>
                     lookAhead (eof >> return ()),
               self = NoInclude}

{-DHUN| parses the mediawiki 'includeonly' tag DHUN-}

includep :: MyParser Char
includep
  = baseParser{start =
                 \ _ -> string "<includeonly>" >> return (Str ""),
               end = \ _ -> string "</includeonly>" >> return (),
               self = IncludeOnly}

{-DHUN| parses the mediawiki 'onlyinclude' tag DHUN-}

includep2 :: MyParser Char
includep2
  = baseParser{start =
                 \ _ -> string "<onlyinclude>" >> return (Str ""),
               end = \ _ -> string "</onlyinclude>" >> return (),
               self = IncludeOnly}

{-DHUN| parses the mediawiki 'gallery' tag DHUN-}

galleryp :: MyParser Char
galleryp
  = baseParser{start =
                 \ _ ->
                   do _ <- string "<gallery"
                      skipMany (try (attr))
                      _ <- try (many (try (oneOf " \n"))) <|> return []
                      _ <- char '>'
                      return (Str ""),
               end = \ _ -> string "</gallery>" >> return (), self = Gallery}

{-DHUN| parses a wikilink inside a gallery DHUN-}

gallerywlp :: MyParser Char
gallerywlp
  = baseParser{bad =
                 \ _ ->
                   do _ <- lookAhead (try (string "</gallery>"))
                      return (),
               start = \ _ -> string "\n" >> return (Str ""),
               end =
                 \ _ ->
                   do _ <- lookAhead (string "\n")
                      return (),
               modify = \ _ x -> dropWhile (== (C ' ')) x, allowed = [Gallery],
               self = Wikilink}

{-DHUN| parses the mediawiki 'imagemap' tag DHUN-}

imagemapp :: MyParser Char
imagemapp
  = baseParser{start =
                 \ _ ->
                   do _ <- string "<imagemap"
                      skipMany (try (attr))
                      _ <- try (many (try (oneOf " \n"))) <|> return []
                      _ <- char '>'
                      return (Str ""),
               end = \ _ -> string "</imagemap>" >> return (), self = ImageMap}

{-DHUN| parses a wikilink inside and imagemap DHUN-}

imagemapwlp :: MyParser Char
imagemapwlp
  = baseParser{bad =
                 \ _ ->
                   do _ <- lookAhead (try (string "</imagemap>"))
                      return (),
               start =
                 \ _ ->
                   (do _ <- string "\n"
                       try (lookAhead (string "Image:")) <|>
                         try (lookAhead (string "File:"))
                         <|> (lookAhead (string "Datei:")))
                     >> return (Str ""),
               end =
                 \ _ ->
                   do _ <- lookAhead (string "\n")
                      return (),
               allowed = [ImageMap], self = Wikilink}

{-DHUN| matches a sequence of arbitrary characters up to the character (an excluding it) where one of the strings given as first parameter matches DHUN-}

myany :: [String] -> Parser String
myany x
  = do b <- (try (lookAhead (oneOfTheStrings x) >> return False)) <|>
              return True
       if b then
         do c <- anyChar
            cs <- (myany x)
            return (c : cs)
         else return ""

{-DHUN| parses the mediawiki template DHUN-}

wikitemplatep :: MyParser Char
wikitemplatep
  = baseParser{start =
                 \ _ ->
                   do _ <- string "{{"
                      s <- myany ["}}", "|"]
                      return (Str s),
               end = \ _ -> string "}}" >> return (),
               allowed =
                 everywhere ++
                   wikilinkwhere ++ [TemplateInsideVerbatim, SpaceIndent],
               self = Template}

{-DHUN| a special stack frame for parsing the inside of a template DHUN-}

madframe :: MyStackFrame Char
madframe
  = MyStackFrame{endparser =
                   (try (lookAhead (oneOfTheStrings ["}}", "|", "="]))) >> return (),
                 startdata = Str "", environment = TemplateInside,
                 badparser = \ _ -> pzero, parsernumber = 0, nestingdepth = 0}

{-DHUN| parses the inside of a mediawiki template DHUN-}

templateinsidep :: MyParser Char
templateinsidep
  = baseParser{start =
                 \ _ ->
                   do _ <- string "|"
                      (_, bb) <- lookAhead
                                   (try (parseAnything2 [madframe] (remake parsers) []))
                      if
                        all
                          (\ g ->
                             case g of
                                 Open _ _ _ _ -> False
                                 _ -> True)
                          bb
                        then
                        try
                          (do s <- myany ["|", "}}", "="]
                              b <- (try (string "=") >> return True) <|> return False
                              if b then return (Str s) else pzero)
                          <|> return (Str "")
                        else return (Str ""),
               end = \ _ -> lookAhead (oneOfTheStrings ["|", "}}"]) >> return (),
               allowed = [Template], self = TemplateInside}

{-DHUN| parses the inside of a mediawiki template, it is parser verbatim that means inner structures are parsed, but returned as plain characters in the parse tree. Needed for templates use for source codes DHUN-}

templateinsideverbatimp :: MyParser Char
templateinsideverbatimp
  = baseParser{start =
                 \ sta ->
                   case sta of
                       (g : _) -> case (startdata g) of
                                      Str gg -> if
                                                  (trim gg) `elem`
                                                    ["HaskellGHCiExample",
                                                     "\"HaskellGHCi\",Visual Basic .NET: Vorlage:Code",
                                                     "C++-Programmierung/ Vorlage:Syntax", "syntax",
                                                     "Syntax", "C++-Programmierung/ Vorlage:Code",
                                                     "BigJava", "LaTeX/Usage", "LaTeX/LaTeX","java","Java",
                                                     "Latex Index"]
                                                  then
                                                  do _ <- string "|"
                                                     try
                                                       (do s <- myany ["|", "}}", "="]
                                                           b <- (try (string "=") >> return True)
                                                                  <|> return False
                                                           if b then return (Str s) else pzero)
                                                       <|> return (Str "")
                                                  else pzero
                                      _ -> pzero
                       [] -> pzero,
               end = \ _ -> lookAhead (oneOfTheStrings ["|", "}}"]) >> return (),
               allowed = [Template], self = TemplateInsideVerbatim,
               reenv = const TemplateInside}

{-DHUN| parses the inside of a template parameter DHUN-}

wikiparamp :: MyParser Char
wikiparamp
  = baseParser{start = \ _ -> string "{{{" >> return (Str ""),
               end = \ _ -> string "}}}" >> return (),
               allowed = everywhere ++ wikilinkwhere ++ [SpaceIndent],
               self = Parameter}

{-DHUN| parses a wikilink DHUN-}

wikilinkp :: MyParser Char
wikilinkp
  = baseParser{start = \ _ -> string "[[" >> return (Str ""),
               end = \ _ -> string "]]" >> return (),
               bad =
                 \ pchr ->
                   if
                     not $
                       (C '|') `elem`
                         (takeWhile
                            (\ g ->
                               case g of
                                   Open _ Wikilink _ _ -> False
                                   _ -> True)
                            pchr)
                     then
                     do _ <- lookAhead
                               ((try ((string "]") >> notFollowedBy (string "]")) <|>
                                   (try (string "[" >> return ()))
                                   <|> (try (string "\n") >> return ())))
                        return ()
                     else pzero,
               allowed = everywhere ++ wikilinkwhere ++ [SpaceIndent],
               self = Wikilink}

{-DHUN| parses a wikilink template for wikipedia links DHUN-}

templatewikilinkp :: MyParser Char
templatewikilinkp
  = baseParser{start = \ _ -> string "{{w|" >> return (Str ""),
               end = \ _ -> string "}}" >> return (),
               allowed = everywhere ++ wikilinkwhere,
               modify = \ _ x -> (C 'w') : (C ':') : x, self = Wikilink}

{-DHUN| parses a link DHUN-}

linkp :: MyParser Char
linkp
  = baseParser{bad =
                 \ _ ->
                   do _ <- lookAhead (do char '\n')
                      return (),
               start =
                 \ _ ->
                   do _ <- string "["
                      skipMany (char ' ')
                      s <- oneOfTheStrings ["http", "https", "mailto", "//"]
                      return (Str (if s == "//" then "http://" else s)),
               end = \ _ -> string "]" >> return (), allowed = everywherel,
               self = Link}

{-DHUN| parses a link, in contrast to linkp this does not match inside links and does not require the square bracket notation DHUN-}

linkp2 :: MyParser Char
linkp2
  = baseParser{start =
                 \ _ ->
                   do s <- oneOfTheStrings ["http://", "https://"]
                      return (Str s),
               end = \ _ -> lookAhead (oneOf " \n\r\t<>|\"") >> return (),
               allowed = everywherel2, self = Link2, reenv = const Link}

{-DHUN| parses a wikitable DHUN-}

wikitablep :: MyParser Char
wikitablep
  = baseParser{start =
                 \ _ ->
                   do _ <- try (char '\n') <|> return '\n'
                      skipMany (char ' ')
                      _ <- try (string "{|") <|> try (string "{{(!}}")
                      s <- many (noneOf "\n")
                      return (Str s),
               end =
                 \ _ ->
                   do _ <- string "\n"
                      skipMany (char ' ')
                      _ <- try (string "|}") <|> try (string "{{!)}}")
                      return (),
               self = Wikitable}

{-DHUN| parses a heading, can be a chapter heading as well as a section heading and so on  DHUN-}

wikiheadingp :: MyParser Char
wikiheadingp
  = baseParser{bad =
                 \ _ ->
                   lookAhead
                     (do _ <- string "\n"
                         return ()),
               start =
                 \ _ ->
                   do a1 <- string "\n="
                      a2 <- try (string "=") <|> return ""
                      a3 <- try (string "=") <|> return ""
                      a4 <- try (string "=") <|> return ""
                      a5 <- try (string "=") <|> return ""
                      a6 <- try (string "=") <|> return ""
                      case (a1 ++ a2 ++ a3 ++ a4 ++ a5 ++ a6) of
                          (_ : xs) -> return (Str xs)
                          [] -> pzero,
               end = \y->
                 case y of 
                  (Str x) -> do _ <- string x
                                _ <- notFollowedBy (char '=')
                                return ()
                  _ -> mzero,
               self = Wikiheading, allowed = everywherel2}

{-DHUN| parses an italic text DHUN-}

italicp :: MyParser Char
italicp
  = baseParser{start =
                 \ _ ->
                   do _ <- string "''"
                      return (Str "''"),
               end =
                 \ _ ->
                   do (do _ <- try (string "''")
                          _ <- notFollowedBy (string "'")
                          return ())
                        <|> notFollowedBy (noneOf "\n")
                      return (),
               allowed = SpaceIndent : Wikitable : Bold : everywherebi,
               self = Italic}

{-DHUN| parses a bold text DHUN-}

boldp :: MyParser Char
boldp
  = baseParser{start =
                 \ _ ->
                   do _ <- string "'''"
                      return (Str "'''"),
               end =
                 \ _ ->
                   do (do _ <- try (string "'''")
                          return ())
                        <|> notFollowedBy (noneOf "\n")
                      return (),
               allowed = SpaceIndent : Wikitable : Italic : everywherebi,
               self = Bold}

{-DHUN| parses a table caption DHUN-}

tablecapp :: MyParser Char
tablecapp
  = baseParser{bad =
                 \ _ ->
                   lookAhead
                     (try
                        (do _ <- string "||"
                            return ())
                        <|>
                        try
                          (do _ <- char '\n'
                              _ <- notFollowedBy (try (char '|') <|> (char '!'))
                              return ())
                        <|>
                        do _ <- string "!!"
                           return ()),
               start =
                 \ _ ->
                   do _ <- string "\n"
                      skipMany (char ' ')
                      _ <- string "|+"
                      _ <- notFollowedBy (oneOf "-}")
                      return (Str ""),
               end =
                 \ _ ->
                   lookAhead
                     (do _ <- char '\n'
                         _ <- try (char '|') <|> char '!'
                         notFollowedBy . char $ '}'
                         return ()),
               allowed = [Wikitable], self = TableCap}

{-DHUN| parses a table caption with additional parameter given to the beginning of the caption element in the wiki source  DHUN-}

tablecapp2 :: MyParser Char
tablecapp2
  = baseParser{start =
                 \ _ ->
                   do _ <- string "\n"
                      skipMany (char ' ')
                      _ <- string "|+"
                      _ <- many (noneOf "|")
                      _ <- char '|'
                      _ <- notFollowedBy (oneOf "-}")
                      return (Str ""),
               end =
                 \ _ ->
                   lookAhead
                     (try
                        (do _ <- char '\n'
                            _ <- try (char '|') <|> char '!'
                            notFollowedBy . char $ '}'
                            return ())
                        <|>
                        (try ((string "||" >> return ())) <|> (string "!!" >> return ()))),
               allowed = [Wikitable], self = TableCap}

{-DHUN| parses a table caption DHUN-}

tablecapp3 :: MyParser Char
tablecapp3
  = baseParser{start =
                 \ _ ->
                   do _ <- string "\n"
                      skipMany (char ' ')
                      _ <- string "|+"
                      _ <- notFollowedBy (oneOf "-}")
                      return (Str "2"),
               allowed = [Wikitable], self = TableCap}

{-DHUN| parses a table row separator DHUN-}

rowsepp :: MyParser Char
rowsepp
  = baseParser{start =
                 \ _ ->
                   do _ <- string "\n"
                      skipMany (char ' ')
                      _ <- try (string "|-") <|> try (string "{{!-}}")
                      s <- many (noneOf "\n")
                      return (Str s),
               allowed = [Wikitable], self = TableRowSep}

{-DHUN| parses a table column separator, with additional parameters parser to the beginning of the environment. This parser actually parses only the beginning elements and whats inside it See  DHUN-}

colsepp :: MyParser Char
colsepp
  = baseParser{bad =
                 \ _ ->
                   lookAhead
                     (try
                        (do _ <- try (string "||") <|> try (string "{{!!}}")
                            return ())
                        <|>
                        try
                          (do _ <- char '\n'
                              return ())
                        <|>
                        do _ <- try (string "!!")
                           return ()),
               start =
                 \ _ ->
                   try
                     (try
                        (do _ <- string "\n"
                            skipMany (char ' ')
                            _ <- try (string "|") <|> try (string "{{!}}")
                            _ <- notFollowedBy (oneOf "-}")
                            return (Str ""))
                        <|>
                        do _ <- try (string "||") <|> try (string "{{!!}}")
                           return (Str "")),
               end =
                 \ _ ->
                   try
                     (do _ <- try (string "|") <|> try (string "{{!}}")
                         _ <- notFollowedBy (oneOf "}|")
                         return ()),
               allowed = [Wikitable], self = TableColSep}

{-DHUN| parses a column separator without anything inside it DHUN-}

colsepp2 :: MyParser Char
colsepp2
  = baseParser{start =
                 \ _ ->
                   try
                     (do _ <- string "\n"
                         skipMany (char ' ')
                         _ <- try (string "|") <|> try (string "{{!}}")
                         _ <- notFollowedBy (oneOf "-}")
                         return (Str "2"))
                     <|>
                     do _ <- try (string "||") <|> try (string "{{!!}}")
                        return (Str "2"),
               allowed = [Wikitable], self = TableColSep}

{-DHUN| parses a table header column separator, with additional parameters parser to the beginning of the environment. This parser actually parses only the beginning elements and whats inside it See  DHUN-}

headcolsepp :: MyParser Char
headcolsepp
  = baseParser{bad =
                 \ _ ->
                   lookAhead
                     (try
                        (do _ <- try (string "||") <|> (string "{{!!}}")
                            return ())
                        <|>
                        try
                          (do _ <- char '\n'
                              return ())
                        <|>
                        do _ <- try (string "!!")
                           return ()),
               start =
                 \ _ ->
                   try
                     (do _ <- string "\n"
                         skipMany (char ' ')
                         _ <- string "!"
                         _ <- notFollowedBy (oneOf "-}")
                         return (Str ""))
                     <|>
                     do _ <- string "!!"
                        return (Str ("")),
               end =
                 \ _ ->
                   do _ <- try (char '|') <|> ((string "{{!}}") >> return '|')
                      notFollowedBy (oneOf "-}|")
                      return (),
               allowed = [Wikitable], self = TableHeadColSep}

{-DHUN| parses a header column separator without anything inside it DHUN-}

headcolsepp2 :: MyParser Char
headcolsepp2
  = baseParser{start =
                 \ _ ->
                   try
                     (do _ <- string "\n"
                         skipMany (char ' ')
                         _ <- string "!"
                         _ <- notFollowedBy (oneOf "-}")
                         return (Str ""))
                     <|>
                     do _ <- string "!!"
                        return (Str ""),
               allowed = [Wikitable], self = TableHeadColSep}

attrinside :: String -> GenParser Char () Char
attrinside x = try (string "&amp;" >> return '&') <|> (noneOf x)

{-DHUN| matches a key value pair . So an attribute of an HTML element DHUN-}

attr :: GenParser Char () ([Char], [Char])
attr
  = do try (skipMany1 (oneOf " \t\n")) <|> (lookAhead (char '=')>> return ())
       k <- try (many1 (try (alphaNum) <|> oneOf ":-")) <|> (((lookAhead (char '=')) >> return "class"))
       v <- try
              (do skipMany (oneOf " \t\n")
                  _ <- char '='
                  skipMany (oneOf " \t\n")
                  vv <- try
                          (do _ <- try (char '"')
                              vvv <- many (attrinside "\"><")
                              _ <- try (char '"') <|> try (noneOf "<|>") <|> return ' '
                              return vvv)
                          <|>
                          try
                            (do _ <- (char '\'')
                                vvv <- many (attrinside "'><")
                                _ <- try (char '\'') <|> try (noneOf "<|>") <|> return ' '
                                return vvv)
                          <|>
                          (do vvv <- many (attrinside "\"'></| ")
                              _ <- try (char '\'') <|> try (char '"') <|> try (noneOf "</|> ")
                                     <|> return ' '
                              return vvv)
                  return vv)
              <|> return ""
       return (k, v)

{-DHUN| Matches a key value pair. So an attribute of an HTML element DHUN-}

attrns :: GenParser Char u ([Char], [Char])
attrns
  = do k <- many1 (try (alphaNum) <|> oneOf ":-")
       v <- do _ <- char '='
               skipMany (oneOf " \n")
               vv <- try
                       (do _ <- try (char '"') <|> char '\''
                           vvv <- many (noneOf "\"'><|")
                           _ <- try (char '"') <|> try (noneOf "<|>") <|> return ' '
                           return vvv)
                       <|>
                       (do vvvv <- (noneOf "\"'></| =")
                           vvv <- many (noneOf "\"'></| ")
                           _ <- try (char '"') <|> try (char '\'') <|> try (noneOf "</|> ")
                                  <|> return ' '
                           return (vvvv : vvv))
               return vv
       _ <- try (many (oneOf " \n")) <|> return []
       return (k, v)

{-DHUN| Matches a list of key value pairs . So all attributes of an HTML element DHUN-}

attrp :: MyParser Char
attrp
  = baseParser{start =
                 \ _ ->
                   do atr <- try (attrns)
                      return (Attr atr),
               allowed = [TableHeadColSep, TableColSep, TableCap],
               self = Attribute}

{-DHUN| Parses the HTML 'pre' tag DHUN-}

prep :: MyParser Char
prep
  = baseParser{start =
                 \ _ ->
                   do _ <- string "<"
                      p <- try (string "pre") <|> try (string "PRE") <|>
                             try (string "xmp")
                             <|> string "XMP"
                      atr <- many (try (attr))
                      _ <- try (many (oneOf " \n")) <|> return []
                      _ <- char '>'
                      return (TagAttr p (Map.fromList atr)),
               end = \y->
                 case y of
                  (TagAttr x _) ->
                   do _ <- char '<'
                      _ <- char '/'
                      _ <- string x
                      _ <- char '>'
                      return ()
                  _ -> mzero,
               allowed = everywhere, self = Preformat}

{-DHUN| Parses the HTML 'br' tag DHUN-}

brparser :: MyParser Char
brparser
  = baseParser{start =
                 \ _ ->
                   do _ <- string "<"
                      _ <- try (string "/") <|> return ""
                      _ <- try (string "br") <|> string ("BR")
                      skipMany (try (attr))
                      _ <- try (many (oneOf " \n")) <|> return []
                      _ <- try (string "/") <|> return ""
                      _ <- try (many (oneOf " \n")) <|> return []
                      _ <- char '>'
                      return (TagAttr "br" Map.empty),
               allowed = SpaceIndent : everywhere, self = Tag}

{-DHUN| Returns a Parser that matches all HTML elements, given by the list of strings given as first input parameter. The parser does not match inside tables. Use makettagparser for that DHUN-}

maketagparser :: [String] -> MyParser Char
maketagparser tags
  = baseParser{start =
                 \ _ ->
                   do _ <- string "<"
                      t <- (oneOfTheStrings tags)
                      atr <- many (try (attr))
                      _ <- try (many (oneOf " \n")) <|> return []
                      _ <- char '>'
                      return (TagAttr (t) (Map.fromList atr)),
               end = \y-> 
                case y of
                  (TagAttr x _) ->
                   do _ <- char '<'
                      _ <- try (many (oneOf " ")) <|> return []
                      _ <- if x `elem` nonNestTags then try (char '/') <|> (return '/')
                             else (char '/')
                      _ <- string x
                      _ <- char '>'
                      return ()
                  _ -> mzero,    
               allowed = SpaceIndent : everywheretbl, self = Tag}

{-DHUN| Parser for the 'meta' and 'link' tag of HTML DHUN-}

metatagparser :: MyParser Char
metatagparser
  = baseParser{start =
                 \ _ ->
                   do _ <- string "<"
                      t <- (oneOfTheStrings ["meta", "link", "img"])
                      atr <- many (try (attr))
                      _ <- try (many (oneOf " \n")) <|> return []
                      _ <- try (char '/') <|> (return '/')
                      _ <- try (char '>') <|> (return '>')
                      return (TagAttr (t) (Map.fromList atr)),
               allowed = SpaceIndent : everywhere, self = Tag}

{-DHUN| Parser for the !DOCTYPE tag of HTML DHUN-}

doctagparser :: MyParser Char
doctagparser
  = baseParser{start =
                 \ _ ->
                   do _ <- string "<"
                      t <- (oneOfTheStrings ["!DOCTYPE"])
                      _ <- many (noneOf ">")
                      _ <- char '>'
                      return (TagAttr (t) (Map.fromList [])),
               allowed = SpaceIndent : everywhere, self = Tag}

{-DHUN| Parser for closing HTML tags that have not matching opening tag. DHUN-}

ctagparser :: [String] -> GenParser Char () ()
ctagparser tags
  = do _ <- string "</"
       _ <- (oneOfTheStrings tags)
       _ <- try (many (oneOf " \n")) <|> return []
       _ <- char '>'
       return ()

{-DHUN| Returns a parser that matches all HTML elements, given by the list of strings given as first input parameter. The parser matches only the opening part of the tag. The inside of it is not processed by this parser. The opening tag may also self closing like <br/> because of the tailing /. DHUN-}

maketagparser2 :: [String] -> MyParser Char
maketagparser2 tags
  = baseParser{start =
                 \ _ ->
                   do _ <- string "<"
                      t <- (oneOfTheStrings tags)
                      atr <- many (try (attr))
                      _ <- try (many (oneOf " \n")) <|> return []
                      _ <- try (char '/') <|> (return '/')
                      _ <- char '>'
                      return (TagAttr (t) (Map.fromList atr)),
               allowed = Wikitable : SpaceIndent : everywheretbl, self = Tag}

{-DHUN| Returns a parser that matches all HTML elements, given by the list of strings given as first input parameter. The parser matches only the opening part of the tag. The inside of it is not processed by this parser. The opening tag may not be self closing so not like <br/> because of the tailing /. DHUN-}

maketagparser3 :: [String] -> MyParser Char
maketagparser3 tags
  = baseParser{start =
                 \ _ ->
                   do _ <- string "<"
                      t <- (oneOfTheStrings tags)
                      atr <- many (try (attr))
                      _ <- try (many (oneOf " \n")) <|> return []
                      _ <- char '>'
                      return (TagAttr (t) (Map.fromList atr)),
               allowed = SpaceIndent : everywheretbl, self = Tag}

{-DHUN| tags that can not be nested and are thus allowed to closed by an opening tag instead of a closing one DHUN-}

nonNestTags :: [String]
nonNestTags
  = ["tt", "pre", "TT", "PRE", "b", "B", "i", "I", "sc", "SC",
     "code", "CODE"]

{-DHUN| Returns a parser that matches all HTML elements, given by the list of strings given as first input parameter. The parser matches only inside tables. Use maketagparser for that DHUN-}

makettagparser :: [String] -> MyParser Char
makettagparser tags
  = baseParser{bad =
                 \ _ ->
                   do _ <- lookAhead (do try (string "\n|") <|> (string "||"))
                      return (),
               start =
                 \ _ ->
                   do _ <- string "<"
                      t <- (oneOfTheStrings tags)
                      atr <- many (try (attr))
                      _ <- try (many (oneOf " \n")) <|> return []
                      _ <- char '>'
                      return (TagAttr (t) (Map.fromList atr)),
               end = \y->
                case y of 
                 (TagAttr x _) ->
                   do _ <- char '<'
                      _ <- try (many (oneOf " ")) <|> return []
                      _ <- if x `elem` nonNestTags then try (char '/') <|> (return '/')
                             else (char '/')
                      _ <- string x
                      _ <- char '>'
                      return ()
                 _ -> mzero,     
               allowed = [Wikitable], self = Tag}

{-DHUN| Returns a parser that matches all HTML elements, given by the list of strings given as first input parameter. The parser matches only the opening part of the tag. The inside of it is not processed by this parser. The opening tag may also self closing like <br/> because of the tailing /. Matches only inside tables DHUN-}

makettagparser2 :: [String] -> MyParser Char
makettagparser2 tags
  = baseParser{bad =
                 \ _ ->
                   do _ <- lookAhead
                             (do _ <- anyChar
                                 try (string "\n|") <|> (string "||"))
                      return (),
               start =
                 \ _ ->
                   do _ <- string "<"
                      t <- (oneOfTheStrings tags)
                      atr <- many (try (attr))
                      _ <- try (many (oneOf " \n")) <|> return []
                      _ <- char '/'
                      _ <- char '>'
                      return (TagAttr (t) (Map.fromList atr)),
               allowed = [Wikitable], self = Tag}

{-DHUN| maketagparser for all HTML elements see documentation on function maketagparser DHUN-}

tagparser :: MyParser Char
tagparser = maketagparser listOfTags

{-DHUN| maketagparser for the 'pre' HTML tag see documentation on function maketagparser DHUN-}

tagparserp :: MyParser Char
tagparserp = maketagparser ["pre"]

{-DHUN| maketagparser for the HTML tags for HTML tables see documentation on function maketagparser DHUN-}

tagparsert :: MyParser Char
tagparsert
  = (maketagparser listOfTableTags){self = TableTag,
                                    reenv = const Tag}

{-DHUN| makettagparser for all HTML elements see documentation on function makettagparser DHUN-}

ttagparser :: MyParser Char
ttagparser = makettagparser listOfTags

{-DHUN| makettagparser for the 'pre' HTML tag see documentation on function makettagparser DHUN-}

ttagparserp :: MyParser Char
ttagparserp = makettagparser ["pre"]

{-DHUN| makettagparser for the HTML tags for HTML tables see documentation on function makettagparser DHUN-}

ttagparsert :: MyParser Char
ttagparsert
  = (makettagparser listOfTableTags){self = TableTag,
                                     reenv = const Tag}

{-DHUN| maketagparser2 for all HTML elements see documentation on function maketagparser2 DHUN-}

tagparser2 :: MyParser Char
tagparser2 = maketagparser2 listOfTags

{-DHUN| maketagparser2 for the 'pre' HTML tag see documentation on function maketagparser2 DHUN-}

tagparser2p :: MyParser Char
tagparser2p = maketagparser2 ["pre"]

{-DHUN| maketagparser2 for the HTML tags for HTML tables see documentation on function maketagparser2 DHUN-}

tagparser2t :: MyParser Char
tagparser2t
  = (maketagparser2 listOfTableTags){self = TableTag,
                                     reenv = const Tag}

{-DHUN| makettagparser2 for all HTML elements see documentation on function makettagparser2 DHUN-}

ttagparser2 :: MyParser Char
ttagparser2 = makettagparser2 listOfTags

{-DHUN| makettagparser2 for the 'pre' HTML tag see documentation on function makettagparser2 DHUN-}

ttagparser2p :: MyParser Char
ttagparser2p = makettagparser2 ["pre"]

{-DHUN| makettagparser2 for the HTML tags for HTML tables see documentation on function makettagparser2 DHUN-}

ttagparser2t :: MyParser Char
ttagparser2t
  = (makettagparser2 listOfTableTags){self = TableTag,
                                      reenv = const Tag}

{-DHUN| a parser for mediawiki source extension tags DHUN-}

tagparsers :: MyParser Char
tagparsers
  = (maketagparser ["source", "syntaxhighlight"]){self = Source}

{-DHUN| a parser for mediawiki source extension tags inside tables DHUN-}

ttagparsers :: MyParser Char
ttagparsers
  = (maketagparser ["source", "syntaxhighlight"]){self = Source,
                                                  allowed = [Wikitable]}


tagparsers2 :: MyParser Char
tagparsers2
  = (maketagparser2 ["source", "syntaxhighlight"]){self = Source}

{-DHUN| a parser for mediawiki source extension tags inside tables DHUN-}

ttagparsers2 :: MyParser Char
ttagparsers2
  = (maketagparser2 ["source", "syntaxhighlight"]){self = Source,
                                                  allowed = [Wikitable]}

{-DHUN| a parser for HTML tables DHUN-}

mytablep :: MyParser Char
mytablep
  = (maketagparser ["table"]){self = TableTag,
                              reenv = const Wikitable,
                              allowed = Wikitable : SpaceIndent : everywheretbl}

{-DHUN| a parser for HTML table rows DHUN-}

mytrsepp :: MyParser Char
mytrsepp
  = (maketagparser3 ["tr"]){reenv = const TableRowSep,
                            allowed = everywhere}

{-DHUN| a parser for normal HTML table cells DHUN-}

mytcolsepp :: MyParser Char
mytcolsepp
  = (maketagparser3 ["td"]){reenv = const TableColSep,
                            allowed = everywhere}

{-DHUN| a parser for HTML table captions cells DHUN-}

mytcapp :: MyParser Char
mytcapp
  = (maketagparser ["caption"]){reenv = const TableCap,
                                allowed = everywhere}

{-DHUN| a parser for HTML table header cells, so th tags DHUN-}

mythcolsepp :: MyParser Char
mythcolsepp
  = (maketagparser3 ["th"]){reenv = const TableHeadColSep,
                            allowed = everywhere}





mytrsepp2 :: MyParser Char
mytrsepp2
  = (maketagparser ["tr"]){reenv = const TableRowSep,
                            allowed = everywhere}

{-DHUN| a parser for normal HTML table cells DHUN-}

mytcolsepp2 :: MyParser Char
mytcolsepp2
  = (maketagparser ["td"]){reenv = const TableColSep,
                            allowed = everywhere}

{-DHUN| a parser for HTML table captions cells DHUN-}

mytcapp2 :: MyParser Char
mytcapp2
  = (maketagparser ["caption"]){reenv = const TableCap,
                                allowed = everywhere}

{-DHUN| a parser for HTML table header cells, so th tags DHUN-}

mythcolsepp2 :: MyParser Char
mythcolsepp2
  = (maketagparser ["th"]){reenv = const TableHeadColSep,
                            allowed = everywhere}







{-DHUN| a parser for HTML tables for html parse mode only DHUN-}

htmytablep :: MyParser Char
htmytablep = (maketagparser ["table"])

{-DHUN| a parser for HTML table rows for html parse mode only DHUN-}

htmytrsepp :: MyParser Char
htmytrsepp = (maketagparser ["tr"])

{-DHUN| a parser for normal HTML table cells for html parse mode only DHUN-}

htmytcolsepp :: MyParser Char
htmytcolsepp = (maketagparser ["td"])

{-DHUN| a parser for HTML table captions cells for html parse mode only DHUN-}

htmytcapp :: MyParser Char
htmytcapp = (maketagparser ["caption"])

{-DHUN| a parser for HTML table header cells, so th tags for html parse mode only DHUN-}

htmythcolsepp :: MyParser Char
htmythcolsepp = (maketagparser ["th"])

{-DHUN| a parser for closing HTML tags which don't have an opening partner. This parser is only allowed to match within itemization enumerations etc. DHUN-}

itagparser :: MyParser Char
itagparser
  = baseParser{start =
                 \ _ ->
                   do _ <- string "<"
                      skipMany (char '/')
                      t <- (oneOfTheStrings ["small"])
                      atr <- many (try (attr))
                      _ <- try (many (oneOf " \n")) <|> return []
                      _ <- char '>'
                      return (TagAttr (t) (Map.fromList atr)),
               allowed = [Itemgroup], self = Tag}

{-DHUN| a parser for closing HTML tags which don't have an opening partner DHUN-}

rtagparser :: MyParser Char
rtagparser
  = baseParser{start =
                 \ _ ->
                   do _ <- string "<"
                      skipMany (char '/')
                      s <- (oneOfTheStrings (listOfTags ++ listOfTableTags))
                      atr <- many (try (attr))
                      _ <- try (many (oneOf " \n")) <|> return []
                      _ <- char '>'
                      return (TagAttr ("bad" ++ s) (Map.fromList atr)),
               allowed = everywhere, self = Tag}

{-DHUN| a parser for HTML opening tags which might be self closing but never have a matching closing partner DHUN-}

stagparser :: MyParser Char
stagparser
  = baseParser{start =
                 \ _ ->
                   do _ <- string "<"
                      t <- (oneOfTheStrings (listOfTags ++ listOfTableTags))
                      atr <- many (try (attr))
                      _ <- try (many (oneOf " ")) <|> return []
                      _ <- try (char '/') <|> return 'f'
                      _ <- char '>'
                      return (TagAttr t (Map.fromList atr)),
               self = Tag, allowed = []}

{-DHUN| a parser for HTML page breaks DHUN-}

pagebreakp :: MyParser Char
pagebreakp
  = baseParser{start =
                 \ _ ->
                   do _ <- string "<div style=\"page-break-before:always\"/>"
                      return (Str ""),
               end = \ _ -> return (), allowed = everywhere, self = PageBreak}

{-DHUN| a parser for HTML comments DHUN-}

commentp :: MyParser Char
commentp
  = baseParser{start =
                 \ _ ->
                   do _ <- string "<!--"
                      return (Str ""),
               allowed = everywhere ++ [Tag, TemplateInsideVerbatim],
               end =
                 \ _ ->
                   do _ <- string "-->"
                      return (),
               self = Comment}

{-DHUN| a parser for mediawiki reserved words DHUN-}

reservedp :: MyParser Char
reservedp
  = baseParser{start =
                 \ _ ->
                   do s <- try (string "__NOTOC__") <|>
                             try (string "__NOEDITSECTION__")
                             <|> try (string "__notoc__")
                             <|> try (string "__TOC__")
                             <|> try (string "__Toc__")
                             <|> try (string "__INHALTSVERZEICHNIS__")
                             <|> try (string "__KEIN_INHALTSVERZEICHNIS__")
                             <|> try (string "__FORCETOC__")
                             <|> string "&nbsp;"
                      return (Str s),
               self = Reserved, allowed = SpaceIndent : everywhere}

{-DHUN| See documentation on evaluateItemgroup and itemParserLevelTwoDHUN-}

itemStartString :: Anything Char -> String
itemStartString (ItemStart x) = x : []
itemStartString _ = ""

{-DHUN| See documentation on evaluateItemgroup and itemParserLevelTwo DHUN-}

itemStopString :: Anything Char -> String
itemStopString (ItemStop x) = x : []
itemStopString _ = ""

{-DHUN| see documentation on evaluateItemgroup. Parser to parse the ItemStart and ItemStop elements which are essentially bracket into environments which are essentially nodes with children in the parse tree DHUN-}

itemParserLevelTwo :: MyParser (Anything Char)
itemParserLevelTwo
  = MyParser{bad = \ _ -> pzero,
             start =
               \ _ ->
                 do pos <- getPosition
                    z <- token show (\ _ -> pos)
                           (\ x -> if (itemStartString x == []) then Nothing else Just x)
                    return (Str (itemStartString z)),
             end = \z ->
              case z of
               (Str y) ->
                 do pos <- getPosition
                    _ <- token show (\ _ -> pos)
                           (\ x -> if (itemStopString x == y) then Just x else Nothing)
                    return ()
               _->mzero,
             allowed = [Root, ItemEnv], self = ItemEnv, modify = \ _ x -> x,
             reenv = id}

{-DHUN| see documentation on evaluateItemgroup. helper function to generate a list of only opening or only closing brackets, that is ItemStart or ItemStop values. The string given as first parameter states which kinds of bracket shall be opened or closed. For Openning brackets you have to call this function ItemStart as second and True as third parameter. For closing brackets you have to use ItemStop and False DHUN-}

generateEnvironmentTagsHelper ::
                              String -> (Char -> Anything Char) -> Bool -> [Anything Char]
generateEnvironmentTagsHelper (c : cs) t b
  = if b then
      (t c) : ([Item c]) ++ (generateEnvironmentTagsHelper cs t b) else
      (generateEnvironmentTagsHelper cs t b) ++ [(t c)]
generateEnvironmentTagsHelper [] _ _ = []

{-DHUN| see documentation on evaluateItemgroup and insertEnvironmentTags. The first parameter is a string consisting of the characters #:* that is the begging of the last Itemline. The second is the same for the current ItemLine. This function return a list of ItemStart and ItemStop values for the difference between the first and the second parameter DHUN-}

generateEnvironmentTags :: String -> String -> [Anything Char]
generateEnvironmentTags (o : os) (n : ns)
  = if (o == n) then generateEnvironmentTags os ns else
      (generateEnvironmentTagsHelper (o : os) ItemStop False) ++
        (generateEnvironmentTagsHelper (n : ns) ItemStart True)
generateEnvironmentTags (o : os) []
  = generateEnvironmentTagsHelper (o : os) ItemStop False
generateEnvironmentTags [] (n : ns)
  = generateEnvironmentTagsHelper (n : ns) ItemStart True
generateEnvironmentTags [] [] = []

{-DHUN| see documentation on evaluateItemgroup. The first parameter is a string consisting of the characters #:* that is the begging of the last Itemline. The second parameter is the content of an Itemgroup. This function returns the parse tree with the ItemStart and ItemStop values inserted   DHUN-}

insertEnvironmentTags ::
                      String -> [Anything Char] -> [Anything Char]
insertEnvironmentTags s ((Environment ItemLine (Str x) _) : xs)
  = (generateEnvironmentTags s x) ++
      ((if
          (((length s) > (length x)) &&
             (if length (generateEnvironmentTags s x) > 0 then
                (case last (generateEnvironmentTags s x) of
                     Item _ -> False
                     _ -> True)
                else True)
               && (x /= ""))
            || (s == x)
          then [Item (last s)] else [])
         ++ insertEnvironmentTags x xs)
insertEnvironmentTags s (x : xs) = x : insertEnvironmentTags s xs
insertEnvironmentTags _ [] = []

{-DHUN| see documentation on evaluateItemgroup. Inserts the ItemStart and ItemStop values as preparation for running the second level parse on the content of an ItemGroup. The first pararmenter is a string consisting of the characters #:* that is the begging of the first Itemline. The second parameter is the content of an Itemgroup. This function returns the parse tree with the ItemStart and ItemStop values inserted  DHUN-}

toEnvironmentTags :: String -> [Anything Char] -> [Anything Char]
toEnvironmentTags s l
  = insertEnvironmentTags ""
      ((Environment ItemLine (Str s) []) :
         l ++ [(Environment ItemLine (Str "") [])])

{-DHUN| see documentation on evaluateItemgroup. Get the second level parse result, back to the first level parse result DHUN-}

convertFromParsingLevelTwoToLevelOne ::
                                     [Anything (Anything Char)] -> [Anything Char]
convertFromParsingLevelTwoToLevelOne ((C x) : xs)
  = x : convertFromParsingLevelTwoToLevelOne xs
convertFromParsingLevelTwoToLevelOne ((Environment Root _ x) : xs)
  = (convertFromParsingLevelTwoToLevelOne x) ++
      (convertFromParsingLevelTwoToLevelOne xs)
convertFromParsingLevelTwoToLevelOne
  ((Environment ItemEnv s l) : xs)
  = (Environment ItemEnv s
       (findMatchingBrackets (remake parsers)
          ((convertFromParsingLevelTwoToLevelOne l))))
      : convertFromParsingLevelTwoToLevelOne xs
convertFromParsingLevelTwoToLevelOne (_ : xs)
  = (convertFromParsingLevelTwoToLevelOne xs)
convertFromParsingLevelTwoToLevelOne [] = []

{-DHUN| see documentation on evaluateItemgroup. Runs the parser itemParserLevelTwo on the inner part of an ItemGroup DHUN-}

runItemGroupPraserLevelTwo ::
                           GenParser (Anything Char) () [Anything (Anything Char)] ->
                             [Anything Char] -> [Anything (Anything Char)]
runItemGroupPraserLevelTwo p input
  = case (parse p "" input) of
        Left _ -> []
        Right x -> x

{-DHUN| this is for parsing itemization. It is implemented as two step process in the first step Itemgroup is created with ItemLine s in it. And ItemLine is a line starting with any combination of *#: . And Itemgroup is a sequence of lines of that kind. After this is done this function gets called with the content of an itemgroup. In the second step ItemStart and ItemStop elements are created, those are essentially the bracketed of the opening an closing part of the enumerations itemizations etc. . The function toEnvironmentTags does the insertion of the ItemStop and ItemStart values and remove the ItemLine values. parseAnything2 is run on it using the itemParserLevelTwo. This is turned into the parse tree by being passed to decon2 and put out of the parser monad by runItemGroupPraserLevelTwo. The resulting parse tree is one of type [Anything (Anything Char)] this needs to be but one level down to [Anything Char] this is done be convertFromParsingLevelTwoToLevelOne. The first parameter is a string consisting of the characters #:* that is the begging of the first Itemline. The second parameter is the content of an Itemgroup  DHUN-}

evaluateItemgroup :: String -> [Anything Char] -> [Anything Char]
evaluateItemgroup s l
  = convertFromParsingLevelTwoToLevelOne
      (runItemGroupPraserLevelTwo
         (decon2 (remake [itemParserLevelTwo])
            (parseAnything2
               [MyStackFrame{endparser = pzero, startdata = Str "",
                             environment = Root, badparser = \ _ -> pzero, parsernumber = 0,
                             nestingdepth = 0}]
               (remake [itemParserLevelTwo])
               []))
         (toEnvironmentTags s l))

{-DHUN| a parser for a group of lines starting with one of *:;# representing in enumeration itemization etc. . This particular parser is allowed to match nearly everywhere DHUN-}

itempgroupp :: MyParser Char
itempgroupp
  = MyParser{bad = \ _ -> pzero,
             start =
               \ _ ->
                 (do _ <- string "\n"
                     a <- many1 (oneOf ['*', ':', ';', '#'])
                     return (Str a)),
             end =
               \ _ ->
                 lookAhead
                   (try
                      (do _ <- string "\n"
                          notFollowedBy ((oneOf ['*', ':', ';', '#'])))),
             allowed = [Root, Wikitable, TemplateInside, Tag], self = Itemgroup,
             modify = \ z-> 
                case z of 
                  (Str x) -> evaluateItemgroup x
                  _ -> id,
             reenv = id}

{-DHUN| a parser for a group of lines starting with one of *:;# representing in enumeration itemization etc. . This particular parser is only allowed to match within templates DHUN-}

itempgrouppt :: MyParser Char
itempgrouppt
  = MyParser{bad = \ _ -> pzero,
             start =
               \ _ ->
                 do _ <- string "\n"
                    a <- many1 (oneOf ['*', ':', ';', '#'])
                    return (Str a),
             end =
               \ _ ->
                 (try
                    (lookAhead (string "}}") <|>
                       (lookAhead (string "\n" >> (many (char ' ')) >> string "|")))
                    >> return ()),
             allowed = [TemplateInside], self = Itemgroup,
             modify = \z->
               case z of 
                 (Str x) -> evaluateItemgroup x
                 _ -> id,
             reenv = id}

{-DHUN| a parser for a preformat created by indenting with space DHUN-}

presectionp :: MyParser Char
presectionp
  = MyParser{bad = \ _ -> try (string "}}") >> (return ()),
             start =
               \stack -> do if TableTag `elem` (map environment stack) then mzero else return ()
                            _ <- string "\n"
                            _ <- char ' '
                            return (Str ""),
             end =
               \ _ ->
                 lookAhead
                   (do _ <- string "\n"
                       notFollowedBy (char ' ')),
             allowed = [Root, Tag], self = SpaceIndent,
             modify = \ _ x -> (C ' ') : x, reenv = id}

{-DHUN| a parser for a preformat created by indenting with space withing templates DHUN-}

presectionpt :: MyParser Char
presectionpt
  = MyParser{bad = \ _ -> try (string "}}") >> (return ()),
             start =
               \ _ ->
                 do _ <- string "\n"
                    _ <- notFollowedBy ((many1 (char ' ')) >> (char '|'))
                    _ <- char ' '
                    return (Str ""),
             end =
               \ _ ->
                 lookAhead
                   (do _ <- string "\n"
                       notFollowedBy (char ' ')),
             allowed = [TemplateInside], self = SpaceIndent,
             modify = \ _ x -> (C ' ') : x, reenv = id}

{-DHUN| a parser for a line starting with one of *:;# representing in enumeration itemization etc. DHUN-}

itemlinep :: MyParser Char
itemlinep
  = MyParser{bad = \ _ -> pzero,
             start =
               \ _ ->
                 do _ <- string "\n"
                    a <- many1 (oneOf ['*', ':', ';', '#'])
                    return (Str a),
             end = \ _ -> (return ()), allowed = [Itemgroup], self = ItemLine,
             modify = \ _ x -> x, reenv = id}

droplinks :: [Anything Char] -> [Anything Char]
droplinks ll = concat (map go ll)
  where go :: Anything Char -> [Anything Char]
        go (Environment Tag (TagAttr "div" m) _) | ((Map.lookup "class" m) == (Just "NavContent")) = []
        go (Environment _ (TagAttr _ m) _)  | ((Map.lookup "class" m) == (Just "navbox-abovebelow")) = []
        go (Environment _ (TagAttr _ m) _) | ((Map.lookup "class" m) == (Just "navbox-group")) = []
        go (Environment _ (TagAttr _ m) _) | (maybe False (isInfixOf "navbox-list") (Map.lookup "class" m)) = []
        go (Environment _ (TagAttr _ m) _) | (maybe False (isInfixOf "nv-edit") (Map.lookup "class" m)) = []
        go (Environment _ (TagAttr _ m) _) | (maybe False (isInfixOf "nv-talk") (Map.lookup "class" m)) = []
        go (Environment _ (TagAttr _ m) _) | (maybe False (isInfixOf "nv-view") (Map.lookup "class" m)) = [] 
        go (Environment _ (TagAttr _ m) _) | (maybe False (isInfixOf "collaps") (Map.lookup "class" m)) = []  
        go (Environment Tag (TagAttr "a" _) l) = droplinks l
        go (Environment x y l) = [Environment x y (droplinks l)]
        go x = [x]

{-DHUN| takes a parse tree that was created form the HTML returned by MediaWiki when being requested for the print version of a wiki page. And returns a modified version of that parse tree ready for being converted to LaTeX with treeToLaTeX3 |DHUN-}

kartopred :: Map.Map [Char] [Char] -> Bool
kartopred m = case (Map.lookup "class" m) of
                Just x -> isInfixOf "mw-kartographer-container" x
                _ -> False


printPrepareTree2 :: Bool->[Anything Char] -> [Anything Char]
printPrepareTree2 vt ll =  printPrepareTree vt  ((getLemma ll)++(List.nub (getHead ll))++(getContent ll))


getLemma :: [Anything Char] -> [Anything Char]
getLemma ((Environment DhunUrl s l):_) = [Environment DhunUrl s l]
getLemma ((Environment _ _ lll):xs)  = (getLemma lll)++(getLemma xs)
getLemma (_:xs)  = getLemma xs
getLemma []  = []


getContent :: [Anything Char] -> [Anything Char]
getContent ((Environment Tag (TagAttr "div" m) lll):_) | (check (Map.lookup "class" m)) = lll
  where
    check (Just s) = isInfixOf "mw-parser-output" s 
    check _ = False
getContent ((Environment _ _ lll):xs)  = (getContent lll)++(getContent xs)
getContent (_:xs)  = getContent xs
getContent []  = []
getHead :: [Anything Char] -> [Anything Char]
getHead ((Environment Tag (TagAttr "span" m) lll):_) | ((Map.lookup "class" m) == (Just "mw-page-title-main")) = [Environment Tag (TagAttr "h1" Map.empty) lll]
getHead ((Environment _ _ lll):xs)  = (getHead lll)++(getHead xs)
getHead (_:xs)  = getHead xs
getHead []  = []



printPrepareTree :: Bool->[Anything Char] -> [Anything Char]
printPrepareTree vt ll = concat (map printPrepareNode ll)
  where printPrepareNode :: Anything Char -> [Anything Char]
        printPrepareNode (Environment Tag (TagAttr "table" m) lll) | vt = [(Environment Wikitable (TagAttr "table" m) lll)] 
        printPrepareNode (Environment Wikitable (TagAttr "table" m) lll) | vt = [(Environment Wikitable (TagAttr "table" m) lll)] 
        printPrepareNode (Environment Tag (TagAttr "div" m) lll) | kartopred m = [(Environment Tag (TagAttr "div" m) lll)]
        printPrepareNode (Environment Tag (TagAttr "div" mm) l)
          | (Map.lookup "class" mm) == (Just "thumbinner") =
            case
              do (m, llll) <- case filter (mypred "a") (reducediv l) of
                                  [(Environment Tag (TagAttr "a" mmm) lll)] -> return (mmm, lll)
                                  _ -> mzero
                 tt <- case filter (mypred "div") (reducediv l) of
                           [(Environment Tag (TagAttr "div" mmm) tt)] | (Map.lookup "class"
                                                                           mmm)
                                                                          == (Just "thumbcaption")
                                                                        ->
                                                                        return .
                                                                          (dropWhile (== (C '\n')))
                                                                            . (filter magnpred)
                                                                          $ tt
                           _ -> mzero
                 return $ imgfun m (printPrepareTree vt llll) (Just (printPrepareTree vt tt))
              of
                Just x -> x
                _ -> printPrepareTree vt l
 
 
        printPrepareNode (Environment Wikitable (TagAttr "table" m) _)
          | (Map.lookup "class" m) == (Just "toc") = []
        printPrepareNode (Environment Tag (TagAttr "tbody" _) x)
          = printPrepareTree vt x
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | ((Map.lookup "class" m) == (Just "toc") ||
               (Map.lookup "id" m) == (Just "toc"))
            = []
            
            
        printPrepareNode (Environment Tag (TagAttr "input" _) l)
            = printPrepareTree vt l
            
        printPrepareNode (Environment Tag (TagAttr "main" _) l)
            = printPrepareTree vt l
        printPrepareNode (Environment Tag (TagAttr "label" _) _)
            = []
        printPrepareNode (Environment Tag (TagAttr "form" _) _)
            = []
        printPrepareNode (Environment Tag (TagAttr "ul" m) _)
          | ((Map.lookup "class" m) == (Just "vector-toc-contents") ||
               (Map.lookup "id" m) == (Just "mw-panel-toc-list"))
            = []
            
        printPrepareNode (Environment TableRowSep (TagAttr "tr" m) _)
          | (Map.lookup "style" m) == (Just "display:none;") 
            = []            
        printPrepareNode (Environment Tag (TagAttr "a" m) _)
          | (Map.lookup "class" m) == (Just "mw-logo") 
            = []
            
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "class" m) == (Just "vector-header-start") 
            = []
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "id" m) == (Just "p-search") 
            = []
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "id" m) == (Just "vector-user-links-dropdown") 
            = []
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "id" m) == (Just "p-vector-user-menu-overflow") 
            = []
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "id" m) == (Just "vector-user-links-dropdown") 
            = []
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "class" m) == (Just "vector-settings") 
            = []
            
            
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "id" m) == (Just "p-personal") 
            = []
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "id" m) == (Just "p-user-menu-anon-editor") 
            = []
            
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "id" m) == (Just "vector-main-menu") 
            = []
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "id" m) == (Just "p-tb-label") 
            = []
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "id" m) == (Just "p-coll-print_export-label") 
            = []
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "id" m) == (Just "vector-page-titlebar-toc") 
            = []
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "id" m) == (Just "p-variants") 
            = []

        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "id" m) == (Just "p-views") 
            = []
            
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "id" m) == (Just "p-coll-print_export-labe") 
            = []
     
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "class" m) == (Just "vector-menu-content") 
            = []
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "class" m) == (Just "vector-menu-heading") 
            = []
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "data-name" m) == (Just "vector-toc") 
            = []
            
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "id" m) == (Just "p-coll-print_export-label") 
            = []
            
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "id" m) == (Just "p-lang-btn") 
            = []
           
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "id" m) == (Just "p-cactions") 
            = []
           
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "id" m) == (Just "p-tb") 
            = []
            
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "id" m) == (Just "vector-toc") 
            = []
            
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "class" m) == (Just "vector-main-menu-action vector-main-menu-action-lang-alert") 
            = []
            
            

        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "id" m) == (Just "p-coll-print_export") 
            = []

        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "id" m) == (Just "vector-page-tools-dropdown") 
            = []

        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "id" m) == (Just "vector-page-tools") 
            = []


{-
        printPrepareNode (Environment Tag (TagAttr "header" m) _)
          | (Map.lookup "class" m) == (Just "mw-body-header vector-page-titlebar") 
            = []
  -}         
        
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "class" m) == (Just "vector-pinnable-header vector-toc-pinnable-header vector-pinnable-header-pinned") 
            = []
            
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "class" m) == (Just "vector-menu-heading") 
            = []
            
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "style" m) == (Just "display:none") 
            = []
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "class" m) == (Just "LanguageBar") 
            = []
            
        printPrepareNode (Environment Tag (TagAttr "button" _) _) 
           = []
            
           
            
        printPrepareNode (Environment Wikitable (TagAttr "table" m) _)
          | (Map.lookup "class" m) == (Just "navbox") = []
        printPrepareNode (Environment Tag (TagAttr "title" _) _) = []
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "id" m) == (Just "siteSub") = []
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "id" m) == (Just "catlinks") = []
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "id" m) == (Just "jump-to-nav") = []
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "id" m) == (Just "footer") = []
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "id" m) == (Just "p-lang") = []
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "id" m) == (Just "contentSub") = []
        printPrepareNode (Environment Tag (TagAttr "span" m) _)
          | (Map.lookup "style" m) == (Just "display:none") = []
        printPrepareNode (Environment Tag (TagAttr "span" m) _)
          | (Map.lookup "class" m) == (Just "error") = []
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "class" m) == (Just "printfooter") = []





       {-  printPrepareNode (Environment Wikitable (TagAttr "table" m) l)
          | maybe False (\x->or (map ($ x) (map isInfixOf ["navbox", "infobox"]))) (Map.lookup "class" m)  = [Environment Wikitable (TagAttr "table" m) (droplinks (printPrepareTree vt (droplinks l)))] -}
        printPrepareNode (Environment Tag (TagAttr "span" m) _)
          | (Map.lookup "class" m) == (Just "mw-cite-backlink") = []
        printPrepareNode (Environment Tag (TagAttr "a" m) _)
          | (Map.lookup "class" m) == (Just "mw-jump-link") = []
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "id" m) == (Just "mw-navigation") = []

        printPrepareNode (Environment Source _ []) = []



        printPrepareNode (Environment Tag (TagAttr "sup" m) l)
          | (Map.lookup "class" m) == (Just "reference") =
            [Environment Tag (TagAttr "sup" m) (reducea l)]
        printPrepareNode (Environment Tag (TagAttr "li" m) l)
          | (Map.lookup "class" m) == (Just "gallerybox") =
            case
              (do llll <- case deepGet "div" "class" "thumb" l of
                              [Environment Tag (TagAttr "div" _) lll] -> return lll
                              _ -> mzero
                  (mmm, lll) <- case deepGet "a" "class" "image" llll of
                                    [Environment Tag (TagAttr "a" mmm) lll] -> return (mmm, lll)
                                    _ -> case deepGet "a" "class" "mw-file-description" llll of
                                             [Environment Tag (TagAttr "a" mmm) lll] -> return (mmm, lll)
                                             _ -> mzero
                  te <- case deepGet "div" "class" "gallerytext" l of
                            [Environment Tag (TagAttr "div" _) te] -> return (printPrepareTree vt te)
                            _ -> mzero
                  return $ imgfun mmm (printPrepareTree vt lll) (Just te))
              of
                Just x -> x
                _ -> printPrepareTree vt l
        printPrepareNode (Environment TableHeadColSep (TagAttr t m) l)
          = [Environment TableHeadColSep (TagAttr t m)
               (
                  (map (\ x -> Environment Attribute (Attr x) []) (Map.toList m)))]++(printPrepareTree vt l)
        printPrepareNode (Environment TableColSep (TagAttr t m) l)
          = [Environment TableColSep (TagAttr t m)
               (
                  (map (\ x -> Environment Attribute (Attr x) []) (Map.toList m)))]++(printPrepareTree vt l)
        printPrepareNode (Environment TableRowSep (TagAttr t m) l)
          = [Environment TableRowSep (TagAttr t m)
               ((map (\ x -> Environment Attribute (Attr x) []) (Map.toList m)))]++(printPrepareTree vt l)
        printPrepareNode (Environment Tag (TagAttr "pre" m) l)
          = [Environment Preformat (TagAttr "pre" m) l]
        printPrepareNode (Environment Tag (TagAttr "span" m) _)
          | (Map.lookup "class" m) == (Just "editsection") = []
        printPrepareNode (Environment Tag (TagAttr "span" m) _)
          | (Map.lookup "class" m) == (Just "mw-editsection") = []
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "id" m) == (Just "mw-navigation") = []   
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "id" m) == (Just "mw-panel") = []
        printPrepareNode (Environment Tag (TagAttr "script" _) _) = []           
          

        printPrepareNode (Environment Tag (TagAttr "ul" m) _)
          | (Map.lookup "id" m) == (Just "footer-places") = []
        printPrepareNode (Environment Tag (TagAttr "ul" m) _)
          | (Map.lookup "id" m) == (Just "footer-icons") = []
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "id" m) == (Just "mw-panel") = []
        printPrepareNode (Environment Tag (TagAttr "figure" _) l)
          = case 
              (do (l3,m3) <- case deepGet "a" "class" "mw-file-description" l of
                            ((Environment Tag (TagAttr "a" m2) l2):_) -> return (l2,m2)
                            _-> mzero
                  llll <- case deepGetFigCaption l of
                            ((Environment Tag (TagAttr _ _) lll):_) -> return lll
                            _-> mzero
                  return $ imgfun m3 (printPrepareTree vt l3) (Just (printPrepareTree vt llll)))
             of 
               Just x -> x  
               _ -> case (do res <- Just (deepGetFigRes  l)
                             llll <- case deepGetFigCaption l of
                                        [Environment Tag (TagAttr _ _) lll] -> return lll
                                        _-> mzero
                             return $ [Environment Wikilink (Str "") (res ++[C '|']++ (printPrepareTree vt llll))])
                     of 
                      Just x -> x
                      _-> printPrepareTree vt l
        printPrepareNode (Environment Tag (TagAttr "a" m) l)
          = case (Map.lookup "class" m) of
                (Just "image") -> imgfun m (printPrepareTree vt l) Nothing
                (Just "mw-file-description") -> imgfun m (printPrepareTree vt l) Nothing
                _ -> case (Map.lookup "class" m) of
                         (Just "external free") -> [Environment Tag (TagAttr "a" m) []]
                         _ -> [Environment Tag (TagAttr "a" m) (printPrepareTree vt l)]
        printPrepareNode (Environment Tag (TagAttr "div" m) _)
          | (Map.lookup "class" m) == (Just "bodyContent") = []
        printPrepareNode (Environment Tag (TagAttr "math" m) l)
          = case deepGet "annotation" "encoding" "application/x-tex" l of
                [Environment Tag (TagAttr "annotation" _) x] -> [Environment Math
                                                                   (TagAttr "math" m)
                                                                   (map C
                                                                      (replace2
                                                                         (replace2
                                                                            (replace2
                                                                               (shallowFlatten x)
                                                                               "&amp;"
                                                                               "&")
                                                                            "&lt;"
                                                                            "<")
                                                                         "&gt;"
                                                                         ">"))]
                _ -> []
        printPrepareNode (Environment Tag (TagAttr "h1" _) l) = [(Environment Wikiheading (Str "=") l)]
        printPrepareNode (Environment Tag (TagAttr "h2" _) l) = [(Environment Wikiheading (Str "==") l)]
        printPrepareNode (Environment Tag (TagAttr "h3" _) l) = [(Environment Wikiheading (Str "===") l)]
        printPrepareNode (Environment Tag (TagAttr "h4" _) l) = [(Environment Wikiheading (Str "====") l)]
        printPrepareNode (Environment Tag (TagAttr "h5" _) l) = [(Environment Wikiheading (Str "=====") l)]
        printPrepareNode (Environment Tag (TagAttr "h6" _) l) = [(Environment Wikiheading (Str "======") l)]
        printPrepareNode (Environment Tag (TagAttr "img" m) l)
        
          | (Map.lookup "class" m) `elem`
              (map Just
                 ["tex", "mwe-math-fallback-png-inline tex",
                  "mwe-math-fallback-image-inline tex"])
            =
            case Map.lookup "alt" m of
                Just x -> [Environment Math (TagAttr "math" m)
                             (map C
                                (replace2 (replace2 (replace2 x "&amp;" "&") "&lt;" "<") "gt;"
                                   ">"))]
                Nothing -> [(Environment Tag (TagAttr "img" m)
                               (printPrepareTree vt l))]
        printPrepareNode (Environment Tag (TagAttr "div" m) l)
          = case
              do c <- Map.lookup "class" m
                 guard $ ((isInfixOf "source" c) || (isInfixOf "highlight" c))
                 return $
                   Environment Source
                     (TagAttr "source"
                        (Map.fromList [("lang", (takeWhile (/= ' ') c))]))
                     (deepFlatten l)
              of
                Nothing -> [Environment Tag (TagAttr "div" m) (printPrepareTree vt l)]
                Just x -> [x]
        printPrepareNode (Environment x y l)
          = [Environment x y (printPrepareTree vt l)]
        printPrepareNode x = [x]
        
        mypred :: String -> Anything Char -> Bool
        mypred x y
          = case y of
                (Environment Tag (TagAttr z _) _) | z == x -> True
                _ -> False
        
        magnpred :: Anything Char -> Bool
        magnpred y
          = case y of
                (Environment Tag (TagAttr "div" m) _) | (Map.lookup "class" m) ==
                                                          (Just "magnify")
                                                        -> False
                _ -> True
        unEsc x
          = let z = unEscapeString x in
              if isUTF8Encoded z then decodeString z else z
        imgfun m l tt
          = maybeToList $
              do t <- case
                        tt `mplus` (fmap (parseit imgparsers) (Map.lookup "title" m))  of
                          Just x -> return $ [C '|'] ++ x
                          Nothing -> return []
                 h <- case filter (mypred "img") l of
                          [(Environment Tag (TagAttr "img" mmm) _)] -> case
                                                                         Map.lookup "src" mmm of
                                                                           Just s -> if
                                                                                       length
                                                                                         (reverse
                                                                                            (splitOn
                                                                                               "/"
                                                                                               s))
                                                                                         >= 2
                                                                                       then
                                                                                       return
                                                                                         ("File:" ++
                                                                                            (unEsc
                                                                                               ((reverse
                                                                                                   (splitOn
                                                                                                      "/"
                                                                                                      s))
                                                                                                  !!
                                                                                                  (if
                                                                                                     "thumb"
                                                                                                       `elem`
                                                                                                       splitOn
                                                                                                         "/"
                                                                                                         s
                                                                                                     then
                                                                                                     1
                                                                                                     else
                                                                                                     0))))
                                                                                       else mzero
                                                                           _ -> mzero
                          _ -> mzero
                 w <- case
                        case filter (mypred "img") l of
                            [(Environment Tag (TagAttr "img" mmm) _)] -> do ma <- Map.lookup
                                                                                    "width"
                                                                                    mmm
                                                                            guard $ tt == Nothing
                                                                            return ma
                            _ -> Nothing
                        of
                          Just x -> return $ [C '|'] ++ (map C (x ++ "px"))
                          Nothing -> return []
                 return (Environment Wikilink (Str "") ((map C h) ++ w ++ t))
                 

