
module Grammatik where

data Plingular = Singular
               | Plural
               deriving Show

data Casus = Nominativ
           | Dativ
           | Genitiv
           | Akkusativ
           deriving Show

data Person = Ich
            | Du
            | ErSieEs
            deriving Show

data Verbform = VerbformPerson Person
              | Paeteritum
              | Partizip2
              | Lonjunktiv
              | Imperativ Plingular
              | Grundform
              deriving Show

data Adjektivfrom = Positiv
                  | Koperativ
                  | Superlativ
                  deriving Show

data Genus = Maskulin
           | Feminin
           | Neutrum
           deriving Show

data GenusArticelis = MaskulinA
                    | FemininA
                    | NeutrumA
                    | PluralA
                    deriving Show

data Bestimmung = Demonstativ
                | Definit
                | Unbestimmt
                deriving Show

data Wort = Nomen String Genus Casus Plingular
          | Verb String Verbform
          | Adjektiv Adjektivfrom
          | Artikel Bestimmung GenusArticelis Casus
          deriving Show

data SatzTeil = Unbekannt [Wort]
              | Subjekt Wort
              | VerbS Wort
              | Objekt Wort
              deriving Show
