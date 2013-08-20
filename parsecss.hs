{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative (pure, (*>), (<$>), (<*), (<*>))
import           Data.Char           (digitToInt)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Text.Parsec
import           Text.Parsec.Text

type CssComment = String

data CssSelectorsGroup = Group CssSelector [CssSelector] deriving Show

data CssSelector = Selector CssSimpleSelectorSequence [(CssSelectorCombinator, CssSimpleSelectorSequence)] deriving Show

data CssSelectorCombinator = Descendant
                           | Child
                           | AdjacentSibling
                           | Sibling
                           deriving Show

data CssSimpleSelectorSequence = TypedSequence CssTypeSelector [CssSimpleSelector]
                               | UniversalSequence CssUniversalSelector [CssSimpleSelector]
                               | SimpleSequence CssSimpleSelector [CssSimpleSelector]
                               deriving Show

data CssTypeSelector = Type (Maybe CssNamespacePrefix) CssElementName deriving Show

data CssNamespacePrefix = Namespace CssIdentifier
                        | AllNamespaces
                        | NoNamespace
                        deriving Show

type CssElementName = CssIdentifier

data CssUniversalSelector = Universal (Maybe CssNamespacePrefix) deriving Show

data CssSimpleSelector = HashSelector CssHash
                       | ClassSelector CssClass
                       | AttribSelector CssAttrib
                       | PseudoSelector CssPseudo
                       | NegatedTypeSelector CssTypeSelector
                       | NegatedUniversalSelector CssUniversalSelector
                       | NegatedHashSelector CssHash
                       | NegatedClassSelector CssClass
                       | NegatedAttribSelector CssAttrib
                       | NegatedPseudoSelector CssPseudo
                       deriving Show

data CssHash = Hash CssName deriving Show

data CssClass = Class CssIdentifier deriving Show

data CssAttrib = Attrib (Maybe CssNamespacePrefix) CssIdentifier (Maybe (CssAttribMatcher, CssAttribValue)) deriving Show

data CssAttribMatcher = PrefixMatcher
                      | SuffixMatcher
                      | SubstringMatcher
                      | EqualMatcher
                      | IncludeMatcher
                      | HyphenMatcher
                      deriving Show

data CssAttribValue = IdentifierValue CssIdentifier
                    | StringValue CssString
                    deriving Show

data CssPseudoType = PseudoClassType | PseudoElementType deriving Show

data CssPseudo = Pseudo CssPseudoType CssIdentifier (Maybe CssExpression) deriving Show

data CssExpression = Expression CssExpressionTerm [CssExpressionTerm] deriving Show

data CssExpressionTerm = Plus
                       | Minus
                       | Dimension Double (Maybe CssIdentifier)
                       | String CssString
                       | Identifier CssIdentifier
                       deriving Show

data CssString = Quote Text deriving Show

type CssIdentifier = Text
type CssName = Text

-- SEE http://www.w3.org/TR/css3-selectors/#lex
-- SEE http://hackage.haskell.org/package/parsec
-- SEE http://book.realworldhaskell.org/read/using-parsec.html
-- TODO return datatype instead of strings for all of this

--cssSelectorGroup = (++) <$> cssSelector <*> (try group <|> pure "")
--  where group = (:) <$> char ',' <* whiteSpace <*> cssSelectorGroup

cssSelector :: Parser CssSelector
cssSelector = Selector <$> simpleSelectorSequence <*> many combined
  where simpleSelectorSequence = (try (TypedSequence <$> cssTypeSelector)
                             <|> try (UniversalSequence <$> cssUniversal)
                             <|> SimpleSequence <$> cssSimpleSelector)
                             <*> many cssSimpleSelector
        combined = (,) <$> (combinator <* whiteSpace) <*> simpleSelectorSequence
        combinator = try (whiteSpace *>
                      (   char '+' *> pure AdjacentSibling
                      <|> char '>' *> pure Child
                      <|> char '~' *> pure Sibling))
                 <|> space *> pure Descendant <?> "selector combinator"

cssNamespacePrefix :: Parser (Maybe CssNamespacePrefix)
cssNamespacePrefix = try ((try ((Just . Namespace) <$> cssIdentifier) <|> (char '*' *> pure (Just AllNamespaces)) <|> pure (Just NoNamespace)) <* char '|') <|> pure Nothing

cssElementName :: Parser CssIdentifier
cssElementName = cssIdentifier

cssUniversal :: Parser CssUniversalSelector
cssUniversal = Universal <$> (cssNamespacePrefix <* char '*') <?> "universal selector"

cssTypeSelector :: Parser CssTypeSelector
cssTypeSelector = Type <$> cssNamespacePrefix <*> cssElementName <?> "type selector"

cssSimpleSelector :: Parser CssSimpleSelector
cssSimpleSelector = (try (string ":not") *> between (char '(') (char ')') (whiteSpace *> negationArg <* whiteSpace) <?> "negation")
                <|> PseudoSelector <$> cssPseudo
                <|> AttribSelector <$> cssAttrib
                <|> ClassSelector <$> cssClass
                <|> HashSelector <$> cssHash
                where negationArg = NegatedUniversalSelector <$> cssUniversal
                                <|> NegatedHashSelector <$> cssHash
                                <|> NegatedClassSelector <$> cssClass
                                <|> NegatedAttribSelector <$> cssAttrib
                                <|> NegatedPseudoSelector <$> cssPseudo
                                <|> NegatedTypeSelector <$> cssTypeSelector

cssClass :: Parser CssClass
cssClass = Class <$> (char '.' *> cssIdentifier) <?> "class"

cssAttrib :: Parser CssAttrib
cssAttrib = between (char '[') (char ']') (Attrib <$> (whiteSpace *> cssNamespacePrefix) <*> (cssIdentifier <* whiteSpace) <*> optionMaybe attrib) <?> "attribute"
  where attrib = (,) <$> matcher <*> value
        matcher = try (string "~=") *> pure IncludeMatcher
              <|> try (string "|=") *> pure HyphenMatcher
              <|> try (string "^=") *> pure PrefixMatcher
              <|> try (string "$=") *> pure SuffixMatcher
              <|> try (string "*=") *> pure SubstringMatcher
              <|> string "=" *> pure EqualMatcher
        value = try (IdentifierValue <$> cssIdentifier) <|> (StringValue <$> cssString)

cssPseudo :: Parser CssPseudo
cssPseudo = Pseudo <$> pseudoColon <*> pseudoIdentifier <*> pseudoExpression <?> "pseudo"
  where pseudoColon = (try (string "::") *> pure PseudoElementType) <|> (char ':' *> pure PseudoClassType)
        pseudoIdentifier = cssIdentifier <* whiteSpace
        pseudoExpression = between (char '(') (char ')') (Just <$> functionParams) <|> pure Nothing
        functionParams = Expression <$> cssExpressionTerm <*> try (many (many1 space *> cssExpressionTerm) <|> pure [])

cssExpressionTerm :: Parser CssExpressionTerm
cssExpressionTerm = char '+' *> pure Plus
                <|> char '-' *> pure Minus
                <|> try (Dimension <$> cssNumber <*> optionMaybe cssIdentifier)
                <|> try (String <$> cssString)
                <|> (Identifier <$> cssIdentifier)

cssIdentifier :: Parser CssIdentifier
cssIdentifier = T.append <$> (T.pack <$> (string "-" <|> pure "")) <*> (T.append <$> cssNameStart <*> (T.pack <$> many cssNameChar)) <?> "identifier"

cssNameStart :: Parser Text
cssNameStart = (T.singleton <$> char '_') <|> (T.singleton <$> letter) <|> (T.singleton <$> cssNonAscii) <|> cssEscape <?> "start of name character"

cssNonAscii :: Parser Char
cssNonAscii = satisfy (\c -> c >= '\o240' && c <= '\o4177777') <?> "non ascii character"

cssEscape :: Parser Text
cssEscape = cssUnicode <|> (T.append <$> (T.singleton <$> char '\\') <*> (T.singleton <$> noneOf ("\n\r\f" ++ cssHexDigits))) <?> "escape sequence"

cssHexDigits :: String
cssHexDigits = ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']

cssUnicode :: Parser Text
cssUnicode = T.append <$> (T.singleton <$> char '\\') <*> (T.pack <$> (try (count 6 hexDigit) <|> ((++) <$> (many1 hexDigit <* unicodeEnd) <*> pure " "))) <?> "unicode escape sequence"
  where unicodeEnd = try (string "\r\n") <|> string " " <|> string "\n" <|> string "\r" <|> string "\t" <|> string "\f" <?> "end of unicode escape sequence"

cssNameChar :: Parser Char
cssNameChar = oneOf "-_" <|> alphaNum <?> "name character"

cssName :: Parser CssName
cssName = T.pack <$> many1 cssNameChar <?> "name"

cssHash :: Parser CssHash
cssHash = Hash <$> ((T.pack <$> string "#") *> cssName) <?> "hash selector"

cssString :: Parser CssString
cssString = (Quote . T.pack) <$> between (char '"') (char '"') (many stringPart) <?> "string literal"
  where stringPart = try (string "\\\"" *> pure '"') <|> noneOf "\""

cssNumber :: Parser Double
cssNumber = try ((+) <$> (numberParser <|> pure 0) <*> (char '.' *> fractionParser)) <|> numberParser <?> "number"
  where numberParser = (fromInteger . foldl (\x d -> 10*x + toInteger (digitToInt d)) 0) <$> many1 digit
        fractionParser = foldr (\d f -> (f + fromIntegral (digitToInt d))/10.0) 0.0 <$> many1 digit

--newLine :: Parser Text
--newLine = T.pack <$> (try $ string "\r\n" <|> string "\n" <|> string "\r" <|> string "\f")

whiteSpace :: Parser ()
whiteSpace = skipMany space

--parseCSS :: Text -> Either ParseError CssSelectorsGroup
--parseCSS input = parse cssSelectorGroup "culo" input

main :: IO ()
main = putStrLn "Error parsing input:"
  --do c <- getContents
  --   case parse cssHash "(stdin)" (T.pack c) of
  --        Left e -> do putStrLn "Error parsing input:"
  --                     print e
  --        Right r -> mapM_ print r
