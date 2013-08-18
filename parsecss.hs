{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Data.Text (Text)
import Text.Parsec
import Text.Parsec.Text
import Control.Applicative (pure, (<*>), (<*), (*>), (<$>))

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
                               | UntypedSequence CssSimpleSelector [CssSimpleSelector]
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
                       | PseudoElementSelector CssPseudo
                       | PseudoClassSelector CssPseudo
                       | NegatedTypeSelector CssTypeSelector
                       | NegatedUniversalSelector CssUniversalSelector
                       | NegatedHashSelector CssHash
                       | NegatedClassSelector CssClass
                       | NegatedAttribSelector CssAttrib
                       | NegatedPseudoElementSelector CssPseudo
                       | NegatedPseudoClassSelector CssPseudo
                       deriving Show

data CssHash = Hash CssName deriving Show

data CssClass = Class CssIdentifier deriving Show

data CssAttrib = Attrib (Maybe CssNamespacePrefix) CssIdentifier (Maybe (CssAttribMatcher, CssAttribValue)) deriving Show

data CssAttribMatcher = PrefixMatcher
                      | SuffixMatcher
                      | SubstringMatcher
                      | EqualMatcher
                      | IncludeMatcher
                      | DashMatcher
                      deriving Show

data CssAttribValue = IdentifierValue CssIdentifier
                    | StringValue CssString
                    deriving Show

data CssPseudo = IdentifierPseudo CssIdentifier
               | FunctionalPseudo CssIdentifier CssExpression
               deriving Show

data CssExpression = Expression CssExpressionTerm [CssExpressionTerm] deriving Show

data CssExpressionTerm = Plus
                       | Minus
                       | Dimension Float CssIdentifier
                       | Number Float
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
--cssSelector = (++) <$> simpleSelectorSequence <*> (try combined <|> pure "") <* whiteSpace
--  where combined = (:) <$> combinator <*> cssSelector
--        combinator = (try (whiteSpace *> (char '+' <|> char '>' <|> char '~')) <|> space) <* whiteSpace
--        simpleSelectorSequence = try ((++) <$> (try cssTypeSelector <|> cssUniversal) <*> (try selSequence <|> pure "")) <|> selSequence -- todo selSequence should be array
--        selSequence = cssHash <|> cssClass <|> cssAttrib <|> cssPseudo <|> cssNegation
--cssTypeSelector = cssNamespacePrefixed cssElementName

cssNamespacePrefix :: Parser (Maybe CssNamespacePrefix)
cssNamespacePrefix = try ((try ((Just . Namespace) <$> cssIdentifier) <|> (char '*' *> pure (Just AllNamespaces)) <|> pure (Just NoNamespace)) <* char '|') <|> pure Nothing

--cssElementName = cssIdentifier

cssUniversal :: Parser CssUniversalSelector
cssUniversal = Universal <$> (cssNamespacePrefix <* char '*') <?> "universal selector"

cssClass :: Parser CssClass
cssClass = Class <$> (char '.' *> cssIdentifier) <?> "class"

--cssAttrib = between (char '[') (char ']') ((++) <$> (whiteSpace *> cssNamespacePrefixed cssIdentifier <* whiteSpace) <*> cssAttribValue)
--  where cssAttribValue = try $ option "" ((++) <$> cssOpt <*> (cssIdentifier <|> cssString))
--        cssOpt =  try (string "~=")
--              <|> try (string "|=")
--              <|> try (string "^=")
--              <|> try (string "$=")
--              <|> try (string "*=")
--              <|> string "="
--cssPseudo = (++) <$> pseudoColumn <*> (try functionalPseudo <|> cssIdentifier)
--  where pseudoColumn = try (string "::") <|> string ":"
--        functionalPseudo = (++) <$> (cssIdentifier <* whiteSpace) <*> between (char '(') (char ')') functionParams
--        functionParams = (++) <$> cssExpression <*> (try (many1 space *> functionParams) <|> pure "")

--cssNegation = (++) <$> (try $ string ":not") <*> (whiteSpace *> between (char '(') (char ')') cssNegationArg <* whiteSpace)
--  where cssNegationArg = cssUniversal <|> cssHash <|> cssClass <|> cssAttrib <|> cssPseudo <|> cssTypeSelector

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
cssNameChar = oneOf "-_" <|> alphaNum

cssName :: Parser CssName
cssName = T.pack <$> many1 cssNameChar

cssHash :: Parser CssHash
cssHash = Hash <$> ((T.pack <$> string "#") *> cssName)

cssString :: Parser CssString
cssString = (Quote . T.pack) <$> between (char '"') (char '"') (many stringPart)
  where stringPart = try (string "\\\"" *> pure '"') <|> noneOf "\""

--cssExpression :: Parser CssExpression
--cssExpression = many $ noneOf "( )" -- todo this should be [ [ PLUS | '-' | DIMENSION | NUMBER | STRING | IDENT ] S* ]+

--newLine :: Parser Text
--newLine = T.pack <$> (try $ string "\r\n" <|> string "\n" <|> string "\r" <|> string "\f")

--whiteSpace :: Parser ()
--whiteSpace = skipMany space


--parseCSS :: Text -> Either ParseError CssSelectorsGroup
--parseCSS input = parse cssSelectorGroup "culo" input

--main =
--    do c <- getContents
--       case parse csvFile "(stdin)" c of
--            Left e -> do putStrLn "Error parsing input:"
--                         print e
--            Right r -> mapM_ print r
