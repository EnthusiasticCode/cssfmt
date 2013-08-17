{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

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
                        | Wildcard
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

data CssString = Quote String deriving Show

type CssIdentifier = String
type CssName = String

-- SEE http://www.w3.org/TR/css3-selectors/#lex
-- SEE http://hackage.haskell.org/package/parsec
-- SEE http://book.realworldhaskell.org/read/using-parsec.html
-- TODO return datatype instead of strings for all of this

cssSelectorGroup = (++) <$> cssSelector <*> (try group <|> pure "")
  where group = (:) <$> char ',' <* whiteSpace <*> cssSelectorGroup
cssSelector = (++) <$> simpleSelectorSequence <*> (try combined <|> pure "") <* whiteSpace
  where combined = (:) <$> combinator <*> cssSelector
        combinator = (try (whiteSpace *> (char '+' <|> char '>' <|> char '~')) <|> space) <* whiteSpace
        simpleSelectorSequence = try ((++) <$> (try cssTypeSelector <|> cssUniversal) <*> (try selSequence <|> pure "")) <|> selSequence -- todo selSequence should be array
        selSequence = cssHash <|> cssClass <|> cssAttrib <|> cssPseudo <|> cssNegation
cssTypeSelector = cssNamespacePrefixed cssElementName
cssNamespacePrefixed p = try ((++) <$> cssNamespacePrefix <*> p) <|> p
  where cssNamespacePrefix = (++) <$> (try cssIdentifier <|> string "*" <|> pure "") <*> string "|"
cssElementName = cssIdentifier
cssUniversal = cssNamespacePrefixed (string "*")
cssClass = (:) <$> char '.' <*> cssIdentifier
cssAttrib = between (char '[') (char ']') ((++) <$> (whiteSpace *> cssNamespacePrefixed cssIdentifier <* whiteSpace) <*> cssAttribValue)
  where cssAttribValue = try $ option "" ((++) <$> cssOpt <*> (cssIdentifier <|> cssString))
        cssOpt =  try (string "~=")
              <|> try (string "|=")
              <|> try (string "^=")
              <|> try (string "$=")
              <|> try (string "*=")
              <|> string "="
cssPseudo = (++) <$> pseudoColumn <*> (try functionalPseudo <|> cssIdentifier)
  where pseudoColumn = try (string "::") <|> string ":"
        functionalPseudo = (++) <$> (cssIdentifier <* whiteSpace) <*> between (char '(') (char ')') functionParams
        functionParams = (++) <$> cssExpression <*> (try (many1 space *> functionParams) <|> pure "")

cssNegation = (++) <$> (try $ string ":not") <*> (whiteSpace *> between (char '(') (char ')') cssNegationArg <* whiteSpace)
  where cssNegationArg = cssUniversal <|> cssHash <|> cssClass <|> cssAttrib <|> cssPseudo <|> cssTypeSelector

cssIdentifier = (++) <$> (string "-" <|> pure "") <*> ((:) <$> cssNameStart <*> many cssNameChar)
  where cssNameStart = char '_' <|> letter
cssNameChar = oneOf "-_" <|> alphaNum
cssName = many1 cssNameChar
cssHash = (:) <$> char '#' <*> cssName
cssString = between (char '"') (char '"') (many stringPart)
  where stringPart = try (string "\\\"" *> pure '"') <|> noneOf "\""
cssExpression = many $ noneOf "( )" -- todo this should be [ [ PLUS | '-' | DIMENSION | NUMBER | STRING | IDENT ] S* ]+

newLine = try $ string "\r\n" <|> string "\n" <|> string "\r" <|> string "\f"
whiteSpace = skipMany space


parseCSS :: String -> Either ParseError String
parseCSS input = parse cssSelectorGroup "culo" input

--main =
--    do c <- getContents
--       case parse csvFile "(stdin)" c of
--            Left e -> do putStrLn "Error parsing input:"
--                         print e
--            Right r -> mapM_ print r
