module Parser 
    ( pSlides
    ) where

import Token
import UU.Parsing
import Scanner
import AbstractGrammar

pSlides = Slides <$> pList pSlide

pSlide = Slide <$> pTitleSlide <*> pBodySlide <* pEndSlide "---"

pTitleSlide = TitleSlide <$ pKeyword "!" <*> pStrings

-- pBodySlide = BodySlide <$> pList pMarkdownBlock

pBodySlide = BodySlide <$ pOpenBlock "{" <*> pList pMarkdownBlock <* pEndBlock "}"

pMarkdownBlock = MdParagraph <$> pStrings
             <|> MdH2 <$ pKeyword "##" <*> pStrings
             <|> MdH3 <$ pKeyword "###" <*> pStrings
             <|> MdH4 <$ pKeyword "####" <*> pStrings
             <|> MdH5 <$ pKeyword "#####" <*> pStrings
             <|> MdH6 <$ pKeyword "######" <*> pStrings
             <|> MdEmphasisText <$ pKeyword "_" <*> pStrings <* pKeyword "_"
             <|> MdStrongEmphasisText <$ pKeyword "*" <*> pStrings <* pKeyword "*"
             <|> MdStrikethroughText <$ pKeyword "~" <*> pStrings <* pKeyword "~"
            --  <|> MdStrikethroughText <$> pMdStrikethroughText
             <|> MdQuoteText <$ pKeyword ">" <*> pStrings
             <|> MdURLs <$> pLinks

instance Symbol Token where

getValue :: Token -> String
getValue (Token _ v _ _) = v

tSym :: Type -> String -> Parser Token String
tSym typ value = getValue <$> pSym (Token typ value 0 0)

tStr = getValue <$> pSym (Token String "" 0 0)

pKeyword :: String -> Parser Token String
-- pKeyword cad = tSym Keyword cad
pKeyword = tSym Keyword

pOpenBlock :: String -> Parser Token String
pOpenBlock = tSym OpenBlock

pEndBlock :: String -> Parser Token String
pEndBlock = tSym EndBlock

pEndSlide :: String -> Parser Token String
pEndSlide = tSym EndSlide

pStrings :: Parser Token String
-- pStrings = tSym String
pStrings = tStr

pLinks = MdImage <$ pKeyword "<" <*> pStrings <* pKeyword ">"
            <|> MdLink <$ pKeyword "<<" <*> pStrings <* pKeyword ">>"

-- pMdStrikethroughText = MdStrikethrough <$ pKeyword "~" <*> pStrings <* pKeyword "~"
