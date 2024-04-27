module AbstractGrammar where

type Strings = String
type Paragraph = Strings
-- type H1 = Strings
type H2 = Strings
type H3 = Strings
type H4 = Strings
type H5 = Strings
type H6 = Paragraph

type Emphasis = Strings
type StrongEmphasis = Strings
type Strikethrough = Strings

data Slides = Slides [Slide] 
        deriving (Show)

data Slide = Slide TitleSlide BodySlide 
        deriving (Show)

-- data TitleSlide = TitleSlide Cad deriving (Show)

data TitleSlide = TitleSlide Strings 
        deriving (Show)  

-- data Cad = Cad Strings deriving (Show)

data BodySlide = BodySlide [MarkdownBlock] 
        deriving (Show)

-- data MarkdownBlock = MdParagraph Paragraph | MdHeader1 HeaderH1 deriving (Show)

data MarkdownBlock = MdParagraph Paragraph 
                --     | MdH1 H1 
                    | MdH2 H2
                    | MdH3 H3
                    | MdH4 H4
                    | MdH5 H5
                    | MdH6 H6
                    | MdEmphasisText Emphasis
                    | MdStrongEmphasisText StrongEmphasis
                    | MdStrikethroughText Strikethrough
                    | MdQuoteText Strings
                    | MdURLs Link
                    deriving (Show)

-- data Paragraph = Paragraph Strings deriving (Show)

-- data HeaderH1 = HeaderH1 Strings deriving (Show)

data Link = MdLink Strings 
          | MdImage Strings  
          deriving (Show)

data CodeBlock = CodeBlock { language :: String, code :: String }
  deriving (Show)
