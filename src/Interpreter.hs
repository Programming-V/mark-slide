module Interpreter 
    ( generateHTML, 
      writeHTMLFile ) where

import AbstractGrammar 

slideToHTML :: (Slide, Int) -> String
slideToHTML ((Slide (TitleSlide title) (BodySlide content)), index) =
  "<div class=\"swiper-slide\" style=\"border: 0px solid #555; background-color: " ++ slideColor ++ ";\">\n" ++
  "  <h1>" ++ title ++ "</h1>\n" ++
  concatMap contentToHTML content ++
  "</div>\n"
  where
    slideColors = ["#A9DFBF", "#F5B7B1", "#AED6F1", "#F9E79F", "#D2B4DE", "#ABEBC6", "#F5CBA7", "#85C1E9"]
    slideColor = slideColors !! (index `mod` length slideColors)

contentToHTML :: MarkdownBlock -> String
contentToHTML (MdH2 text) = "<h2 class=\"title\">" ++ text ++ "</h2>\n"
contentToHTML (MdH3 text) = "<h3 class=\"title\">" ++ text ++ "</h3>\n"
contentToHTML (MdH4 text) = "<h4 class=\"title\">" ++ text ++ "</h4>\n"
contentToHTML (MdH5 text) = "<h5 class=\"title\">" ++ text ++ "</h5>\n"
contentToHTML (MdH6 text) = "<h6 class=\"title\">" ++ text ++ "</h6>\n"
contentToHTML (MdParagraph text) = "<p class=\"content\">" ++ text ++ "</p>\n"
contentToHTML (MdEmphasisText text) = "<p class=\"content\"><em>" ++ text ++ "</em></p>\n"
contentToHTML (MdStrongEmphasisText text) = "<p class=\"content\"><strong>" ++ text ++ "</strong></p>\n"
contentToHTML (MdStrikethroughText text) = "<p class=\"content\"><s>" ++ text ++ "</s></p>\n"
contentToHTML (MdURLs (MdLink link)) = "<a href=\"" ++ link ++ "\">" ++ link ++ "</a>\n"
contentToHTML (MdURLs (MdImage image)) = "<img src=\"" ++ image ++ "\" alt=\"" ++ image ++ "\" width=\"" ++ width ++ "\" height=\"" ++ height ++ "\">\n"
  where
    width = "450"  
    height = "360"

generateHTML :: Slides -> String
generateHTML (Slides slides) =
  "<!DOCTYPE html>\n<html>\n<head>\n<title>Slides</title>\n" ++
  "<style>\n" ++
  "body { margin: 0; font-family: Arial, sans-serif; display: flex; align-items: center; justify-content: center; height: 100vh; background-color: #f0f0f0; }\n" ++
  ".swiper-container { width: 90%; height: 90%; background-color: transparent; position: relative; }\n" ++
  ".swiper-slide { text-align: center; overflow: hidden; background-color: #FFD700; }\n" ++
  ".swiper-slide:nth-child(odd) { background-color: #87CEEB; }\n" ++
  ".swiper-button-next, .swiper-button-prev { position: absolute; top: 50%; transform: translateY(-50%); width: 30px; height: 30px; background-color: rgba(255, 255, 255, 0.5); border-radius: 50%; text-align: center; line-height: 30px; cursor: pointer; opacity: 0; transition: opacity 0.3s ease; }\n" ++
  ".swiper-button-next:hover, .swiper-button-prev:hover { opacity: 1; }\n" ++
  ".swiper-button-next { right: 10px; }\n" ++
  ".swiper-button-prev { left: 10px; }\n" ++
  ".swiper-pagination { position: absolute; bottom: 10px; left: 50%; transform: translateX(-50%); }\n" ++
  "</style>\n" ++
  "<link rel=\"stylesheet\" href=\"https://unpkg.com/swiper/swiper-bundle.min.css\">\n" ++
  "</head>\n<body>\n" ++
  "<div class=\"swiper-container\">\n" ++
  "<div class=\"swiper-wrapper\">\n" ++
  concatMap slideToHTML (zip slides [1..]) ++
  "</div>\n" ++
  "<div class=\"swiper-button-prev\">‹</div>\n" ++
  "<div class=\"swiper-button-next\">›</div>\n" ++
  "<div class=\"swiper-pagination\"></div>\n" ++
  "</div>\n" ++
  "<script src=\"https://unpkg.com/swiper/swiper-bundle.min.js\"></script>\n" ++
  "<script>\n" ++
  "var mySwiper = new Swiper('.swiper-container', {\n" ++
  "  loop: true,\n" ++
  "  pagination: {\n" ++
  "    el: '.swiper-pagination',\n" ++
  "    clickable: true\n" ++
  "  },\n" ++
  "  navigation: {\n" ++
  "    nextEl: '.swiper-button-next',\n" ++
  "    prevEl: '.swiper-button-prev',\n" ++
  "  },\n" ++
  "  keyboard: {\n" ++
  "    enabled: true,\n" ++
  "    onlyInViewport: false,\n" ++
  "  },\n" ++
  "});\n" ++
  "</script>\n" ++
  "</body>\n</html>"

writeHTMLFile :: FilePath -> String -> IO ()
writeHTMLFile filePath html = writeFile filePath html
