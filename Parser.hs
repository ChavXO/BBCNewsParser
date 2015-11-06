--BBC headline scraper
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
import Text.XML.HXT.Core
import Network.HTTP.Conduit
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as L

data HeadLine = HeadLine {title, link ::String} 

instance Show HeadLine where
    show HeadLine {title = t, link = l} = (map toUpper t) ++ ": " ++ "http://www.bbc.com" ++ l

url' = "http://www.bbc.com/news"

atTag tag = deep (isElem >>> hasName tag)
text = getChildren >>> getText

getHeadlines = atTag "a" >>>
        proc x -> do
                linkA <- getAttrValue "href" -< x
                titleA <- text <<< atTag "span" -< x
                returnA -< HeadLine {title = titleA, link = linkA}

parseHTML = readString [withValidate no, withParseHTML yes, withWarnings no]

main = do
        url'' <- simpleHttp url'
        let url = L.unpack url''
        headlines' <- runX (parseHTML url >>> getHeadlines)
        let headlines = unlines $ map show $ drop 18 headlines'
        putStrLn headlines
