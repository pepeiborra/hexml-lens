{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS -Wno-unused-top-binds #-}

import           Control.Applicative        (optional)
import qualified Control.Category           as C
import           Control.Lens               hiding (children)
import qualified Data.ByteString.Lazy.Char8 as LB
import           Network.Wreq
import           Text.XML.Hexml
import           Text.XML.Hexml.Lens

url,url2 :: String
url = "http://aiweb.cs.washington.edu/research/projects/xmltk/xmldata/data/courses/reed.xml"
url2 = "http://aiweb.cs.washington.edu/research/projects/xmltk/xmldata/data/courses/uwm.xml"

type Strings = String

data Place s = Place
  { building :: s
  , room     :: Int
  }
  deriving Show

data Course s = Course
  { title      :: s
  , instructor :: Maybe s
  , place      :: Maybe (Place s)
  , sections   :: [Section s]
  }
  deriving Show

data Section s = Section
  { name       :: s
  , days       :: s
  , start, end :: s
  , instructor :: s
  }
  deriving Show

main :: IO ()
main = do
  r <- get url2
  let courses = take 10 $ r ^.. responseBody . to stripDocTypeB . _XML . node "course_listing" . courseF
  print courses

stripDocTypeB :: LB.ByteString -> LB.ByteString
stripDocTypeB = LB.unlines . drop 2 . LB.lines

courseF :: Fold Node (Course Strings)
courseF = runFold $ do
  title <- field "title"
  instructor <- optional $ field "instructor"
  place <- optional $ Fold $ node "place" . placeF
  sections <- Fold $ multiple $ node "section_listing" . sectionF
  return Course{..}

sectionF :: Fold Node (Section Strings)
sectionF = runFold $ do
  instructor <- field "instructor"
  name <- field "section"
  days <- field "days"
  (start, end) <- Fold $ node "hours" . runFold hoursF
  return Section{..}

hoursF :: ReifiedFold Node (Strings, Strings)
hoursF = (,) <$> field "start" <*> field "end"

placeF :: Fold Node (Place Strings)
placeF = runFold $ do
  building <- field "building"
  room     <- Fold _Show C.. field "room"
  return (Place building room)

field :: (XML a, XML t) => t -> ReifiedFold Node a
field n = Fold $ node n . _inner
