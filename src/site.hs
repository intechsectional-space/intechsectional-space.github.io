{-# language DeriveGeneric     #-}
{-# language FlexibleContexts  #-}
{-# language OverloadedStrings #-}
{-# language TupleSections     #-}


-- aeson
import           Data.Aeson (FromJSON, decode')

-- base
import           Control.Monad      (liftM)
import           Data.List          (intercalate, isInfixOf, isPrefixOf, isSuffixOf, sort, sortBy)
import           Data.Maybe         (fromJust, fromMaybe, isJust)
import           Data.Ord           (comparing)
import           GHC.Generics       (Generic)
import           System.Environment (lookupEnv)
import           Text.Read          (readMaybe)

-- blaze-html
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A

-- containers
import qualified Data.Map as M

-- filepath
import           System.FilePath (takeFileName)

-- hakyll
import           Hakyll

-- MissingH
import           Data.String.Utils (strip)

-- string-conv
import           Data.String.Conv (toS)

-- tagsoup
import           Text.HTML.TagSoup (Tag (..))

-- time
import           Data.Time.Clock  (UTCTime (..))


main :: IO ()
main = do
  commitDetails <- strip <$> readFile "metadata/gitinfo"
  imageMetaData <- computeImageMetaData

  hakyllWith config $ do
    match "templates/*" $ do
      route idRoute
      compile templateBodyCompiler


    match "css/*.hs" $ do
      -- See: https://jaspervdj.be/hakyll/tutorials/using-clay-with-hakyll.html
      route   $ setExtension "css"

      let cssStr = getResourceString >>= withItemBody (unixFilter "runghc" [])

      compile $ fmap compressCss <$>  cssStr


    match "images/**" $ do
      route idRoute
      compile copyFileCompiler


    match "posts/**.md" $ do
      route $ setExtension "html"
      tags <- buildTagsWith getTags "posts/**" (fromCapture "tags/*.html")
      let ctx =  constField "commit"  commitDetails
              <> postContext

      compile $ getResourceBody
                  >>= renderPandoc
                  >>= loadAndApplyTemplate "templates/post.html"  ctx
                  >>= loadAndApplyTemplate "templates/default.html" ctx
                  >>= lqipImages imageMetaData
                  >>= relativizeUrls

      tagsRules tags $ \tag pattern -> do
        route idRoute
        compile $ do
          posts        <- recentFirst =<< loadAll pattern
          tagCloud     <- renderTagCloudWith makeLink (intercalate " ") 90 180 tags

          let tagCtx =  constField "tag"      tag
                     <> listField  "posts"    postContext (return posts)
                     <> constField "tagCloud" tagCloud
                     <> constField "commit"   commitDetails
                     <> postContext

          makeItem ""
            >>= loadAndApplyTemplate "templates/tag.html"     tagCtx
            >>= loadAndApplyTemplate "templates/default.html" tagCtx
            >>= lqipImages imageMetaData
            >>= relativizeUrls



    match "blog/**.md" $ do
      route $ setExtension "html"
      tags <- buildTagsWith getTags "blog/**" (fromCapture "tags/*.html")
      let ctx =  constField "commit"  commitDetails
              <> postContext

      compile $ getResourceBody
                  >>= renderPandoc
                  >>= loadAndApplyTemplate "templates/guide.html"  ctx
                  >>= loadAndApplyTemplate "templates/default.html" ctx
                  >>= lqipImages imageMetaData
                  >>= relativizeUrls

      tagsRules tags $ \tag pattern -> do
        route idRoute
        compile $ do
          posts        <- recentFirst =<< loadAll pattern
          tagCloud     <- renderTagCloudWith makeLink (intercalate " ") 90 180 tags

          let tagCtx =  constField "tag"      tag
                     <> listField  "posts"    postContext (return posts)
                     <> constField "tagCloud" tagCloud
                     <> constField "commit"   commitDetails
                     <> postContext

          makeItem ""
            >>= loadAndApplyTemplate "templates/tag.html"     tagCtx
            >>= loadAndApplyTemplate "templates/default.html" tagCtx
            >>= lqipImages imageMetaData
            >>= relativizeUrls


    match "blog.html" $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/**"

        let ctx =  constField "commit" commitDetails
                <> listField "posts" postContext (return posts)
                <> constField "title" "Blog"
                <> postContext

        getResourceBody
          >>= applyAsTemplate ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= lqipImages imageMetaData
          >>= relativizeUrls


    match "blog.html" $ do
      route idRoute
      compile $ do
        blog <- recentFirst =<< loadAll "blog/**"

        let ctx =  constField "commit" commitDetails
                <> listField  "blog" postContext (pure blog)
                <> constField "title" "Blog"
                <> postContext

        getResourceBody
          >>= applyAsTemplate ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= lqipImages imageMetaData
          >>= relativizeUrls

    match "CNAME" $ do
      route idRoute
      compile copyFileCompiler

    match "index.html" $ do
      route idRoute
      compile $ do
        let ctx =  constField "commit" commitDetails
                <> constField "title" "Home"
                <> context

        getResourceBody
          >>= applyAsTemplate ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= lqipImages imageMetaData
          >>= relativizeUrls


postContext :: Context String
postContext
  =  asList "authors"
  <> asList "tags"
  <> context
    where
      asList fieldName
        = listFieldWith fieldName context (\i -> do
            let identifier = itemIdentifier i
            metadata <- getMetadata identifier
            let metas = maybe [] id $ lookupStringList fieldName metadata
            return $ map (\x -> Item (fromFilePath x) x) (sort metas)
          )


context :: Context String
context =
  constField "rootUrl" "https://intechsectional.space"
  <> dateField "date" "%B %e, %Y"
  <> defaultContext


config :: Configuration
config = defaultConfiguration
    { ignoreFile = ignoreFile'
    }
  where
    ignoreFile' path
        | "."     `isPrefixOf` fileName = True
        | "#"     `isPrefixOf` fileName = True
        | "~"     `isSuffixOf` fileName = True
        | ".swp"  `isSuffixOf` fileName = True
        --
        -- For git annoyances related to zsh.
        --
        | "/.git/" `isInfixOf` path     = True
        | otherwise                     = False
      where
        fileName = takeFileName path



-- ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
--
-- LQIP stuff
--
-- ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

data ImageData = ImageData
  { base64String :: !String
  , width        :: !Int
  , height       :: !Int
  , name         :: !String
  } deriving (Generic)


instance FromJSON ImageData


type ImageMetaDataMap = M.Map String ImageData


lqipImages :: ImageMetaDataMap -> Item String -> Compiler (Item String)
lqipImages  = lqipImages' True

lqipImages' :: Bool -> ImageMetaDataMap -> Item String -> Compiler (Item String)
-- No LQIP
-- lqipImages imageMetaData = return . fmap id
-- Full LQIP
lqipImages' doRel imageMetaData item =
  do
    route <- getRoute $ itemIdentifier item
    case route of
      Nothing -> pure item
      Just r  -> do
        pure $ fmap (withTags . switchInLqipImages' doRel (toSiteRoot r) $ imageMetaData) $ item



computeImageMetaData :: IO (ImageMetaDataMap)
computeImageMetaData = do
  items <- lines <$> readFile "./metadata/images.jsonl"

  let decoded' :: [Maybe ImageData]
      decoded' = map (decode' . toS) items
      decoded  = map fromJust (filter isJust decoded')

  return $ M.fromList (map (\i -> (name i, i)) decoded)

switchInLqipImages :: String -> ImageMetaDataMap -> Tag String -> Tag String
switchInLqipImages = switchInLqipImages' True

switchInLqipImages' :: Bool -> String -> ImageMetaDataMap -> Tag String -> Tag String
switchInLqipImages' doRel r imageMetaDataMap t@(TagOpen "img" attrs) = newTag
  where
    doLqip      = True -- Could be condition on some class.
    -- classes     = splitOn " " (fromMaybe "" $ M.lookup "class" attrDict)
    attrDict    = M.fromList attrs
    nonSrcAttrs = [ (k, v) | (k, v) <- attrs, v /= "src" ]
    --
    src       = fromMaybe (error $ "No source for tag: " ++ show t) (M.lookup "src" attrDict)
    mkRel root t =
      let
            isRel x = "/" `isPrefixOf` x && not ("//" `isPrefixOf` x)
            rel x   = if isRel x then root ++ x else x
       in if doRel then rel t else  t

    imageData = M.lookup (drop 1 src) imageMetaDataMap
    script    = ("onload", "this.src = '" ++ mkRel r src ++ "'; this.onload = null;")
    --
    newAttrs  = (\d -> script : ("src", "data:image/png;base64," ++ base64String d) : nonSrcAttrs) <$> imageData
    newTag    = case newAttrs of
                  Nothing -> t
                  Just nt -> if doLqip then (TagOpen "img" nt) else t
switchInLqipImages' _ _ _ t = t



makeLink
  :: Double
  -> Double
  -> String
  -> String
  -> Int
  -> Int
  -> Int
  -> String
makeLink minSize maxSize tag url count min' max' =
    let diff     = 1 + fromIntegral max' - fromIntegral min'
        relative = (fromIntegral count - fromIntegral min') / diff
        size     = floor $ minSize + relative * (maxSize - minSize) :: Int
    in renderHtml $
        H.a ! A.style (toValue $ "font-size: " ++ show size ++ "%")
            ! A.href (toValue url)
            ! A.class_ (toValue $ tag ++ " tag")
            $ toHtml tag
