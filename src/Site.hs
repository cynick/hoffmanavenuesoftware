module Site where

import Data.Monoid ((<>))
import qualified Data.List as DL
import Data.String
import Data.Binary
import Data.Typeable
import Data.Time
import Data.Function (on)
import Control.Monad

import Hakyll hiding (applyTemplate)

siteTitle :: IsString a => a
siteTitle = "Hoffman Avenue Software"

siteBase :: IsString a => a
siteBase = "http://local.hoffmanavenuesoftware.com"

markdownPosts :: IsString a => a
markdownPosts = "posts/*.md"

dateContext :: Context String
dateContext = dateField "date" "%B %e, %Y" <> defaultContext

postContext :: Context String
postContext = mconcat
  [ dateContext
  , bodyField "body"
  , metadataField
  , constField "base" siteBase
  ]

type Version = Pattern

loadAllWithPat :: Pattern -> Maybe String -> Compiler [Item String]
loadAllWithPat pat ver = recentFirst =<< loadAll pat'
  where pat' = maybe pat ((pat .&&.) . hasVersion) ver

renderedPosts :: Compiler [Item String]
renderedPosts = loadAllWithPat markdownPosts (Just "post")

renderedPostPaths :: Compiler [FilePath]
renderedPostPaths = fmap (fmap (toFilePath . itemIdentifier)) renderedPosts

postNavToUrl :: String -> String
postNavToUrl = absRouteForPostIdent . fromFilePath

navToUrl :: String -> String
navToUrl = (siteBase <>)

absRouteForPostIdent :: Identifier -> FilePath
absRouteForPostIdent = (siteBase <>) . routeForPostIdent

{- This function converts identifiers of the form
   posts/yyyy-mm-dd-<post-name> into
   yyyy/mm/<post-name
-}
routeForPostIdent :: Identifier -> FilePath
routeForPostIdent ident = takeWhile (/= '.') $ convert (toFilePath ident)
  where
    convert path =
      let
        name = drop 1 . dropWhile (/= '/') $ path
        y = take 4 name
        m = take 2 (drop 5 name)
      in y <> "/" <> m <> "/" <> drop 11 name

makeListField :: String -> Compiler [Item String] -> Context a
makeListField name = listField name defaultContext

applyTemplate :: Identifier -> Context a -> Item a -> Compiler (Item String)
applyTemplate t context = loadAndApplyTemplate t context >=> relativizeUrls

pageTemplate :: IsString a => a
pageTemplate = "templates/page.html"

archiveRow :: IsString a => a
archiveRow = "templates/archive-row.html"

applyPageTemplate :: Context a -> Item a -> Compiler (Item String)
applyPageTemplate = applyTemplate pageTemplate

{--
 Given an element, and a list, return a tuple containing
 the element's left and right neighbors.
--}
findAdj :: (Show a, Eq a) => a -> [a] -> (Maybe a, Maybe a)
findAdj x xs | x `elem` xs = r
             | otherwise = err
  where
    err = error $ "Expected " ++ show x ++ " to be a member of " ++ show xs
    xs' = Just <$> xs
    p = Nothing : Nothing : xs'
    n = xs' ++ [Just (head xs)]
    Just r = lookup x (zip xs (drop 1 (zip p n)))

adjOf :: (Show a, Eq a) => a -> [a] -> (Maybe a, Maybe a)
adjOf a xs | a `elem` xs = go xs
           | otherwise = err
  where
    go [] = (Nothing,Nothing)
    go [_] = (Nothing,Nothing)
    go [x,y] | x == a = (Nothing, Just y)
                | y == a = (Just x, Nothing)
                | otherwise = (Nothing, Nothing)
    go xs'@(x:y:z:_) | x == a = (Nothing, Just y)
                  | y == a = (Just x, Just z)
                  | otherwise = go (tail xs')

    err = error $ "Expected " ++ show a ++ " to be a member of " ++ show xs

type Navigation = (Maybe String, Maybe String)

navUrlsFor ::
  MonadMetadata m =>
  Identifier
  -> m [String]
  -> (String -> String) -- function to convert identifiers into URLs
  -> m Navigation
navUrlsFor ident items convertToUrl = do
  paths <- items
  let
    path = toFilePath ident
    toRet f (a,b) = (f b, f a)
  return $ toRet (fmap convertToUrl) (findAdj path paths)

navContext :: Navigation -> Context String
navContext u = uncurry go u <> postContext
  where
    go (Just p) (Just n) = constField "prev" p <> constField "next" n
    go Nothing (Just n) = constField "next" n
    go (Just p) Nothing = constField "prev" p
    go Nothing Nothing = mempty

compileAssetsFor ::
  (Writable a, Binary a, Typeable a)
  => Pattern -> Compiler (Item a) -> Rules ()
compileAssetsFor pat compiler = match pat $ route idRoute >> compile compiler

compileAssets :: Rules ()
compileAssets = do
  compileAssetsFor "images/*" copyFileCompiler
  compileAssetsFor "fonts/*" copyFileCompiler
  compileAssetsFor "js/*" copyFileCompiler
  compileAssetsFor "css/*" copyFileCompiler -- compressCssCompiler
  compileAssetsFor "favicon.ico" copyFileCompiler
  compileAssetsFor "robots.txt" copyFileCompiler

compilePosts :: Rules ()
compilePosts = match markdownPosts $ do
  route $ customRoute routeForPostIdent `composeRoutes` setExtension ""

  compile $ do
    ident <- getUnderlying
    let pat = fromString (toFilePath ident)
    posts <- loadAllWithPat pat (Just "post")
    navUrls <- navUrlsFor ident renderedPostPaths postNavToUrl
    let
      context =
        listField "posts" postContext (return posts) <>
        navContext navUrls

    pandocCompiler >>= applyPageTemplate context


indexRoute :: String -> Routes
indexRoute path = customRoute (\_ -> path <> "/index.html")

createIndex :: Rules ()
createIndex =
  create ["index.html"] $ do
    route idRoute
    compile $ do
      posts <- take 1 <$> renderedPosts
      navUrls <- navUrlsFor (itemIdentifier (head posts)) renderedPostPaths postNavToUrl
      let
        context =
          mconcat
          [ constField "title" siteTitle
          , listField "posts" postContext (return posts)
          , navContext navUrls
          ]
      makeItem "" >>= applyPageTemplate context

site :: IO ()
site = hakyll $ do
  match "templates/*" $ compile templateBodyCompiler

  match markdownPosts $ version "post" $
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" postContext

  compileAssets
  compilePosts

  createIndex



