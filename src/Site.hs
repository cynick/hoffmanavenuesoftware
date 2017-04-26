module Site (module Site) where

import Data.Monoid ((<>))
import qualified Data.List as DL
import qualified Data.List.Split as DL
import qualified Data.HashMap.Lazy as M
import Data.String
import Data.Maybe
import qualified Data.Aeson as A
import qualified Data.Text as T
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

renderedNavs :: Compiler [Item String]
renderedNavs = loadAllWithPat markdownPosts (Just "nav")

routeFromMetadata :: Metadata -> String
routeFromMetadata md = toStr $ fromJust (M.lookup "slug" md)
  where
    toStr (A.String s) = T.unpack s
    toStr _ = error "Expected slug to be a String value"


postRoute :: Routes
postRoute = metadataRoute (customRoute . const . routeFromMetadata)

makeListField :: String -> Compiler [Item String] -> Context a
makeListField name = listField name defaultContext

applyTemplate :: Identifier -> Context a -> Item a -> Compiler (Item String)
applyTemplate t context = loadAndApplyTemplate t context >=> relativizeUrls

pageTemplate :: IsString a => a
pageTemplate = "templates/page.html"

standalone :: IsString a => a
standalone = "templates/standalone.html"

archiveRow :: IsString a => a
archiveRow = "templates/archive-row.html"

applyPageTemplate :: Context a -> Item a -> Compiler (Item String)
applyPageTemplate = applyTemplate pageTemplate

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

compileStandalone :: Pattern -> Rules ()
compileStandalone pat = match pat $ do
 route $ setExtension ""
 compile $ pandocCompiler
   >>= applyTemplate standalone postContext

compileAbout :: Rules ()
compileAbout = compileStandalone "about.md"

compileNavs :: Rules ()
compileNavs = match markdownPosts $ version "nav" $ do
  compile $ do
    ident <- getUnderlying
    metadata <- getMetadata ident
    let

      toString (A.String s) = T.unpack s
      toString _ = error "Expected string value"
      link = fromJust $ M.lookup "link" metadata
      title = fromJust $ M.lookup "title" metadata
      context =
        constField "link" (toString link) <>
        constField "title" (toString title)

    pandocCompiler >>= applyTemplate "templates/naventry.html" context

compilePosts :: Rules ()
compilePosts = match markdownPosts $ do
  route postRoute

  compile $ do
    ident <- getUnderlying
    let pat = fromString (toFilePath ident)
    allNavs <- renderedNavs
    posts <- loadAllWithPat pat (Just "post")
    about <- loadAllWithPat "about.md" Nothing

    let
      context =
        listField "posts" postContext (return posts) <>
        listField "about" postContext (return about) <>
        listField "navs" postContext (return allNavs) <>
        postContext

    pandocCompiler >>= applyPageTemplate context

indexRoute :: String -> Routes
indexRoute path = customRoute (\_ -> path <> "/index.html")

createIndex :: Rules ()
createIndex =
  create ["index.html"] $ do
    route idRoute
    compile $ do
      posts <- renderedPosts
      about <- loadAllWithPat "about.md" Nothing
      allNavs <- renderedNavs

      let
        context =
          mconcat
          [ constField "title" siteTitle
          , listField "posts" postContext (return (DL.take 1 posts))
          , listField "about" postContext (return about)
          , listField "navs" postContext (return allNavs)
          , postContext
          ]
      makeItem "" >>= applyPageTemplate context

site :: IO ()
site = hakyll $ do
  match "templates/*" $ compile templateBodyCompiler

  match markdownPosts $ version "post" $
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" postContext

  match "about.md" $
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/standalone.html" postContext

  compileNavs
  compileAssets
  compilePosts

  createIndex



