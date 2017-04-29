--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Hakyll.Web.Sass (sassCompilerWith)
import           Text.Sass.Options
import qualified Bindings.Libsass as Lib


saasOptions = SassOptions
      { sassPrecision         = 5
      , sassOutputStyle       = Lib.SassStyleNested
      , sassSourceComments    = False
      , sassSourceMapEmbed    = False
      , sassSourceMapContents = False
      , sassOmitSourceMapUrl  = False
      , sassIsIndentedSyntax  = False
      , sassIndent            = "  "
      , sassLinefeed          = "\n"
      , sassInputPath         = Nothing
      , sassOutputPath        = Nothing
      , sassPluginPaths       = Nothing
      , sassIncludePaths      = Just ["./scss/foundation-components/", "./scss/"]
      , sassSourceMapFile     = Nothing
      , sassSourceMapRoot     = Nothing
      , sassFunctions         = Nothing
      , sassHeaders           = Nothing
      , sassImporters         = Nothing
      }
--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "fonts/*" $ do
       route idRoute
       compile copyFileCompiler
 
    match "scss/style.scss" $ do
      route $ constRoute "css/style.css"
      compile $ sassCompilerWith saasOptions

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["run.html"] $ do
      route idRoute
      compile copyFileCompiler

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
