{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib
  ( run
  , readConfig
  , App(..)
  , AppError(..)
  ) where

import CMark (commonmarkToHtml, optUnsafe)
import Control.Monad.Except (ExceptT, MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (second)
import Data.List (sort)
import Data.Text (Text, unpack)
import qualified Data.Text as T (lines, unlines)
import qualified Data.Text.IO as T (readFile, writeFile)
import Data.Time.Calendar (Day)
import System.Directory (getDirectoryContents)
import System.FilePath ((-<.>), (</>), isExtensionOf)
import qualified Text.Mustache as M
  ( ToMustache(..)
  , compileTemplate
  , object
  , substitute
  )
import Text.Mustache (ToMustache, (~>))
import Text.Parsec.Error (ParseError)
import qualified Toml
  ( _Text
  , arrayOf
  , day
  , decode
  , dioptional
  , list
  , string
  , table
  , text
  )
import Toml (Codec(..), TomlCodec, TomlDecodeError, (.=), (<!>))

data Page = Page
  { readPath :: FilePath
  , writePath :: FilePath
  , meta :: Meta
  , body :: Text
  } deriving (Show, Eq)

data Templates = Templates
  { indexPath :: FilePath
  , pagesPath :: FilePath
  }

newtype HtmlPage =
  HtmlPage Page
  deriving (Show)

newtype MarkdownPage =
  MarkdownPage Page
  deriving (Show, Ord, Eq)

data Meta = Meta
  { title :: Text
  , date :: Maybe Day
  , tags :: [Text]
  } deriving (Show, Eq)

data Config = Config
  { sources :: [Source]
  , templates :: Templates
  }

data Source = Source
  { path :: FilePath
  , name :: Text
  , pages :: [MarkdownPage]
  }

data AppError
  = TemplateError ParseError
  | ConfigError [TomlDecodeError]

newtype App a = App
  { runApp :: (ExceptT AppError IO) a
  } deriving (Monad, Functor, Applicative, MonadIO, MonadError AppError)

instance Show Source where
  show = unpack . name

instance ToMustache Config where
  toMustache c = M.object ["sources" ~> sources c]

instance ToMustache Source where
  toMustache s = M.object ["pages" ~> pages s, "name" ~> name s]

instance ToMustache MarkdownPage where
  toMustache (MarkdownPage p) = M.toMustache p

instance ToMustache Page where
  toMustache p =
    M.object ["url" ~> writePath p, "meta" ~> meta p, "body" ~> body p]

instance ToMustache Meta where
  toMustache m =
    M.object ["title" ~> title m, "date" ~> (show <$> date m), "tags" ~> tags m]

instance Ord Page where
  compare Page {meta = m1} Page {meta = m2} = compare m1 m2

instance Ord Meta where
  compare (Meta t1 d1 ts1) (Meta t2 d2 ts2) =
    compare (d2, t1, ts1) (d1, t2, ts2)

run :: Config -> App ()
run c = do
  ss <- mapM readSource (sources c)
  mapM_ (writePage c) (concatMap pages ss)
  writeIndex (c {sources = ss})

readConfig :: FilePath -> App Config
readConfig p = mkConfig =<< liftIO (T.readFile p)

readSource :: Source -> App Source
readSource s@(Source p _ _) = do
  pagesPaths <- listPaths p
  ps <- mapM readPage pagesPaths
  return $ s {pages = sort ps}

readPage :: FilePath -> App MarkdownPage
readPage p = do
  c <- liftIO (T.readFile p)
  mkPage p c

listPaths :: FilePath -> App [FilePath]
listPaths p =
  map (p </>) . filter (isExtensionOf ".md") <$> liftIO (getDirectoryContents p)

writeIndex :: Config -> App ()
writeIndex c = do
  t <- liftIO $ T.readFile (indexPath $ templates c)
  applied <- applyIndexTemplate c t
  liftIO $ T.writeFile "index.html" applied

writePage :: Config -> MarkdownPage -> App ()
writePage c p@(MarkdownPage Page {writePath = path'}) = do
  liftIO $ putStrLn (" - " ++ path')
  t <- liftIO $ T.readFile (pagesPath $ templates c)
  applied <- applyPageTemplate (convertPage p) t
  liftIO $ T.writeFile path' applied

mkConfig :: Text -> App Config
mkConfig t = either (throwError . ConfigError) return (configFromText t)

mkPage :: FilePath -> Text -> App MarkdownPage
mkPage p t =
  either
    (throwError . ConfigError)
    (\m ->
       return $
       MarkdownPage
         Page
         {readPath = p, writePath = p -<.> ".html", meta = m, body = markdown'})
    (metaFromText meta')
  where
    (meta', markdown') =
      both T.unlines . second tail . break (== "%%%") $ T.lines t

applyIndexTemplate :: Config -> Text -> App Text
applyIndexTemplate c t =
  either
    (throwError . TemplateError)
    (\x -> return $ M.substitute x c)
    (M.compileTemplate "index" t)

applyPageTemplate :: HtmlPage -> Text -> App Text
applyPageTemplate (HtmlPage p) t =
  either
    (throwError . TemplateError)
    (\x -> return $ M.substitute x p)
    (M.compileTemplate "index" t)

convertPage :: MarkdownPage -> HtmlPage
convertPage (MarkdownPage p@Page {body = b}) =
  HtmlPage $ p {body = commonmarkToHtml [optUnsafe] b}

metaFromText :: Text -> Either [TomlDecodeError] Meta
metaFromText = Toml.decode metaCodec

configFromText :: Text -> Either [TomlDecodeError] Config
configFromText = Toml.decode configCodec

--
-- Codec
--
metaCodec :: TomlCodec Meta
metaCodec =
  Meta <$> Toml.text "title" .= title <*>
  Toml.dioptional (Toml.day "date") .= date <*>
  listOrEmpty (Toml.arrayOf Toml._Text "tags") .= tags

configCodec :: TomlCodec Config
configCodec =
  Config <$> Toml.list sourceCodec "source" .= sources <*>
  Toml.table templatesCodec "templates" .= templates

sourceCodec :: TomlCodec Source
sourceCodec =
  Source <$> Toml.string "path" .= path <*> Toml.text "name" .= name ?? []

templatesCodec :: TomlCodec Templates
templatesCodec =
  Templates <$> Toml.string "index" .= indexPath <*>
  Toml.string "pages" .= pagesPath

--
-- Helpers
--
both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

infixl 4 ??

(??) :: Functor f => f (a -> b) -> a -> f b
(??) ff x = (\f -> f x) <$> ff

listOrEmpty :: Codec i [a] -> Codec i [a]
listOrEmpty Codec {..} =
  Codec {codecRead = codecRead <!> \_ -> pure [], codecWrite = codecWrite}
