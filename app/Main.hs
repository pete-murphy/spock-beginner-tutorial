{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad          (forM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.IORef
import           Data.Semigroup         ((<>))
import           Data.Text              (Text)
import           Lucid
import           Web.Spock
import           Web.Spock.Config
import           Web.Spock.Lucid        (lucid)

data Note = Note
  { author   :: Text
  , contents :: Text
  }

newtype ServerState = ServerState
  { notes :: IORef [Note]
  }

type Server a = SpockM () () ServerState a

app :: Server ()
app = do
  get root $ do
    ns' <- getState >>= (liftIO . readIORef . notes)
    lucid $ do
      h1_ "Notes"
      ul_ $
        forM_ ns' $ \n ->
          li_ $ do
            toHtml (author n)
            ": "
            toHtml (contents n)
      h2_ "New note"
      form_ [method_ "post"] $ do
        label_ "Name"
        input_ [type_ "text", name_ "author"]
        label_ "Contents"
        input_ [type_ "text", name_ "contents"]
        button_ "Submit"
  post root $ do
    author <- param' "author"
    contents <- param' "contents"
    notesRef <- notes <$> getState
    liftIO $
      atomicModifyIORef' notesRef $ \notes ->
        (notes <> [Note author contents], ())
    redirect "/"

main :: IO ()
main = do
  st <-
    ServerState <$>
    newIORef
      [Note "Alice" "Hey nice to meet you", Note "Bob" "Pleasure was all mine"]
  cfg <- defaultSpockCfg () PCNoDatabase st
  runSpock 9000 (spock cfg app)
