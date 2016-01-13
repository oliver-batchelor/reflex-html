{-# LANGUAGE OverloadedStrings,  NoMonomorphismRestriction, FlexibleContexts, RecursiveDo, TupleSections #-}

module Main where

import Reflex
import Reflex.Monad

import Reflex.Html.Html
import Reflex.Html.Input
import Reflex.Html.Element
import Reflex.Html.Elements.Html

import Reflex.Html.Prelude

import Control.Lens hiding (element)
import Data.Default
import Data.String

import Prelude hiding (div)

myForm :: MonadWidget t m => [DomString] -> m (Dynamic t [DomString])
myForm questions = workflow start where
  start = Workflow $ form' questions []

  form' [] ans = do
    text "All finished"
    b <- button_ [] $ text "Again!"
    return (ans, start <$ clicked b)
  form' (q:qs) ans = do
    answer <- inputOk q
    return (ans, Workflow . form' qs . (: ans)  <$> answer)


inputOk :: MonadWidget t m => DomString -> m (Event t DomString)
inputOk q = do
  text q >> br []
  t <- textInput [] $ def
  b <-  button_  [] $ text "Ok!"
  dynText (value t)

  return $ tag (current $ value t) (clicked b)

inputOk' :: MonadWidget t m => DomString -> m ((), Event t ())
inputOk' = fmap (\e -> ((), void e)) . inputOk


main = htmlBody widget

widget :: MonadWidget t m => m ()
widget = div [] $ do
  rec
    h1_ [] $ dynText =<< mapDyn (\x -> "hello world" <> fromString (show x)) c
    b <- button_ [] $ text "click me"
    h <- button_ [] $ text "hidden"
    new <- button_ [] $ text "add new"

    m <- holdMapDyn =<< collection (inputOk' <$> ["1", "2"]) ([inputOk' "new item"] <$ clicked new)
    p [] $ dynText =<< mapDyn (fromString . show) m


    p [] $ myForm ["How are you today?", "What is the meaning of life?"]

    c <- count (clicked b)
    hidden <- toggle False (clicked h)

    div [ hidden_ :~ hidden ] $ do
      f <- button_ [] $ text "focus!"
      t <- textInput [] $ def
         & setValue .~ (fromString . show <$> updated c)
         & setFocus .~ (True <$ clicked f)

      dynText (value t)

      p [] $ do
        text "changed: "
        dynText =<< holdDyn "" (changed t)
  return ()

