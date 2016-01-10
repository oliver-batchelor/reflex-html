{-# LANGUAGE OverloadedStrings,  NoMonomorphismRestriction, FlexibleContexts, RecursiveDo #-}

module Main where

import Reflex
import Reflex.Monad

import Reflex.Html.Html
import Reflex.Html.Input
import Reflex.Html.Element
import Reflex.Html.HtmlElements

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

    text q >> br []
    t <- textInput [] $ def
    b <-  button_  [] $ text "Ok!"
    dynText (value t)

    let answer = tag (current $ value t) (clicked b)
    return (ans, Workflow . form' qs . (: ans)  <$> answer)


main = htmlBody widget

widget :: MonadWidget t m => m ()
widget = div [] $ do
  rec
    h1_ [] $ dynText =<< mapDyn (\x -> "hello world" <> fromString (show x)) c
    b <- button_ [] $ text "click me"
    h <- button_ [] $ text "hidden"

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

