{-# LANGUAGE  NoMonomorphismRestriction, FlexibleContexts, RecursiveDo #-}

module Main where

import Reflex
import Reflex.Monad

import Reflex.Html.Render
import Reflex.Html.Html
import Reflex.Html.Input

import Control.Lens hiding (element)
import Data.Default

html = element htmlNs
html_ = element_ htmlNs
html' = element' htmlNs

p' = html' "p"
p_ = html_ "p"

br = html "br"

div' = html' "div"
div_ = html_ "div"

button_ = html_ "button"
button' = html' "button"

h1_ = element_ htmlNs "h1"
h1' = element' htmlNs "h1"


form :: Renderer t => [String] -> Html t (Dynamic t [String])
form questions = workflow start where
  start = Workflow $ form' questions []

  form' [] ans = do
    text "All finished"
    (_, b) <- button' [] $ text "Again!"
    return (ans, start <$ clicked b)
  form' (q:qs) ans = do



    text q >> br []
    t <- textInput [] $ def
    (_, b) <-  button' [] $ text "Ok!"
    dynText (value t)

    let answer = tag (current $ value t) (clicked b)
    return (ans, Workflow . form' qs . (: ans)  <$> answer)





main = htmlBody $ do
  div_ [] $ do
    rec
      h1_ [] $ dynText =<< mapDyn (\x -> "hello world" ++ show x) c
      (_, b) <- button' [] $ text "click me"
      (_, h) <- button' [] $ text "hidden"

      p_ [] $ form ["How are you today?", "What is the meaning of life?"]

      c <- count (clicked b)
      hidden <- toggle False (clicked h)

      div_ [ hidden_ :~ hidden ] $ do
        (_, f) <- button' [] $ text "focus!"
        t <- textInput [] $ def
          & setValue .~ (show <$> updated c)
          & setFocus .~ (True <$ clicked f)

        dynText (value t)

        p_ [] $ do
          text "changed: "
          dynText =<< holdDyn "" (changed t)


    return b

  return ()
