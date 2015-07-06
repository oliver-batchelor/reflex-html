module Reflex.Dom.Html.Elements 
  ( module Reflex.Dom.Html.Elements
  , Element  
  , IsElement (..)
  )
  where

import Reflex.Dom hiding (Attributes)
import Reflex.Dom.Html.Internal.Element
import Reflex.Dom.Html.Internal.Attributes

import Reflex.Dom.Html.Internal.Html
import Reflex.Dom.Html.Internal.Tag


import Data.Promotion.Prelude


type InModel (cm :: ContentModel) models = (Elem cm models) ~ True 
type InTag (e :: Tag) tags = (Elem e tags) ~ True 



type PhrasingFlow p = (Elem p [Flow, Phrasing]) ~ True 
type AllowsText p = (Elem p [TextOnly, Flow, Phrasing]) ~ True 
  

empty :: (MonadWidget t m) => Html cm e m ()
empty = return () 


html_ ::  (MonadWidget t m) => Attributes t m -> Html None Html_ m a -> Html cm p m a
html_  = element_ 

head_ ::  (MonadWidget t m) => Attributes t m -> Html Meta Head_ m a -> Html None Html_ m a
head_  = element_ 

title_ ::  (MonadWidget t m) => Attributes t m -> Html TextOnly Title_ m a -> Html Meta Head_ m a
title_  = element_ 


base_ ::  forall t m. (MonadWidget t m) => Attributes t m  -> Html Meta Head_ m ()
base_ attr = element_ attr (empty :: Html None Base_ m ())

link_ ::  forall t m p. (MonadWidget t m) => Attributes t m  -> Html Meta p m ()
link_ attr = element_ attr (empty :: Html None Link_ m ())


noscript_ ::  forall t m cm p. (MonadWidget t m, InModel cm [Phrasing, Flow, Meta]) => Attributes t m -> Html cm p m ()
noscript_ attr = element_ attr (empty :: Html None NoScript_ m ())


body_ ::  (MonadWidget t m) => Attributes t m -> Html Flow Body_ m a -> Html None Html_ m a
body_  = element_ 


section_ ::  (MonadWidget t m) => Attributes t m -> Html Flow Section_ m a -> Html Flow p m a
section_  = element_ 


nav_ ::  (MonadWidget t m) => Attributes t m -> Html Flow Nav_ m a -> Html Flow p m a
nav_  = element_ 

article_ ::  (MonadWidget t m) => Attributes t m -> Html Flow Article_ m a -> Html Flow p m a
article_  = element_ 

aside_ ::  (MonadWidget t m) => Attributes t m -> Html Flow Aside_ m () -> Html Flow p m () 
aside_  = element_ 

h1 ::  (MonadWidget t m) => Attributes t m -> Html Flow H1_ m () -> Html Flow p m () 
h1 = element_

h2 ::  (MonadWidget t m) => Attributes t m -> Html Flow H2_ m () -> Html Flow p m () 
h2 = element_

h3 ::  (MonadWidget t m) => Attributes t m -> Html Flow H3_ m () -> Html Flow p m () 
h3 = element_

h4 ::  (MonadWidget t m) => Attributes t m -> Html Flow H4_ m () -> Html Flow p m () 
h4 = element_

h5 ::  (MonadWidget t m) => Attributes t m -> Html Flow H5_ m () -> Html Flow p m () 
h5 = element_

h6 ::  (MonadWidget t m) => Attributes t m -> Html Flow H6_ m () -> Html Flow p m () 
h6 = element_


header_ ::  (MonadWidget t m) => Attributes t m -> Html Flow Header_ m () -> Html Flow p m () 
header_  = element_ 

footer_ ::  (MonadWidget t m) => Attributes t m -> Html Flow Footer_ m () -> Html Flow p m () 
footer_  = element_ 


address_ ::  (MonadWidget t m) => Attributes t m -> Html Flow Address_ m () -> Html Flow p m () 
address_  = element_ 


p_ ::  (MonadWidget t m, PhrasingFlow cm) => Attributes t m -> Html Phrasing P_ m () -> Html cm p m () 
p_  = element_ 


hr_ ::  (MonadWidget t m) => Attributes t m -> Html Flow Hr_ m () -> Html Flow p m () 
hr_  = element_ 


br_ ::  forall t m cm. (MonadWidget t m, PhrasingFlow cm) => Attributes t m  -> Html cm p m ()
br_ attr = element_ attr (empty :: Html None Br_ m ())


pre_ ::  (MonadWidget t m, PhrasingFlow cm) => Attributes t m -> Html Phrasing Pre_ m () -> Html cm p m () 
pre_  = element_ 


dialog_ ::  (MonadWidget t m) => Attributes t m -> Html None Dialog_ m () -> Html Flow p m () 
dialog_  = element_ 

blockquote_ ::  (MonadWidget t m) => Attributes t m -> Html Flow BlockQuote_ () -> Html Flow p m () 
blockquote_  = element_ 

ol_ ::  (MonadWidget t m) => Attributes t m -> Html List Ol_ () -> Html Flow p m () 
ol_  = element_ 

ul_ ::  (MonadWidget t m) => Attributes t m -> Html List Ul_ () -> Html Flow p m () 
ul_  = element_ 

li_ ::  (MonadWidget t m) => Attributes t m -> Html Flow Li_ () -> Html List p m () 
li_  = element_ 


dl_ ::  (MonadWidget t m) => Attributes t m -> Html None Dl_ () -> Html Flow p m () 
dl_  = element_ 

dt_ ::  (MonadWidget t m, InTag p [Dl_, Dialog_]) => Attributes t m -> Html Phrasing Dt_ () -> Html None p m () 
dt_  = element_ 


dd_ ::  (MonadWidget t m, InTag p [Dl_, Dialog_]) => Attributes t m -> Html Flow Dd_ () -> Html None p m () 
dd_  = element_ 


a_ ::  (MonadWidget t m, PhrasingFlow cm) => Attributes t m -> Html cm A_ m a -> Html cm p m a
a_  = element_ 




audio_ ::  (MonadWidget t m, PhrasingFlow cm) => Attributes t m -> Html cm Audio_ m a -> Html cm p m a
audio_  = element_ 