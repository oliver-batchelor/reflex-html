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



html_ ::  (MonadWidget t m) => Attributes t m -> (Attributes t m, Html Meta m a) -> (Attributes t m, Html Flow m b) ->  Html cm m (a, b)
html_ attrs head' body = element_ "html" attrs $ do
  a <- uncurry (element_ "head") head'
  b <- uncurry (element_ "body") body
  return (a, b)
  

title_ ::  (MonadWidget t m) => Attributes t m -> Html Meta m a -> Html Head m a
title_  = element_ "title"


base_ ::  forall t m. (MonadWidget t m) => Attributes t m  -> Html Head m ()
base_ = empty_ "base"

link_ ::  (MonadWidget t m) => Attributes t m -> Html Meta m ()
link_ = empty "link"
  

-- Kind of pointless!
noscript_ ::  forall t m cm p. (MonadWidget t m, InModel cm [Phrasing, Flow, Meta]) => Attributes t m -> Html cm p m ()
noscript_ = empty_ "noscript"


section_ ::  (MonadWidget t m) => Attributes t m -> Html Flow m a -> Html Flow m a
section_  = element_ "section"

nav_ ::  (MonadWidget t m) => Attributes t m -> Html Flow m a -> Html Flow m a
nav_  = element_ "nav"

type Article'  = Html Flow Article_
article_ ::  (MonadWidget t m) => Attributes t m -> Html Flow m a -> Html Flow m a
article_  = element_ 


type Aside'  = Html Flow Aside_
aside_ ::  (MonadWidget t m) => Attributes t m -> Html Flow m a -> Html Flow m () 
aside_  = element_ 

type H1' = Html Flow H1_
h1 ::  (MonadWidget t m) => Attributes t m -> Html Flow m a -> Html Flow m () 
h1 = element_

type H2' = Html Flow H2_
h2 ::  (MonadWidget t m) => Attributes t m -> Html Flow m a -> Html Flow m () 
h2 = element_

type H3' = Html Flow H3_
h3 ::  (MonadWidget t m) => Attributes t m -> Html Flow m a -> Html Flow m () 
h3 = element_

type H4' = Html Flow H4_
h4 ::  (MonadWidget t m) => Attributes t m -> Html Flow m a -> Html Flow m () 
h4 = element_

type H5' = Html Flow H5_
h5 ::  (MonadWidget t m) => Attributes t m -> Html Flow m a -> Html Flow m () 
h5 = element_

type H6' = Html Flow H6_
h6 ::  (MonadWidget t m) => Attributes t m -> Html Flow m a -> Html Flow m () 
h6 = element_


type Header'  = Html Flow Header_
header_ ::  (MonadWidget t m) => Attributes t m -> Html Flow m a -> Html Flow m () 
header_  = element_ 

type Footer'  = Html Flow Footer_
footer_ ::  (MonadWidget t m) => Attributes t m -> Html Flow m a -> Html Flow m () 
footer_  = element_ 

type Address'  = Html Flow Address_
address_ ::  (MonadWidget t m) => Attributes t m -> Html Flow m a -> Html Flow m () 
address_  = element_ 


type P'  = Html Phrasing P_
p_ ::  (MonadWidget t m, PhrasingFlow cm) => Attributes t m -> Html Phrasing m a -> Html cm p m () 
p_  = element_ 


type Hr'  = Html Flow Hr_
hr_ ::  (MonadWidget t m) => Attributes t m -> Html Flow m a -> Html Flow m () 
hr_  = element_ 

br_ ::  forall t m cm. (MonadWidget t m, PhrasingFlow cm) => Attributes t m  -> Html cm p m ()
br_ = empty "br"


type Pre'  = Html Phrasing Pre_
pre_ ::  (MonadWidget t m, PhrasingFlow cm) => Attributes t m -> Html Phrasing m a -> Html cm p m () 
pre_  = element_ 


type Dialog'  = Html Flow Dialog_
dialog_ ::  (MonadWidget t m) => Attributes t m -> Html Flow m a -> Html Flow m () 
dialog_  = element_ 

type BlockQuote' = Html Flow BlockQuote_
blockquote_ ::  (MonadWidget t m) => Attributes t m -> Html Flow m a -> Html Flow m () 
blockquote_  = element_ 

type Ol' = Html List Ol_
ol_ ::  (MonadWidget t m) => Attributes t m -> Html List m a -> Html Flow m () 
ol_  = element_ 

type Ul' = Html List Ul_
ul_ ::  (MonadWidget t m) => Attributes t m -> Html List m a -> Html Flow m () 
ul_  = element_ 

type Li' = Html Flow Li_
li_ ::  (MonadWidget t m) => Attributes t m -> Html Flow m a -> List' p m () 
li_  = element_ 


dl_ ::  (MonadWidget t m) => Attributes t m -> Html DList m a -> Html Flow m a
dl_  = element_ 

dt_ :: MonadWidget t m => Attributes t m -> Html Flow m a -> Html DList m a
dt_ = element_ "dt"

dd_ :: MonadWidget t m => Attributes t m -> Html Flow m a -> Html DList m a
dd_ = element_ "dd"


a_ ::  (MonadWidget t m, PhrasingFlow cm) => Attributes t m -> Html cm m a -> Html cm m a
a_  = element_ "a"


audio_ ::  (MonadWidget t m, PhrasingFlow cm) => Attributes t m -> Html Tracks m a -> Html cm m b -> Html cm m (a, b)
audio_ attrs tracks rest = element_ "audio" attrs $ do
  (,) <$> unsafeCoerce tracks <*> rest


q_ ::  (MonadWidget t m, PhrasingFlow cm) => Attributes t m -> Html Phrasing m a -> Html cm m a
q_  = element_  "q"


cite_ ::  (MonadWidget t m, PhrasingFlow cm) => Attributes t m -> Html Phrasing m a -> Html cm m a
cite_  = element_ "cite"
  

em_ ::  (MonadWidget t m, PhrasingFlow cm) => Attributes t m -> Html Phrasing m a -> Html cm m a
em_  = element_ "em"


strong_ ::  (MonadWidget t m, PhrasingFlow cm) => Attributes t m -> Html Phrasing m a -> Html cm m a
strong_  = element_ "strong"


small_ ::  (MonadWidget t m, PhrasingFlow cm) => Attributes t m -> Html Phrasing m a -> Html cm m a
small_  = element_ "small"

mark_ ::  (MonadWidget t m, PhrasingFlow cm) => Attributes t m -> Html Phrasing m a -> Html cm m a
mark_  = element_ "mark"

  
dfn_ ::  (MonadWidget t m, PhrasingFlow cm) => Attributes t m -> Html Phrasing m a -> Html cm m a
dfn_  = element_ "dfn"

abbr_ ::  (MonadWidget t m, PhrasingFlow cm) => Attributes t m -> Html Phrasing m a -> Html cm m a
abbr_  = element_ "abbr"

time_ ::  (MonadWidget t m, PhrasingFlow cm) => Attributes t m -> Html Phrasing m a -> Html cm m a
time_  = element_ "time"

progress_ ::  (MonadWidget t m, PhrasingFlow cm) => Attributes t m -> Html Phrasing m a -> Html cm m a
progress_  = element_ "progress"

meter_ ::  (MonadWidget t m, PhrasingFlow cm) => Attributes t m -> Html Phrasing m a -> Html cm m a
meter_  = element_ "meter"

code_ ::  (MonadWidget t m, PhrasingFlow cm) => Attributes t m -> Html Phrasing m a -> Html cm m a
code_  = element_ "code"

var_ ::  (MonadWidget t m, PhrasingFlow cm) => Attributes t m -> Html Phrasing m a -> Html cm m a
var_  = element_ "var"

samp_ ::  (MonadWidget t m, PhrasingFlow cm) => Attributes t m -> Html Phrasing m a -> Html cm m a
samp_  = element_ "samp"


kbd_ ::  (MonadWidget t m, PhrasingFlow cm) => Attributes t m -> Html Phrasing m a -> Html cm m a
kbd_  = element_ "kbd"


sub_ ::  (MonadWidget t m, PhrasingFlow cm) => Attributes t m -> Html Phrasing m a -> Html cm m a
sub_  = element_ "sub"

sup_ ::  (MonadWidget t m, PhrasingFlow cm) => Attributes t m -> Html Phrasing m a -> Html cm m a
sup_  = element_ "sup"


i_ ::  (MonadWidget t m, PhrasingFlow cm) => Attributes t m -> Html Phrasing m a -> Html cm m a
i_  = element_ "i"

b_ ::  (MonadWidget t m, PhrasingFlow cm) => Attributes t m -> Html Phrasing m a -> Html cm m a
b_  = element_ "b"

bdo_ ::  (MonadWidget t m, PhrasingFlow cm) => Attributes t m -> Html Phrasing m a -> Html cm m a
bdo_  = element_ "bdo"


ruby_ ::  (MonadWidget t m, PhrasingFlow cm) => Attributes t m -> Html Ruby m a -> Html cm m a
ruby_  = element_ "ruby"


rt ::  (MonadWidget t m) => Attributes t m -> Html Phrasing m a -> Html Ruby m a
rt  = element_ "rt"

rtc ::  (MonadWidget t m) => Attributes t m -> Html Phrasing m a -> Html Ruby m a
rtc  = element_ "rtc"


rp ::  (MonadWidget t m) => Attributes t m -> Html Ruby m a -> Html Ruby m a
rp = element_ "rt"
