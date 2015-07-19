{-# LANGUAGE TemplateHaskell #-}

module Reflex.Html.Internal.DomEvents where

import qualified GHCJS.DOM.Types as Dom
import qualified GHCJS.DOM.Event as Dom
import qualified GHCJS.DOM.EventM as Dom
import qualified GHCJS.DOM.UIEvent as Dom
import qualified GHCJS.DOM.Element as Dom

import Data.GADT.Compare.TH

type KeyCode = Int

data EventTag
  = AbortTag
  | BlurTag
  | ChangeTag
  | ClickTag
  | ContextmenuTag
  | DblclickTag
  | DragTag
  | DragendTag
  | DragenterTag
  | DragleaveTag
  | DragoverTag
  | DragstartTag
  | DropTag
  | ErrorTag
  | FocusTag
  | InputTag
  | InvalidTag
  | KeydownTag
  | KeypressTag
  | KeyupTag
  | LoadTag
  | MousedownTag
  | MouseenterTag
  | MouseleaveTag
  | MousemoveTag
  | MouseoutTag
  | MouseoverTag
  | MouseupTag
--   | MousewheelTag -- webkitgtk only provides elementOnmousewheel (not elementOnwheel), but firefox does not support the mousewheel event; we should provide wheel (the equivalent, standard event), but we will need to make sure webkitgtk supports it first
  | ScrollTag
  | SelectTag
  | SubmitTag
--   | WheelTag -- See MousewheelTag
  | BeforecutTag
  | CutTag
  | BeforecopyTag
  | CopyTag
  | BeforepasteTag
  | PasteTag
  | ResetTag
  | SearchTag
  | SelectstartTag
  | TouchstartTag
  | TouchmoveTag
  | TouchendTag
  | TouchcancelTag

data EventName :: EventTag -> * where
  Abort       :: EventName 'AbortTag
  Blur        :: EventName 'BlurTag
  Change      :: EventName 'ChangeTag
  Click       :: EventName 'ClickTag
  Contextmenu :: EventName 'ContextmenuTag
  Dblclick    :: EventName 'DblclickTag
  Drag        :: EventName 'DragTag
  Dragend     :: EventName 'DragendTag
  Dragenter   :: EventName 'DragenterTag
  Dragleave   :: EventName 'DragleaveTag
  Dragover    :: EventName 'DragoverTag
  Dragstart   :: EventName 'DragstartTag
  Drop        :: EventName 'DropTag
  Error       :: EventName 'ErrorTag
  Focus       :: EventName 'FocusTag
  Input       :: EventName 'InputTag
  Invalid     :: EventName 'InvalidTag
  Keydown     :: EventName 'KeydownTag
  Keypress    :: EventName 'KeypressTag
  Keyup       :: EventName 'KeyupTag
  Load        :: EventName 'LoadTag
  Mousedown   :: EventName 'MousedownTag
  Mouseenter  :: EventName 'MouseenterTag
  Mouseleave  :: EventName 'MouseleaveTag
  Mousemove   :: EventName 'MousemoveTag
  Mouseout    :: EventName 'MouseoutTag
  Mouseover   :: EventName 'MouseoverTag
  Mouseup     :: EventName 'MouseupTag
  --Mousewheel :: EventName 'MousewheelTag
  Scroll      :: EventName 'ScrollTag
  Select      :: EventName 'SelectTag
  Submit      :: EventName 'SubmitTag
  --Wheel     :: EventName 'WheelTag
  Beforecut   :: EventName 'BeforecutTag
  Cut         :: EventName 'CutTag
  Beforecopy  :: EventName 'BeforecopyTag
  Copy        :: EventName 'CopyTag
  Beforepaste :: EventName 'BeforepasteTag
  Paste       :: EventName 'PasteTag
  Reset       :: EventName 'ResetTag
  Search      :: EventName 'SearchTag
  Selectstart :: EventName 'SelectstartTag
  Touchstart  :: EventName 'TouchstartTag
  Touchmove   :: EventName 'TouchmoveTag
  Touchend    :: EventName 'TouchendTag
  Touchcancel :: EventName 'TouchcancelTag

type family EventType en where
  EventType 'AbortTag     =  Dom.UIEvent
  EventType 'BlurTag        = Dom.UIEvent
  EventType 'ChangeTag      = Dom.UIEvent
  EventType 'ClickTag       = Dom.MouseEvent
  EventType 'ContextmenuTag = Dom.MouseEvent
  EventType 'DblclickTag    = Dom.MouseEvent
  EventType 'DragTag        = Dom.MouseEvent
  EventType 'DragendTag     = Dom.MouseEvent
  EventType 'DragenterTag   = Dom.MouseEvent
  EventType 'DragleaveTag   = Dom.MouseEvent
  EventType 'DragoverTag    = Dom.MouseEvent
  EventType 'DragstartTag   = Dom.MouseEvent
  EventType 'DropTag        = Dom.MouseEvent
  EventType 'ErrorTag       = Dom.UIEvent
  EventType 'FocusTag       = Dom.UIEvent
  EventType 'InputTag       = Dom.UIEvent
  EventType 'InvalidTag     = Dom.UIEvent
  EventType 'KeydownTag     = Dom.UIEvent
  EventType 'KeypressTag    = Dom.UIEvent
  EventType 'KeyupTag       = Dom.UIEvent
  EventType 'LoadTag        = Dom.UIEvent
  EventType 'MousedownTag   = Dom.MouseEvent
  EventType 'MouseenterTag  = Dom.UIEvent
  EventType 'MouseleaveTag  = Dom.UIEvent
  EventType 'MousemoveTag   = Dom.MouseEvent
  EventType 'MouseoutTag    = Dom.MouseEvent
  EventType 'MouseoverTag   = Dom.MouseEvent
  EventType 'MouseupTag     = Dom.MouseEvent
  --EventType 'MousewheelTag = Dom.MouseEvent
  EventType 'ScrollTag      = Dom.UIEvent
  EventType 'SelectTag      = Dom.UIEvent
  EventType 'SubmitTag      = Dom.UIEvent
  --EventType 'WheelTag     = Dom.UIEvent
  EventType 'BeforecutTag    = Dom.UIEvent
  EventType 'CutTag         = Dom.UIEvent
  EventType 'BeforecopyTag  = Dom.UIEvent
  EventType 'CopyTag        = Dom.UIEvent
  EventType 'BeforepasteTag = Dom.UIEvent
  EventType 'PasteTag       = Dom.UIEvent
  EventType 'ResetTag       = Dom.UIEvent
  EventType 'SearchTag      = Dom.UIEvent
  EventType 'SelectstartTag = Dom.UIEvent
  EventType 'TouchstartTag  = Dom.UIEvent
  EventType 'TouchmoveTag   = Dom.UIEvent
  EventType 'TouchendTag    = Dom.UIEvent
  EventType 'TouchcancelTag = Dom.UIEvent

onEventName :: Dom.IsElement e => EventName en -> e -> Dom.EventM (EventType en) e () -> IO (IO ())
onEventName en = case en of
  Abort         -> Dom.elementOnabort
  Blur          -> Dom.elementOnblur
  Change        -> Dom.elementOnchange
  Click         -> Dom.elementOnclick
  Contextmenu   -> Dom.elementOncontextmenu
  Dblclick      -> Dom.elementOndblclick
  Drag          -> Dom.elementOndrag
  Dragend       -> Dom.elementOndragend
  Dragenter     -> Dom.elementOndragenter
  Dragleave     -> Dom.elementOndragleave
  Dragover      -> Dom.elementOndragover
  Dragstart     -> Dom.elementOndragstart
  Drop          -> Dom.elementOndrop
  Error         -> Dom.elementOnerror
  Focus         -> Dom.elementOnfocus
  Input         -> Dom.elementOninput
  Invalid       -> Dom.elementOninvalid
  Keydown       -> Dom.elementOnkeydown
  Keypress      -> Dom.elementOnkeypress
  Keyup         -> Dom.elementOnkeyup
  Load          -> Dom.elementOnload
  Mousedown     -> Dom.elementOnmousedown
  Mouseenter    -> Dom.elementOnmouseenter
  Mouseleave    -> Dom.elementOnmouseleave
  Mousemove     -> Dom.elementOnmousemove
  Mouseout      -> Dom.elementOnmouseout
  Mouseover     -> Dom.elementOnmouseover
  Mouseup       -> Dom.elementOnmouseup
  --Mousewheel  -> Dom.elementOnmousewheel
  Scroll        -> Dom.elementOnscroll
  Select        -> Dom.elementOnselect
  Submit        -> Dom.elementOnsubmit
  --Wheel       -> Dom.elementOnwheel
  Beforecut     -> Dom.elementOnbeforecut
  Cut           -> Dom.elementOncut
  Beforecopy    -> Dom.elementOnbeforecopy
  Copy          -> Dom.elementOncopy
  Beforepaste   -> Dom.elementOnbeforepaste
  Paste         -> Dom.elementOnpaste
  Reset         -> Dom.elementOnreset
  Search        -> Dom.elementOnsearch
  Selectstart   -> Dom.elementOnselectstart
  Touchstart    -> Dom.elementOntouchstart
  Touchmove     -> Dom.elementOntouchmove
  Touchend      -> Dom.elementOntouchend
  Touchcancel   -> Dom.elementOntouchcancel

newtype EventResult en = EventResult { unEventResult :: EventResultType en }

type family EventResultType (en :: EventTag) :: * where
  EventResultType 'ClickTag       = ()
  EventResultType 'DblclickTag    = ()
  EventResultType 'KeypressTag    = KeyCode
  EventResultType 'KeydownTag     = KeyCode
  EventResultType 'ScrollTag      = Int
  EventResultType 'MousemoveTag   = (Int, Int)
  EventResultType 'MousedownTag   = (Int, Int)
  EventResultType 'MouseupTag     = (Int, Int)
  EventResultType 'MouseenterTag  = ()
  EventResultType 'MouseleaveTag  = ()
  EventResultType 'FocusTag       = ()
  EventResultType 'BlurTag        = ()
  EventResultType 'InputTag       = ()

  
  
deriveGEq ''EventName
deriveGCompare ''EventName