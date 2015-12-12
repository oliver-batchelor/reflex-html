{-# LANGUAGE TemplateHaskell, FlexibleContexts, TypeFamilies, GADTs, DataKinds, KindSignatures #-}

module Reflex.Html.Event where

import qualified GHCJS.DOM.Element as Dom
import qualified GHCJS.DOM.EventM as Dom
import qualified GHCJS.DOM.UIEvent as Dom
import qualified GHCJS.DOM.Types as Dom

import Data.Bitraversable
import Data.GADT.Compare.TH


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
   | MousewheelTag
   | ScrollTag
   | SelectTag
   | SubmitTag
   | WheelTag
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
  Abort          :: EventName 'AbortTag
  Blur           :: EventName 'BlurTag
  Change         :: EventName 'ChangeTag
  Click          :: EventName 'ClickTag
  Contextmenu    :: EventName 'ContextmenuTag
  Dblclick       :: EventName 'DblclickTag
  Drag           :: EventName 'DragTag
  Dragend        :: EventName 'DragendTag
  Dragenter      :: EventName 'DragenterTag
  Dragleave      :: EventName 'DragleaveTag
  Dragover       :: EventName 'DragoverTag
  Dragstart      :: EventName 'DragstartTag
  Drop           :: EventName 'DropTag
  Error          :: EventName 'ErrorTag
  Focus          :: EventName 'FocusTag
  Input          :: EventName 'InputTag
  Invalid        :: EventName 'InvalidTag
  Keydown        :: EventName 'KeydownTag
  Keypress       :: EventName 'KeypressTag
  Keyup          :: EventName 'KeyupTag
  Load           :: EventName 'LoadTag
  Mousedown      :: EventName 'MousedownTag
  Mouseenter     :: EventName 'MouseenterTag
  Mouseleave     :: EventName 'MouseleaveTag
  Mousemove      :: EventName 'MousemoveTag
  Mouseout       :: EventName 'MouseoutTag
  Mouseover      :: EventName 'MouseoverTag
  Mouseup        :: EventName 'MouseupTag
  Mousewheel     :: EventName 'MousewheelTag
  Scroll         :: EventName 'ScrollTag
  Select         :: EventName 'SelectTag
  Submit         :: EventName 'SubmitTag
  Wheel          :: EventName 'WheelTag
  Beforecut      :: EventName 'BeforecutTag
  Cut            :: EventName 'CutTag
  Beforecopy     :: EventName 'BeforecopyTag
  Copy           :: EventName 'CopyTag
  Beforepaste    :: EventName 'BeforepasteTag
  Paste          :: EventName 'PasteTag
  Reset          :: EventName 'ResetTag
  Search         :: EventName 'SearchTag
  Selectstart    :: EventName 'SelectstartTag
  Touchstart     :: EventName 'TouchstartTag
  Touchmove      :: EventName 'TouchmoveTag
  Touchend       :: EventName 'TouchendTag
  Touchcancel    :: EventName 'TouchcancelTag

type family EventType en where
  EventType 'AbortTag            = Dom.UIEvent
  EventType 'BlurTag             = Dom.FocusEvent
  EventType 'ChangeTag           = Dom.Event
  EventType 'ClickTag            = Dom.MouseEvent
  EventType 'ContextmenuTag      = Dom.MouseEvent
  EventType 'DblclickTag         = Dom.MouseEvent
  EventType 'DragTag             = Dom.MouseEvent
  EventType 'DragendTag          = Dom.MouseEvent
  EventType 'DragenterTag        = Dom.MouseEvent
  EventType 'DragleaveTag        = Dom.MouseEvent
  EventType 'DragoverTag         = Dom.MouseEvent
  EventType 'DragstartTag        = Dom.MouseEvent
  EventType 'DropTag             = Dom.MouseEvent
  EventType 'ErrorTag            = Dom.UIEvent
  EventType 'FocusTag            = Dom.FocusEvent
  EventType 'InputTag            = Dom.Event
  EventType 'InvalidTag          = Dom.Event
  EventType 'KeydownTag          = Dom.KeyboardEvent
  EventType 'KeypressTag         = Dom.KeyboardEvent
  EventType 'KeyupTag            = Dom.KeyboardEvent
  EventType 'LoadTag             = Dom.UIEvent
  EventType 'MousedownTag        = Dom.MouseEvent
  EventType 'MouseenterTag       = Dom.MouseEvent
  EventType 'MouseleaveTag       = Dom.MouseEvent
  EventType 'MousemoveTag        = Dom.MouseEvent
  EventType 'MouseoutTag         = Dom.MouseEvent
  EventType 'MouseoverTag        = Dom.MouseEvent
  EventType 'MouseupTag          = Dom.MouseEvent
  EventType 'MousewheelTag       = Dom.MouseEvent
  EventType 'ScrollTag           = Dom.UIEvent
  EventType 'SelectTag           = Dom.UIEvent
  EventType 'SubmitTag           = Dom.Event
  EventType 'WheelTag            = Dom.WheelEvent
  EventType 'BeforecutTag        = Dom.Event
  EventType 'CutTag              = Dom.Event
  EventType 'BeforecopyTag       = Dom.Event
  EventType 'CopyTag             = Dom.Event
  EventType 'BeforepasteTag      = Dom.Event
  EventType 'PasteTag            = Dom.Event
  EventType 'ResetTag            = Dom.Event
  EventType 'SearchTag           = Dom.Event
  EventType 'SelectstartTag      = Dom.Event
  EventType 'TouchstartTag       = Dom.TouchEvent
  EventType 'TouchmoveTag        = Dom.TouchEvent
  EventType 'TouchendTag         = Dom.TouchEvent
  EventType 'TouchcancelTag      = Dom.TouchEvent

onEventName :: (Dom.IsElement e) => EventName en -> e -> Dom.EventM e (EventType en) () -> IO (IO ())
onEventName en e = case en of
    Abort       -> Dom.on e Dom.abort
    Blur        -> Dom.on e Dom.blurEvent
    Change      -> Dom.on e Dom.change
    Click       -> Dom.on e Dom.click
    Contextmenu -> Dom.on e Dom.contextMenu
    Dblclick    -> Dom.on e Dom.dblClick
    Drag        -> Dom.on e Dom.drag
    Dragend     -> Dom.on e Dom.dragEnd
    Dragenter   -> Dom.on e Dom.dragEnter
    Dragleave   -> Dom.on e Dom.dragLeave
    Dragover    -> Dom.on e Dom.dragOver
    Dragstart   -> Dom.on e Dom.dragStart
    Drop        -> Dom.on e Dom.drop
    Error       -> Dom.on e Dom.error
    Focus       -> Dom.on e Dom.focusEvent
    Input       -> Dom.on e Dom.input
    Invalid     -> Dom.on e Dom.invalid
    Keydown     -> Dom.on e Dom.keyDown
    Keypress    -> Dom.on e Dom.keyPress
    Keyup       -> Dom.on e Dom.keyUp
    Load        -> Dom.on e Dom.load
    Mousedown   -> Dom.on e Dom.mouseDown
    Mouseenter  -> Dom.on e Dom.mouseEnter
    Mouseleave  -> Dom.on e Dom.mouseLeave
    Mousemove   -> Dom.on e Dom.mouseMove
    Mouseout    -> Dom.on e Dom.mouseOut
    Mouseover   -> Dom.on e Dom.mouseOver
    Mouseup     -> Dom.on e Dom.mouseUp
    Mousewheel  -> Dom.on e Dom.mouseWheel
    Scroll      -> Dom.on e Dom.scroll
    Select      -> Dom.on e Dom.select
    Submit      -> Dom.on e Dom.submit
    Wheel       -> Dom.on e Dom.wheel
    Beforecut   -> Dom.on e Dom.beforeCut
    Cut         -> Dom.on e Dom.cut
    Beforecopy  -> Dom.on e Dom.beforeCopy
    Copy        -> Dom.on e Dom.copy
    Beforepaste -> Dom.on e Dom.beforePaste
    Paste       -> Dom.on e Dom.paste
    Reset       -> Dom.on e Dom.reset
    Search      -> Dom.on e Dom.search
    Selectstart -> Dom.on e Dom.selectStart
    Touchstart  -> Dom.on e Dom.touchStart
    Touchmove   -> Dom.on e Dom.touchMove
    Touchend    -> Dom.on e Dom.touchEnd
    Touchcancel -> Dom.on e Dom.touchCancel

newtype EventResult en = EventResult { unEventResult :: EventResultType en }

type family EventResultType (en :: EventTag) :: * where
  EventResultType 'ClickTag       = ()
  EventResultType 'DblclickTag    = ()
  EventResultType 'KeypressTag    = Int
  EventResultType 'KeydownTag     = Int
  EventResultType 'KeyupTag       = Int
  EventResultType 'ScrollTag      = Int
  EventResultType 'MousemoveTag   = (Int, Int)
  EventResultType 'MousedownTag   = (Int, Int)
  EventResultType 'MouseupTag     = (Int, Int)
  EventResultType 'MouseenterTag  = ()
  EventResultType 'MouseleaveTag  = ()
  EventResultType 'FocusTag       = ()
  EventResultType 'BlurTag        = ()
  EventResultType 'ChangeTag      = ()
  EventResultType 'DragTag        = ()
  EventResultType 'DragendTag     = ()
  EventResultType 'DragenterTag   = ()
  EventResultType 'DragleaveTag   = ()
  EventResultType 'DragoverTag    = ()
  EventResultType 'DragstartTag   = ()
  EventResultType 'DropTag        = ()
  EventResultType 'AbortTag       = ()
  EventResultType 'ContextmenuTag = ()
  EventResultType 'ErrorTag       = ()
  EventResultType 'InputTag       = ()
  EventResultType 'InvalidTag     = ()
  EventResultType 'LoadTag        = ()
  EventResultType 'MouseoutTag    = ()
  EventResultType 'MouseoverTag   = ()
  EventResultType 'SelectTag      = ()
  EventResultType 'SubmitTag      = ()
  EventResultType 'BeforecutTag   = ()
  EventResultType 'CutTag         = ()
  EventResultType 'BeforecopyTag  = ()
  EventResultType 'CopyTag        = ()
  EventResultType 'BeforepasteTag = ()
  EventResultType 'PasteTag       = ()
  EventResultType 'ResetTag       = ()
  EventResultType 'SearchTag      = ()
  EventResultType 'SelectstartTag = ()
  EventResultType 'TouchstartTag  = ()
  EventResultType 'TouchmoveTag   = ()
  EventResultType 'TouchendTag    = ()
  EventResultType 'TouchcancelTag = ()
  EventResultType 'MousewheelTag  = ()
  EventResultType 'WheelTag       = ()


getKeyEvent :: Dom.EventM e Dom.KeyboardEvent Int
getKeyEvent = do
  e <- Dom.event
  which <- Dom.getWhich e
  if which /= 0
     then return which
     else do
       charCode <- Dom.getCharCode e
       if charCode /= 0
         then return charCode
         else Dom.getKeyCode e


defaultDomEventHandler :: Dom.IsElement e => e -> EventName en -> Dom.EventM e (EventType en) (Maybe (EventResult en))
defaultDomEventHandler e evt = Just . EventResult <$> case evt of
  Click       -> return ()
  Dblclick    -> return ()
  Keypress    -> getKeyEvent
  Scroll      -> Dom.getScrollTop e
  Keydown     -> getKeyEvent
  Keyup       -> getKeyEvent
  Mousemove   -> Dom.mouseXY
  Mouseup     -> Dom.mouseXY
  Mousedown   -> Dom.mouseXY
  Mouseenter  -> return ()
  Mouseleave  -> return ()
  Focus       -> return ()
  Blur        -> return ()
  Change      -> return ()
  Drag        -> return ()
  Dragend     -> return ()
  Dragenter   -> return ()
  Dragleave   -> return ()
  Dragover    -> return ()
  Dragstart   -> return ()
  Drop        -> return ()
  Abort       -> return ()
  Contextmenu -> return ()
  Error       -> return ()
  Input       -> return ()
  Invalid     -> return ()
  Load        -> return ()
  Mouseout    -> return ()
  Mouseover   -> return ()
  Select      -> return ()
  Submit      -> return ()
  Beforecut   -> return ()
  Cut         -> return ()
  Beforecopy  -> return ()
  Copy        -> return ()
  Beforepaste -> return ()
  Paste       -> return ()
  Reset       -> return ()
  Search      -> return ()
  Selectstart -> return ()
  Touchstart  -> return ()
  Touchmove   -> return ()
  Touchend    -> return ()
  Touchcancel -> return ()
  Mousewheel  -> return ()
  Wheel       -> return ()


deriveGEq ''EventName
deriveGCompare ''EventName
