module Builder.Html
  ( module Builder.Html
  , module Builder.Html.Attributes
  , module Builder.Html.Elements
  , module Builder.Element
  , module Builder.Attribute

  ) where

import Builder.Html.Attributes
import Builder.Html.Elements hiding (style_, cite_, content_, form_, label_, summary_, title_, dir_, span_, main)

import Builder.Element
import Builder.Attribute

import Reflex.Dom hiding (selectInput, textInput)

selectElem :: forall t m a. (DomBuilder t m, PostBuild t m) 
           => [Property t]
           ->  SelectElementConfig EventResult t (DomBuilderSpace m) 
           -> m a 
           -> m (SelectElement EventResult (DomBuilderSpace m) t, a)
selectElem props config child = do
  
  elemConfig <- configure Nothing props
  selectElement (config & selectElementConfig_elementConfig .~ elemConfig) child
  
selectElem_ :: forall t m a. (DomBuilder t m, PostBuild t m) 
             => [Property t]
             ->  SelectElementConfig EventResult t (DomBuilderSpace m) 
             -> m a
             -> m (SelectElement EventResult (DomBuilderSpace m) t)
selectElem_ props config child = fst <$> selectElem props config child
  
  
inputElem :: forall t m a. (DomBuilder t m, PostBuild t m) 
           => [Property t] 
           -> InputElementConfig EventResult t (DomBuilderSpace m) 
           -> m (InputElement EventResult (DomBuilderSpace m) t)
inputElem props config = do
  
  elemConfig <- configure Nothing props
  inputElement (config & inputElementConfig_elementConfig .~ elemConfig)
  