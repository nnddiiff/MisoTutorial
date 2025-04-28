{-# language NamedFieldPuns    #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards   #-}

module Main where

import System.Random
import Data.Time.LocalTime

import Miso
import Miso.String (MisoString, toMisoString, null)

main :: IO ()
main = startApp App { .. }
  where
    initialAction = None
    model         = Model initialItems ""
    update        = updateModel
    view          = viewModel
    events        = defaultEvents
    subs          = []
    mountPoint    = Nothing
    logLevel      = Off

data ListItem
  = ListItem {
      liId   :: MisoString
    , liText :: MisoString
    , liDone :: Bool
    }
  deriving (Show, Eq)

data Model 
  = Model {
       items       :: [ListItem]
     , newItemText ::  MisoString
  } deriving (Show, Eq)

initialItems :: [ListItem]
initialItems
  = [ ListItem "lunch"    "Have lunch"           True
    , ListItem "workshop" "Give a Miso workshop" False
    ]

data Action
  = None 
  | ToggleState {toggleId :: MisoString}
  | AddToDo     { newListItem :: ListItem }
  | ChangeNewItemText { newText :: MisoString }
  | AddToDoClick
  
  deriving (Show, Eq)

updateModel :: Action -> Model -> Effect Action Model
updateModel None model = noEff model
updateModel (ToggleState toggleId) model
  = let new = flip map (items model)
                (\ li@ListItem { .. } ->
                    if liId == toggleId
                      then li { liDone = not liDone }
                      else li)
    in noEff $ model { items = new }
updateModel (AddToDo newLi) model 
  = noEff ( model { items = items model <> [newLi] } )
updateModel (ChangeNewItemText newText) model
  = noEff $ model { newItemText = newText }
updateModel AddToDoClick model@Model { .. }
  = model { newItemText = "" } 
      <# do tme <- toMisoString . show <$> getZonedTime
            let liId = tme 
                liText = newItemText
                newLi = ListItem { liDone = False, ..}
            pure $ AddToDo newLi

bootstrapUrl :: MisoString
bootstrapUrl = "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css"

viewModel :: Model -> View Action
viewModel model
  = div_ [ class_ "container"]
         [ header model
         , ul_ [ class_ "list-group" ] (map viewListItem  (items model))
         , link_ [ rel_ "stylesheet"
                 , href_ bootstrapUrl ] ]

viewListItem :: ListItem -> View Action
viewListItem ListItem { .. }
  = li_ [ class_ "list-group-item"]
        [ div_  [ class_ "custom-control custom-checkbox"]
                [ input_ [ class_   "custom-control-input"
                         , type_    "checkbox"
                         , value_   "on"
                         , checked_ liDone
                         , id_      liId 
                         , onChange (\_ -> ToggleState liId)]
                , label_ [ class_ (if liDone
                                      then "custom-control-label text-muted"
                                      else "custom-control-label")
                         , for_   liId ]
                         [ text liText ]] ]
 
header :: Model -> View Action
header Model { .. }
  = nav_ [ class_ "navbar navbar-dark bg-dark"]
         [ h2_ [ class_ "bd-title text-light" ]
               [ text "To-do "
               , span_ [ class_ "badge badge-warning"]
                       [ text "in miso!"] ]
         , form_ [ class_ "form-inline" ]
                 [ input_  [ class_       "form-control mr-sm-2"
                           , type_        "text" 
                           , placeholder_ "Do this" 
                           , value_       newItemText 
                           , onChange     ChangeNewItemText ]
                 , button_ [ class_   "btn btn-outline-warning"
                           , type_    "button"
                           , onClick  AddToDoClick
                           , disabled_ (Miso.String.null newItemText) ]
                           [ text "New to-do" ] ] ]
