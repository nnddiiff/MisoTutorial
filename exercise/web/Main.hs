{-# language NamedFieldPuns    #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards   #-}
{-# language OverloadedLists   #-}

module Main where

import Data.Foldable (asum)
import Data.List (transpose, (!!))
import Data.Map ()
import Data.Maybe (isJust, isNothing)

import Miso
import Miso.String (MisoString, toMisoString)

main :: IO ()
main = startApp App { .. }
  where
    initialAction = None
    model         = Model { grid = emptyGrid, player = X, winner = Nothing
                          , player1 = fst playerNames, player2 = snd playerNames
                          , firstTime = True
                          }
    update        = updateModel
    view          = viewModel
    events        = defaultEvents
    subs          = []
    mountPoint    = Nothing
    logLevel      = Off

iff :: Bool -> a -> a -> a
iff True   tr _  = tr
iff _   _  fl    = fl

playerNames = ( "Player 1", "Player 2" )

onIndex :: Int -> (a -> a) -> ([a] -> [a])
onIndex n f = if n < 0 then id else loop n where
    loop _ [] = []
    loop 0 (x:xs) = f x:xs
    loop n (x:xs) = x:loop (n-1) xs

replaceAtMatrix :: (Int, Int) -> a -> [[a]] -> [[a]]
replaceAtMatrix (m, n) = onIndex m . onIndex n . const

gameFinished :: Model -> Bool
gameFinished Model { .. } = isFinished grid || isJust winner
             where 
              isFinished :: Grid -> Bool  
              isFinished = isJust . sequenceA . concat

allGridNothing :: Model -> Bool
allGridNothing Model { .. } = isAllNothing grid
                where
                  isAllNothing grid = all (\item -> isNothing item) $ concat grid

data Square
  = X | O
  deriving (Show, Eq)

type Grid = [[Maybe Square]]

emptyGrid, aGrid :: Grid
emptyGrid = replicate 3 (replicate 3 Nothing)
aGrid = [ [ Just X,  Nothing, Nothing ]
        , [ Nothing, Just O,  Nothing ]
        , replicate 3 Nothing ]

hasWinner :: Grid -> Maybe Square
hasWinner g
  = asum (map isWinnerRow thingToCheck)
  where
    thingToCheck
      = g ++ transpose g 
          ++ [ [g !! 0 !! 0, g !! 1 !! 1, g !! 2 !! 2]
             , [g !! 0 !! 2, g !! 1 !! 1, g !! 2 !! 0] ]
    isWinnerRow :: [Maybe Square] -> Maybe Square
    isWinnerRow row
      | all isJust row, all (== head row) row = head row
      | otherwise                             = Nothing

data Model
  = Model { grid :: Grid, player :: Square, winner :: Maybe Square
          , player1 :: MisoString, player2 :: MisoString, firstTime :: Bool }
  deriving (Show, Eq)


data Action
  = None
  | ClickSquare Int Int
  | NewGame
  | ChangeName Square MisoString
  deriving (Show, Eq)

playing (Model { .. }) = not $ isNothing winner

updateModel :: Action -> Model -> Effect Action Model
updateModel None model  = noEff model
updateModel (ClickSquare rowId colId) (Model { .. })
    = let replace = isNothing ((grid !! rowId) !! colId)
          grid'   = iff replace (replaceAtMatrix (rowId, colId) (Just player) grid) grid
          player' = iff (player == X) O X
          winner'  = hasWinner grid'
      in noEff (Model { grid=grid', player=player', winner=winner', firstTime = False, .. })
updateModel NewGame (Model { .. }) = noEff $ Model {grid = emptyGrid, player = X, winner = Nothing, firstTime = True, .. } 
updateModel (ChangeName square name) Model { .. } = noEff $ iff (square == X)
                                     (Model { player1 = name, .. }) (Model{ player2 = name, .. })

bootstrapUrl :: MisoString
bootstrapUrl = "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css"

viewModel :: Model -> View Action
viewModel model
  = div_ [ class_ "container"]
         [ headerView
         , newGameView model
         , contentView model
         -- , statsView m
         , link_ [ rel_ "stylesheet"
                 , href_ bootstrapUrl ] ]

headerView :: View Action
headerView
  = nav_ [ class_ "navbar navbar-dark bg-dark"]
         [ h2_ [ class_ "bd-title text-light" ]
               [ text "Tic-Tac-Toe "
               , span_ [ class_ "badge badge-warning"]
                       [ text "in miso!"] ] ]

newGameView :: Model -> View Action
newGameView model@Model { .. }
  = let
      gameIsOn = not $ gameFinished model
      inputButtonsFirstTime = not $ allGridNothing model
    in
      nav_ [ class_ "navbar navbar-light bg-light"]
           [ form_  [ class_ "form-inline" ]
                    [ input_  [ class_       "form-control mr-sm-2"
                              , type_        "text" 
                              , value_        player1
                              , onChange  (ChangeName X)
                              , placeholder_ $ fst playerNames
                              , disabled_  inputButtonsFirstTime]
                    -- , select_ [ class_       "custom-select"
                    --           , style_ [("margin-right", "15px")] ]
                    --           (flip map ["A", "B"] $ \option ->
                    --              option_ [ ] [ text option])
                    , input_  [ class_       "form-control mr-sm-2"
                              , type_        "text" 
                              , value_        player2
                              , onChange  (ChangeName O)
                              , placeholder_ $ snd playerNames
                              , disabled_  inputButtonsFirstTime ]
                    -- , select_ [ class_       "custom-select"
                    --           , style_ [("margin-right", "15px")] ]
                    --           (flip map ["A", "B"] $ \option ->
                    --              option_ [ ] [ text option])
                    , button_ [ class_       "btn btn-outline-warning"
                              , type_        "button"
                              , onClick (NewGame)
                              , disabled_ gameIsOn]
                              [ text "New game" ] ] ] 

contentView :: Model -> View Action
contentView model@Model { .. }
  = div_ [ style_ [("margin", "20px")]]
         [ gridView model
         , alertView $ toMisoString $ whoWin winner]
          where
            whoWin = maybe "" (\square -> "We have a winner! " <> iff (square == X) player1 player2 <> " !")

gridView :: Model -> View Action
gridView Model { .. }
  = div_ [ style_ [("margin", "20px")]]
         [ div_ [ class_ "row justify-content-around align-items-center" ]
                [ h3_ [iff (player == X) (class_ "text-white bg-dark") (class_ "text-secondary")] [ text player1 ]
                , div_ [ style_ [("display", "inline-block")] ]
                       [fieldset_ (iff (isJust winner) [stringProp "disabled" "disabled"] [stringProp "enabled" "enabled"] )
                       [ div_ [ style_ [ ("display", "grid")
                                       , ("grid-template-rows", "1fr 1fr 1fr")
                                       , ("grid-template-columns", "1fr 1fr 1fr")
                                       , ("grid-gap", "2px") ] ]
                               ( flip concatMap (zip [0 ..] grid) $ \(rowId, row) ->
                                   flip map (zip [0 ..] row) $ \(colId, sq) ->
                                     cell rowId colId sq )]]
                , h3_ [iff (player == O) (class_ "text-white bg-dark") (class_ "text-secondary")] [ text player2] ] ]
  where
    cell :: Int -> Int -> Maybe Square -> View Action
    cell rowId colId square
      = div_ [ style_ [("width", "100px"), ("height", "100px")] ]
             [ button_ [ type_  "button"
                       , style_  [ ("width", "100%"), ("height", "100%")
                                 , ("font-size", "xxx-large") ]
                       , class_  "btn btn-outline-secondary"
                       , onClick (ClickSquare rowId colId) 
                       , iff (isJust square) (disabled_ True) (disabled_ False)]
                       [ showText square] ]

    showText :: Maybe Square -> View Action
    showText Nothing = text "."
    showText (Just square) = text $ toMisoString $ show square

alertView :: MisoString -> View Action
alertView v
  = div_ [ class_ "alert alert-warning"
         , style_ [("text-align", "center")] ]
         [ h4_ [ class_ "alert-heading" ]
               [ text v ] ]

fakeStats :: [MisoString]
fakeStats
  = [ "A - B, won by A in 3 moves"
    , "Quijote - Sancho, won by Sancho in 5 moves" ]

statsView :: Model -> View Action
statsView _
  = div_ [ class_ "row justify-content-around align-items-center"
         , style_ [("margin-bottom", "20px")] ]
         [ ul_ [ class_ "list-group"]
               ( flip map fakeStats $ \elt ->
                   ul_ [ class_ "list-group-item" ] [ text elt ] ) ]