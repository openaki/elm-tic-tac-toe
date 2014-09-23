module Main where

import Window
import Array as A
import Random as R
import Graphics.Input (..)
import Maybe (isJust)

type GameState = {
  ind : Int,
  dim : (Int, Int),
  emptyElem : Int -> Element,
  winningLine : Maybe ([Int], WiningDirection),
  board: A.Array Int
}

initState : GameState
initState = {
  ind = 1,
  dim = (500,500),
  emptyElem = clickableElem,
  winningLine = Nothing,
  board = A.fromList [0,0,0,
                      0,0,0,
                      0,0,0]
  }

type Index = Int
clickInput : Input Index
clickInput = input -1

main = scaleSpace 
          <~ Window.dimensions
           ~ (foldp step initState 
                <| lift2 (,) clickInput.signal <|
                             R.range 0 8 (clickInput.signal))

step : (Int, Int) -> GameState -> GameState
step (n, rnd) st =
  let newSt = {st | board <- A.set n st.ind st.board,
                    ind <- st.ind * -1} in
  let blankElem _ = spacer 100 100 in
  case hasWon newSt.board st.ind of
  Just winner -> {newSt | emptyElem <- blankElem, winningLine <- Just winner}
  _ -> playAIIfNeeded newSt rnd

possibleMoves : A.Array Int -> [Int]
possibleMoves lst =
   let unUsedFn = (\(ind, val) -> if val == 0 then True else False) in
   map fst <| filter unUsedFn <| A.toIndexedList lst

aiLevel1 : A.Array Int -> [Int] -> Int -> Int
aiLevel1 brd mvs rnd =
  let id = rem rnd <| length mvs in
  let movesA = A.fromList mvs in
  A.getOrFail id movesA

aiLevel2 : A.Array Int -> [Int] -> Int -> Int
aiLevel2 brd mvs rnd =
  let winFn pl x = if isJust (hasWon (A.set x pl brd) pl)
                   then Just x
                   else Nothing  in
  let fn pl =  filterMap identity <| map (winFn pl) mvs in
  let wins = fn -1 in
  let stopLoss = fn 1 in
  case isEmpty wins of
    False -> head wins
    _ -> case isEmpty stopLoss of
           False -> head stopLoss
           _ -> aiLevel1 brd mvs rnd

playAIIfNeeded st rnd =
 let brd = st.board in
 let pMoves = possibleMoves brd in
 case st.ind of
  -1 -> case isEmpty pMoves of
      False -> step (aiLevel2 brd pMoves rnd, rnd) st
      _ -> st
  _ -> st 

linePath = segment (-150,0) (150,0)
lineForm = traced defaultLine linePath

singleCircle = filled red <| circle 30
singleSquare = filled green <| square 50
singleCross = rotate (degrees 45) <| 
  traced {defaultLine | color<-blue, width<-5.0} <| 
    path [(-30,0), (30,0), (0,0), (0,30), (0,-30)]

clickableElem n = 
  let t f = collage 100 100 [f] in
  opacity 0.2 <| clickable clickInput.handle n (t singleSquare)

data WiningDirection = H | V | C45 | C135
winingCombs = [([0,1,2], H), ([3,4,5], H), ([6,7,8], H), -- The horizontal
               ([0,3,6], V), ([1,4,7], V), ([2,5,8], V), -- The verticals
               ([0,4,8], C135), ([2,4,6], C45)] -- The cross

hasWon arr ind =
  let isOneCombinationSet arr ind (comb, d) =
     all (\x -> ind == A.getOrFail x arr) comb in
  foldl (\x acc -> if isOneCombinationSet arr ind x
                     then (Just x)
                     else acc) Nothing winingCombs 

-- Display the game state

scaleSpace : (Int, Int) -> GameState -> Element
scaleSpace (x,y) st = 
  let minV = min x y in
  let calcScaleFactor x = x / 400 in
  let brdGraphics = 
    scale (calcScaleFactor (toFloat minV)) <| toForm <| createSpace st in
  collage minV minV <| [brdGraphics] 

drawCrossOrCircle st n =
  --let x = toFloat <| div n 3 in
  --let y = toFloat <| rem n 3 in
  let x = 0 in
  let y = 0 in
  let t f = collage 100 100 <| [f]  ++ winLine st n in
  case A.getOrFail n st.board of
    -1 -> t singleCross
    1 -> t singleCircle
    _  -> st.emptyElem n

winLine st n =
  let wl = traced defaultLine <| segment (-50,50) (50,-50) in
  let hl = rotate (degrees 45) wl in
  let rotatedLine n lst deg = 
    case any (\x -> x == n) lst of
       False -> []
       True -> [rotate (degrees deg) hl] in
  case st.winningLine of
   Nothing -> []
   Just (lst, H) -> rotatedLine n lst 0
   Just (lst, V) -> rotatedLine n lst 90
   Just (lst, C45) -> rotatedLine n lst 45
   Just (lst, C135) -> rotatedLine n lst 135


moveOnIndex : Float -> Float -> Form -> Form
moveOnIndex x y f = move ((-100 + (100*y)), (100 - (100*x))) f

drawBoard =
  [
    traced lineStyle <| segment (-150,50) (150,50),
    traced lineStyle <| segment (-150,-50) (150,-50),

    traced lineStyle <| segment (50,150) (50,-150),
    traced lineStyle <| segment (-50,150) (-50,-150)
  ]
  
lineStyle = {defaultLine | width <-20, cap <- Round}

createSpace st =
  let playNum id = case id of
                  -1 -> "2"
                  _ -> "1" in
  let d = drawCrossOrCircle st in
  let showMove = case st.winningLine of
     Nothing -> toText <| "Move: Player " ++ (playNum st.ind)
     Just _ -> toText <| "Winner: Player " ++ (playNum (st.ind * -1)) in
  flow down <| (centered showMove) :: [
              flow right <| [d 0, vSpacer, d 1, vSpacer, d 2],
              horizontalSpacer,
              flow right <| [d 3, vSpacer, d 4, vSpacer, d 5],
              horizontalSpacer,
              flow right <| [d 6, vSpacer, d 7, vSpacer, d 8]
            ]

vSpacer = collage 10 100 [traced lineStyle <| segment (0,-50) (0,50)]
horizontalSpacer
  = collage 320 10 [traced lineStyle <| segment (-160,0) (160,0)]

displayGameState st = 
  let allChoices = [0..8] in
  map (drawCrossOrCircle st) allChoices
