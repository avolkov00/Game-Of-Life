-- Основной файл проекта содержащий все функции логики и основную функцию отображения на UI

module Main where

import Data.Char
import Data.List
import Data.Maybe
import GHC.Float.RealFracMethods
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Game
import System.IO

speed :: Int
speed = 5

type Pos = (Int, Int)

data Board = Board -- Основная структура описания поля, передающаяся во все функции
  { flag :: Bool, -- флаг установки паузы
    coords :: [Pos], -- массив координат закрашенных клеток
    mv :: (Float, Float), -- свиг системы координат
    zoom :: Float, -- зум
    rmb :: Bool, -- флаг зажатия пкм
    lastCoords :: (Float, Float), -- последние координаты курсова для drag-а
    rotateAngle :: Int, -- поворот шаблона при появлении
    selectedPattern :: Maybe [Pos] -- выбранный шаблон
  }
  deriving (Show)

boardCC :: Board -> [Pos] -> Board -- Конструктор копирования, также позволяющий изменить координаты клеток
boardCC brd crd =
  brd {coords = crd}

boardCRMB :: Board -> Bool -> (Float, Float) -> Board -- Конструктор копирования, также позволяющий изменить сдвиг поля с использованием ПКМ
boardCRMB brd lm lc =
  brd {rmb = lm, lastCoords = lc}

boardCF :: Board -> Bool -> Board -- Конструктор копирования, также позволяющий установить флаг паузы
boardCF brd flg =
  brd {flag = flg}

boardCDrag :: Board -> (Float, Float) -> Board -- Конструктор копирования, также позволяющий сдвинуть поле
boardCDrag brd (x, y) =
  brd {mv = ((fst (mv brd)) + ((x - (fst (lastCoords brd))) / zoom brd), (snd (mv brd)) + ((y - snd (lastCoords brd)) / zoom brd)), lastCoords = (x, y)}

boardCArrow :: Board -> (Float, Float) -> Board -- Конструктор копирования, также позволяющий сдвинуть поле
boardCArrow brd (x, y) =
  brd {mv = ((fst (mv brd)) + x, (snd (mv brd)) + y), lastCoords = (x, y)}

boardCZ :: Board -> Bool -> Board -- Конструктор копирования, также позволяющий изменить зум
boardCZ brd a =
  brd {zoom = if a then zoom brd * 1.1 else zoom brd / 1.1}

boardCR :: Board -> Board -- Конструктор копирования, также позволяющий изменить поворот шаблонов
boardCR brd =
  brd {rotateAngle = mod (rotateAngle brd + 1) 4}

rotateC :: Pos -> Pos -- Функция поворота клетки на 90 градусов
rotateC (x, y) = (y, (-1) * x)

rotateTimes :: Pos -> Int -> Pos -- Функция поворота клетки на n*90 градусов
rotateTimes (x, y) n =
  if n == 0
    then (x, y)
    else rotateTimes (rotateC (x, y)) (n - 1)

example :: Board -- Пример доски по умолчанию
example =
  Board
    { flag = True,
      coords = [(1, 1), (1, 2), (1, 3)],
      mv = (1.0, 1.0),
      zoom = 1,
      rmb = False,
      lastCoords = (0, 0),
      rotateAngle = 0,
      selectedPattern = Nothing
    }

isAlive :: Board -> Pos -> Bool -- Проверка находится ли клетка на доске
isAlive b p = p `elem` coords b

isEmpty :: Board -> Pos -> Bool -- Проверка отсутствует ли клетка на доске
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos] -- Список координат всех соседей клетки
neighbs (x, y) =
  [ (x - 1, y - 1),
    (x, y - 1),
    (x + 1, y - 1),
    (x - 1, y),
    (x + 1, y),
    (x - 1, y + 1),
    (x, y + 1),
    (x + 1, y + 1)
  ]

liveneighbs :: Board -> Pos -> Int -- Количество живых соседей клетки
liveneighbs brd = length . filter (isAlive brd) . neighbs

survivors :: Board -> [Pos] -- Координаты выживших клеток
survivors brd = [p | p <- coords brd, liveneighbs brd p `elem` [2, 3]]

births :: Board -> [Pos] -- Координаты новых клеток
births brd =
  [ p | p <- rmdups (concatMap neighbs (coords brd)), isEmpty brd p, liveneighbs brd p == 3
  ]

rmdups :: (Eq a) => [a] -> [a] -- Удаление дубликатов из списка
rmdups [] = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

nextgen :: Board -> Board -- Новая доска, на основе предыдущей
nextgen brd = boardCC brd (survivors brd ++ births brd)

addElementToBoard :: Board -> Pos -> Board -- Добавление клетки на доску
addElementToBoard brd pos =
  if pos `elem` coords brd
    then brd
    else boardCC brd (pos : coords brd) -- Добавление клетки

addElementsToBoard :: Board -> Pos -> [Pos] -> Board -- Добавление шаблона на доску
addElementsToBoard brd o [] = brd
addElementsToBoard brd o (x : xs) = addElementsToBoard (addElementToBoard brd ((fst o) + (fst x1), (snd o) + (snd x1))) o xs
  where
    x1 = rotateTimes x (rotateAngle brd)

calcCoords :: Board -> (Float, Float) -> Pos -- Рассчитать координаты клетки на доске с учетом сдвигов и зума
calcCoords brd (xc, yc) = (x, y)
  where
    x = roundFloatInt ((xc / zoom brd - fst (mv brd)) / 10.0)
    y = roundFloatInt ((yc / zoom brd - snd (mv brd)) / 10.0)

update :: p1 -> Board -> Board -- Основная функция работы программы, в которой создаются новые поколения
update _ brd =
  if not (flag brd)
    then brd
    else nextgen brd

render :: [(String, [Pos])] -> Board -> Picture -- Функция отрисовки доски
render patterns brd = pictures ([scale (zoom brd) (zoom brd) (pictures $ map (toPicture . rescale) (coords brd))] 
  ++ patternButtons 
  ++ [translate 208 250 (scale 0.15 0.15 (text "r - rotate template"))] 
  ++ [translate 208 230 (scale 0.15 0.15 (text " spacebar - pause"))] 
  ++ [translate 208 210 (scale 0.15 0.15 (text " hold RMB - drag"))] 
  ++ [translate 213 190 (scale 0.15 0.15 (text "    LMB - draw"))] 
  ++ [translate 201 170 (scale 0.15 0.15 (text "PgUp/Down - zoom"))] 
  
  )
  where
    rescale (x, y) = (fromIntegral (x * 10), fromIntegral (y * 10))
    toPicture (x, y) = translate (x + fst (mv brd)) (y + snd (mv brd)) $ rectangleSolid 10 10
    patternButtons = zipWith drawButton [0 ..] (map fst patterns)
    drawButton i name = translate (-400) (250 - fromIntegral i * 30) $ scale 0.1 0.1 $ pictures [text name, translate (400) (100) (rectangleWire 800 300)]

handleKeyPress :: [(String, [Pos])] -> Event -> Board -> Board -- Функция обработки нажатий
handleKeyPress patterns (EventKey (SpecialKey KeySpace) Down _ _) brd = boardCF brd (not (flag brd))
handleKeyPress patterns (EventKey (MouseButton LeftButton) Down _ (xc, yc)) brd =
  case selectedPattern brd of
    Just pattern -> addElementsToBoard brd {selectedPattern = Nothing} pos pattern
    Nothing ->
      let buttonIndex = floor ((280 - yc) / 30)
       in if buttonIndex >= 0 && buttonIndex < length patterns && xc < -280
            then brd {selectedPattern = Just (snd (patterns !! buttonIndex))}
            else addElementsToBoard brd pos [(0, 0)] -- нарисовать ячейку
  where
    pos = calcCoords brd (xc, yc)
handleKeyPress patterns (EventKey (MouseButton RightButton) Down _ (xc, yc)) brd = boardCRMB brd True (xc, yc)
handleKeyPress patterns (EventKey (MouseButton RightButton) Up _ (xc, yc)) brd = boardCRMB brd False (xc, yc)
handleKeyPress patterns (EventKey (SpecialKey KeyUp) Down _ _) brd = boardCArrow brd (0.0, -10)
handleKeyPress patterns (EventKey (SpecialKey KeyDown) Down _ _) brd = boardCArrow brd (0.0, 10)
handleKeyPress patterns (EventKey (SpecialKey KeyLeft) Down _ _) brd = boardCArrow brd (10, 0.0)
handleKeyPress patterns (EventKey (SpecialKey KeyRight) Down _ _) brd = boardCArrow brd (-10, 0.0)
handleKeyPress patterns (EventKey (SpecialKey KeyPageUp) _ _ _) brd = boardCZ brd True
handleKeyPress patterns (EventKey (SpecialKey KeyPageDown) _ _ _) brd = boardCZ brd False
handleKeyPress patterns (EventMotion (x, y)) brd =
  if rmb brd
    then boardCDrag brd (x, y)
    else brd
handleKeyPress patterns (EventKey (Char a) Down _ (xc, yc)) brd =
  if a == 'r'
    then boardCR brd
    else brd
handleKeyPress _ _ brd = brd

main :: IO ()
main = do
  patterns <- loadPatterns "patterns.txt"
  play window background fps example (render patterns) (handleKeyPress patterns) update
  where
    window = InWindow "Haskell Conway's Game of Life" (800, 600) (50, 50)
    background = white
    fps = speed

loadPatterns :: FilePath -> IO [(String, [Pos])] -- Функция чтение шаблонов из файла
loadPatterns path = do
  contents <- readFile path
  return $ map parsePattern (lines contents)

parsePattern :: String -> (String, [Pos]) -- Функция парсинга шаблонов в файле
parsePattern line =
  let (name, rest) = break (== ':') line
      positions = read (drop 2 rest) :: [Pos]
   in (name, positions)