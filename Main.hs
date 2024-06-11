-- Основной файл проекта содержащий все функции логики и основную функцию отображения на UI

module Main where

import Data.Char
import Data.IORef
import GHC.Float.RealFracMethods
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game

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
    rotateAngle :: Int -- поворот шаблона при появлении
  }
  deriving (Show)

boardCC :: Board -> [Pos] -> Board -- Конструктор копирования, также позволяющий изменить координаты клеток
boardCC brd crd =
  Board
    { flag = flag brd,
      coords = crd,
      mv = mv brd,
      zoom = zoom brd,
      rmb = rmb brd,
      lastCoords = lastCoords brd,
      rotateAngle = rotateAngle brd
    }

boardCRMB :: Board -> Bool -> (Float, Float) -> Board -- Конструктор копирования, также позволяющий изменить сдвиг поля с использованием ПКМ
boardCRMB brd lm lc =
  Board
    { flag = flag brd,
      coords = coords brd,
      mv = mv brd,
      zoom = zoom brd,
      rmb = lm,
      lastCoords = lc,
      rotateAngle = rotateAngle brd
    }

boardCF :: Board -> Bool -> Board -- Конструктор копирования, также позволяющий установить флаг паузы
boardCF brd flg =
  Board
    { flag = flg,
      coords = coords brd,
      mv = mv brd,
      zoom = zoom brd,
      rmb = rmb brd,
      lastCoords = lastCoords brd,
      rotateAngle = rotateAngle brd
    }

boardCDrag :: Board -> (Float, Float) -> Board -- Конструктор копирования, также позволяющий сдвинуть поле
boardCDrag brd (x, y) =
  Board
    { flag = flag brd,
      coords = coords brd,
      mv = ((fst (mv brd)) + ((x - (fst (lastCoords brd))) / zoom brd), (snd (mv brd)) + ((y - snd (lastCoords brd)) / zoom brd)),
      zoom = zoom brd,
      rmb = rmb brd,
      lastCoords = (x, y),
      rotateAngle = rotateAngle brd
    }

boardCArrow :: Board -> (Float, Float) -> Board -- Конструктор копирования, также позволяющий сдвинуть поле
boardCArrow brd (x, y) =
  Board
    { flag = flag brd,
      coords = coords brd,
      mv = ((fst (mv brd)) + x, (snd (mv brd)) + y),
      zoom = zoom brd,
      rmb = rmb brd,
      lastCoords = (x, y),
      rotateAngle = rotateAngle brd
    }

boardCZ :: Board -> Bool -> Board -- Конструктор копирования, также позволяющий изменить зум
boardCZ brd a =
  Board
    { flag = flag brd,
      coords = coords brd,
      mv = mv brd,
      zoom =
        if a
          then zoom brd * 1.1
          else zoom brd / 1.1,
      rmb = rmb brd,
      lastCoords = lastCoords brd,
      rotateAngle = rotateAngle brd
    }

boardCR :: Board -> Board -- Конструктор копирования, также позволяющий изменить поворот шаблонов
boardCR brd =
  Board
    { flag = flag brd,
      coords = coords brd,
      mv = mv brd,
      zoom = zoom brd,
      rmb = rmb brd,
      lastCoords = lastCoords brd,
      rotateAngle = mod (rotateAngle brd + 1) 4
    }

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
      rotateAngle = 0
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

render brd = scale (zoom brd) (zoom brd) (pictures $ map (toPicture . rescale) (coords brd)) -- Функция отрисовки доски
  where
    rescale (x, y) = (fromIntegral (x * 10), fromIntegral (y * 10))
    toPicture (x, y) = translate (x + fst (mv brd)) (y + snd (mv brd)) $ rectangleSolid 10 10

handleKeyPress :: Event -> Board -> Board -- Функция обработки нажатий
handleKeyPress (EventKey (SpecialKey KeySpace) Down _ _) brd = boardCF brd (not (flag brd)) -- установка паузы
handleKeyPress (EventKey (MouseButton LeftButton) Down _ (xc, yc)) brd = addElementsToBoard brd pos [(0, 0)] -- нарисовать ячейку
  where
    pos = calcCoords brd (xc, yc)
handleKeyPress (EventKey (MouseButton RightButton) Down _ (xc, yc)) brd = boardCRMB brd True (xc, yc) -- начать драг
handleKeyPress (EventKey (MouseButton RightButton) Up _ (xc, yc)) brd = boardCRMB brd False (xc, yc) -- закончить драг
handleKeyPress (EventKey (SpecialKey KeyUp) Down _ _) brd = boardCArrow brd (0.0, -10) -- вверх
handleKeyPress (EventKey (SpecialKey KeyDown) Down _ _) brd = boardCArrow brd (0.0, 10) -- вниз
handleKeyPress (EventKey (SpecialKey KeyLeft) Down _ _) brd = boardCArrow brd (-10, 0.0) -- влево
handleKeyPress (EventKey (SpecialKey KeyRight) Down _ _) brd = boardCArrow brd (10, 0.0) -- вправо
handleKeyPress (EventKey (MouseButton WheelUp) _ _ _) brd = boardCZ brd True -- zoom in
handleKeyPress (EventKey (MouseButton WheelDown) _ _ _) brd = boardCZ brd False -- zoom out
handleKeyPress (EventKey (SpecialKey KeyPageUp) _ _ _) brd = boardCZ brd True -- zoom in
handleKeyPress (EventKey (SpecialKey KeyPageDown) _ _ _) brd = boardCZ brd False -- zoom out
handleKeyPress (EventMotion (x, y)) brd =
  if rmb brd -- драг
    then boardCDrag brd (x, y)
    else brd
handleKeyPress (EventKey (Char a) Down _ (xc, yc)) brd =  -- нарисовать ячейку
  if ind >= 0 && ind < (templLength allDraws)
    then addElementsToBoard brd pos (allDraws !! ind)
    else
      if a == 'r'
        then boardCR brd
        else if a =='c'
          then boardCC brd []
          else brd
  where
    pos = calcCoords brd (xc, yc)
    ind = ord a - ord '0' - 1
handleKeyPress _ brd = brd -- Ignore other events

main = play window background fps example render handleKeyPress update -- Главная функция GUI
  where
    window = InWindow "Haskell Conway's Game of Life" (800, 600) (50, 50)
    background = white
    fps = speed


-- Шаблоны
dart :: [Pos]
dart = [(0, 0), (1, 1), (-1, 1), (2, 2), (-2, 2), (0, 3), (1, 3), (-1, 3), (2, 5), (-2, 5), (3, 5), (-3, 5), (1, 6), (-1, 6), (5, 6), (-5, 6), (1, 7), (-1, 7), (5, 7), (-5, 7), (6, 7), (-6, 7), (1, 8), (-1, 8), (7, 8), (-7, 8), (1, 9), (-1, 9), (3, 9), (-3, 9), (4, 9), (-4, 9), (-6, 9), (6, 9)]

clock :: [Pos]
clock = [(0, 0), (1, 1), (2, 1), (0, 2), (-1, 2), (1, 3)]

arrow :: [Pos]
arrow = [(0, 0), (1, 1), (-1, 1), (0, 2), (0, 3), (2, 3), (3, 3), (-2, 3), (-3, 3), (0, 4), (0, 5), (2, 5), (-2, 5), (0, 6)]

gospelGun :: [Pos]
gospelGun = [(0, 0), (-1, 0), (-1, 1), (-2, 2), (-1, -1), (-2, -2), (-3, 0), (-4, -3), (-4, 3), (-5, -3), (-5, 3), (-6, -2), (-6, 2), (-7, -1), (-7, 1), (-7, 0), (-16, 0), (-16, -1), (-17, 0), (-17, -1), (3, -1), (3, -2), (3, -3), (4, -1), (4, -2), (4, -3), (5, -4), (5, 0), (7, -4), (7, 0), (7, -5), (7, 1), (17, -2), (17, -3), (18, -2), (18, -3)]

glider :: [Pos]
glider = [(0, 0), (0, 1), (0, 2), (1, 2), (2, 1)]

allDraws :: [[Pos]] -- все шаблоны
allDraws = [glider, dart, clock, arrow, gospelGun]

templLength :: [[Pos]] -> Int -- функция рассчета количества шаблонов 
templLength xs = foldr (\x -> (+) 1) 0 xs