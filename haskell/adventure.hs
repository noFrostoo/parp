-- Przygody Gerwanta z Riviery, by Drygaś Filip, Lew Filip, Lipniacki Daniel.
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}

import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

-- game content

introductionText =
  [ "Gerwant z Riviery, szkoły nosacza, podróżuje szlakami Królestw Północy już wiele dni.",
    "Towarzyszy mu jedynie deszcz i jego wierny koń Piwonia."
  ]

instructionsText =
  [ "Dostępne polecenia:",
    "polecenia              -- by wyświetlić listę poleceń.",
    "idź <kierunek>         -- by pójść w danym kierunku.",
    "podnieś <przedmiot>    -- by podnieść przedmiot.",
    "upuść <przedmiot>      -- by upuścić przedmiot.",
    "ekwipunek              -- by sprawdzić ekwipunek.",
    "obejrzyj <przedmiot>   -- by obejrzeć przedmiot.",
    "rozmawiaj <npc>        -- by porozmawiać z NPC.",
    "spytaj <npc> <coś>     -- by spytać NPC o coś.",
    "rozejrzyj-się          -- by dowiedzieć się, gdzie jesteś.",
    "zakończ                -- by zakończyć grę."
  ]

type Item = String

type LocationName = String

type Path = String

type Person = String

data TimeOfDay = Day | Night
  deriving (Eq)

data Location = Location
  { people :: [Person],
    exits :: Map Path LocationName
  }

locations :: Map LocationName Location
locations =
  [ ("aaa", Location ["Wieśniak"] [("north", "bbb")])
  ]

itemPowers :: Map Item Double
itemPowers =
  [ ("Gnomski Gwyhyr", 2)
  ]

-- game state

data GameState = GameState
  { location :: String,
    inventory :: Set Item,
    itemLocations :: Map LocationName (Set Item),
    facts :: Set String,
    hp :: Int,
    enemyHp :: Int,
    timeOfDay :: TimeOfDay,
    dayTurnCounter :: Int
  }

initialState :: GameState
initialState =
  GameState
    { location = "Święty Dąb",
      inventory = Set.empty,
      itemLocations = Map.empty,
      facts = Set.empty,
      hp = 100,
      enemyHp = 100,
      timeOfDay = Day,
      dayTurnCounter = 0
    }

-- game actions

describe :: String -> String
describe "name" = "description"
describe _ = "Nie ma takiego przedmiotu"

go :: Monad m => LocationName -> Path -> StateT GameState m Bool
go src path = do
  case Map.lookup src locations >>= (Map.lookup path . exits) of
    Just loc -> do
      game <- get
      put $ game {location = loc}
      advanceDay
      return True
    Nothing -> return False

lookAround :: Monad m => StateT GameState m String
lookAround = undefined

pickUp :: Monad m => Item -> StateT GameState m Bool
pickUp item = do
  game <- get
  let items = fromMaybe Set.empty (Map.lookup (location game) (itemLocations game))
  if item `elem` items
    then do
      put $
        game
          { itemLocations = Map.insert (location game) (Set.delete item items) (itemLocations game),
            inventory = Set.insert item (inventory game)
          }
      return True
    else return False

dropItem :: Monad m => Item -> StateT GameState m Bool
dropItem item = do
  game <- get
  if item `elem` inventory game
    then do
      let items = fromMaybe Set.empty (Map.lookup (location game) (itemLocations game))
      put $
        game
          { itemLocations = Map.insert (location game) (Set.insert item items) (itemLocations game),
            inventory = Set.delete item (inventory game)
          }
      return True
    else return False

talkTo :: Monad m => Person -> StateT GameState m String
talkTo person = undefined

askPerson :: Monad m => Person -> String -> StateT GameState m String
askPerson person topic = undefined

buyBeer :: Monad m => StateT GameState m Bool
buyBeer = undefined

prepareForFight :: Monad m => String -> StateT GameState m Bool
prepareForFight = undefined

attack :: Monad m => Item -> StateT GameState m Int
attack = undefined

advanceDay :: Monad m => StateT GameState m TimeOfDay
advanceDay = do
  game <- get
  if dayTurnCounter game == 11
    then do
      let newTimeOfDay = if timeOfDay game == Day then Night else Day
      put $ game {dayTurnCounter = 0, timeOfDay = newTimeOfDay}
      return newTimeOfDay
    else return $ timeOfDay game

-- command line logic

printLines :: [String] -> IO ()
printLines xs = putStr (unlines xs)

printIntroduction = printLines introductionText

printInstructions = printLines instructionsText

readCommand :: IO [String]
readCommand = do
  putStr "> "
  fmap words getLine

checkNumArgs :: (MonadTrans t) => Int -> [String] -> t IO () -> t IO ()
checkNumArgs num args expr =
  if length args == num
    then expr
    else lift $ putStrLn $ "Niewłaściwa ilość argumentów" ++ show (length args) ++ "(oczekiwano " ++ show num ++ ")."

gameLoop :: StateT GameState IO ()
gameLoop = do
  cmd <- lift readCommand
  case cmd of
    ("polecenia" : args) -> checkNumArgs 0 args do
      lift printInstructions
      gameLoop
    ("idź" : args) -> checkNumArgs 1 args do
      game <- get
      success <- go (location game) (head args)
      if success
        then do
          desc <- lookAround
          lift $ putStrLn desc
        else lift $ putStrLn $ "Nie możesz iść w kierunku '" ++ head args ++ "'."
      gameLoop
    ("podnieś" : args) -> checkNumArgs 1 args do
      success <- pickUp $ head args
      if success
        then lift $ putStrLn $ "Podniosłeś przedmiot " ++ head args
        else lift $ putStrLn $ "Nie ma tutaj przedmiotu '" ++ head args ++ "'."
      gameLoop
    ("upuść" : args) -> checkNumArgs 1 args do
      success <- dropItem $ head args
      if success
        then lift $ putStrLn $ "Upuściłeś przedmiot " ++ head args
        else lift $ putStrLn $ "Nie posiadasz przedmiotu '" ++ head args ++ "'."
      gameLoop
    ("ekwipunek" : args) -> checkNumArgs 0 args do
      game <- get
      lift $ putStrLn $ unlines $ map describe (Set.toList $ inventory game)
      gameLoop
    ("obejrzyj" : args) -> checkNumArgs 1 args do
      lift $ putStrLn $ describe $ head args
      gameLoop
    ("rozmawiaj" : args) -> checkNumArgs 1 args do
      answer <- talkTo $ head args
      lift $ putStrLn answer
      gameLoop
    ("spytaj" : args) -> checkNumArgs 2 args do
      answer <- askPerson (head args) (args !! 1)
      lift $ putStrLn answer
      gameLoop
    ("rozejrzyj-się" : args) -> checkNumArgs 0 args do
      desc <- lookAround
      lift $ putStrLn desc
      gameLoop
    ("kup-piwo" : args) -> checkNumArgs 0 args do
      success <- buyBeer
      if success
        then lift $ putStrLn "Gerwant pije piwo. Żywotność zostaje przywrócona."
        else lift $ putStrLn "Nie masz wystarczająco złota."
    ("zakończ" : _) -> return ()
    _ -> do
      lift $ putStr "Nieznane polecenie. Wpisz 'polecenia' by wyświetlić listę."
      gameLoop

main = do
  printIntroduction
  printInstructions
  execStateT gameLoop initialState
