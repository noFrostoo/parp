-- Przygody Gerwanta z Riviery, by Drygaś Filip, Lew Filip, Lipniacki Daniel.
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}

import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO

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
  [ ("Święty Dąb", Location ["Wieśniak"] [("north", "bbb")])
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
    dayTurnCounter :: Int,
    attackStrength :: Int
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
      dayTurnCounter = 0,
      attackStrength = 10
    }

-- game actions

describe :: String -> String
describe "name" = "description"
describe _ = "Nie ma takiego przedmiotu"

talkPerson :: String -> String
talkPerson "Wieśniak" = "elo"
talkPerson _ = "Nie ma takiej osoby"


askPersonText :: String -> String -> String
askPersonText "Wieśniak" "Costam" = "elo elo"
askPersonText person topic = "Nie możesz zapytać " ++ person ++ " o " ++ topic

itemBoost :: String -> Int
itemBoost _ = 0  


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
            inventory = Set.insert item (inventory game),
            attackStrength = attackStrength game + itemBoost item
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
            inventory = Set.delete item (inventory game),
            attackStrength = attackStrength game - itemBoost item
          }
      return True
    else return False

talkTo :: Monad m => Person -> StateT GameState m String
talkTo person = do 
  game <- get
  let cur_loc = fromJust (Map.lookup (location game) (locations))
  let people_loc = people cur_loc
  if person `elem` people_loc
    then do
      return (talkPerson person)
    else return "Nie ma takiej osoby w tej lokacji"

askPerson :: Monad m => Person -> String -> StateT GameState m String
askPerson person topic = do
  game <- get
  let cur_loc = fromJust (Map.lookup (location game) (locations))
  let people_loc = people cur_loc
  if person `elem` people_loc
    then do
      return (askPersonText person topic)
    else return "Nie ma takiej osoby w tej lokacji"

buyBeer :: Monad m => StateT GameState m Bool
buyBeer = do
  game <- get
  if "mieszek" `elem` inventory game
    then do
      let items = fromMaybe Set.empty (Map.lookup (location game) (itemLocations game))
      put $
        game
          { inventory = Set.delete "mieszek" (inventory game),
            hp = 100
          }
      return True
    else return False

boostAttack :: Monad m => Int -> StateT GameState m ()
boostAttack boost = do
  game <- get
  put $
    game
      { attackStrength = (attackStrength game) + boost
      }

attack :: Monad m => StateT GameState m String
attack = do 
  game <- get
  if location game == "Pieczara"
    then do 
      put $
        game
          { enemyHp = enemyHp game - attackStrength game,
            hp = hp game - 15
          }
      return $ "Zataakowałęs za" ++ show (attackStrength game) ++ ". Twoje aktualne życie to" ++ show (hp game) ++ ".\nPrzeciwynik zatakował za" ++ show 15 ++ ". Jego zdrowie to " ++ show (enemyHp game)
    else return "Nie ma potfora w tej lokacji"

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
  hFlush stdout
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
    ("kup" : "piwo" : args) -> checkNumArgs 0 args do
      success <- buyBeer
      if success
        then lift $ putStrLn "Gerwant pije piwo. Żywotność zostaje przywrócona."
        else lift $ putStrLn "Nie masz wystarczająco złota."
    ("przygotuj" : "się" : "do" : "walki" : args) -> checkNumArgs 0 args do
      game <- get 
      if location game == "Pieczara" 
        then do 
          lift $ putStrLn "Do walki z jakim potworem musi się przygotować Gerwant z Riviery?"
          monster <- lift readCommand
          case monster of
            ("Xxxx": args) -> checkNumArgs 0  args do 
              lift $ putStrLn "Poprawna odpowiedz, ta walka bedzie łatwa"
              boostAttack 5 -- boost for corrent anwser
            _ -> do
              lift $ putStrLn "Nie poprawna odpowiedz, twoja walka bedzie trudniesza"
              boostAttack (-5)
        else lift $ putStrLn "W tej lokacji nie ma walki"
      gameLoop
    ("zakończ" : _) -> return ()
    _ -> do
      lift $ putStr "Nieznane polecenie. Wpisz 'polecenia' by wyświetlić listę."
      gameLoop

main = do
  printIntroduction
  printInstructions
  execStateT gameLoop initialState
