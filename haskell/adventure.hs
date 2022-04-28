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

data Location = Location
  { people :: [Person],
    exits :: Map Path LocationName
  }

locations :: Map LocationName Location
locations =
  [ ("aaa", Location ["Wieśniak"] [("north", "bbb")])
  ]

-- game state

data GameState = GameState
  { location :: String,
    inventory :: Set Item,
    itemLocations :: Map LocationName (Set Item),
    hp :: Int,
    enemyHp :: Int,
    facts :: Set String
  }

-- game actions

describe :: String -> String
describe "name" = "description"
describe _ = "Nie ma takiego przedmiotu"

go :: LocationName -> Path -> Maybe LocationName
go src path =
  Map.lookup src locations
    >>= (Map.lookup path . exits)

lookAround :: Location -> LocationName -> String
lookAround contents location = undefined

pickUp :: Item -> State GameState Bool
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

drop :: Item -> State GameState Bool
drop item = do
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

talkTo :: Location -> Person -> String
talkTo location person = undefined

askPerson :: Location -> Person -> String -> String
askPerson location person topic = undefined

buyBeer :: State GameState String
buyBeer = undefined

prepareForFight :: String -> State GameState Bool
prepareForFight = undefined

attack :: Item -> State GameState Int
attack = undefined

-- command line logic

printLines :: [String] -> IO ()
printLines xs = putStr (unlines xs)

printIntroduction = printLines introductionText

printInstructions = printLines instructionsText

readCommand :: IO [String]
readCommand = do
  putStr "> "
  fmap words getLine

gameLoop :: IO ()
gameLoop = do
  cmd <- readCommand
  case cmd of
    ("polecenia" : _) -> do
      printInstructions
      gameLoop
    ("zakończ" : _) -> return ()
    _ -> do
      putStr "Nieznane polecenie. Wpisz 'polecenia' by wyświetlić listę."
      gameLoop

main = do
  printIntroduction
  printInstructions
  gameLoop
