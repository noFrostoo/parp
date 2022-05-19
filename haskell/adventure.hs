-- Przygody Gerwanta z Riviery, by Drygaś Filip, Lew Filip, Lipniacki Daniel.
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}

import Control.Monad.State
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO
import Data.Set (toList)

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
    "rozejrzyj się          -- by dowiedzieć się, gdzie jesteś.",
    "zakończ                -- by zakończyć grę.",
    "Żadne zwierze nie ucierpiało podczas tworzenia tej gry"
  ]

type Item = String

type LocationName = String

type Path = String

type Person = String

data TimeOfDay = Dzień | Noc -- polish here for show method 
  deriving (Eq, Show)

data Location = Location
  { people :: [Person],
    exits :: Map Path LocationName
  }

locations :: Map LocationName Location
locations =
  [ ("ŚwiętyDąb", Location ["Wieśniak"] [("północ", "WieśGrobla")]),
    ("WieśGrobla", Location [] [("Karczma", "Karczma"), ("DomSołtysa", "DomSołtysa"), ("południe", "ŚwiętyDąb")]),
    ("DomSołtysa", Location ["Sołtys"] [("wyjdź", "WieśGrobla")]),
    ("Karczma", Location ["Drwale", "Karczmarz"] [("wyjdź", "WieśGrobla")]),
    ("PolanaKołoChatyDrwali", Location [] [("zachód", "ŚwiętyDąb"), ("chata", "ChataDrwali"), ("Puszcza", "Puszcza")]),
    ("ChataDrwali", Location [] [("wyjdź", "PolanaKołoChatyDrwali")]),
    ("Puszcza", Location [] [("polana", "PolanaKołoChatyDrwali"), ("pieczara", "Pieczara")]),
    ("Pieczara", Location [] [("wyjdź", "Puszcza")])
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
    { location = "ŚwiętyDąb",
      inventory = Set.empty,
      itemLocations = [
        ("ŚwiętyDąb", ["Ciało", "Totem"]),
        ("WieśGrobla", ["Studnia", "TablicaOgłoszeń"]),
        ("DomSołtysa", []),
        ("Karczma", []),
        ("PolanaKołoChatyDrwali", ["CidaryjskaPrzeszywanica", "Trup"]),
        ("ChataDrwali", ["Topór", "Czaszka", "Mieszek"]),
        ("Puszcza", ["Statua"])
      ],
      facts = Set.empty,
      hp = 100,
      enemyHp = 100,
      timeOfDay = Dzień,
      dayTurnCounter = 7,
      attackStrength = 10
    }

-- game actions

describeItem :: Item -> String
describeItem "Totem" = "Analiza Gerwanta z Riviery: Totem został zbudowany ze szczątek i poroża jelenia. To wygląda na ostrzeżenie."
describeItem "Ciało" = "Analiza Wiedźmina: Rany od szponów i kłów."

describeItem "Studnia" = " Rivierijczyk zauważa *gnomskiGwyhyr* na dnie studni."
describeItem "GnomskiGwyhyr" = "Gwyhyr wykuty przez gnomy. Jest sprawnie naostrzony. Na klindze ma wyryte runy."
describeItem "TablicaOgłoszeń" = "Nie ma takiego przedmiotu."

describeItem "CidaryjskaPrzeszywanica" = "Przeszywanica popularna pośród piechurów w Cidaris"
describeItem "Trup" = "Analiza Gerwanta z Riviery: \"Poturbowany i mocno poobijany. Krew nie została wyssana, ale był pogryziony przez wilki\""

describeItem "Topór" = "Analiza Gerwanta z Riviery: \"W rysach topora zastała się zaschnięta krew zwierzęcia\""
describeItem "Czaszka" = "Gerwanta z Riviery: \"Ta czaszka należy do młodego niedźwiedzia\""
describeItem "Mieszek" = "Mieszek pełen złota. W środku znajduje się 10 monet. Wystarczy na piwo."

describeItem "Statua" = " Analiza Gerwanta z Riviery: \"Medalion Drży. To magiczny totem. Podobny totem widziałem pod Świętym dębem\". Wiedźmin postanawia przeczytać bestiariusz. W wiedźmińskim almanachu Gerwant znajduje informację, że magiczne totemy to znak rozpoznawczny terytorium potwora zwanego jako *Leszy*."
describeItem _ = "Nie ma takiego przedmiotu."

describeLocation :: String -> String
describeLocation "ŚwiętyDąb" = "Gerwant dociera do Świętego Dębu.\ 
\Niestety te sakramentalne miejsce zostało zbrukane ludzką krwią."
describeLocation "WieśGrobla" = "Gerwant dociera do Wsi Grobla."
describeLocation "DomSołtysa" = "Zabójca potworów wchodzi do domu sołtysa"
describeLocation "Karczma" = "Gerwant wchodzi do Karczmy"
describeLocation "PolanaKołoChatyDrwali" = "Zabójca potworów znajduje się na polanie wokoło Chaty Drwali."
describeLocation "ChataDrwali" = "Łowca potworów wchodzi do Chaty Drwali"
describeLocation "Puszcza" = "Mistrz Gerwant wkracza w samo serce puszczy."
describeLocation "Pieczara" = "Gerwant z Riviery wchodzi do pieczary. \
\Gerwant wnioskuje, że natrafił na leże potwora. \
\Monstrum nie ma w jaskini, więc może na niego zaczekać (komenda przygotuj się do walki). \
\Aczkolwiek zanim wiedźmin zdecyduje się na spotkanie oko w oko z bestia \
\powinien się porządnie przygotować, a zatem musi wywnioskować z jakim potworem ma do czynienia."

describeLocation _ = "Nie ma takiego miejsca."

talkPerson :: Person -> String
talkPerson "Wieśniak" = "Wieśniak jest sparaliżowany strachem i majaczy"

talkPerson "Drwale" = "Dobre jest tutaj piwo!"
talkPerson "Karczmarz" = "Pordóżniku, zapraszam na piwo!"

talkPerson "Sołtys" = "Gerwant z Riviery pyta o zlecenie na potwora. Sołtys opowiada mu o atakach potwora w lasach na południe od wioski. Proponuje porozmawiać z *ocalałymi drwalami*, przesiadującymi w karczmie i spytać ich o *atak*."

talkPerson _ = "Nie ma takiej osoby"

askPersonText :: Person -> String -> String
askPersonText "Drwale" "atak" = "Grupa drwali odpowiada, że zobaczyła wysokiego potwora z drewna i kości. Następnie zaatakowały ich wilki. Zostali zaatakowani koło Chaty drwali na *wschód* od Świętego Dębu"
askPersonText person topic = "Nie możesz zapytać " ++ person ++ " o " ++ topic ++ "."

itemBoost :: String -> Int
itemBoost "GnomskiGwyhyr" = 110
itemBoost "CidaryjskaPrzeszywanica" = 50
itemBoost "Topór" = 50
itemBoost _ = 0

-- Game information


go :: Monad m => LocationName -> Path -> StateT GameState m Bool
go src path = do
  case Map.lookup src locations >>= (Map.lookup path . exits) of
    Just loc -> do
      game <- get
      put $ game {location = loc}
      return True
    Nothing -> do
      game <- get 
      if "ŚwiętyDąb" == src && path == "wschód" && "OdkrytePolana" `elem` (facts game)
        then do
          put $ game {location = "PolanaKołoChatyDrwali"}
          return True
      else return False


lookAround :: Monad m => StateT GameState m String
lookAround = do
  game <- get
  let peopleInLoc = people $ fromJust $ Map.lookup (location game) locations
  let exitsInLoc = Map.keys $ exits $ fromJust $ Map.lookup (location game) locations
  let itemsInLoc = toList $ fromJust $ Map.lookup (location game) (itemLocations game) 
  return $
    describeLocation (location game)
        ++ "\n\nZnajdują się tu następujące osoby:\n"
        ++ intercalate "\n" (map ("  - " ++) peopleInLoc)
        ++ "\n\nZnajdują się tu następujące przedmioty:\n"
        ++ intercalate "\n" (map ("  - " ++) itemsInLoc)
        ++ "\n\nGerwant może stąd pójść w następujących kierunkach:\n"
        ++ intercalate "\n" (map ("  - " ++) exitsInLoc)
        ++ if "ŚwiętyDąb" == (location game)  && "OdkrytePolana" `elem` (facts game) then "\n   - PolanaKołoChatyDrwali" else ""

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
  let cur_loc = fromJust (Map.lookup (location game) locations)
  let people_loc = people cur_loc
  if person `elem` people_loc
    then do
      return (talkPerson person)
    else return "Nie ma takiej osoby w tym miejscu."

askPerson :: Monad m => Person -> String -> StateT GameState m String
askPerson person topic = do
  game <- get
  let cur_loc = fromJust (Map.lookup (location game) locations)
  let people_loc = people cur_loc
  if person `elem` people_loc
    then do
      when (person == "Drwale" && topic == "atak") do
        put $
          game
            { facts = Set.insert "OdkrytePolana" (facts game)
            }
      return (askPersonText person topic)
    else return "Nie ma takiej osoby w tym miejscu."

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
  put $ game {attackStrength = attackStrength game + boost}

attack :: Monad m => StateT GameState m String
attack = do
  game <- get
  if location game == "Pieczara"
    then if enemyHp game > 0
      then do
        let gerwantAttack = max (enemyHp game) (attackStrength game)
        let enemyAttack = max (hp game) 15
        let gerwantHpAfter = hp game - enemyAttack
        let enemyHpAfter = enemyHp game - gerwantAttack
        put $
          game
            { enemyHp = enemyHpAfter,
              hp = gerwantHpAfter
            }
        when (gerwantHpAfter <= 0) (put initialState)
        return $ "Gerwant atakuje za " ++ show gerwantAttack ++ " pkt.\n"
          ++ if enemyHpAfter <= 0
            then "Potwór pada martwy od ciosu."
            else "Przeciwnik atakuje za "
              ++ show enemyAttack
              ++ " pkt.\n"
              ++ if gerwantHpAfter <= 0
                then "Wiedźmin pada od ciosu potwora. Gerwant umarł, a cała płeć żeńska na kontynencie uroniła łzę.\nGra zaczyna się od początku."
                else "Zdrowie Gerwanta wynosi "
                ++ show gerwantHpAfter
                ++ ".\n"
                ++ "Zdrowie potwora wynosi "
                ++ show enemyHpAfter
                ++ "."
      else return "Potwór jest już martwy."
    else return "To miejsce jest spokojne. Gerwant nie chce wszczynać burd, więc nikogo nie atakuje."


checkIfAddGnomskiGwyhyr :: Monad m => String -> StateT GameState m ()
checkIfAddGnomskiGwyhyr itemSrc  = do
    game <- get
    when (itemSrc == "Studnia") do
      let items = fromMaybe Set.empty (Map.lookup "WieśGrobla" (itemLocations game))
      put $ 
        game { itemLocations = Map.insert "WieśGrobla"  ( Set.insert "GnomskiGwyhyr" items) (itemLocations game) }

advanceTime :: Monad m => StateT GameState m String
advanceTime = do
  game <- get
  put $ game {dayTurnCounter = (dayTurnCounter game) + 1}
  if dayTurnCounter game == 11
    then do
      let newTimeOfDay = if timeOfDay game == Dzień then Noc else Dzień
      put $ game {dayTurnCounter = 0, timeOfDay = newTimeOfDay}
      return $ "Nastała nowa pora dnia " ++ (show (timeOfDay game)) ++ ". Godzina to " ++ (show (dayTurnCounter game)) ++ "."
    else return  $ "Godzina to " ++ (show (dayTurnCounter game)) ++ "."

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

checkNumArgs :: MonadTrans t => Int -> [String] -> t IO () -> t IO ()
checkNumArgs num args expr =
  if length args == num
    then expr
    else lift $ putStrLn $ "Niewłaściwa ilość argumentów" ++ show (length args) ++ "(oczekiwano " ++ show num ++ ")."

gameLoop :: StateT GameState IO ()
gameLoop = do
  dayTimeStinng <- advanceTime
  lift $ putStrLn $ dayTimeStinng
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
      lift $ putStrLn $ unlines $ map describeItem (Set.toList $ inventory game)
      gameLoop
    ("obejrzyj" : args) -> checkNumArgs 1 args do
      checkIfAddGnomskiGwyhyr $ head args
      lift $ putStrLn $ describeItem $ head args
      gameLoop
    ("rozmawiaj" : args) -> checkNumArgs 1 args do
      answer <- talkTo $ head args
      lift $ putStrLn answer
      gameLoop
    ("spytaj" : args) -> checkNumArgs 2 args do
      answer <- askPerson (head args) (args !! 1)
      lift $ putStrLn answer
      gameLoop
    ("rozejrzyj" : "się" : args) -> checkNumArgs 0 args do
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
            ["leszy"] -> do
              lift $ putStrLn "Poprawna odpowiedź. Gerwant warzy odpowiednie eliksiry, które poprawią jego zdolności bojowe"
              boostAttack 5 -- boost for correct anwser
            ["Leszy"] -> do
              lift $ putStrLn "Poprawna odpowiedź. Gerwant warzy odpowiednie eliksiry, które poprawią jego zdolności bojowe"
              boostAttack 5 -- boost for correct anwser
            _ -> do
              lift $ putStrLn "Niepoprawna odpowiedź. Gerwant warzy złe eliksiry, które mu nie pomogą, a będzie musiał zmagać się z efektami ubocznymi."
              boostAttack (-5)
        else lift $ putStrLn "To miejsce jest spokojne. Po co Gerwant miałby przygotowywać się do walki?"
      gameLoop
    ("zakończ" : _) -> return ()
    _ -> do
      lift $ putStr "Nieznane polecenie. Wpisz 'polecenia' by wyświetlić listę."
      gameLoop

initGame :: StateT GameState IO ()
initGame = do
  lookAround
  gameLoop

main = do
  printIntroduction
  printInstructions
  execStateT initGame initialState
