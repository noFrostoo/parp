"The germ of a text adventure game
Drygaś Filip, Lew Filip, Lipniacki Filip"

"Game class

Responsibilities:
* print/read outputs/inputs
* handle game loop
* handle 'instructions' and 'halt' commands

Collaborators:
* ..."

Object subclass: Game [
    | isGameOver 
        location 
        inventory 
        itemLocations 
        facts 
        hp 
        enemyHp 
        timeOfDay 
        dayTurnCounter 
        attacksStrength
        itemDescription
        locationDescription
        npcDialog 
        paths 
        npcAskAbout|

    introductionText := #(
        'Gerwant z Riviery, szkoły nosacza, podróżuje szlakami Królestw Północy już wiele dni.'
        'Towarzyszy mu jedynie deszcz i jego wierny koń Piwonia.'
        ''
        'Pewnego popołudnia dociera do Świętego Dębu'
        'Niestety te sakramentalne miejsce zostało zbrukane ludzką krwią.'
        ''
    ).

    instructionsText := #(
        'Dostępne polecenia:'
        ''
        'polecenia              -- by wyświetlić listę poleceń.'
        'idź <kierunek>         -- by pójść w danym kierunku.'
        'podnieś <przedmiot>    -- by podnieść przedmiot.'
        'upuść <przedmiot>      -- by upuścić przedmiot.'
        'ekwipunek              -- by sprawdzić ekwipunek.'
        'obejrzyj <przedmiot>   -- by obejrzeć przedmiot.'
        'rozmawiaj <npc>        -- by porozmawiać z NPC.'
        'spytaj <npc> <coś>     -- by spytać NPC o coś.'
        'rozejrzyj się          -- by dowiedzieć się, gdzie jesteś.'
        'przygotuj się do walki -- by odgadnąć rodzaj potwora i zwiększyć swoje szanse na wygraną.'
        'zakończ                -- by zakończyć grę.'
        ''
    ).
 
    Game class >> new [
        | g |
        g := super new.
        g init.
        ^g
    ]

    init [
        isGameOver := false.
        hp := 100.
        enemyHp := 100.
        dayTurnCounter := 0.
        timeOfDay := 'Day'.
        attacksStrength := 10.
        facts := Set new.
        location := 'ŚwiętyDąb'.
        inventory := Set new.

        locationDescription := Dictionary new.
        locationDescription at: 'ŚwiętyDąb' put: 'Gerwant z Riviery, szkoły nosacza, podróżuje szlakami Królestw Północy już wiele dni. Towarzyszy mu jedynie deszcz i jego wierny koń Piwonia. Pewnego popołudnia dociera do Świętego Dębu. Niestety te sakramentalne miejsce zostało zbrukane ludzką krwią.'.
        locationDescription at: 'WieśGrobla' put: 'Gerwant dociera do Wsi Grobla.'.
        locationDescription at: 'DomSołtysa' put: 'Zabójca potworów wchodzi do domu sołtysa.'.
        locationDescription at: 'Karczma' put: 'Gerwant wchodzi do Karczmy.'.
        locationDescription at: 'PolanaKołoChatyDrwali' put: 'Zabójca potworów znajduje się na polanie wokoło Chaty Drwali.'.
        locationDescription at: 'ChataDrwali' put: 'Łowca potworów wchodzi do Chaty Drwali.'.
        locationDescription at: 'Puszcza' put: 'Mistrz Gerwant wkracza w samo serce puszczy.'.

        itemLocations := Dictionary new.
        itemLocations at: 'ŚwiętyDąb' put: #('Ciało' 'Totem').
        itemLocations at: 'WieśGrobla' put: #('Studnia' 'Tablica Ogłoszeń').
        itemLocations at: 'DomSołtysa' put: #().
        itemLocations at: 'Karczma' put: #().
        itemLocations at: 'PolanaKołoChatyDrwali' put: #('CidaryjskaPrzeszywanica' 'Trup').
        itemLocations at: 'ChataDrwali' put: #('Topór' 'Czaszka' 'Mieszek').
        itemLocations at: 'Puszcza' put: #('Statua').
        
        itemDescription := Dictionary new.
        itemDescription at: 'Totem' put: 'Analiza Gerwanta z Riviery: Totem został zbudowany ze szczątek i poroża jelenia. To wygląda na ostrzeżenie.'.
        itemDescription at: 'Ciało' put: 'Analiza Wiedźmina: Rany od szponów i kłów.'.
        itemDescription at: 'Studnia' put: 'Rivierijczyk zauważa *gnomskiGwyhyr* na dnie studni.'.
        itemDescription at: 'GnomskiGwyhyr' put: 'Gwyhyr wykuty przez gnomy. Jest sprawnie naostrzony. Na klindze ma wyryte runy.'.
        itemDescription at: 'Tablica Ogłoszeń' put: 'Nie ma takiego przedmiotu.'.
        itemDescription at: 'CidaryjskaPrzeszywanica' put: 'Przeszywanica popularna pośród piechurów w Cidaris.'.
        itemDescription at: 'Trup' put: 'Analiza Gerwanta z Riviery: "Poturbowany i mocno poobijany. Krew nie została wyssana, ale był pogryziony przez wilki"'.
        itemDescription at: 'Topór' put: 'Analiza Gerwanta z Riviery: "W rysach topora zastała się zaschnięta krew zwierzęcia"'.
        itemDescription at: 'Czaszka' put: 'Gerwanta z Riviery: "Ta czaszka należy do młodego niedźwiedzia"'.
        itemDescription at: 'Mieszek' put: 'Mieszek pełen złota. W środku znajduje się 10 monet. Wystarczy na piwo.'.
        itemDescription at: 'Statua' put: 'Analiza Gerwanta z Riviery: "Medalion Drży. To magiczny totem. Podobny totem widziałem pod Świętym dębem". Wiedźmin postanawia przeczytać bestiariusz. W wiedźmińskim almanachu Gerwant znajduje informację, że magiczne totemy to znak rozpoznawczny terytorium potwora zwanego jako *Leszy*.'.

        paths := Dictionary new.
        paths at: 'ŚwiętyDąb' put: #(('północ' 'WieśGrobla')).
        paths at: 'WieśGrobla' put: #(('Karczma' 'Karczma') ('DomSołtysa' 'DomSołtysa') ('południe' 'ŚwiętyDąb')).
        paths at: 'DomSołtysa' put: #(('wyjdź' 'WieśGrobla')).
        paths at: 'Karczma' put: #(('wyjdź' 'WieśGrobla')).
        paths at: 'PolanaKołoChatyDrwali' put: #(('zachód' 'ŚwiętyDąb') ('chata' 'ChataDrwali') ('Puszcza' 'Puszcza')).
        paths at: 'ChataDrwali' put: #(('wyjdź' 'PolanaKołoChatyDrwali')).
        paths at: 'Puszcza' put: #(('polana' 'PolanaKołoChatyDrwali') ('pieczara' 'Pieczara')).
        paths at: 'Pieczara' put: #(('wyjdź' 'Puszcza')).

        npcDialog := Dictionary new.
        npcDialog at: 'Wieśniak' put: 'Wieśniak jest sparaliżowany strachem i majaczy.'.
        npcDialog at: 'Drwale' put: 'Dobre jest tutaj piwo!'.
        npcDialog at: 'Karczmarz' put: 'Pordóżniku, zapraszam na piwo!'.
        npcDialog at: 'Sołtys' put: 'Gerwant z Riviery pyta o zlecenie na potwora. Sołtys opowiada mu o atakach potwora w lasach na południe od wioski. Proponuje porozmawiać z *ocalałymi drwalami*, przesiadującymi w karczmie i spytać ich o *atak*.'.

        npcAskAbout := Dictionary new.
        npcAskAbout at: #('Drwale' 'atak') put: 'Grupa drwali odpowiada, że zobaczyła wysokiego potwora z drewna i kości. Następnie zaatakowały ich wilki. Zostali zaatakowani koło Chaty drwali na *wschód* od Świętego Dębu.'

    ]

    "Print strings from array in separate lines."
    printLines: anArray [
        anArray do: [ :string | Transcript show: string; cr ]
    ]

    printIntroduction [
       self printLines: introductionText.
    ]

    printInstructions [
        self printLines: instructionsText.
    ]

    readCommand [
        Transcript show: '> '.
        ^ FileStream stdin nextLine
    ]

    go :path [
        | exits, newLoc |
        exits := paths at: location
        exits do: [ :exit
            exit at: 1 = path
                ifTrue: [^ exit at: 2]
        ]
        ^nil
    ]

    "Run the game."
    run [
        | args isUnknown firstCmd |

        self printIntroduction.
        self printInstructions.

        "Game loop."
        [isGameOver] whileFalse: [

            isUnknown := true.
            args := self readCommand substrings: ' '.
            firstCmd := args at: 1.
            args removeFirst.


            firstCmd = 'polecenia' ifTrue: [
                self printInstructions.
                isUnknown := false
            ].

            firstCmd = 'rozmawiaj' ifTrue: [


                isUnknown := false
            ].

            firstCmd = 'spytaj' ifTrue: [

                
                isUnknown := false
            ].

            firstCmd = 'obejrzyj' ifTrue: [

                
                isUnknown := false
            ].

            firstCmd = 'idź' ifTrue: [

                (args at: 1) printNl.
                isUnknown := false
            ].

            firstCmd = 'podnieś' ifTrue: [

                isUnknown := false
            ].

            firstCmd = 'ewkipunek' ifTrue: [

                isUnknown := false
            ].

            firstCmd = 'utrać' ifTrue: [

                isUnknown := false
            ].

            firstCmd = 'kupPiwo' ifTrue: [

                isUnknown := false
            ].

            firstCmd = 'przygotujSięDoWalki' ifTrue: [

                isUnknown := false
            ].

            firstCmd = 'rozejrzyjSię' ifTrue: [

                isUnknown := false
            ].

            firstCmd = 'zakończ' ifTrue: [
                isGameOver := true.
                isUnknown := false
            ].

            isUnknown ifTrue: [
                self printLines: #('Unknown command.' '')
            ]
        ]
    ]
]

Game new run.
