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
        attackStrength
        itemDescription
        locationDescription
        npcDialog 
        paths 
        npcAskAbout
        itemStrengthBoost
        npcLocations|

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
        attackStrength := 10.
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
        npcAskAbout at: #('Drwale' 'atak') put: 'Grupa drwali odpowiada, że zobaczyła wysokiego potwora z drewna i kości. Następnie zaatakowały ich wilki. Zostali zaatakowani koło Chaty drwali na *wschód* od Świętego Dębu.'.

        itemStrengthBoost := Dictionary new.
        itemStrengthBoost at: 'GnomskiGwyhyr' put: 110.
        itemStrengthBoost at: 'CidaryjskaPrzeszywanica' put: 51.
        itemStrengthBoost at: 'Topór' put: 50.

        npcLocations := Dictionary new .
        npcLocations at: 'ŚwiętyDąb' put: #('Wieśniak').
        npcLocations at: 'WieśGrobla' put: #().
        npcLocations at: 'DomSołtysa' put: #('Sołtys').
        npcLocations at: 'Karczma' put: #('Drwale' 'Karczmarz').
        npcLocations at: 'PolanaKołoChatyDrwali' put: #().
        npcLocations at: 'ChataDrwali' put: #().
        npcLocations at: 'Puszcza' put: #()
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

    go: path [
        | exits newLoc |
        exits := paths at: location
        exits do: [ 
            :exit |
            (exit at: 1) = path ifTrue: [
                location := exit at: 2.
                ^true
            ].
        ].
        ^false
    ]

    lookAround [
        (locationDescription at: location) printNl.
        'Znajdują się tu następujące osoby:' printNl.
        npcLocations at: location do: [ :npc |
            '  - ' print.
            npc printNl.
        ].
        '' printNl.
        'Znajdują się tu następujące przedmioty:' printNl.
        itemLocations at: location do: [ :item |
            '  - ' print.
            item printNl.
        ].
        '' printNl.
        'Gerwant może stąd pójść w następujących kierunkach:' printNl.
        paths at: location do: [ :path |
            '  - ' print.
            (path at: 1) printNl.
        ]
    ]

    pickUp: item [
        itemLocations at: location includes: item
            ifTrue: [
                itemLocations at: location remove: item.
                inventory add: item.
                itemStrengthBoost includes: item ifTrue: [attackStrength := attackStrength + (itemStrengthBoost at: item)].
                ^true
            ].
        ^false
    ]

    dropItem: item [
        inventory includes: item
            ifTrue: [
                inventory remove: item.
                itemLocations at: location add: item.
                itemStrengthBoost includes: item ifTrue: [attackStrength := attackStrength - (itemStrengthBoost at: item)].
                ^true
            ].
        ^false
    ]

    talkTo: person [
        | people |
        people := npcLocations at: location.
        people do: [
            :p |
            p == person
                ifTrue: [
                    ^npcDialog at: person
                ]
        ].
        ^'Nie ma tu takiej osoby' 
    ]

    askPerson: person about:topic [
        person == 'Drwale' and topic == 'atak'
            ifTrue: [
                paths at: 'ŚwiętyDąb' add: #('wschód' 'PolanaKołoChatyDrwali').
                ^'Grupa drwali odpowiada, że zobaczyła wysokiego potwora z drewna i kości. Następnie zaatakowały ich wilki. Zostali zaatakowani koło Chaty drwali na *wschód* od Świętego Dębu'
            ].
        ^'Nie możesz zapytać tej osoby o ten temat'
    ]

    buyBeer [
        inventory includes: 'Mieszek' and location = 'Karczma'
            ifTrue: [
                inventory remove: 'Mieszek'.
                hp = 100.
                ^true
            ].
        ^false
    ]

    attack [
        | gerwantAttack enemyAttack gerwantHpAfter enemyHpAfter |
        location = 'Pieczara'
            ifFalse: [^'To miejsce jest spokojne. Gerwant nie chce wszczynać burd, więc nikogo nie atakuje.'].
        enemyHp <= 0
            ifFalse: [^'Potwór jest już martwy'].
        
        gerwantAttack := enemyHp max: attackStrength.
        enemyAttack := hp max: 15.
        hp := hp - enemyAttack.
        enemyHp := enemyHp - gerwantAttack.

        'Gerwant atakuje za ' print.
        gerwantAttack printLn.
        enemyHp = 0 ifTrue: [
            'Potwór pada martwy od ciosu.' printLn.
            ^nil
        ].

        'Potwór atakuje za ' print.
        enemyAttack printLn.
        hp = 0 ifTrue: [
            'Wiedźmin pada od ciosu potwora. Gerwant umarł, a cała płeć żeńska na kontynencie uroniła łzę.' printLn.
            'Gra zaczyna się od początku.' printLn.
            self init.
            ^nil
        ].

        'Zdrowie Gerwanta wynosi ' print.
        hp printLn.
        'Zdrowie potwora wynosi ' print.
        enemyHp printLn.
    ]

    interact: item [
        | items |
        item == 'Studnia' and location == 'WieśGrobla'
            ifTrue: [
                (itemLocations at: 'WieśGrobla') add: 'GnomskiGwyhyr'
            ].
        items := itemLocations at: location.
        items do: [
            :i |
            i == item
                ifTrue: [
                    ^itemDescription at: item.
                ]
        ].
        ^'Nie ma tu takiego przedmiotu' 
    ]

    "Run the game."
    run [
        | cmd isUnknown cmdWordsNumber|

        self printIntroduction.
        self printInstructions.

        "Game loop."
        [isGameOver] whileFalse: [

            isUnknown := true.
            cmd := self readCommand substrings: ' '.
            cmdWordsNumber := cmd size.

            cmd.
            (cmdWordsNumber = 1) ifTrue: [
                (cmd at: 1) = 'polecenia' & isUnknown  ifTrue: [
                    self printInstructions.
                    isUnknown := false
                ].

                (cmd at: 1) = 'ewkipunek' & isUnknown  ifTrue: [

                    isUnknown := false
                ].

                (cmd at: 1) = 'zakończ' & isUnknown  ifTrue: [
                    isGameOver := true.
                    isUnknown := false
                ]
            ].



            (cmdWordsNumber = 2) ifTrue: [
                (cmd at: 1) = 'rozmawiaj' & isUnknown  ifTrue: [
                    self talkTo: (cmd at: 2).
                    isUnknown := false
                ].

                (cmd at: 1) = 'kup' & (cmd at: 2) = 'piwo' & isUnknown ifTrue: [
                    (cmd) printNl.
                    isUnknown := false
                ].

                (cmd at: 1) = 'obejrzyj' & isUnknown  ifTrue: [
                    (cmd) printNl.
                    isUnknown := false
                ].

                (cmd at: 1) = 'idź' & isUnknown  ifTrue: [
                    (cmd) printNl.
                    (cmd at: 1) printNl.
                    isUnknown := false
                ].

                (cmd at: 1) = 'podnieś' & isUnknown  ifTrue: [

                    isUnknown := false
                ].

                (cmd at: 1) = 'utrać' & isUnknown  ifTrue: [

                    isUnknown := false
                ].

                (cmd at: 1) = 'rozejrzyj' & (cmd at: 2) = 'się' & isUnknown ifTrue: [
                    (cmd) printNl.
                    isUnknown := false
                ]
            ].



            (cmdWordsNumber = 3) ifTrue: [
                (cmd at: 1) = 'spytaj' & isUnknown ifTrue: [
                    self askPerson: (cmd at: 2) about: (cmd at: 3).
                    isUnknown := false
                ]
            ].



            (cmdWordsNumber = 4) ifTrue: [
                (cmd at: 1) = 'przygotuj' & (cmd at: 2) = 'się' & (cmd at: 3) = 'do' & (cmd at: 4) = 'walki' & isUnknown  ifTrue: [
                    isUnknown := false
                ].
            ].

            

            isUnknown ifTrue: [
                self printLines: #('Nieznana lub niepełna komenda. Być może brakuje argumentów.' '')
            ]
        ]
    ]
]

Game new run.
