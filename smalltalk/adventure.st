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
        npcLocations
        emptyString |

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
        emptyString := ' '.
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
        locationDescription at: 'ŚwiętyDąb' put: #( 'Gerwant dociera do Świętego Dębu.' 'Niestety te sakramentalne miejsce zostało zbrukane ludzką krwią.' '').
        locationDescription at: 'WieśGrobla' put: #('Gerwant dociera do Wsi Grobla.' '').
        locationDescription at: 'DomSołtysa' put: #('Zabójca potworów wchodzi do domu sołtysa.' '').
        locationDescription at: 'Karczma' put: #('Gerwant wchodzi do Karczmy.' '').
        locationDescription at: 'PolanaKołoChatyDrwali' put: #('Zabójca potworów znajduje się na polanie wokoło Chaty Drwali.' '').
        locationDescription at: 'ChataDrwali' put: #('Łowca potworów wchodzi do Chaty Drwali.' '').
        locationDescription at: 'Puszcza' put: #('Mistrz Gerwant wkracza w samo serce puszczy.' '').

        itemLocations := Dictionary new.
        itemLocations at: 'ŚwiętyDąb' put: #('Ciało' 'Totem').
        itemLocations at: 'WieśGrobla' put: #('Studnia' 'Tablica Ogłoszeń').
        itemLocations at: 'DomSołtysa' put: #().
        itemLocations at: 'Karczma' put: #().
        itemLocations at: 'PolanaKołoChatyDrwali' put: #('CidaryjskaPrzeszywanica' 'Trup').
        itemLocations at: 'ChataDrwali' put: #('Topór' 'Czaszka' 'Mieszek').
        itemLocations at: 'Puszcza' put: #('Statua').
        
        itemDescription := Dictionary new.
        itemDescription at: 'Totem' put: #('Analiza Gerwanta z Riviery: Totem został zbudowany ze szczątek i poroża jelenia. To wygląda na ostrzeżenie.' '').
        itemDescription at: 'Ciało' put: #('Analiza Wiedźmina: Rany od szponów i kłów.' '').
        itemDescription at: 'Studnia' put: #('Rivierijczyk zauważa *GnomskiGwyhyr* na dnie studni.' '').
        itemDescription at: 'GnomskiGwyhyr' put: #('Gwyhyr wykuty przez gnomy. Jest sprawnie naostrzony. Na klindze ma wyryte runy.' '').
        itemDescription at: 'Tablica Ogłoszeń' put: #('Nie ma takiego przedmiotu.' '').
        itemDescription at: 'CidaryjskaPrzeszywanica' put: #('Przeszywanica popularna pośród piechurów w Cidaris.' '').
        itemDescription at: 'Trup' put: #('Analiza Gerwanta z Riviery: "Poturbowany i mocno poobijany. Krew nie została wyssana, ale był pogryziony przez wilki"' '').
        itemDescription at: 'Topór' put: #('Analiza Gerwanta z Riviery: "W rysach topora zastała się zaschnięta krew zwierzęcia"' '').
        itemDescription at: 'Czaszka' put: #('Gerwanta z Riviery: "Ta czaszka należy do młodego niedźwiedzia"' '').
        itemDescription at: 'Mieszek' put: #('Mieszek pełen złota. W środku znajduje się 10 monet. Wystarczy na piwo.' '').
        itemDescription at: 'Statua' put: #(
            'Analiza Gerwanta z Riviery: "Medalion Drży. To magiczny totem. Podobny totem widziałem pod Świętym dębem".'
            'Wiedźmin postanawia przeczytać bestiariusz.'
            ''
            ' W wiedźmińskim almanachu Gerwant znajduje informację, że magiczne totemy to znak rozpoznawczny terytorium potwora zwanego jako *Leszy*.' 
            ''
        ).

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
        npcDialog at: 'Wieśniak' put: #('Wieśniak jest sparaliżowany strachem i majaczy.' '').
        npcDialog at: 'Drwale' put: #('Dobre jest tutaj piwo!' '').
        npcDialog at: 'Karczmarz' put: #('Pordóżniku, zapraszam na piwo!' '').
        npcDialog at: 'Sołtys' put: #(
            'Gerwant z Riviery pyta o zlecenie na potwora. Sołtys opowiada mu o atakach potwora w lasach na południe od wioski.'
            'Proponuje porozmawiać z *ocalałymi drwalami*, przesiadującymi w karczmie i spytać ich o *atak*.'
            '').

        npcAskAbout := Dictionary new.
        npcAskAbout at: #('Drwale' 'atak') put: #(
            'Grupa drwali odpowiada, że zobaczyła wysokiego potwora z drewna i kości.'
            'Następnie napadły ich wilki. 
            Zostali zaatakowani koło Chaty drwali na *wschód* od Świętego Dębu.'
            ''
            'Gerwant może teraz udać się na wschód od lokacji *ŚwiętyDąb*'
            ''
            ).

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

    printLinesNoNewLine: anArray [
        anArray do: [ :string | Transcript show: string]
    ]

    printIntroduction [
       self printLines: introductionText.
       self lookAround
    ]

    printInstructions [
        self printLines: instructionsText.
    ]

    readCommand [
        Transcript show: '> '.
        ^ FileStream stdin nextLine
    ]

    go: path [
        | exits |
        exits := paths at: location.
        exits do: [ :exit |
            (exit at: 1) = path ifTrue: [
                location := (exit at: 2).
                self lookAround.
                ^nil
            ]
        ].
        self printLines: #('Nie ma takiej ścieżki').
        ^nil
    ]

    lookAround [
        | npcLocationsHere itemLocationsHere pathsHere |
        self printLines: (locationDescription at: location).

        self printLines: #('Znajdują się tu następujące osoby:').
        npcLocationsHere := npcLocations at: location.
        npcLocationsHere do: [ :npc |
            self printLines: (Array with: npc)
        ].
        self printLines: #('').

        self printLines: #('Znajdują się tu następujące przedmioty:').
        itemLocationsHere := itemLocations at: location.
        itemLocationsHere do: [ :item |
            self printLines: (Array with: item)
        ].
        self printLines: #('').

        self printLines: #('Gerwant może stąd pójść w następujących kierunkach:').
        pathsHere := paths at: location.
        pathsHere do: [ :path |
            self printLines: ( Array with: (path at: 1) )
        ].
        self printLines: #('')
    ]

    pickUp: item [
        ((itemLocations at: location) includes: item)
            ifTrue: [
                | itemLocationsHere |
                itemLocationsHere := (itemLocations at: location).
                itemLocationsHere select: [:e | e ~~ item].
                itemLocations at: location put: itemLocationsHere.
                inventory add: item.
                self printLinesNoNewLine: #('Gerwant podnosi '). 
                self printLines: (Array with: item).
                (itemStrengthBoost includesKey: item) 
                    ifTrue: [
                        attackStrength := attackStrength + (itemStrengthBoost at: item)
                    ].
                ^true
            ].
        self printLines: #('Nie ma tu takiego przedmiotu' '').
        ^false
    ]

    dropItem: item [
        (inventory includes: item)
            ifTrue: [
                inventory select: [:e | e ~~ item].
                "TODO to dodawanie nie działa"
                itemLocations at: location add: item.
                (itemStrengthBoost includesKey: item) 
                    ifTrue: [
                        attackStrength := attackStrength - (itemStrengthBoost at: item)
                    ].
                ^true
            ].
        self printLines: #('Nie posiadasz takiego przedmiotu' '').
        ^false
    ]

    talkTo: person [
        | people |
        people := npcLocations at: location.
        (people includes: person)
            ifNotNil: [
                ^(npcDialog at: person)
            ].
        ^'Nie ma tu takiej osoby' 
    ]

    askPerson: person about:topic [
        person == 'Drwale' & topic == 'atak'
            ifTrue: [
                paths at: 'ŚwiętyDąb' add: #('wschód' 'PolanaKołoChatyDrwali').
                ^'Grupa drwali odpowiada, że zobaczyła wysokiego potwora z drewna i kości. Następnie zaatakowały ich wilki. Zostali zaatakowani koło Chaty drwali na *wschód* od Świętego Dębu.'
            ].
        ^'Nie możesz zapytać tej osoby o ten temat'
    ]

    buyBeer [
        inventory includes: 'Mieszek' & location = 'Karczma'
            ifTrue: [
                inventory remove: 'Mieszek'.
                hp = hp + 50.
                self printLines: #('Piwo wzmacnia Gerwanta. Gerwant zyskuje 50hp.').
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
        
        gerwantAttack := enemyHp min: attackStrength.
        enemyAttack := hp min: 15.
        hp := hp - enemyAttack.
        enemyHp := enemyHp - gerwantAttack.


        self printLinesNoNewLine: #('Gerwant atakuje za '). 
        self printLines: (Array with: gerwantAttack).
        enemyHp = 0 ifTrue: [
            self printLines: #('Potwór pada martwy od ciosu.'). 
            ^nil
        ].

        self printLinesNoNewLine: #('Potwór atakuje za '). 
        self printLines: (Array with: enemyAttack).
        hp = 0 ifTrue: [
            self printLines: ('Wiedźmin pada od ciosu potwora. Gerwant umarł, a cała płeć żeńska na kontynencie uroniła łzę.').
            self printLines: ('Gra zaczyna się od początku.').
            self init.
            ^nil
        ].

        self printLinesNoNewLine: #('Zdrowie Gerwanta wynosi '). 
        self printLines: (Array with: hp).
        self printLinesNoNewLine: #('Zdrowie potwora wynosi ').
        self printLines: (Array with: enemyHp).
    ]

    interact: item [
        | items |
        item = 'Studnia' & location = 'WieśGrobla'
            ifTrue: [
                itemLocations at: 'WieśGrobla' put: #('Studnia' 'Tablica Ogłoszeń' 'GnomskiGwyhyr')
            ].
        items := itemLocations at: location.
        items do: [
            :i |
            i = item
                ifTrue: [
                    ^(itemDescription at: item)
                ]
        ].
        ^'Nie ma tu takiego przedmiotu' 
    ]

    dispInventory [
        inventory do: [
            :item |
            self printLinesNoNewLine: (Array with: item).
            self printLines: (itemDescription at: item).
        ]
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
                isUnknown & (cmd at: 1) = 'polecenia' ifTrue: [
                    self printInstructions.
                    isUnknown := false
                ].

                isUnknown & (cmd at: 1) = 'ekwipunek' ifTrue: [
                    self dispInventory.
                    isUnknown := false
                ].

                isUnknown & (cmd at: 1) = 'zakończ' ifTrue: [
                    isGameOver := true.
                    isUnknown := false
                ]
            ].



            (cmdWordsNumber = 2) ifTrue: [
                isUnknown & (cmd at: 1) = 'rozmawiaj' ifTrue: [
                    self printLines: (self talkTo: (cmd at: 2)).
                    isUnknown := false
                ].

                isUnknown & (cmd at: 1) = 'kup' & (cmd at: 2) = 'piwo' ifTrue: [
                    self buyBeer.
                    isUnknown := false
                ].

                isUnknown & (cmd at: 1) = 'obejrzyj' ifTrue: [
                    self printLines: (self interact: (cmd at: 2)).
                    isUnknown := false
                ].

                isUnknown & (cmd at: 1) = 'idź' ifTrue: [
                    self go: (cmd at: 2).
                    isUnknown := false
                ].

                isUnknown & (cmd at: 1) = 'podnieś' ifTrue: [
                    self pickUp: (cmd at: 2).
                    isUnknown := false
                ].

                isUnknown & (cmd at: 1) = 'utrać' ifTrue: [
                    self dropItem: (cmd at: 2).
                    isUnknown := false
                ].

                isUnknown & (cmd at: 1) = 'rozejrzyj' & (cmd at: 2) = 'się' ifTrue: [
                    self lookAround.
                    isUnknown := false
                ]
            ].



            (cmdWordsNumber = 3) ifTrue: [
                isUnknown & (cmd at: 1) = 'spytaj' ifTrue: [
                    self askPerson: (cmd at: 2) about: (cmd at: 3).
                    isUnknown := false
                ]
            ].



            (cmdWordsNumber = 4) ifTrue: [
                isUnknown & (cmd at: 1) = 'przygotuj' & (cmd at: 2) = 'się' & (cmd at: 3) = 'do' & (cmd at: 4) = 'walki' ifTrue: [
                    self attack.
                    isUnknown := false
                ]
            ].

            

            isUnknown ifTrue: [
                self printLines: #('Nieznana lub niepełna komenda. Być może brakuje argumentów.' '')
            ]
        ]
    ]
]

Game new run.
