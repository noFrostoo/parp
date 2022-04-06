/* <The name of this game>, by <your name goes here>. */
 
/* TODO:
games fackts
inventory

interact 
talk
invetory
kup piwo


 */
:- dynamic i_am_at/1, at/2, holding/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)), retractall(has(_)).


/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */
/* things to implement */

/* lokacja 1 - swiętyDąb */
i_am_at(swiętyDąb).

describe(swiętyDąb) :- write("
Gerwant z Riviery podróżując szlakiem dociera do Świętego Dębu. Niestety te sakramentalne miejsce zostało zbrukane ludzką krwią.

Zabójca potworów może udać się na:
- północ
"), nl.

describe(cialo) :- write("Analiza Wiedźmina: Rany od szponów i kłów"), nl.
describe(totem) :- write("Analiza Gerwanta z Riviery: Totem został zbudowany ze szczątek i poroża jelenia. To wygląda na ostrzeżenie."), nl.

chat(wiesniak) :- write("Wieśniak jest sparaliżowane strachem i majaczy"), nl.

at(cialo,swiętyDąb).
at(totem,swiętyDąb).

be(wiesniak ,swiętyDąb).

path(swiętyDąb, n, wieśGrobla).
path(swiętyDąb, n, wieśGrobla).
forbiddenMove(polanaKoloChatyDrwali).

/* lokacja 2 - Wieś Grobla */

describe(wieśGrobla) :- write("
Gerwant dociera do Wsi Grobla.

Wiedźmak może udać się do:
- dom Sołtysa (komenda odwiedzSołtysa)
- karczma (komenda wejdzDoKarczmy)
"), nl.

describe(studnia) :- write("Znalezienie miecza *Gnomski Gwyhyr*"), nl.
describe(tablicaOgłoszeń) :- write("Gerwant znajduje Zlecenie na *Potwora Lasu* z podpisem *sołtys* wsi Grobla"), nl.

at(studnia, wieśGrobla).
at(tablicaOgłoszeń, wieśGrobla)

path(wieśGrobla, domSołtysa,domSołtysa).
path(wieśGrobla, karczma, karczma).

/* lokacja 3 - Dom sołtysa */

describe(domSołtysa) :- write("
Zabójca potworów wchodzi do domu sołtysa

Gerwant z Riviery może udać się na:
- zewnątrz (komenda wyjdz)
"), nl.

be(sołtys, domSołtysa).

chat(sołtys) :- write("Gerwant z Riviery pyta o zlecenie na potwora. Sołtys opowiada mu o atakach potwora w lasach na południe od wioski. Proponuje porozmawiać z *ocalałymi drwalami*, przesiadującymi w karczmie i spytać ich o *atak*"), nl.

path(domSołtysa, wyjdz, wieśGrobla).

/* lokacja 4 - Wieś Grobla */

describe(karczma) :- write("
Gerwant wchodzi do Karczmy

Możesz kupic piwo(komenda buy_beer)

Rivierijczyk może wyjść na:
- zewnątrz (komenda wyjdz)
"), nl.

be(drwale, karczma).
be(karczmarz, karczma).

chat(drwale) :- write("Grupa drwali odpowiada, że zobaczyła wysokiego potwora z drewna i kości. Następnie zaatakowały ich wilki. Zostali zaatakowani koło Chaty drwali na *wschód* od Świętego Dębu"), retract(forbiddenMove(polanaKoloChatyDrwali)) nl.
chat(karczmarz) :- write("Pordóżniku, zapraszam na piwo"), nl.

path(karczmarz, wyjdz, wieśGrobla).








in_inventory(kubek) :- write("KUBEK, staty: ."), nl.
in_inventory(lll) :- write("lll, staty: ."), nl.



interact(X) :-
        i_am_at(Place),
        at(X, Place),
        describe(X).

talk(X) :-
        i_am_at(Place),
        be(X, Place),
        chat(X).

buy_beer :-
        i_am_at(karczma),
        has(mieszek),
        write("Gerwant pije piwo").

buy_beer :-
        write("Nie masz tyle hajsu").

/* These rules describe how to pick up an object. */

take(X) :-
        holding(X),
        write('You''re already holding it!'),
        !, nl.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assert(holding(X)),
        write('OK.'),
        !, nl.

take(_) :-
        write('I don''t see it here.'),
        nl.


/* These rules describe how to put down an object. */

drop(X) :-
        holding(X),
        i_am_at(Place),
        retract(holding(X)),
        assert(at(X, Place)),
        write('OK.'),
        !, nl.

drop(_) :-
        write('You aren''t holding it!'),
        nl.


/* These rules define the direction letters as calls to go/1. */

n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).

wejdzDoKarczmy :- go(karczma).

odwiedzSołtysa :- go(domSołtysa).

wyjdz :- go(wyjdz).

/* This rule tells how to move in a given direction. */

go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, There),
        forbiddenMove(There),
        write('Nie mozesz tam iść').

go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, There),
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
        !, look.

go(_) :-
        write('Nie mozesz tam iść').


/* This rule tells how to look about you. */

look :-
        i_am_at(Place),
        describe(Place),
        nl,
        notice_objects_at(Place),
        notice_people_at(Place),
        nl.


/* These rules set up a loop to mention all the objects
   in your vicinity. */

notice_objects_at(Place) :-
        at(X, Place),
        write('Wiedźmin zauważa:'), write(X), write(' tutaj.'), nl,
        fail.

notice_objects_at(_).

notice_people_at(Place) :-
        be(X, Place),
        write('Wiedźmin może pogadać z: '), write(X), write(' tutaj.'), nl,
        fail.

notice_people_at(_).


/* inventory */

list_items :-
        forall(has(X), in_inventory(X)).


inventory(X) :-
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assert(has(X)),
        write('You took item into inventory.'),
        !, nl.

remove_item(X) :- 
        has(X),
        i_am_at(Place),
        retract(has(X)),
        assert(at(X, Place)),
        write('You droped item.'),
        !, nl.

/*FIXME!!!!!!!: if holds item */
equip_item(_) :-
        holding(item),
        write('You are holding an item.'),
        !, nl.

equip_item(X) :-
        has(X),
        retract(has(X)),
        assert(holding(X)),
        write('You droped item.'),
        !, nl.



/* This rule tells how to die. */

die :-
        finish.


/* Under UNIX, the "halt." command quits Prolog but does not
   remove the output window. On a PC, however, the window
   disappears before the final output can be seen. Hence this
   routine requests the user to perform the final "halt." */

finish :-
        nl,
        write('The game is over. Please enter the "halt." command.'),
        nl.


/* This rule just writes out game instructions. */

instructions :-
        nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.             -- to start the game.'), nl,
        write('n.  s.  e.  w.     -- to go in that direction.'), nl,
        write('take(Object).      -- to pick up an object.'), nl,
        write('drop(Object).      -- to put down an object.'), nl,
        write('look.              -- to look around you again.'), nl,
        write('instructions.      -- to see this message again.'), nl,
        write('halt.              -- to end the game and quit.'), nl,
        nl.


/* This rule prints out instructions and tells where you are. */

start :-
        instructions,
        look.




