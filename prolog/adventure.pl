/* Przygody Gerwanta z Riviery, by Drygaś Filip, Lew, Filip, Lipniacki Daniel. */
:- dynamic i_am_at/1, at/2, holding/1, forbiddenMove/1, has/1, wEkwipunku/1, spytaj/2, obejrzyj/1, rozmawiaj/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)), retractall(has(_)).
:- discontiguous obejrzyj/1, obejrzyjOK/1, path/3, obejrzyjOK/1, be/2, rozmawiajOK/1, at/2, wEkwipunku/1, spytajOK/2.

/*==========================================
============================================

                GAME WORLD

============================================
==========================================*/

/*=================== 
lokacja 1 - świętyDąb 
===================*/
i_am_at(świętyDąb).
obejrzyjOK(świętyDąb) :- write("
Gerwant z Riviery, szkoły nosacza, podróżuje już szlakiem wiele dni. Towarzyszy mu jedynie deszcz i jego wierny koń Knebel. Pewnego popołudnia dociera do Świętego Dębu. Niestety te sakramentalne miejsce zostało zbrukane ludzką krwią.


Zabójca potworów może udać się na:
- północ"), (forbiddenMove(polanaKolorozmawiajOKyDrwali)-> nl; nl, write("- wschód"), nl).

obejrzyjOK(ciało) :- write("Analiza Wiedźmina: Rany od szponów i kłów"), nl.
at(ciało,świętyDąb).

obejrzyjOK(totem) :- write("Analiza Gerwanta z Riviery: Totem został zbudowany ze szczątek i poroża jelenia. To wygląda na ostrzeżenie."), nl.
at(totem,świętyDąb).

rozmawiajOK(wieśniak) :- write("Wieśniak jest sparaliżowany strachem i majaczy."), nl.
be(wieśniak, świętyDąb).

path(świętyDąb, północ, wieśGrobla).
path(świętyDąb, wschód, polanaKolorozmawiajOKyDrwali).
forbiddenMove(polanaKolorozmawiajOKyDrwali).

/*===================== 
lokacja 2 - Wieś Grobla 
=====================*/

obejrzyjOK(wieśGrobla) :- write("
Gerwant dociera do Wsi Grobla.

Wiedźmak może udać się do:
- dom Sołtysa
- karczma
- południe"), nl.

obejrzyjOK(studnia) :- write("Rivierijczyk zauważa *gnomskiGwyhyr* na dnie studni"), nl.
at(studnia, wieśGrobla).

at(gnomskiGwyhyr, wieśGrobla).
wEkwipunku(gnomskiGwyhyr) :- write("Gwyhyr wykuty przez gnomy. Jest sprawnie naostrzony. Na klindze ma wyryte runy."), nl.
ukrytyPrzedmiot(gnomskiGwyhyr).


obejrzyjOK(tablicaOgłoszeń) :- write("Gerwant znajduje zlecenie na Potwora Lasu z podpisem *sołtys* wsi Grobla"), nl.
at(tablicaOgłoszeń, wieśGrobla).

path(wieśGrobla, domSołtysa, domSołtysa).
path(wieśGrobla, karczma, karczma).
path(wieśGrobla, południe, świętyDąb).

/*===================== 
lokacja 3 - Dom sołtysa 
=====================*/

obejrzyjOK(domSołtysa) :- write("
Zabójca potworów wchodzi do domu sołtysa

Gerwant z Riviery może udać się na:
- zewnątrz"), nl.

rozmawiajOK(sołtys) :- 
write("Gerwant z Riviery pyta o zlecenie na potwora. Sołtys opowiada mu o atakach potwora w lasach na południe od wioski. Proponuje porozmawiać z *ocalałymi drwalami*, przesiadującymi w karczmie i spytać ich o atak"), 
nl.

be(sołtys, domSołtysa).

path(domSołtysa, zewnątrz, wieśGrobla).

/*=====================
lokacja 4 - Karczma
=====================*/

obejrzyjOK(karczma) :- write("
Gerwant wchodzi do Karczmy

Możesz kupic piwo(komenda kupPiwo)

Rivierijczyk może wyjść na:
- zewnątrz"), nl.

rozmawiajOK(drwale) :- write("Dobre jest tutaj piwo!"), nl.
spytajOK(drwale, atak) :- 
write("Grupa drwali odpowiada, że zobaczyła wysokiego potwora z drewna i kości. Następnie zaatakowały ich wilki. Zostali zaatakowani koło rozmawiajOKy drwali na *wschód* od Świętego Dębu"), 
retract(forbiddenMove(polanaKolorozmawiajOKyDrwali)), 
nl.
be(drwale, karczma).

rozmawiajOK(karczmarz) :- write("Podróżniku, zapraszam na piwo!"), nl.
be(karczmarz, karczma).

path(karczma, zewnątrz, wieśGrobla).


/*==================================
lokacja 5 - Polana Koło Chaty Drwali
==================================*/

obejrzyjOK(polanaKolorozmawiajOKyDrwali) :- write("
Zabójca potworów znajduje się na polanie wokoło Chaty Drwali.

Gerwant z Riviery pójść w tych kierunkach:
- chata
- zachód
- puszcza"), nl.

obejrzyjOK(trup) :- write('Analiza Gerwanta z Riviery: "Poturbowany i mocno poobijany. Krew nie została wyssana, ale był pogryziony przez wilki"'), nl.
at(trup, polanaKolorozmawiajOKyDrwali).

path(polanaKolorozmawiajOKyDrwali, zachód, świętyDąb).
path(polanaKolorozmawiajOKyDrwali, puszcza, wielkaPuszcza).
path(polanaKolorozmawiajOKyDrwali, chata, chataDrwali).


/*======================
lokacja 6 - Chata Drwali
======================*/

obejrzyjOK(chataDrwali) :- write("
Łowca potworów wchodzi do Chaty Drwali

Gerwant może wyjść na: 
- zewnątrz"), nl.

obejrzyjOK(topór) :- write('Analiza Gerwanta z Riviery: "W rysach topora zastała się zaschnięta krew zwierzęcia"'), nl.
at(topór, chataDrwali).

wEkwipunku(mieszek) :- write("Mieszek pełen złota. W środku znajduje się 10 monet. Wystarczy na piwo."), nl.
at(mieszek, chataDrwali).

obejrzyjOK(czaszka) :- write('Analiza Gerwanta z Riviery: "Ta czaszka należy do młodego niedźwiedzia"'), nl.
at(czaszka, chataDrwali).

path(chataDrwali, zewnątrz, polanaKolorozmawiajOKyDrwali).


/*=================
lokacja 7 - Puszcza
=================*/

obejrzyjOK(wielkaPuszcza) :- write("
Mistrz Gerwant wkracza w samo serce puszczy.

Widźmak może się udać się w tych kierunkach:
- polana
- pieczara"), nl.

obejrzyjOK(statua) :- write('Analiza Gerwanta z Riviery: "Medalion Drży. To magiczny totem. Podobny totem widziałem pod Świętym dębem". Wiedźmin postanawia przeczytać bestiariusz. W wiedźmińskim almanachu Gerwant znajduje informację, że magiczne totemy to znak rozpoznawczny terytorium potwora zwanego jako *Leszy*.'), nl.
at(statua, wielkaPuszcza).



path(wielkaPuszcza, polana, polanaKolorozmawiajOKyDrwali).
path(wielkaPuszcza, pieczara, pieczaraBestii).


/*=================
lokacja 8 - Pieczara Bestii
=================*/

obejrzyjOK(pieczaraBestii) :- write("
Gerwant z Riviery wchodzi do pieczary. Gerwant wnioskuje, że natrafił na leże potwora. Monstrum nie ma w jaskini, więc może na niego zaczekać (komenda przygotujSięDoWalki). Aczkolwiek zanim wiedźmin zdecyduje się na spotkanie oko w oko z bestia powinien się porządnie przygotować, a zatem musi wywnioskować z jakim potworem ma do czynienia.

Wiedźmak może wrócić do:
- puszcza"), nl.

path(pieczaraBestii, puszcza, wielkaPuszcza).


/*==========================================
============================================

                GAME COMMANDS

============================================
==========================================*/


/* debug */
path(świętyDąb, drogaCzitera, pieczaraBestii).


przygotujSięDoWalki :-
        i_am_at(pieczaraBestii),
        write("Do walki z jakim potworem musi się przygotować Gerwant z Riviery?"), nl,
        read(X),
        (X == "Leszy" -> 
                write("Gerwant poprawnie wywnioskował, że czeka go starcie z Leszym i odpowiednio się przygotował. Dzięki temu wygrał walkę"), finish; 
                write("Gerwant niepoprawnie wywnioskował z jakim potworem czeka go starcie."), nl, die
        ).


obejrzyj(X) :-
        i_am_at(Place),
        ((at(X, Place) ; be(X, Place)) ->
                (obejrzyjOK(X) -> 
                        nl; 
                        write("Gerwant nie zaobserwował nic ciekawego na temat: "), write(X),nl
                );
                write("W lokacji "), write(Place), write(" nie ma "), write(X), nl
        ).

rozmawiaj(X) :-
        i_am_at(Place),
        ((at(X, Place) ; be(X, Place)) ->
                (rozmawiajOK(X) -> 
                        nl; 
                        write("Próba rozmowy z "), write(X), write(" kończy sie fiaskiem"),nl
                );
                write("W lokacji "), write(Place), write(" nie ma "), write(X), nl
        ).

spytaj(X, Y) :-
        i_am_at(Place),
        ((at(X, Place) ; be(X, Place)) ->
                (spytajOK(X, Y) -> 
                        nl; 
                        write(X), write(" nic nie wie o: "), write(Y), nl
                );
                write("W lokacji "), write(Place), write(" nie ma "), write(X), nl
        ).

kupPiwo :-
        i_am_at(karczma),
        (has(mieszek) -> 
                write("Gerwant pije piwo"), nl, utrać(mieszek); 
                write("Nie masz wystarczająco złota")
        ).

/* These rules define the direction letters as calls to idź/1. */


/* redundant */
n :- idź(n).

s :- idź(s).

e :- idź(e).

w :- idź(w).

wejdzDoKarczmy :- idź(karczma).

odwiedzSołtysa :- idź(domSołtysa).


/* This rule tells how to move in a given direction. */

idź(Direction) :-
        i_am_at(Here),
        path(Here, Direction, There),
        (not(forbiddenMove(There)) -> 
                retract(i_am_at(Here)), assert(i_am_at(There)), !, rozejrzyjSię; 
                write('Nie możesz tam iść')
        ).
idź(_) :-
        write('Nie możesz tam iść').


/* This rule tells how to rozejrzyjSię about you. */

rozejrzyjSię :-
        i_am_at(Place),
        obejrzyjOK(Place),
        nl,
        notice_objects_at(Place),
        notice_people_at(Place),
        nl.


/* These rules set up a loop to mention all the objects
   in your vicinity. */

notice_objects_at(Place) :-
        at(X, Place),
        not(ukrytyPrzedmiot(X)), write('Wiedźmin zauważa: '), write(X), nl,
        fail.

notice_objects_at(_).

notice_people_at(Place) :-
        be(X, Place),
        write('Wiedźmin może porozmawiać z: '), write(X), nl,
        fail.

notice_people_at(_).


/* inventory */

ekwipunek :-
        forall(has(X), (write(X), write('-'), wEkwipunku(X), nl)).


podnieś(X) :-
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assert(has(X)),
        write('Wziąłeś przedmiot '),
        write(X),
        !, nl.

utrać(X) :- 
        has(X),
        i_am_at(Place),
        retract(has(X)),
        assert(at(X, Place)),
        write('Utraciłeś przedmiot '),
        write(X),
        !, nl.




/* This rule tells how to die. */

die :-
        write('Gerwant umarł, a cała płeć żeńska na kontynencie uroniła łzę.'),
        finish.


/* Under UNIX, the "halt." command quits Prolog but does not
   remove the output window. On a PC, however, the window
   disappears before the final output can be seen. Hence this
   routine requests the user to perform the final "halt." */

finish :-
        nl,
        halt.


/* This rule just writes out game instructions. */

instructions :-
        nl,
        write('Wprowadź komendy zgodnie ze składnią prologa.'), nl,
        write('Dostępne komendy:'), nl,
        write('idź(kierunek).           -- by pójść w danym kierunku.'), nl,
        write('podnieś(obiekt).         -- by podnieść przedmiot.'), nl,
        write('utrać(obiekt).           -- by utracić przedmiot.'), nl,
        write('ekwipunek.               -- by sprawdzić swój ekwipunek.'), nl,
        write('obejrzyj(obiekt).        -- by obejrzeć przedmiot.'), nl,
        write('rozmawiaj(npc).          -- by porozmawiać z NPC.'), nl,
        write('spytaj(npc, coś).          -- by spytać NPC o coś.'), nl,
        write('rozejrzyjSię.            -- by dowiedzieć się, gdzie jesteś.'), nl,
        nl.

/* This rule prints out instructions and tells where you are. */

start :-
        instructions,
        rozejrzyjSię.
