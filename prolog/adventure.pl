/* Przygody Gerwanta z Riviery, by Drygaś Filip, Lew, Filip, Lipniacki Daniel. */
:- dynamic jestem_w/1, w_lokacji/2, trzyma/1, zakazany_ruch/1, has/1, wEkwipunku/1, spytaj/2, obejrzyj/1, rozmawiaj/1.
:- retractall(w_lokacji(_, _)), retractall(jestem_w(_)), retractall(alive(_)), retractall(has(_)).
:- discontiguous obejrzyj/1, obejrzyjOK/1, ścieżka/3, obejrzyjOK/1, jest/2, rozmawiajOK/1, w_lokacji/2, wEkwipunku/1, spytajOK/2.

/*==========================================
============================================

                GAME WORLD

============================================
==========================================*/

/*=================== 
lokacja 1 - świętyDąb 
===================*/
jestem_w(świętyDąb).
obejrzyjOK(świętyDąb) :- write("
Gerwant z Riviery, szkoły nosacza, podróżuje szlakami Królestw Północy już wiele dni.
Towarzyszy mu jedynie deszcz i jego wierny koń Piwonia. 
Pewnego popołudnia dociera do Świętego Dębu. 
Niestety te sakramentalne miejsce zostało zbrukane ludzką krwią.


Zabójca potworów może udać się na:
- północ"), (zakazany_ruch(polanaKoloChatyDrwali)-> nl; nl, write("- wschód"), nl).

obejrzyjOK(ciało) :- write("Analiza Wiedźmina: Rany od szponów i kłów"), nl.
w_lokacji(ciało,świętyDąb).

obejrzyjOK(totem) :- write("Analiza Gerwanta z Riviery: Totem został zbudowany ze szczątek i poroża jelenia. To wygląda na ostrzeżenie."), nl.
w_lokacji(totem,świętyDąb).

rozmawiajOK(wieśniak) :- write("Wieśniak jest sparaliżowany strachem i majaczy."), nl.
jest(wieśniak, świętyDąb).

ścieżka(świętyDąb, północ, wieśGrobla).
ścieżka(świętyDąb, wschód, polanaKoloChatyDrwali).
zakazany_ruch(polanaKoloChatyDrwali).

/*===================== 
lokacja 2 - Wieś Grobla 
=====================*/

obejrzyjOK(wieśGrobla) :- write("
Gerwant dociera do Wsi Grobla.

Wiedźmak może udać się do:
- dom Sołtysa
- karczma
- południe"), nl.

obejrzyjOK(studnia) :- 
(w_lokacji(gnomskiGwyhyr, wieśGrobla)-> write("Rivierijczyk zauważa *gnomskiGwyhyr* na dnie studni"), nl; write("Nie ma tu nic ciekawego"), nl).
w_lokacji(studnia, wieśGrobla).

w_lokacji(gnomskiGwyhyr, wieśGrobla).
wEkwipunku(gnomskiGwyhyr) :- write("Gwyhyr wykuty przez gnomy. Jest sprawnie naostrzony. Na klindze ma wyryte runy."), nl.
ukrytyPrzedmiot(gnomskiGwyhyr).


obejrzyjOK(tablicaOgłoszeń) :- write("Gerwant znajduje zlecenie na Potwora Lasu z podpisem *sołtys* wsi Grobla"), nl.
w_lokacji(tablicaOgłoszeń, wieśGrobla).

ścieżka(wieśGrobla, domSołtysa, domSołtysa).
ścieżka(wieśGrobla, karczma, karczma).
ścieżka(wieśGrobla, południe, świętyDąb).

/*===================== 
lokacja 3 - Dom sołtysa 
=====================*/

obejrzyjOK(domSołtysa) :- write("
Zabójca potworów wchodzi do domu sołtysa

Gerwant z Riviery może udać się na:
- zewnątrz"), nl.

rozmawiajOK(sołtys) :- 
write("Gerwant z Riviery pyta o zlecenie na potwora. Sołtys opowiada mu o atakach potwora w lasach na południe od wioski. Proponuje porozmawiać z *ocalałymi drwalami*, przesiadującymi w karczmie i spytać ich o *atak*"), 
nl.

jest(sołtys, domSołtysa).

ścieżka(domSołtysa, zewnątrz, wieśGrobla).

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
write("Grupa drwali odpowiada, że zobaczyła wysokiego potwora z drewna i kości. Następnie zatakowały ich wilki. Zostali napadnięci koło chaty drwali na *wschód* od Świętego Dębu"), 
retract(zakazany_ruch(polanaKoloChatyDrwali)), 
nl.
jest(drwale, karczma).

rozmawiajOK(karczmarz) :- write("Podróżniku, zapraszam na piwo!"), nl.
jest(karczmarz, karczma).

ścieżka(karczma, zewnątrz, wieśGrobla).


/*==================================
lokacja 5 - Polana Koło Chaty Drwali
==================================*/

obejrzyjOK(polanaKoloChatyDrwali) :- write("
Zabójca potworów znajduje się na polanie wokoło Chaty Drwali.

Gerwant z Riviery pójść w tych kierunkach:
- chata
- zachód
- puszcza"), nl.

obejrzyjOK(trup) :- write('Analiza Gerwanta z Riviery: "Poturbowany i mocno poobijany. Krew nie została wyssana, ale był pogryziony przez wilki"'), nl.
w_lokacji(trup, polanaKoloChatyDrwali).

ścieżka(polanaKoloChatyDrwali, zachód, świętyDąb).
ścieżka(polanaKoloChatyDrwali, puszcza, wielkaPuszcza).
ścieżka(polanaKoloChatyDrwali, chata, chataDrwali).


/*======================
lokacja 6 - chata Drwali
======================*/

obejrzyjOK(chataDrwali) :- write("
Łowca potworów wchodzi do chaty Drwali

Gerwant może wyjść na: 
- zewnątrz"), nl.

obejrzyjOK(topór) :- write('Analiza Gerwanta z Riviery: "W rysach topora zastała się zaschnięta krew zwierzęcia"'), nl.
w_lokacji(topór, chataDrwali).

wEkwipunku(mieszek) :- write("Mieszek pełen złota. W środku znajduje się 10 monet. Wystarczy na piwo."), nl.
w_lokacji(mieszek, chataDrwali).

obejrzyjOK(czaszka) :- write('Analiza Gerwanta z Riviery: "Ta czaszka należy do młodego niedźwiedzia"'), nl.
w_lokacji(czaszka, chataDrwali).

ścieżka(chataDrwali, zewnątrz, polanaKoloChatyDrwali).


/*=================
lokacja 7 - Puszcza
=================*/

obejrzyjOK(wielkaPuszcza) :- write("
Mistrz Gerwant wkracza w samo serce puszczy.

Widźmak może się udać się w tych kierunkach:
- polana
- pieczara"), nl.

obejrzyjOK(statua) :- write('Analiza Gerwanta z Riviery: "Medalion Drży. To magiczny totem. Podobny totem widziałem pod Świętym dębem". Wiedźmin postanawia przeczytać bestiariusz. W wiedźmińskim almanachu Gerwant znajduje informację, że magiczne totemy to znak rozpoznawczny terytorium potwora zwanego jako *Leszy*.'), nl.
w_lokacji(statua, wielkaPuszcza).



ścieżka(wielkaPuszcza, polana, polanaKoloChatyDrwali).
ścieżka(wielkaPuszcza, pieczara, pieczaraBestii).


/*=================
lokacja 8 - Pieczara Bestii
=================*/

obejrzyjOK(pieczaraBestii) :- write("
Gerwant z Riviery wchodzi do pieczary. Gerwant wnioskuje, że natrafił na leże potwora. Monstrum nie ma w jaskini, więc może na niego zaczekać (komenda przygotujSięDoWalki). Aczkolwiek, zanim wiedźmin zdecyduje się na spotkanie oko w oko z bestią powinien się porządnie przygotować, a zatem musi wywnioskować z jakim potworem ma do czynienia.

Wiedźmak może wrócić do:
- puszcza"), nl.

ścieżka(pieczaraBestii, puszcza, wielkaPuszcza).


/*==========================================
============================================

                GAME COMMANDS

============================================
==========================================*/


/* debug */
ścieżka(świętyDąb, drogaCzitera, pieczaraBestii).


przygotujSięDoWalki :-
        jestem_w(pieczaraBestii),
        write("Do walki z jakim potworem musi się przygotować Gerwant z Riviery?"), nl,
        read(X),
        (X == "Leszy" -> 
                write("Gerwant poprawnie wywnioskował, że czeka go starcie z Leszym i odpowiednio się przygotował. Dzięki temu wygrał walkę"), finish; 
                write("Gerwant niepoprawnie wywnioskował z jakim potworem czeka go starcie."), nl, zgiń
        ).


obejrzyj(X) :-
        jestem_w(Place),
        ((w_lokacji(X, Place) ; jest(X, Place)) ->
                (obejrzyjOK(X) -> 
                        nl; 
                        write("Gerwant nie zaobserwował nic ciekawego na temat: "), write(X),nl
                );
                write("W lokacji "), write(Place), write(" nie ma "), write(X), nl
        ).

rozmawiaj(X) :-
        jestem_w(Place),
        ((w_lokacji(X, Place) ; jest(X, Place)) ->
                (rozmawiajOK(X) -> 
                        nl; 
                        write("Próba rozmowy z "), write(X), write(" kończy sie fiaskiem. "),nl
                );
                write("W lokacji "), write(Place), write(" nie ma "), write(X), nl
        ).

spytaj(X, Y) :-
        jestem_w(Place),
        ((w_lokacji(X, Place) ; jest(X, Place)) ->
                (spytajOK(X, Y) -> 
                        nl; 
                        write(X), write(" nic nie wie o: "), write(Y), nl
                );
                write("W lokacji "), write(Place), write(" nie ma "), write(X), nl
        ).

kupPiwo :-
        jestem_w(karczma),
        (has(mieszek) -> 
                write("Gerwant pije piwo"), nl, utrać(mieszek); 
                write("Nie masz wystarczająco złota")
        ).

/* This rule tells how to move in a given direction. */

idź(Direction) :-
        jestem_w(Here),
        ścieżka(Here, Direction, There),
        (not(zakazany_ruch(There)) -> 
                retract(jestem_w(Here)), assert(jestem_w(There)), !, rozejrzyjSię; 
                write('Nie możesz tam iść')
        ).
idź(_) :-
        write('Nie możesz tam iść').


/* This rule tells how to rozejrzyjSię about you. */

rozejrzyjSię :-
        jestem_w(Place),
        obejrzyjOK(Place),
        nl,
        zauważ_obiekty_w_lokacji(Place),
        zauważ_postacie_w_lokacji(Place),
        nl.


/* These rules set up a loop to mention all the objects
   in your vicinity. */

zauważ_obiekty_w_lokacji(Place) :-
        w_lokacji(X, Place),
        not(ukrytyPrzedmiot(X)), write('Wiedźmin zauważa: '), write(X), nl,
        fail.

zauważ_obiekty_w_lokacji(_).

zauważ_postacie_w_lokacji(Place) :-
        jest(X, Place),
        write('Wiedźmin może porozmawiać z: '), write(X), nl,
        fail.

zauważ_postacie_w_lokacji(_).


/* inventory */

ekwipunek :-
        forall(has(X), (write(X), write('-'), wEkwipunku(X), nl)).


podnieś(X) :-
        jestem_w(Place),
        (w_lokacji(X, Place) ->
                retract(w_lokacji(X, Place)),
                assert(has(X)),
                write('Wziąłeś przedmiot '),
                write(X),
                !, nl
        ;
                write('Nie ma tu takiego przedmiotu.'),
                nl
        ).

utrać(X) :- 
        has(X),
        jestem_w(Place),
        retract(has(X)),
        assert(w_lokacji(X, Place)),
        write('Utraciłeś przedmiot '),
        write(X),
        !, nl.




/* This rule tells how to die. */

zgiń :-
        write('Gerwant umarł, a cała płeć żeńska na kontynencie uroniła łzę.'),
        ukończ.


/* Under UNIX, the "halt." command quits Prolog but does not
   remove the output window. On a PC, however, the window
   disappears before the final output can be seen. Hence this
   routine requests the user to perform the final "halt." */

ukończ :-
        nl,
        halt.


/* This rule just writes out game instructions. */

instrukcje :-
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
        instrukcje,
        rozejrzyjSię.
