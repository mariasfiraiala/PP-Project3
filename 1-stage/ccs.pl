:- dynamic detailed_mode_disabled/0.
:- ensure_loaded('files.pl').

% tile/2
% tile(Index, Tile)
%
% Fiecare soluție a predicatului tile este o corespondență între index
% (numărul piesei în lista din enunț) și reprezentarea internă a piesei
% respective.
%
% Puteți alege orice reprezentare doriți, în așa fel încât să puteți
% reprezenta toate piesele din enunț.
%
% Orice muchie a unei piese este o cetate, un drum, sau o pajiște.
% Pot exista cel mult 1 drum și cel mult 2 castele pe aceeași piesă.
%
% Reprezentarea trebuie să poată fi rotită (vezi predicatul ccw/3 mai
% jos) pentru a produce reprezentarea piesei rotite cu 90 de grade.
%
% Trebuie să definiți reprezentări pentru fiecare dintre cele 16 piese
% din enunțul temei.
%
% Exemplu: apelul tile(1, T1). trebuie să lege T1 la reprezentarea pe
% care o faceți pentru piesa 1. Această reprezentare poate fi transmisă
% celorlalte predicate din temă, pentru a întreba, de exemplu, ce se
% află pe muchia de nord a piesei 1, sau dacă piesa 1 se potrivește cu o
% altă piesă.

tile(1, Tile) :- Tile = [c, c, p, c, 0].
tile(2, Tile) :- Tile = [c, c, d, c, 0].
tile(3, Tile) :- Tile = [c, c, p, p, 0].
tile(4, Tile) :- Tile = [c, c, p, p, 1].
tile(5, Tile) :- Tile = [c, p, c, p, 1].
tile(6, Tile) :- Tile = [c, p, c, p, 0].
tile(7, Tile) :- Tile = [c, p, p, p, 0].
tile(8, Tile) :- Tile = [c, c, d, d, 0].
tile(9, Tile) :- Tile = [c, p, d, d, 0].
tile(10, Tile) :- Tile = [c, d, d, p, 0].
tile(11, Tile) :- Tile = [c, d, p, d, 0].
tile(12, Tile) :- Tile = [c, d, d, d, 0].
tile(13, Tile) :- Tile = [p, p, d, d, 0].
tile(14, Tile) :- Tile = [p, d, p, d, 0].
tile(15, Tile) :- Tile = [p, d, d, d, 0].
tile(16, Tile) :- Tile = [d, d, d, d, 0].



% at/3
% at(+Tile, +Direction, ?What)
%
% Predicatul este adevărat dacă pe piesa Tile are pe muchia de pe
% direcția Direction o entitate de tipul What.
%
% Directions este una dintre n, e, s, w (vezi predicatul directions/1
% din utils.pl).
%
% Entitatea (What) este una dintre c, d, sau p. reprezentând cetate,
% drum, sau pajiște.
%
% De exemplu, piesa 4 are cetate în nord și în este, și pajiște în sud
% și vest. Iar piesa 10 are cetate în nord, drum în este și sud, și
% pajiște în vest.
%
% Dacă What nu este legat, trebuie legat la entitatea care se află pe
% muchia din direcția Dir.
at(Tile, n, What) :- Tile = [What, _, _, _, _].
at(Tile, e, What) :- Tile = [_, What, _, _, _].
at(Tile, s, What) :- Tile = [_, _, What, _, _].
at(Tile, w, What) :- Tile = [_, _, _, What, _].


% atL/3
% atL(+Tile, +Directions, +What)
%
% Predicatul este adevărat dacă piesa Tile are entitatea what pe toate
% direcțiile din lista Directions, cu aceleași valori pentru entități și
% direcții ca și la predicatul at/3.
%
% De exemplu, predicatul este adevărat pentru reprezentarea piesei 1,
% pentru lista [w,n,e], și pentru entitatea c. Este adevărat de asemenea
% pentru reprezentarea piesei 14, pentru lista [e,w], și pentru
% entitatea d.
%
% Atenție! Pentru ca predicatul să fie adevărat, nu este nevoie ca în
% Directions să fie *toate* direcțiile pe care se află entitatea
% respectivă, pot fi doar o submulțime a acestora.
% De exemplu, la piesa 14, predicatul este adevărat pentru entitatea d
% și pentru oricare dintre listele [w], [e] sau [e,w].
atL(_, [], _) :- true.
atL(Tile, [D|Dir], What) :- at(Tile, D, What), atL(Tile, Dir, What).


% hasTwoCitadels/1
% hasTwoCitadels(+Tile)
%
% Predicatul întoarce adevărat dacă pe piesă există două cetăți diferite
% (ca în piesele 4 și 5).
hasTwoCitadels([_, _, _, _, 1]) :- true.


% ccw/3
% ccw(+Tile, +Rotation, -RotatedTile)
% Predicatul este adevărat dacă RotatedTile este reprezentarea piesei cu
% reprezentarea Tile, dar rotită de Rotation ori, în sens trigonometric.
%
% De exemplu, dacă T4 este reprezentarea piesei 4, atunci ccw(4, 1, R)
% va lega R la reprezentarea unei piese care are pajiște la nord și
% vest, și cetate la est și sud.
%
% Pentru piesele cu simetrie, reprezentarea unora dintre rotații este
% identică cu piesa inițială.
% De exemplu, la piesele 5, 6 și 14, rotirea cu Rotation=2 va duce la o
% reprezentare identică cu piesa inițială, și la fel rezultatele pentru
% Rotation=1 și Rotation=3 vor fi identice.
% La piesa 16, orice rotire trebuie să aibă aceeași reprezentare cu
% reprezentarea inițială.

ccw([P1, P2, P3, P4, _], 0, RotatedTile) :- RotatedTile = [P1, P2, P3, P4, _].
ccw([P1, P2, P3, P4, _], 1, RotatedTile) :- RotatedTile = [P2, P3, P4, P1, _].
ccw([P1, P2, P3, P4, _], 2, RotatedTile) :- RotatedTile = [P3, P4, P1, P2, _].
ccw([P1, P2, P3, P4, _], 3, RotatedTile) :- RotatedTile = [P4, P1, P2, P3, _].
ccw(Tile, Rotation, RotatedTile) :- Rotation > 3, NewRotation is Rotation mod 4, ccw(Tile, NewRotation, RotatedTile).


% rotations/2
% rotations(+Tile, -RotationPairs)
%
% Predicatul leagă RotationPairs la o listă de perechi
% (Rotation, RotatedTile)
% în care Rotation este un număr de rotații între 0 și 3 inclusiv și
% RotatedTile este reprezentarea piesei Tile rotită cu numărul respectiv
% de rotații.
%
% Rezultatul trebuie întotdeauna să conțină perechea (0, Tile).
%
% IMPORTANT:
% Rezultatul nu trebuie să conțină rotații duplicate. De exemplu, pentru
% piesele 5,6 și 14 rezultatul va conține doar 2 perechi, iar pentru
% piesa 16 rezultatul va conține o singură pereche.
%
% Folosiți recursivitate (nu meta-predicate).
removeDuplicate(List, List, (_, Tile)) :- member((_, Tile), List).
removeDuplicate(List, [RotationPair | List], RotationPair) :- true.

helperDontKillMeAndrei(Tile, 0, [(0, Tile)]) :- true.
helperDontKillMeAndrei(Tile, Rot, List) :- NewRot is Rot - 1,
                                           ccw(Tile, Rot, RotatedTile),
                                           helperDontKillMeAndrei(Tile, NewRot, NewList),
                                           removeDuplicate(NewList, List, (Rot, RotatedTile)).

rotations(Tile, RotationPairs) :- helperDontKillMeAndrei(Tile, 3, RotationPairs).


% match/3
% match(+Tile, +NeighborTile, +NeighborDirection)
%
% Predicatul întoarce adevărat dacă NeighborTile poate fi pusă în
% direcția NeighborDirection față de Tile și se potrivește, adică muchia
% comună este de același fel.
%
% De exemplu, dacă T2 este reprezentarea piesei 2, iar T16 este
% reprezentarea piesei 16, atunci match(T2, T16, s) este adevărat.
%
% Similar, pentru piesele 8 și 10, este adevărat
% ccw(T8, 3, T8R), match(T8R, T10, w).
%
% Puteți folosi predicatul opposite/2 din utils.pl.
match([Pa1, _, _, _, _], [_, _, Pb1, _, _], n) :- Pa1 == Pb1.
match([_, Pa2, _, _, _], [_, _, _, Pb2, _], e) :- Pa2 == Pb2.
match([_, _, Pa3, _, _], [Pb3, _, _, _, _], s) :- Pa3 == Pb3.
match([_, _, _, Pa4, _], [_, Pb4, _, _, _], w) :- Pa4 == Pb4.


% findRotation/3
% findRotation(+Tile, +Neighbors, -Rotation)
%
% Predicatul leagă Rotation la rotația (între 0 și 3 inclusiv) pentru
% piesa cu reprezentarea Tile, astfel încât piesa să se potrivească cu
% vecinii din Neighbors.
%
% Neighbors este o listă de perechi (NeighborTile, NeighborDirection) și
% specifică că pe direcția NeighborDirection se află piesa cu
% reprezentarea NeighborTile. Este posibil ca Neighbors să conțină mai
% puțin de 4 elemente.
%
% Se vor da toate soluțiile care duc la potrivire.
%
% De exemplu, pentru piesa 11, dacă la nord se află piesa 14 rotită o
% dată (drumul este vertical), iar la sud se află piesa 2 rotită de 2
% ori (drumul este spre nord), atunci posibilele rotații pentru piesa 11
% sunt 1 sau 3, deci findRotation trebuie să aibă 2 soluții, în care
% leagă R la 1, și la 3.
% În același exemplu, dacă am avea și piesa 1 ca vecin spre est, atunci
% soluția de mai sus s-ar reduce doar la rotația 3.
%RotatedTile
% Hint: Prolog face backtracking automat. Folosiți match/3.
findRotation(Tile, Neighbours, Rotations) :- rotations(Tile, Pairs),
                                             member((Rotations, RotatedTile), Pairs), forall(member((NTile, NDir), Neighbours), match(RotatedTile, NTile, NDir)).



