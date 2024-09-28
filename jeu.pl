:- dynamic position/3, mur/2, walls/1, possibility/1, heuristic_possibilities/1.

walls([]).
heuristic_possibilities([]).
possibility(8).

grille_taille(10).

print_possibility :-
    possibility(N),
    write('Possibility: '), write(N), nl.


print_heuristic_possibilities :-
    heuristic_possibilities(List),
    write('Heuristic possibilities: '), write(List), nl.

print_walls([]).
print_walls([(X, Y)|Rest]) :-
    write('('), write(X), write(', '), write(Y), write(')'), nl,
    print_walls(Rest).

print_all_walls :-
    walls(WallList),
    print_walls(WallList).

add_tuple_to_walls(Tuple) :-
    walls(WallList),
    append(WallList, [Tuple], NewWallList),
    retract(walls(WallList)),
    assert(walls(NewWallList)).

add_possibility(N) :-
    heuristic_possibilities(List),
    append(List, [N], NewList),
    retract(heuristic_possibilities(List)),
    assert(heuristic_possibilities(NewList)).

remove_tuple_from_walls(Tuple) :-
    walls(WallList),
    delete(WallList, Tuple, NewWallList),
    retract(walls(WallList)),
    assert(walls(NewWallList)).

test_add_tuple_to_walls :-
    add_tuple_to_walls((1, 1)),
    remove_tuple_from_walls((1, 1)),
    walls(NewWallList).

compte_elements([], 0).

% règle récursive : pour chaque élément, on ajoute 1 au compteur
compte_elements([_|T], N) :-
    compte_elements(T, N1),
    N is N1 + 1.


% Définir les directions de mouvement possibles
mouvement(gauche, -1, 0).
mouvement(droite, 1, 0).
mouvement(haut, 0, -1).
mouvement(bas, 0, 1).
mouvement(haut_gauche, -1, -1).
mouvement(haut_droite, 1, -1).
mouvement(bas_gauche, -1, 1).
mouvement(bas_droite, 1, 1).

% Vérifie si une position est dans les limites de la grille
dans_limites(X, Y) :-
    grille_taille(Taille),
    X >= 0, X < Taille,
    Y >= 0, Y < Taille.

% Vérifie si une case est libre (pas de mur, pas en dehors de la grille)
case_libre(X, Y) :-
    dans_limites(X, Y),
    \+ mur(X, Y).


check_case_for_possibility(CX, CY, Direction, N, N1) :-
    mouvement(Direction, DX, DY),
    NX is CX + DX,
    NY is CY + DY,
    (\+ case_libre(NX, NY) ->
        N1 is N - 1
    ;
        N1 = N
    ).

get_all_cases_around_chat(Cases) :-
    position(chat, CX, CY),
    findall((X, Y), (mouvement(_, DX, DY), NX is CX + DX, NY is CY + DY, case_libre(NX, NY)), Cases).

check_all_cases_for_possibility(CX, CY) :-
    retract(possibility(_)),
    assert(possibility(8)),
    possibility(Possibility),
    check_case_for_possibility(CX, CY, gauche, Possibility, Possibility1),
    check_case_for_possibility(CX, CY, droite, Possibility1, Possibility2),
    check_case_for_possibility(CX, CY, haut, Possibility2, Possibility3),
    check_case_for_possibility(CX, CY, bas, Possibility3, Possibility4),
    check_case_for_possibility(CX, CY, haut_gauche, Possibility4, Possibility5),
    check_case_for_possibility(CX, CY, haut_droite, Possibility5, Possibility6),
    check_case_for_possibility(CX, CY, bas_gauche, Possibility6, Possibility7),
    check_case_for_possibility(CX, CY, bas_droite, Possibility7, PossibilityFinale),
    retract(possibility(_)),
    assert(possibility(PossibilityFinale)),
    add_possibility(PossibilityFinale).

check_all_posibilities_for_all_movements(CX, CY) :-
    retract(heuristic_possibilities(_)),
    assert(heuristic_possibilities([])),
    forall(mouvement(Direction, DX, DY), check_all_cases_for_possibility(CX + DX, CY + DY)).

check_possibility(position(chat, X, Y)) :-
    distance_manhattan(X, Y, 0, 0, Distance),
    Possibility is Possibility - Distance.

negatif(X, Y) :-
    Y is -X.

% Génère les positions autour de (CX, CY) avec un rayon Distance.
positions_autour(CX, CY, Distance, Positions) :-
    negatif(Distance, NegDistance),
    findall((NX, NY),
        (between(NegDistance, Distance, DX),
         between(NegDistance, Distance, DY),
         \+ (DX = 0, DY = 0),  % On exclut la position actuelle du chat
         NX is CX + DX,
         NY is CY + DY),
    Positions).


cases_libres_autour([], []).
cases_libres_autour([(X, Y)|Rest], [(X, Y)|CasesLibres]) :-
    case_libre(X, Y), !,
    cases_libres_autour(Rest, CasesLibres).
cases_libres_autour([_|Rest], CasesLibres) :-
    cases_libres_autour(Rest, CasesLibres).


cases_libres_autour_chat(Distances, CasesLibres) :-
    position(chat, CX, CY),
    findall(CasesLibresDist,
        (member(Distance, Distances),
         positions_autour(CX, CY, Distance, Positions),
         cases_libres_autour(Positions, CasesLibresDist)),
    CasesLibresNested),
    flatten(CasesLibresNested, CasesLibres).

% Distance de Manhattan entre deux points
distance_manhattan(X1, Y1, X2, Y2, Distance) :-
    Distance is abs(X1 - X2) + abs(Y1 - Y2).


heuristique(X1, Y1, X2, Y2, Heuristique) :-
    distance_manhattan(X1, Y1, X2, Y2, Heuristique).

g(X, Y, G) :-
    % On suppose que le coût g(n) est la distance à partir du centre de la grille
    position(chat, CX, CY),
    distance_manhattan(CX, CY, X, Y, G).

% Convertir un tuple en un autre tuple, nécessaire a cause de sort
tuple_conversion((X, Y, Z), ((X, Y), Z)).

% convertir dans l'autre sens pour avoir le bon format de possage de mur
back_conversion(((X, Y), Z), (X, Y, Z)).

a_star(CX, CY, CaseBloquer) :-
    grille_taille(Taille),
    Taille1 is Taille - 1,
    findall((X, Y), (between(0, Taille1, X), between(0, Taille1, Y), case_libre(X, Y)), CasesLibres), % Trouver toutes les cases libres
    maplist(a_star_heuristique(CX, CY), CasesLibres, CasesAvecF), % Calculer g(n) + h(n) pour chaque case libre

    maplist(tuple_conversion, CasesAvecF, CasesAvecFTuples), % Convertir chaque case avec f(n) en (Case, f(n)) pour pouvoir le sort
    % write('Cases avec f(n) (tuples): '), write(CasesAvecFTuples), nl,

    sort(2, @=<, CasesAvecFTuples, SortedCases), % on trie par f(n)
    % write('Cases triées par f(n): '), write(SortedCases), nl,

    nth0(0, SortedCases, FirstElement), 

    back_conversion(FirstElement, FirstElementBack),

    CaseBloquer = FirstElementBack,
    write('Case bloquante: '), write(CaseBloquer), nl.
    
% Calculer f(n) pour une case donnée
a_star_heuristique(CX, CY, (X, Y), (X, Y, F)) :-
    % write('Calcul de f(n) pour ('), write(X), write(Y), write(')...'), nl,
    heuristique(X, Y, CX, CY, H),
    % write('h(n) = '), write(H), nl,
    g(X, Y, G),
    % write('g(n) = '), write(G), nl,
    F is G + H.
    % write('f(n) = '). write(F), nl.

min_max([(LX, LY) | Rest]) :-
    position(chat, CX, CY),
    walls(WallList),
    TmpWalls is WallList,
    append(TmpWalls, [(LX, LY)], NewTmpWalls),

    min_max(Rest).


% Pose un mur pour bloquer le chat
poser_mur :-
    position(chat, CX, CY),
    grille_taille(Taille),
    Taille1 is Taille - 1,
    cases_libres_autour_chat([3], PossibleCases),
    a_star(CX, CY, (X, Y, _)),


    assert(mur(X, Y)),
    % assert(walls([(X, Y)])),
    add_tuple_to_walls((X, Y)),
    write('L\'IA a pose un mur en ('), write(X), write(Y), write(').'), nl.

% Calcule la distance pour chaque case libre par rapport au chat
distance_vers_chat(CX, CY, (X, Y), (X, Y, Distance)) :-
    distance_manhattan(CX, CY, X, Y, Distance).

chat_bloque :-
    position(chat, CX, CY),
    forall(mouvement(_, DX, DY),
           (NX is CX + DX, NY is CY + DY, \+ case_libre(NX, NY))),
    write('Le chat est bloqué! L\'IA a gagné!'), nl.

deplacer_chat(Direction) :-
    mouvement(Direction, DX, DY),
    position(chat, CX, CY),
    NX is CX + DX,
    NY is CY + DY,
    (case_libre(NX, NY) ->
        retract(position(chat, CX, CY)),
        assert(position(chat, NX, NY)),
        write('Le chat s\'est déplacé en '), write(Direction), nl, true
    ; write('Déplacement impossible dans cette direction.'), nl, false).

afficher_grille :-
    grille_taille(Taille),
    MaxIndex is Taille - 1,
    afficher_lignes(0, MaxIndex).

afficher_lignes(Y, MaxIndex) :-
    Y =< MaxIndex,
    afficher_ligne(0, MaxIndex, Y),
    nl,
    Y1 is Y + 1,
    afficher_lignes(Y1, MaxIndex).
afficher_lignes(_, _).

afficher_ligne(X, MaxIndex, Y) :-
    X =< MaxIndex,
    (position(chat, X, Y) -> write(' C ')
    ; mur(X, Y) -> write(' # ')
    ; write(' . ')),
    X1 is X + 1,
    afficher_ligne(X1, MaxIndex, Y).
afficher_ligne(_, _, _).

% Boucle de jeu
jouer_tour :-
    position(chat, X, Y),
    cases_libres_autour_chat([1], Cases),
    compte_elements(Cases, N),
    write('Cases autour du chat: '), write(Cases), nl,
    write('Nombre de cases autour du chat: '), write(N), nl,
    check_all_posibilities_for_all_movements(X, Y),
    write('HEURISTIC Possibilities: '), print_heuristic_possibilities,
    test_add_tuple_to_walls,
    print_all_walls,
    write('Boucle'), nl,
    write('Walls: '), write(mur), nl,
    poser_mur,
    afficher_grille,

    % Condition de fin de jeu, le chat est bloqué
    (chat_bloque -> true ; joueur_deplace_chat, jouer_tour).

joueur_deplace_chat :-
    write('Entrez une direction pour déplacer le chat (gauche, droite, haut, bas, haut_gauche, haut_droite, bas_gauche, bas) : '),
    read(Direction),

    (mouvement(Direction, _, _) -> deplacer_chat(Direction) ; write('Direction invalide.'), nl, joueur_deplace_chat).

demarrer :-
    write('Le jeu commence!'), nl,
    jouer_tour.

init :-
    assert(position(chat, 5, 5)). % Position initiale du chat

% Entrée principale
:- init, demarrer.