:- dynamic position/3, mur/2.

grille_taille(10).

% Définir les directions de mouvement possibles
mouvement(gauche, -1, 0).
mouvement(droite, 1, 0).
mouvement(haut, 0, -1).
mouvement(bas, 0, 1).

% Vérifie si une position est dans les limites de la grille
dans_limites(X, Y) :-
    grille_taille(Taille),
    X >= 0, X < Taille,
    Y >= 0, Y < Taille.

% Vérifie si une case est libre (pas de mur, pas en dehors de la grille)
case_libre(X, Y) :-
    dans_limites(X, Y),
    \+ mur(X, Y).

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

poser_mur :-
    position(chat, CX, CY),
    grille_taille(Taille),
    Taille1 is Taille - 1,
    
    a_star(CX, CY, (X, Y, _)),

    assert(mur(X, Y)),
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
        write('Le chat s\'est déplacé en '), write(Direction), nl
    ; write('Déplacement impossible dans cette direction.'), nl).

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
    afficher_grille,
    poser_mur,

    % Condition de fin de jeu, le chat est bloqué
    (chat_bloque -> true ; joueur_deplace_chat, jouer_tour).

joueur_deplace_chat :-
    write('Entrez une direction pour déplacer le chat (gauche, droite, haut, bas) : '),
    read(Direction),

    (mouvement(Direction, _, _) -> deplacer_chat(Direction) ; write('Direction invalide.'), nl, joueur_deplace_chat).

demarrer :-
    write('Le jeu commence!'), nl,
    jouer_tour.

init :-
    assert(position(chat, 5, 5)). % Position initiale du chat

% Entrée principale
:- init, demarrer.
