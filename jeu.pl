:- dynamic position/3, mur/2.

% Taille de la grille
grille_taille(10).

% Position initiale du chat
position(chat, 5, 5).

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

% Déplacer le chat si la case cible est libre
deplacer_chat(Direction) :-
    position(chat, CX, CY),
    mouvement(Direction, DX, DY),
    NX is CX + DX,
    NY is CY + DY,
    case_libre(NX, NY),
    retract(position(chat, CX, CY)),
    assert(position(chat, NX, NY)),
    write('Le chat s\'est déplacé en '), write(Direction), nl.

% IA pose un mur de manière aléatoire sur une case libre
poser_mur :-
    grille_taille(Taille),
    Taille1 is Taille - 1,  % Calcul correct de la taille maximale de la grille
    random_between(0, Taille1, X),
    random_between(0, Taille1, Y),
    \+ mur(X, Y),  % Vérifie si un mur existe déjà à cet endroit
    assert(mur(X, Y)),
    write('L\'IA a posé un mur en ('), write(X), write(', '), write(Y), write(').'), nl.

% Vérifier si le chat est bloqué (aucun mouvement possible)
chat_bloque :-
    position(chat, CX, CY),
    forall(mouvement(_, DX, DY),
           (NX is CX + DX, NY is CY + DY, \+ case_libre(NX, NY))),
    write('Le chat est bloqué! L\'IA a gagné!'), nl.

% Affichage de la grille
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
    poser_mur, % IA place un mur à chaque tour
    (chat_bloque -> true ; joueur_deplace_chat, jouer_tour).

% Demande à l'utilisateur de déplacer le chat
joueur_deplace_chat :-
    write('Entrez une direction pour déplacer le chat (gauche, droite, haut, bas) : '),
    read(Direction),
    deplacer_chat(Direction).

% Démarrage du jeu
demarrer :-
    write('Le jeu commence!'), nl,
    jouer_tour.

% Entrée principale
:- demarrer.
