% Représentation du graphe pour un échiquier, qui est ici notre plateau de jeu.
% Chaque prédicat connect(X, Y) indique que la case X est connectée aux cases de la liste Y, 
% c'est la manière du programme de gérer les mouvements possibles dans la boucle de jeu.
connect(a1, [a2, b1]).
connect(a2, [a1, a3]).
connect(a3, [a2, a4, b3]).
connect(a4, [a3, a5, b4]).
connect(a5, [a4, a6, b5]).
connect(a6, [a5, a7, b6]).
connect(a7, [a6, a8, b7]).
connect(a8, [a7, b8]).
connect(b1, [a1, b2, c1]).
connect(b2, [b1, b3, a2, c2]).
connect(b3, [b2, b4, a3, c3]).
connect(b4, [b3, b5, a4, c4]).
connect(b5, [b4, b6, a5, c5]).
connect(b6, [b5, b7, a6, c6]).
connect(b7, [b6, b8, a7, c7]).
connect(b8, [b7, a8, c8]).
connect(c1, [b1, c2, d1]).
connect(c2, [c1, c3, b2, d2]).
connect(c3, [c2, c4, b3, d3]).
connect(c4, [c3, c5, b4, d4]).
connect(c5, [c4, c6, b5, d5]).
connect(c6, [c5, c7, b6, d6]).
connect(c7, [c6, c8, b7, d7]).
connect(c8, [c7, b8, d8]).
connect(d1, [c1, d2, e1]).
connect(d2, [d1, d3, c2, e2]).
connect(d3, [d2, d4, c3, e3]).
connect(d4, [d3, d5, c4, e4]).
connect(d5, [d4, d6, c5, e5]).
connect(d6, [d5, d7, c6, e6]).
connect(d7, [d6, d8, c7, e7]).
connect(d8, [d7, c8, e8]).
connect(e1, [d1, e2, f1]).
connect(e2, [e1, e3, d2, f2]).
connect(e3, [e2, e4, d3, f3]).
connect(e4, [e3, e5, d4, f4]).
connect(e5, [e4, e6, d5, f5]).
connect(e6, [e5, e7, d6, f6]).
connect(e7, [e6, e8, d7, f7]).
connect(e8, [e7, d8, f8]).
connect(f1, [e1, f2, g1]).
connect(f2, [f1, f3, e2, g2]).
connect(f3, [f2, f4, e3, g3]).
connect(f4, [f3, f5, e4, g4]).
connect(f5, [f4, f6, e5, g5]).
connect(f6, [f5, f7, e6, g6]).
connect(f7, [f6, f8, e7, g7]).
connect(f8, [f7, e8, g8]).
connect(g1, [f1, g2, h1]).
connect(g2, [g1, g3, f2, h2]).
connect(g3, [g2, g4, f3, h3]).
connect(g4, [g3, g5, f4, h4]).
connect(g5, [g4, g6, f5, h5]).
connect(g6, [g5, g7, f6, h6]).
connect(g7, [g6, g8, f7, h7]).
connect(g8, [g7, f8, h8]).
connect(h1, [g1, h2]).
connect(h2, [h1, h3, g2]).
connect(h3, [h2, h4, g3]).
connect(h4, [h3, h5, g4]).
connect(h5, [h4, h6, g5]).
connect(h6, [h5, h7, g6]).
connect(h7, [h6, h8, g7]).
connect(h8, [h7, g8]).

% Définition du plateau de jeu avec toutes les cases.
board([a1, a2, a3, a4, a5, a6, a7, a8,
       b1, b2, b3, b4, b5, b6, b7, b8,
       c1, c2, c3, c4, c5, c6, c7, c8,
       d1, d2, d3, d4, d5, d6, d7, d8,
       e1, e2, e3, e4, e5, e6, e7, e8,
       f1, f2, f3, f4, f5, f6, f7, f8,
       g1, g2, g3, g4, g5, g6, g7, g8,
       h1, h2, h3, h4, h5, h6, h7, h8]).

% Définition des prédicats dynamiques pour les murs et la position du joueur.
:- dynamic wall/1, player/1.

% Position initiale du joueur.
player(e4).

% Retourne la position actuelle du joueur dans la boucle de jeu.
getplayerpos(X) :-
    player(X).

% Ajoute un mur sur une case.
addwall(X) :-
    \+ player(X),       % Vérifie que la case n'est pas occupée par le joueur.
    \+ wall(X),         % Vérifie que la case n'a pas déjà un mur.
    assert(wall(X)).    % Ajoute le mur à la base de faits.

% Supprime un mur d'une case.
removewall(X) :-
    retract(wall(X)).   % Supprime le mur de la base de faits.

% Permet de déplacer le joueur d'une case à une autre.
move(X, Y) :-
    \+ wall(Y),         % Vérifie que la case où l'ont veux aller est vide.
    assert(player(Y)),  % Met à jour la position du joueur.
    retract(player(X)). % Supprime l'ancienne position du joueur.

% Détermine les mouvements possibles à partir d'une case donnée, en enlevant les cases avec un mur.
possible_moves(X, Walls, Moves) :-
    connect(X, Adjacent),                       % Obtient les cases adjacentes de la case.
    exclude(blocked(Walls), Adjacent, Moves).   % Exclut les cases bloquées par des murs.

% Détermine les endroits possibles pour placer un mur, en tenant compte de la position du joueur et des murs sur le plateau.
possible_walls(PlayerPos, Walls, WallsToPlace) :-
    connect(PlayerPos, Adjacent),                                           % Obtient les cases adjacentes au joueur.
    exclude(occupied_or_blocked(PlayerPos, Walls), Adjacent, WallsToPlace). % Exclut les cases occupées ou bloquées.

% Vérifie si une case est bloquée par un mur.
blocked(Walls, Square) :-
    member(Square, Walls).

% Vérifie si une case est occupée par le joueur ou bloquée par un mur.
occupied_or_blocked(PlayerPos, Walls, Square) :-
    Square == PlayerPos;    % Vérifie si la case est la position du joueur.
    member(Square, Walls).  % Vérifie si la case est dans la liste des murs.

% Algorithme Minimax pour déterminer le meilleur coup pour le démon.
minmax(PlayerPos, Walls, Depth, MaxPlayer, BestMove, BestScore) :-
    Depth > 0, % Vérifie si la profondeur de recherche est supérieure à 0.
    ( MaxPlayer == demon ->                                 % Si c'est le tour du démon.
        possible_walls(PlayerPos, Walls, PossibleWalls),    % Obtient les emplacements possibles pour les murs.
        ( PossibleWalls \= [] ->                            % S'il y a des emplacements possibles pour les murs.
            evaluate_max(PossibleWalls, PlayerPos, Walls, Depth, -1000, none, BestMove, BestScore) % Évalue le meilleur emplacement oùposer un mur.
        ; evaluate_score(PlayerPos, Walls, BestScore),      % Sinon, évalue le score actuel.
          BestMove = none )                                 % Aucun mur n'est placé.
    ; possible_moves(PlayerPos, Walls, PossibleMoves),      % Sinon, si c'est le tour du joueur, obtient les mouvements possibles.
      ( PossibleMoves \= [] ->                              % S'il y a des mouvements possibles pour le joueur.
            evaluate_min(PossibleMoves, PlayerPos, Walls, Depth, 1000, none, BestMove, BestScore) % Évalue le meilleur mouvement.
        ; evaluate_score(PlayerPos, Walls, BestScore),      % Sinon, évalue le score actuel.
          BestMove = none )                                 % Aucun mouvement n'est effectué.
    ).

% Cas de base de l'algorithme Minimax lorsque la profondeur est 0, on évalue le score actuel.
minmax(PlayerPos, Walls, 0, _MaxPlayer, none, Score) :-
    evaluate_score(PlayerPos, Walls, Score).

% Évalue le meilleur emplacement pour le mur en maximisant le score.
evaluate_max([], _PlayerPos, _Walls, _Depth, BestScore, BestWall, BestWall, BestScore).
evaluate_max([Wall|RemainingWalls], PlayerPos, Walls, Depth, CurrentBestScore, CurrentBestWall, BestWall, BestScore) :-
    NewWalls = [Wall|Walls],                                    % Ajoute le mur à la liste des murs.
    Depth1 is Depth - 1,                                        % Décrémente la profondeur.
    minmax(PlayerPos, NewWalls, Depth1, player, _, MoveScore),  % Appelle l'algorithme Minimax pour le joueur.
    ( MoveScore > CurrentBestScore ->                           % Si le score du mouvement est meilleur que le meilleur score actuel.
        NewBestScore = MoveScore,                               % Met à jour le meilleur score.
        NewBestWall = Wall                                      % Met à jour le meilleur emplacement pour le mur.
    ; NewBestScore = CurrentBestScore,                          % Sinon, conserve le meilleur score actuel.
      NewBestWall = CurrentBestWall ),                          % Conserve le meilleur emplacement pour le mur actuel.
    evaluate_max(RemainingWalls, PlayerPos, Walls, Depth, NewBestScore, NewBestWall, BestWall, BestScore). % Évalue les emplacements de murs restants.

% Évalue le meilleur mouvement en minimisant le score.
evaluate_min([], _PlayerPos, _Walls, _Depth, BestScore, BestMove, BestMove, BestScore).
evaluate_min([Move|RemainingMoves], PlayerPos, Walls, Depth, CurrentBestScore, CurrentBestMove, BestMove, BestScore) :-
    Depth1 is Depth - 1,                                        % Décrémente la profondeur.
    minmax(Move, Walls, Depth1, demon, _, WallScore),           % Appelle l'algorithme Minimax pour le démon.
    ( WallScore < CurrentBestScore ->                           % Si le score du mur est inférieur au meilleur score actuel.
        NewBestScore = WallScore,                               % Met à jour le meilleur score.
        NewBestMove = Move                                      % Met à jour le meilleur mouvement.
    ; NewBestScore = CurrentBestScore,                          % Sinon, conserve le meilleur score actuel.
      NewBestMove = CurrentBestMove ),                          % Conserve le meilleur mouvement actuel.
    evaluate_min(RemainingMoves, PlayerPos, Walls, Depth, NewBestScore, NewBestMove, BestMove, BestScore). % Évalue les mouvements restants.

% Évalue le score actuel en fonction du nombre de mouvements possibles pour le joueur.
evaluate_score(PlayerPos, Walls, Score) :-
    possible_moves(PlayerPos, Walls, Moves),
    length(Moves, NumMoves),
    Score is -NumMoves.

% Gère le tour du joueur, en demandant une direction et en effectuant le mouvement.
playerturn(L, X) :-
    write('Entrez une direction pour bouger l\'ange\nCases possibles :\n'),
    write(L),
    nl,
    read(Direction),
    (
        member(Direction, L) ->         % Si la direction est valide.
            move(X, Direction)          % Effectue le mouvement.
        ;
        write('Direction invalide\n'),  % Sinon, affiche un message d'erreur.
        playerturn(L, X)                % Redemande une nouvelle direction au joueur.
    ).

% Gère le tour du démon en plaçant un mur au meilleure endroit.
demonturn :-
    getplayerpos(PlayerPos),                                        % Obtient la position du joueur.
    findall(W, wall(W), Walls),                                     % Obtient la liste des murs.
    ( minmax(PlayerPos, Walls, 3, demon, BestWall, _BestScore) ->   % Appelle l'algorithme Minimax pour le démon.
        ( BestWall \= none ->                                       % Si un mur peut être placé.
            addwall(BestWall),                                      % Ajoute le mur.
            write('Le démon place un mur en: '),
            write(BestWall), nl
        ; write('Le démon ne peut pas placer de mur.'), nl )
    ; write('Le démon ne peut pas déterminer un mur à placer.'), nl ).

% Boucle de jeu principale.
game :-
    board(Board),                   % Obtient le plateau de jeu.
    printboard(Board),              % Affiche le plateau de jeu.
    getplayerpos(X),                % Obtient la position du joueur.
    findall(W, wall(W), Walls),     % Obtient la liste des murs.
    possible_moves(X, Walls, L),    % Obtient les mouvements possibles pour le joueur.
    ( L == [] ->                    % Si le joueur ne peut pas bouger, le démon gagne.
        write('Le joueur ne peut plus bouger. Le démon a gagné.'), nl
    ; nl,
      playerturn(L, X),             % Tour du joueur.
      ( demonturn ->                % Tour du démon.
          game                      % Boucle de jeu.
        ; write('Le jeu se termine car le démon ne peut pas jouer.'), nl ) 
    ).

% Démarre le jeu.
start :-
    game.

% Affiche le plateau de jeu.
printboard([]).
printboard([X|Y]) :-
    (player(X) -> write('O ')       % Affiche 'O' à la la position du joueur.
    ; wall(X) -> write('X ')        % Affiche 'X' pour les murs placés par le démon.
    ; write('. ')),                 % Affiche '.' pour les cases vides.
    (X == a8 -> nl                  % Saut de ligne à la fin de chaque ligne pour que le plateau soit lisible.
    ; X == b8 -> nl
    ; X == c8 -> nl
    ; X == d8 -> nl
    ; X == e8 -> nl
    ; X == f8 -> nl
    ; X == g8 -> nl
    ; X == h8 -> nl
    ; true),
    printboard(Y).                  % Affiche le reste du plateau.