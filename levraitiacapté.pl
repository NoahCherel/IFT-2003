% graph representation for chess board
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
% end of graph representation for chess board
board([a1, a2, a3, a4, a5, a6, a7, a8,
       b1, b2, b3, b4, b5, b6, b7, b8,
       c1, c2, c3, c4, c5, c6, c7, c8,
       d1, d2, d3, d4, d5, d6, d7, d8,
       e1, e2, e3, e4, e5, e6, e7, e8,
       f1, f2, f3, f4, f5, f6, f7, f8,
       g1, g2, g3, g4, g5, g6, g7, g8,
       h1, h2, h3, h4, h5, h6, h7, h8]).

:- dynamic wall/1, player/1, flag/1.

player(e4).


getplayerpos(X) :-
    player(X).

addwall(X) :-
    (not(player(X)), not(wall(X)) ->
        assert(wall(X))
    )
.

addflag(X) :-
    (not(player(X)), not(wall(X)) ->
        assert(flag(X))
    )
.

removeallflags :-
    retractall(flag(_))
.

removeflag(X) :-
    retract(flag(X))
.

possiblemove(X, L) :-
    board(Board),
    member(X, Board),
    findall(Y, (connect(X, L1), member(Y, L1), (not(wall(Y)), not(flag(Y)))), L)
.

possiblewalls(X, L) :-
    board(Board),
    member(X, Board),
    findall(Y, (connect(X, L1), member(Y, L1), (not(wall(Y)), not(flag(Y)))), L)
.


%player move from X to Y
move(X, Y) :-
    (not(wall(Y))) ->
        assert(player(Y)),
        retract(player(X))
.

minmax(PlayerPos, Depth, MaxPlayer, BestWall, BestScore) :-
    Depth > 0,
    (MaxPlayer == demon ->
        possiblewalls(PlayerPos, Walls),
        evaluate_max(Walls, PlayerPos, Depth, -inf, BestWall, BestScore)
    ;
        possiblemove(PlayerPos, PlayerMoves),
        evaluate_min(PlayerMoves, PlayerPos, Depth, inf, BestWall, BestScore)
    )
.

evaluate_max([], _PlayerPos, _Depth, BestScore, _BestWall, BestScore).
evaluate_max([Wall|RemainingWalls], PlayerPos, Depth, CurrentBest, BestWall, BestScore) :-
    addwall(Wall),
    Depth1 is Depth - 1,
    minmax(PlayerPos, Depth1, player, _, MoveScore),
    retract(flag(Wall)),
    (MoveScore > CurrentBest ->
        NewBest = MoveScore,
        NewBestWall = Wall
    ;
        NewBest = CurrentBest,
        NewBestWall = BestWall
    ),
    evaluate_max(RemainingWalls, PlayerPos, Depth, NewBest, NewBestWall, BestScore)
.

evaluate_min([], _PlayerPos, _Depth, BestScore, _BestMove, BestScore).
evaluate_min([Move|RemainingMoves], PlayerPos, Depth, CurrentBest, BestMove, BestScore) :-
    Depth1 is Depth - 1,
    minmax(Move, Depth1, demon, _, WallScore),
    (WallScore < CurrentBest ->
        NewBest = WallScore,
        NewBestMove = Move
    ;
        NewBest = CurrentBest,
        NewBestMove = BestMove
    ),
    evaluate_min(RemainingMoves, PlayerPos, Depth, NewBest, NewBestMove, BestScore)
.

playerturn(L, X) :-
    write('Entrez une direction pour bouger the angel\n Case possible :\n'),
    write(L),
    nl,
    read(Direction),
    member(Direction, L),
    move(X, Direction)
    ;
    write('Direction invalide\n'),
    playerturn(L, X)
.

demonturn :-
    getplayerpos(PlayerPos),
    minmax(PlayerPos, 3, demon, BestWall, _BestScore),
    addwall(BestWall),
    write('Le démon place un mur en: '),
    write(BestWall), nl
.

game :-
    tty_clear,
    board(Board),
    printboard(Board),
    getplayerpos(X),
    possiblemove(X, L),
    nl,
    playerturn(L, _), !,
    demonturn,
    possiblemove(Direction, L1),
    (L1 == [] -> true ; game)
.



% Imprime l'état du plateau de jeu
printboard([]).
printboard([X|Y]) :-
    (player(X) -> write('O ')
    ; (wall(X) -> write('X ')
    ; flag(X) -> write('F ')
    ; write('. '))),
    (X == a8 -> nl
    ; X == b8 -> nl
    ; X == c8 -> nl
    ; X == d8 -> nl
    ; X == e8 -> nl
    ; X == f8 -> nl
    ; X == g8 -> nl
    ; X == h8 -> nl
    ; true),
    printboard(Y)
.