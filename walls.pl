walls([(2, 2), (4, 4)]).
grille_taille(10).

print_walls([]).
print_walls([(X, Y)|Rest]) :-
    write('('), write(X), write(', '), write(Y), write(')'), nl,
    print_walls(Rest).

print_all_walls :-
    walls(WallList),
    print_walls(WallList).

:- print_all_walls.
