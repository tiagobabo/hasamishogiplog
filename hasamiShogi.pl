%tabuleiro


linhaLimite([*,*,*,*,*,*,*,*,*,*,*]).
linhaDivH([*,-,-,-,-,-,-,-,-,-,*]).

tabuleiro(
	 [[1,1,1,1,1,1,1,1,1],
	  [0,0,0,0,0,0,0,0,0],
	  [0,0,0,0,0,0,0,0,0],
	  [0,0,0,0,0,0,0,0,0],
	  [0,0,0,0,0,0,0,0,0],
	  [0,0,0,0,0,0,0,0,0],
	  [0,0,0,0,0,0,0,0,0],
	  [0,0,0,0,0,0,0,0,0],
	  [2,2,2,2,2,2,2,2,2]]).

piece(1, a).
piece(2, b).
piece(0, ' ').
piece(*, *).
piece(-,-).

printl([]).
printl([A|R]):-

	piece(A, X),
	write(X),
	write('|'),
	printl(R).

printl3([]).
printl3([A|R]):-
	piece(A, X),
	write(X),
	write(' '),
	printl3(R).


printl2([]).
printl2([A|R]):-
	linhaDivH(X),
	write('*|'),
	printl(A),
	write('*'),
	nl,
	printl3(X),
	nl,
	printl2(R).

desenha:-
	tabuleiro(T),
	linhaLimite(L),
	linhaDivH(X),
	printl3(L),
	nl,
	printl3(X),
	nl,
	printl2(T),
	printl3(L).


