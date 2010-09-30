%tabuleiro


linhaLimite([*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*]).
linhaDivH([*,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,*]).

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

piece1(1, ' ----- ').
piece1(2, ' ----- ').
piece1(0, '       ').


piece2(1, '|  a  |').
piece2(2, '|  b  |').
piece2(0, '       ').


piece3(1, ' ----- ').
piece3(2, ' ----- ').
piece3(0, '       ').



printLinhaPeca([]).
printLinhaPeca([A|R]):-
	piece1(A, X),
	write(X),
	write('|'),
	printLinhaPeca(R).

printLinhaPeca3([]).
printLinhaPeca3([A|R]):-
	piece3(A, X),
	write(X),
	write('|'),
	printLinhaPeca3(R).

printLinhaPeca2([]).
printLinhaPeca2([A|R]):-
	piece2(A, X),
	write(X),
	write('|'),
	printLinhaPeca2(R).


printLinha([]).
printLinha([A|R]):-
	write(A),
	write(' '),
	printLinha(R).


printPecas([]).
printPecas([A|R]):-
	linhaDivH(X),
	write('*|'),
	printLinhaPeca(A),
	write('*'),
	nl,
	write('*|'),
	printLinhaPeca2(A),
	write('*'),
	nl,
	write('*|'),
	printLinhaPeca3(A),
	write('*'),
	nl,
	printLinha(X),
	nl,
	printPecas(R).

desenha:-
	tabuleiro(T),
	linhaLimite(L),
	linhaDivH(X),
	printLinha(L),
	nl,
	printLinha(X),
	nl,
	printPecas(T),
	printLinha(L).











