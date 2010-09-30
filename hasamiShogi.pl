%DESENHAR O TABULEIRO

linhaLimite:-printLinha([' ',*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,' ']).
linhaLetras:-printLinha([' ',' ',' ',' ',' ',' ','A',' ',' ',' ',' ',' ',' ',' ','B',' ',' ',' ',' ',' ',' ',' ','C',' ',' ',' ',' ',' ',' ',' ','D',' ',' ',' ',' ',' ',' ',' ','E',' ',' ',' ',' ',' ',' ',' ','F',' ',' ',' ',' ',' ',' ',' ','G',' ',' ',' ',' ',' ',' ',' ','H',' ',' ',' ',' ',' ',' ',' ','I',' ',' ',' ',' ',' ']).
linhaLetrasV(['1','2','3','4','5','6','7','8','9']).

linhaDivH:-printLinha([' ',*,' ',-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,' ',*]).

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

piece1(1):- write(' ----- |').
piece1(2):- write(' ----- |').
piece1(0):- write('       |').


piece2(1):- write('|  a  ||').
piece2(2):- write('|  b  ||').
piece2(0):- write(('       |')).


piece3(1):- write(' ----- |').
piece3(2):- write(' ----- |').
piece3(0):- write('       |').

printLinhaPeca([]).
printLinhaPeca([A|R]):-
	piece1(A),
	printLinhaPeca(R).

printLinhaPeca3([]).
printLinhaPeca3([A|R]):-
	piece3(A),
	printLinhaPeca3(R).

printLinhaPeca2([]).
printLinhaPeca2([A|R]):-
	piece2(A),
	printLinhaPeca2(R).


printLinha([]).
printLinha([A|R]):-
	write(A),
	printLinha(R).


printPecas([],[]).
printPecas([A|R],[X|Y]):-
	write(' *|'),
	printLinhaPeca(A),
	write('*'), nl,
	write(X),
	write('*|'),
	printLinhaPeca2(A),
	write('*'), nl,
	write(' *|'),
	printLinhaPeca3(A),
	write('*'), nl,
	linhaDivH, nl,
	printPecas(R, Y).

desenha:-
	tabuleiro(T),
	linhaLetrasV(X),
	linhaLetras, nl,
	linhaLimite, nl,
	linhaDivH, nl,
	printPecas(T,X),
	linhaLimite.

%FIM DO DESENHO DO TABULEIRO











