%DESENHAR O TABULEIRO

linhaLimite:-printLinha([' ',*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,' ']).
linhaLetras:-printLinha([' ',' ',' ',' ',' ',' ','A',' ',' ',' ',' ',' ',' ',' ','B',' ',' ',' ',' ',' ',' ',' ','C',' ',' ',' ',' ',' ',' ',' ','D',' ',' ',' ',' ',' ',' ',' ','E',' ',' ',' ',' ',' ',' ',' ','F',' ',' ',' ',' ',' ',' ',' ','G',' ',' ',' ',' ',' ',' ',' ','H',' ',' ',' ',' ',' ',' ',' ','I',' ',' ',' ',' ',' ']).
linhaNumerosV(['1','2','3','4','5','6','7','8','9']).

linhaDivH:-printLinha([' ',*,' ',-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,' ',*]).

tabuleiro(
	 [[1,1,1,1,1,1,1,1,1],
	  [0,0,0,0,0,0,0,0,0],
	  [0,0,0,0,0,0,0,0,0],
	  [0,0,0,0,0,0,0,0,0],
	  [2,0,0,0,0,0,0,0,0],
	  [0,0,0,0,0,0,0,0,0],
	  [0,0,0,0,0,0,0,0,0],
	  [0,0,0,0,0,0,0,0,0],
	  [0,2,2,2,2,2,2,2,2]]).

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

desenha(Tabuleiro):-
	linhaNumerosV(X),
	linhaLetras, nl,
	linhaLimite, nl,
	linhaDivH, nl,
	printPecas(Tabuleiro,X),
	linhaLimite.

%FIM DO DESENHO DO TABULEIRO

%MENU DO JOGO

menu:-
    write('1 - Jogador vs CPU'),nl,
    write('2 - Jogador vs Jogador'),nl,
    write('3 - CPU vs CPU'),nl,
    write('stop - para sair'),nl,
    write('Escolha uma opcao'),nl,
    read(X),
    write(X),
    verifica(X).

verifica(1):-tabuleiro(T), desenha(T), cicloJogo(T, 1), !.
verifica(2):-tabuleiro(T), desenha(T), !.
verifica(3):-tabuleiro(T), desenha(T), !.
verifica(stop):-!.
verifica(_):-!, menu.

%FIM DO DESENHO DO JOGO

%AVALIACAO DA JOGADA%

troca(1,2).
troca(2,1).
cicloJogo(T, Jogador):- nl,
		write('Peca a mover:')
                ,nl,
		write('Linha (Ex: 1) : '),
		read(Y),
		nl,
		write('Coluna (Ex: A) : '),
		read(X),
		verificaPeca(T,X,Y,Jogador),
		write('Posicao desejada:'),
		nl,
		write('Linha (Ex: 1) : '),
		read(Yf),
		nl,
		write('Coluna (Ex: A) : '),
		read(Xf),
		verificaPeca(T, Xf,Yf,0),
		%validaCaminho(T,Xf,Yf,X,Y,Jogador),
		movePeca(X,Y,Xf,Yf,Jogador,T,TNovo),
		troca(Jogador, Jogador2),
		cicloJogo(TNovo, Jogador2).

verificaPeca(T,X,Y,Jogador) :- verificaPecaAux(T,X,Y,Jogador,1).
verificaPecaAux([],_,_,_,_):- fail.
verificaPecaAux([T|_],X,Y,Jogador,Y) :- verificaPecaLinha(T,X,Jogador, 1).
verificaPecaAux([_|R],X,Y,Jogador,Linha) :-
	                                Linha \== Y,
	                                Linha2 is Linha+1,
					verificaPecaAux(R,X,Y,Jogador,Linha2).

verificaPecaLinha([Jogador|_], X, Jogador,  X).
verificaPecaLinha([], _, _, _) :- fail.
verificaPecaLinha([_|R], X, Jogador, Coluna) :- Coluna\==X,
			          N1 is Coluna+1,
				  verificaPecaLinha(R, X, Jogador, N1).


validaCaminho(T,Xf,Yf,Xf,Yf,Jogador) :- verificaPeca(T,Xf,Yf,Jogador), !.
% Caso o Xf seja igual ao Xi
validaCaminho(T,Xf,Y,Xf,Yf,Jogador) :- Y2 is Y+1,
	                               verificaPeca(T,Xf,Y2, Jogador),
				       validaCaminho(T,Xf,Y2,Xf,Yf,Jogador), !.


% MOVE PECA
movePeca(X, Y, Xf, Yf, Jogador, T, TNovo):- movePecaAux(X,Y,Xf,Yf,Jogador, T, TNovo, 1).
movePecaAux(_,_,_,_,_,[],_,_):-write('Falhou'), nl, !.
movePecaAux(_,_,Xf,Yf,Jogador, [T|_], [F|_], Yf):- trocaPeca(Xf, T, F, Jogador, 1), !.
movePecaAux(X,Y,_,_,_, [T|_], [_|F], Y):- trocaPeca(X,T, F,0,1), !.
movePecaAux(X,Y,Xf,Yf,Jogador,[A|R], [H|T], Linha):-
	Linha2 is Linha+1,
	movePecaAux(X,Y,Xf,Yf,Jogador, R, [A,H|T], Linha2).

trocaPeca(X, [_], [_], _, X).
trocaPeca(_,[],_,_,_):-fail.
trocaPeca(X, [H1|T1], [H2|_], NovaPeca, Coluna):-
	Coluna \== X,
	N1 is Coluna+1,
	trocaPeca(X, T1, [H2,H1|_], NovaPeca, N1).


%valida_jogada(X,Y,Tabuleiro,A,B):-.
%posicao_ocupada(X,Y,Tabuleiro,Resposta):-.
%verifica_caminho(X,Y,Tabuleiro,A,B, Resposta):-.
%verifica_conquistas(Tabuleiro):-.

%FIM DA AVALIACAO DA JOGADA%













