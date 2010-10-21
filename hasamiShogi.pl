%DESENHAR O TABULEIRO

linhaLimite:-printLinha([' ',*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,' ']).
linhaLetras:-printLinha([' ',' ',' ',' ',' ',' ','A',' ',' ',' ',' ',' ',' ',' ','B',' ',' ',' ',' ',' ',' ',' ','C',' ',' ',' ',' ',' ',' ',' ','D',' ',' ',' ',' ',' ',' ',' ','E',' ',' ',' ',' ',' ',' ',' ','F',' ',' ',' ',' ',' ',' ',' ','G',' ',' ',' ',' ',' ',' ',' ','H',' ',' ',' ',' ',' ',' ',' ','I',' ',' ',' ',' ',' ']).
linhaNumerosV(['1','2','3','4','5','6','7','8','9']).
letra(a, 1). letra(b, 2). letra(c, 3). letra(d, 4). letra(e, 5). letra(f, 6). letra(g, 7). letra(h, 8). letra(i, 9).

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

verifica(1):-tabuleiro(T), cicloJogo(T, 1), !.
verifica(2):-tabuleiro(T), desenha(T), !.
verifica(3):-tabuleiro(T), desenha(T), !.
verifica(stop).

%FIM DO DESENHO DO JOGO

%AVALIACAO DA JOGADA%

troca(1,2).
troca(2,1).

if(Condition, TrueClause, FalseClause) :-
	Condition, !, TrueClause;
       !, FalseClause.


cicloJogo(T, Jogador):-
	desenha(T), nl,
	write('Jogador actual: '),
	write(Jogador),nl,
	write('Peca a mover:') ,nl,
	write('Linha (Ex: 1) : '),
	read(Y),
	nl,
	write('Coluna (Ex: A) : '),
	read(Xt),
	letra(Xt, X),
	write('Posicao desejada:'),
	nl,
	write('Linha (Ex: 1) : '),
	read(Yf),
	nl,
	write('Coluna (Ex: A) : '),
	read(Xt2),
	letra(Xt2, Xf),
	if((verificaPeca(T, Xf,Yf,0),verificaPeca(T,X,Y,Jogador), validaCaminho(T, X,Y,Xf,Yf,Jogador)),
	(muda_tab(0,Jogador,Xf,Yf,T,TNovo),
	muda_tab(Jogador,0,X,Y,TNovo,TNovo2),
	troca(Jogador, Jogador2),
	cicloJogo(TNovo2, Jogador2)),cicloJogo(T, Jogador)).

% VERIFICA SE A PECA E' DO UTLIZADOR

verificaPeca(T,X,Y,Jogador) :- verificaPecaAux(T,X,Y,Jogador,1).
verificaPecaAux([],_,_,_,_).
verificaPecaAux([T|_],X,Y,Jogador,Y) :- verificaPecaLinha(T,X,Jogador, 1).
verificaPecaAux([_|R],X,Y,Jogador,Linha) :-
	Linha \== Y,
	Linha2 is Linha+1,
	verificaPecaAux(R,X,Y,Jogador,Linha2).

verificaPecaLinha([Jogador|_], X, Jogador,  X).
verificaPecaLinha([], _, _, _).
verificaPecaLinha([_|R], X, Jogador, Coluna) :- Coluna\==X,
			          N1 is Coluna+1,
				  verificaPecaLinha(R, X, Jogador, N1).
% VERIFICA SE A JOGADA E' VALIDA

validaCaminho(_,Xf,Yf,X,Y,_):-Xf \== X, Yf \== Y, fail.
validaCaminho(T,Xf,Yf,Xf,Yf,_) :- verificaPeca(T,Xf,Yf,0).

validaCaminho(T,Xf,Y,Xf,Yf,Jogador) :-
	Y \== Yf,
	if(Yf > Y, Y2 is Y+1, Y2 is Y-1),
        verificaPeca(T,Xf,Y2, 0),
        validaCaminho(T,Xf,Y2,Xf,Yf,Jogador).

validaCaminho(T,X,Yf,Xf,Yf,Jogador) :-
	X \== Xf,
	if(Xf > X, X2 is X+1, X2 is X-1),
	verificaPeca(T,X2,Yf,0),
	validaCaminho(T,X2,Yf,Xf,Yf,Jogador).

% ALTERA A POSICAO DA PECA DO JOGADOR

muda_tab(Peca,Pnov,X,Y,Tab,NovoTab):-
	muda_tab2(1,Peca,Pnov,X,Y,Tab,NovoTab).

muda_tab2(_,_,_,_,_,[],[]).
muda_tab2(Y,Peca,Pnov,X,Y,[Lin|Resto],[NovLin|Resto2]):-
	muda_linha(1,Peca,Pnov,X,Lin,NovLin),
	N2 is Y+1,
	muda_tab2(N2,Peca,Pnov,X,Y,Resto,Resto2).
muda_tab2(N,Peca,Pnov,X,Y,[Lin|Resto],[Lin|Resto2]):-
	N\=Y, N2 is N+1,
	muda_tab2(N2,Peca,Pnov,X,Y,Resto,Resto2).

muda_linha(_,_,_,_,[],[]).
muda_linha(X,Peca,Pnov,X,[Peca|Resto],[Pnov|Resto2]):-
	N2 is X+1,
	muda_linha(N2,Peca,Pnov,X,Resto,Resto2).

muda_linha(N,Peca,Pnov,X,[El|Resto],[El|Resto2]):-
	N\=X, N2 is N+1,
	muda_linha(N2,Peca,Pnov,X,Resto,Resto2).



%valida_jogada(X,Y,Tabuleiro,A,B):-.
%posicao_ocupada(X,Y,Tabuleiro,Resposta):-.
%verifica_caminho(X,Y,Tabuleiro,A,B, Resposta):-.
%verifica_conquistas(Tabuleiro):-.

%FIM DA AVALIACAO DA JOGADA%













