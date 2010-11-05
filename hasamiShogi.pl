%DESENHAR O TABULEIRO

linhaLimite:-printLinha([' ',*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,' ']).
linhaLetras:-printLinha([' ',' ',' ',' ',' ',' ','A',' ',' ',' ',' ',' ',' ',' ','B',' ',' ',' ',' ',' ',' ',' ','C',' ',' ',' ',' ',' ',' ',' ','D',' ',' ',' ',' ',' ',' ',' ','E',' ',' ',' ',' ',' ',' ',' ','F',' ',' ',' ',' ',' ',' ',' ','G',' ',' ',' ',' ',' ',' ',' ','H',' ',' ',' ',' ',' ',' ',' ','I',' ',' ',' ',' ',' ']).
linhaNumerosV(['1','2','3','4','5','6','7','8','9']).
letra(a, 1). letra(b, 2). letra(c, 3). letra(d, 4). letra(e, 5). letra(f, 6). letra(g, 7). letra(h, 8). letra(i, 9).

linhaDivH:-printLinha([' ',*,' ',-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,' ',*]).

tabuleiro(
	 [[1,1,1,1,0,1,1,1,1],
	  [0,0,0,0,0,1,0,0,0],
	  [0,0,1,0,0,0,0,0,0],
	  [0,0,1,0,0,0,0,0,0],
	  [0,0,0,1,0,0,0,2,1],
	  [0,0,1,0,0,0,0,0,0],
	  [0,0,1,2,0,0,0,0,0],
	  [0,0,0,1,1,0,0,0,0],
	  [0,2,0,0,0,1,0,0,0]]).

piece1(1):- write(' ----- |').
piece1(2):- write(' ----- |').
piece1(0):- write('       |').


piece2(1):- write('|  1  ||').
piece2(2):- write('|  2  ||').
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

:-op(800,fx,se).
:-op(900,xfx,entao).
:-op(800,xfx,senao).

se A entao B senao _ :- A,!,B.
se _ entao _ senao C :- C.

if(A,B,_):-A,!,B.
if(_,_,C):-C.


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
	if((verificaPeca(T, Xf,Yf,0),verificaPeca(T,X,Y,Jogador), verificaCaminho(Jogador, X,Y,Xf,Yf,T)),
	(muda_tab(0,Jogador,Xf,Yf,T,TNovo),
	muda_tab(Jogador,0,X,Y,TNovo,TNovo2),
	troca(Jogador, Jogador2),
	conqHor(TNovo2, Jogador, TNovo3, TNovo2),
	conqHor(TNovo3, Jogador2, TNovo4, TNovo3),
	conqVer(TNovo4, Jogador, TNovo5, TNovo4),
        conqVer(TNovo5, Jogador2, TNovo6, TNovo5),
	if(terminouJogo(TNovo6,Jogador2),menu,cicloJogo(TNovo6, Jogador2))), cicloJogo(T, Jogador)).

% VERIFICA SE A PECA E' DO UTLIZADOR

verificaPeca(T,X,Y,Jogador) :- verificaPecaAux(T,X,Y,Jogador,1).

verificaPecaAux([T|_],X,Y,Jogador,Y) :-
	verificaPecaLinha(T,X,Jogador, 1).
verificaPecaAux([_|R],X,Y,Jogador,Linha) :-
	Linha2 is Linha+1,
	verificaPecaAux(R,X,Y,Jogador,Linha2).

verificaPecaLinha([Jogador|_], X, Jogador, X).
verificaPecaLinha([_|R], X, Jogador, Coluna) :-
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

adjacente(X1,Y1,X2,Y2):-X1 == X2, Y1 =:= Y2+1.
adjacente(X1,Y1,X2,Y2):-X1 == X2, Y1 =:= Y2-1.
adjacente(X1,Y1,X2,Y2):-Y1 == Y2, X1 =:= X2+1.
adjacente(X1,Y1,X2,Y2):-Y1 == Y2, X1 =:= X2-1.

diagonal(X1,Y1,X2,Y2):-X1==X2,  Y1\==Y2.
diagonal(X1,Y1,X2,Y2):-Y1==Y2,  X1\==X2.

verificaCaminho(Jog, X,Y,Xf,Yf, Tab):-
	%write('ola'), nl,
      	%write('ola1'), nl,
	verificaPeca(Tab, X,Y, Jog),
	%write('ola2'), nl,
	verificaPeca(Tab, Xf,Yf, 0),
	%write('ola3'), nl,
      	verificaPeca(Tab, Xa, Ya, 0),
	%write('ola4'), nl,
	diagonal(X,Y,Xf,Yf),
	adjacente(Xa,Ya,X,Y),
      	muda_tab(0,Jog,Xa,Ya,Tab,NovoTab),
       	%write('ola5'), nl,
      	verificaCaminho(Jog, Xa, Ya, Xf, Yf, NovoTab).
verificaCaminho(_,X,Y,X,Y,_).%write('0la2'),nl.

%VERIFICA SE HA PECAS CONQUISTADAS NA HORIZONTAL

conqHor(Tabuleiro, Jogador, TNovo, TabuleiroCop):-conqHorAux(Tabuleiro, Jogador, 1, TNovo, TabuleiroCop).

conqHorAux([],_,_, TNovo, TNovo).
conqHorAux([Linha|R], Jogador,Y, TNovo, TabuleiroCop):-
	Y \== 10,
	conqHorLinha(Linha, Jogador, 1, TNovo2, TabuleiroCop, Y),
	Y2 is Y+1,
	conqHorAux(R,Jogador, Y2, TNovo, TNovo2).

conqHorLinha([],_,_,TNovo,TNovo,_).
conqHorLinha([Elem|R], Jogador, X, TNovo, TabuleiroCop, Y):-
	X \== 10,
	if(Elem = Jogador, (conqHorLinhaAux(R,Jogador, X, TNovo2, TabuleiroCop, 0, Y), X1 is X+1,
	conqHorLinha(R, Jogador, X1, TNovo, TNovo2, Y)),(X1 is X+1,
	conqHorLinha(R, Jogador, X1, TNovo, TabuleiroCop, Y))).

conqHorLinhaAux2(_,X,TNovo,TNovo,X2,_):- X > X2.
conqHorLinhaAux2(Elem, X, TNovo, TabuleiroCop, Xaux, Y):-
	X =< Xaux,
	troca(Elem, Jog2),
	muda_tab(Jog2,0,X,Y,TabuleiroCop,TNovo2),
      	X2 is X+1,
	conqHorLinhaAux2(Elem, X2, TNovo, TNovo2, Xaux, Y).

conqHorLinhaAux([Jogador|_], Jogador, _, TNovo,TNovo,0,_).
conqHorLinhaAux([0|_], _, _, TNovo,TNovo,_,_).
conqHorLinhaAux([Jogador|_], Jogador, X, TNovo,TabuleiroCop,Xaux,Y):-
	Xaux \== 0,
	Xaux3 is Xaux+X,
	X1 is X+1,
	conqHorLinhaAux2(Jogador, X1, TNovo, TabuleiroCop, Xaux3, Y).

conqHorLinhaAux([],_,_,TNovo, TNovo,_,_).
conqHorLinhaAux([Elem|R], Jogador, X, TNovo, TabuleiroCop, Xaux, Y):-
	troca(Jogador, Jogador2),
      	Elem == Jogador2,
	Xaux2 is Xaux+1,
	conqHorLinhaAux(R, Jogador, X, TNovo, TabuleiroCop, Xaux2, Y).
conqHorLinhaAux([_], _, _, TNovo,TNovo,_,_).
% VERIFICA A CONQUISTA DE PEÇAS NA VERTICAL

conqVer(Tabuleiro, Jogador, TNovo, TabuleiroCop):-conqVerAux(Tabuleiro, Jogador, 1, TNovo, TabuleiroCop).

conqVerAux([],_,_, TNovo, TNovo).
conqVerAux([Linha|R], Jogador,Y, TNovo, TabuleiroCop):-
	Y \== 10,
	conqVerLinha(Linha, Jogador, 1, TNovo2, TabuleiroCop, Y),
	Y2 is Y+1,
	conqVerAux(R,Jogador, Y2, TNovo, TNovo2).

conqVerLinha([],_,_,TNovo,TNovo,_).
conqVerLinha([Elem|R], Jogador, X, TNovo, TabuleiroCop, Y):-
	X \== 10,
	if(Elem = Jogador, (Y1 is Y+1, conqVerColuna(Jogador, Y1, TNovo2, TabuleiroCop, 0, X), X1 is X+1,
	conqVerLinha(R, Jogador, X1, TNovo, TNovo2, Y)),(X1 is X+1,conqVerLinha(R, Jogador, X1, TNovo, TabuleiroCop, Y))).

conqVerColunaAux(Elem, Y, TNovo, TabuleiroCop, Yaux, XReferencia):-
	Y =< Yaux,
	troca(Elem, Jog2),
	muda_tab(Jog2,0,XReferencia,Y,TabuleiroCop,TNovo2),
      	Y2 is Y+1,
	conqVerColunaAux(Elem, Y2, TNovo, TNovo2, Yaux, XReferencia).
conqVerColunaAux(_,Y,TNovo,TNovo,Y2,_):- Y == Y2.


conqVerColuna(_, Y, TNovo, TNovo,_, XReferencia):-
	verificaPeca(TNovo, XReferencia, Y, 0).

conqVerColuna(Jogador, Y, TNovo, TabuleiroCop, Yaux, XReferencia) :-
	troca(Jogador,Jogador2),
	verificaPeca(TabuleiroCop, XReferencia, Y, Jogador2),
	Y1 is Y+1,
	Yaux2 is Yaux + 1,
	conqVerColuna(Jogador,Y1,TNovo,TabuleiroCop, Yaux2,XReferencia).

conqVerColuna(Jogador, Y, TNovo, TabuleiroCop, Yaux, XReferencia) :-
	verificaPeca(TabuleiroCop, XReferencia, Y, Jogador),
	Yaux > 0,
	Y2 is Y-Yaux,
	conqVerColunaAux(Jogador, Y2, TNovo, TabuleiroCop, Y, XReferencia).

conqVerColuna(Jogador, Y, TNovo, TNovo, Yaux, XReferencia) :-
	verificaPeca(TNovo, XReferencia, Y, Jogador),
	Yaux == 0.


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

%COMPUTA TODAS AS JOGADAS POSSIVEIS
%VERIFICA SE O JOGO TERMINOU

terminouJogo(T,Jogador) :- terminouJogoaux(T,1,1,0,Jogador).
terminouJogoaux(T,9,9,NPecas,Jogador):-!, NPecas < 3,desenha(T),nl,nl,nl,
	write('Terminou o jogo. O jogador '),
	troca(Jogador, Jogador2), write(Jogador2), write(' venceu.'),nl,nl,nl.

terminouJogoaux(T,X,Y,NPecas,Jogador) :-
       	if(verificaPeca(T, X, Y, Jogador), NPecasNovo is NPecas+1, NPecasNovo is NPecas),
	if(X == 9, (X1 is 1, Y1 is Y+1), (X1 is X+1, Y1 is Y)),
	terminouJogoaux(T,X1,Y1,NPecasNovo, Jogador).














