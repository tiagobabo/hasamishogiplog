%UTILITARIOS

conv(Let,Valor):- maiuscula(Let), Valor is Let-64.
conv(Let,Valor):- minuscula(Let), Valor is Let-96.
conv(Let,Valor):- numero(Let), Valor is Let-48.

maiuscula(Let):- Let>=65, Let=<74.
minuscula(Let):- Let>=97, Let=<106.
numero(Let):- Let>=49, Let=<58.

letra(a, 1). letra(b, 2). letra(c, 3). letra(d, 4). letra(e, 5). letra(f, 6). letra(g, 7). letra(h, 8). letra(i, 9).


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
	  [0,0,0,0,0,0,0,0,0],
	  [0,0,0,0,0,0,0,0,0],
	  [0,0,0,0,0,0,0,0,0],
	  [0,0,0,0,0,0,0,0,0],
	  [2,2,2,2,2,2,2,2,2]]).

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
printLinhaPeca([A|R]):-	piece1(A), printLinhaPeca(R).
printLinhaPeca3([]).
printLinhaPeca3([A|R]):-piece3(A), printLinhaPeca3(R).
printLinhaPeca2([]).
printLinhaPeca2([A|R]):-piece2(A), printLinhaPeca2(R).
printLinha([]).
printLinha([A|R]):-write(A), printLinha(R).
printPecas([],[]).
printPecas([A|R],[X|Y]):- write(' *|'), printLinhaPeca(A), write('*'), nl, write(X), write('*|'), printLinhaPeca2(A), write('*'), nl, write(' *|'), printLinhaPeca3(A), write('*'), nl, linhaDivH, nl, printPecas(R, Y).

desenha(Tabuleiro):- linhaNumerosV(X),linhaLetras, nl, linhaLimite, nl, linhaDivH, nl,printPecas(Tabuleiro,X),linhaLimite.

%FIM DO DESENHO DO TABULEIRO

%ESCOLHA DOS MODOS DE JOGO

menu:-
    write('1 - Jogador vs CPU'),nl,
    write('2 - Jogador vs Jogador'),nl,
    write('3 - CPU vs CPU'),nl,
    write('4 - para sair'),nl,
    repeat,write('Escolha uma opcao (ex: 1) : '), get_code(Op),skip_line,Op>=49, Op=<52,conv(Op,Esc),
    verifica(Esc).

verifica(1):-jVsCpu, !.
verifica(2):-tabuleiro(T), desenha(T), !.
verifica(3):-tabuleiro(T), desenha(T), !.
verifica(_).

%MODO JOGADOR VS CPU

jVsCpu:-
	write('1 - Fácil'),nl,
	write('2 - Intermédio'), nl,
	write('3 - Difícil'),nl,
	write('4 - para sair'),nl,
	repeat,write('Opcao (Ex: 1) : '), get_code(Op),skip_line,Op>=49, Op=<52,conv(Op,Esc),
	dificuldade(Esc).

dificuldade(1):-tabuleiro(T), modoJogador(T, 1, 1), !.
dificuldade(2):-tabuleiro(T), modoJogador(T, 1, 2), !.
dificuldade(3):-jVsCpu, !.
dificuldade(_).

%FIM DA ESCOLHA DOS MODOS DE JOGO

%AVALIACAO DA JOGADA

troca(1,2).
troca(2,1).

if(Condition, TrueClause, FalseClause) :-
	Condition, !, TrueClause;
       !, FalseClause.

interaccaoJogador(T,Y,X,Yf,Xf,Jogador):-
	desenha(T), nl,
	write('Jogador actual: '),write(Jogador),nl,
	write('Peca a mover:'),nl,
	repeat,write('Linha (Ex: 1) : '),read(Y),
	repeat,write('Coluna (Ex: A) : '),read(Xt),letra(Xt, X),
	write('Posicao desejada:'),nl,
	repeat,write('Linha Final (Ex: 1) : '),read(Yf),
	repeat,write('Coluna Final (Ex: A) : '),read(Xt2),letra(Xt2, Xf).

pede_jogada(X,Y):- get_code(SX), conv(SX,X), get_code(SY), conv(SY,Y).

modoJogador(T, Jogador, ModoCPU):-
	Jogador == 1,
	repeat,
	interaccaoJogador(T,Y,X,Yf,Xf,Jogador),
	verificaCaminho(Jogador,X,Y,Xf,Yf,T),
	modificaT(Jogador,X-Y-Xf-Yf,T,TNovo2),
	troca(Jogador, Jogador2),
	conquistaPecas(TNovo2, TNovo3, Jogador),
	conquistaPecas(TNovo3, TNovo4, Jogador2),
	((terminouJogo(TNovo4,Jogador2),menu) ; modoCPU(TNovo4,Jogador2,ModoCPU)).

% CICLO DO BOT DEPENDENDO DA DIFICULDADE ESCOLHIDA
modoCPU(T, Jogador, ModoCPU):-
	ModoCPU == 1,
	Jogador == 2,
	findall(X-Y-Xf-Yf, verificaCaminho(Jogador, X,Y,Xf,Yf, T), L),
	choose(L, M),
	modificaT(Jogador,M,T, TNovo2),
	conquistaPecas(TNovo2, TNovo3, Jogador),
	conquistaPecas(TNovo3, TNovo4, Jogador2),
	troca(Jogador, Jogador2),
	((terminouJogo(TNovo4,Jogador2),menu); modoJogador(TNovo4, Jogador2, ModoCPU)).

% BOT DO MODO INTERMEDIO
modoCPU(T, Jogador, ModoCPU):-
	ModoCPU == 2,
	write('Estou a Pensar! Aguarde um momento por favor...  '),nl,
	Jogador == 2,
	greedy(T,L,1,X, Jogador),
	((L == X,
	findall(X-Y-Xf-Yf, verificaCaminho(Jogador, X,Y,Xf,Yf, T),L1),
	choose(L1, M));choose(L,M)),
	modificaT(Jogador,M,T, TNovo2),
	conquistaPecas(TNovo2, TNovo3, Jogador),
	conquistaPecas(TNovo3, TNovo4, Jogador2),
	troca(Jogador, Jogador2),
	((terminouJogo(TNovo4,Jogador2),menu); modoJogador(TNovo4, Jogador2, ModoCPU)).
greedy(T,L, P, _, J):-
	findall(X-Y-Xf-Yf,(verificaCaminho(J,X,Y,Xf,Yf,T),modificaT(J,X-Y-Xf-Yf,T,TNovo2),conquistas(J,_,_,_,_,TNovo2,1,N),N>P),L1),
	\+ L1 = [],
	P1 is P+1,
	greedy(T,L,P1,L1,J), !.
greedy(_,L, _, L,_).

% FIM DO BOT INTERMEDIO


modificaT(J,X-Y-Xf-Yf,T,TNovo):-
	muda_tab(J,0,X,Y,T,NovoTab),
	muda_tab(0,J,Xf,Yf,NovoTab,TNovo).


choose([], []).
choose(List, Elt) :-
        length(List, Length),
        random(0, Length, Index),
        nth0(Index, List, Elt).


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

% VERIFICA SE HA UM CAMINHO ENTRE PECAS

adjacente(X1,Y1,X2,Y2):-X1 == X2, Y1 =:= Y2+1.
adjacente(X1,Y1,X2,Y2):-X1 == X2, Y1 =:= Y2-1.
adjacente(X1,Y1,X2,Y2):-Y1 == Y2, X1 =:= X2+1.
adjacente(X1,Y1,X2,Y2):-Y1 == Y2, X1 =:= X2-1.
adjacente2(X1,Y1,X2,Y2):-X1 == X2, Y1 =:= Y2+1.
adjacente2(X1,Y1,X2,Y2):-Y1 == Y2, X1 =:= X2+1.
adjacente3(X1,Y1,X2,Y2):-X1 == X2, Y1 =:= Y2-1.
adjacente3(X1,Y1,X2,Y2):-Y1 == Y2, X1 =:= X2-1.
diagonal(X1,Y1,X2,Y2):-X1==X2,  Y1\==Y2.
diagonal(X1,Y1,X2,Y2):-Y1==Y2,  X1\==X2.

verificaCaminho(Jog, X,Y,Xf,Yf, Tab):-
       	verificaPeca(Tab,X,Y,Jog),
	verificaPeca(Tab,Xf,Yf,0),
	adjacente(X,Y,Xf,Yf).
verificaCaminho(Jog, X,Y,Xf,Yf, Tab):-
	verificaPeca(Tab, X,Y, Jog),
	verificaPeca(Tab, Xf,Yf, 0),
      	verificaPeca(Tab, Xa, Ya, 0),
      	diagonal(X,Y,Xf,Yf),
	adjacente(Xa,Ya,X,Y),
      	muda_tab(0,Jog,Xa,Ya,Tab,NovoTab),
      	verificaCaminho(Jog, Xa, Ya, Xf, Yf, NovoTab).

% VERIFICA SE HA PECAS CONQUISTADAS

% PROCESSA TODAS AS CONQUISTAS
conquistaPecas(Tab, NovoTab, Jog):-
	findall(X-Y-Xf-Yf, conquistas(Jog, X,Y,Xf,Yf,Tab,1,_), L),
	processaRemocoes(L, Tab, NovoTab, Jog).

% CHAMA A FUNCAO PARA REMOVER AS PECAS PARA CADA CONQUISTA
processaRemocoes([], Tab, Tab,_).
% CONQUISTA NA VERTICAL
processaRemocoes([X-Y-X-Yf|T], Tab, NovoTab,Jog):-
	Y1 is Y+1,
	Y2 is Yf-1,
	retiraPecas(X,Y1,X,Y2, Tab, NovoTab2, Jog),
	processaRemocoes(T, NovoTab2, NovoTab, Jog).
% CONQUISTA NA HORIZONTAL
processaRemocoes([X-Y-Xf-Y|T], Tab, NovoTab,Jog):-
	X1 is X+1,
	X2 is Xf-1,
	retiraPecas(X1,Y,X2,Y, Tab, NovoTab2, Jog),
	processaRemocoes(T, NovoTab2, NovoTab, Jog).

% RETIRA AS PECAS DO INTERVALO PASSADO COMO ARGUMENTO
retiraPecas(X,Y,X,Y, Tab, TRes, Jog):-
	troca(Jog, Jog2),
	muda_tab(Jog2, 0, X, Y,Tab, TRes).
retiraPecas(X,Y1,X,Y2, Tab, NovoTab2, Jog):-
	troca(Jog, Jog2),
	muda_tab(Jog2, 0, X, Y1,Tab, NovoTab3),
	Y3 is Y1+1,
	retiraPecas(X,Y3,X,Y2,NovoTab3, NovoTab2, Jog).
retiraPecas(X1,Y,X2,Y, Tab, NovoTab2, Jog):-
	troca(Jog, Jog2),
	muda_tab(Jog2, 0, X1, Y,Tab, NovoTab3),
	X3 is X1+1,
	retiraPecas(X3,Y,X2,Y,NovoTab3, NovoTab2, Jog).

% RETORNA UMA CONQUISTA

conquistas(_, X,Y,Xf,Yf, _, N, N):-
	N > 1,
	adjacente3(X,Y,Xf,Yf).

conquistas(Jog, X,Y,Xf,Yf, Tab, N, Cont):-
	verificaPeca(Tab, X,Y, Jog),
	verificaPeca(Tab, Xf,Yf, Jog),
	troca(Jog, Jog2),
      	verificaPeca(Tab, Xa, Ya, Jog2),
      	diagonal(X,Y,Xf,Yf),
	adjacente2(Xa,Ya,X,Y),
      	muda_tab(Jog2,Jog,Xa,Ya,Tab,NovoTab),
	N1 is N+1,
      	conquistas(Jog, Xa, Ya, Xf, Yf, NovoTab, N1,Cont).

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

%VERIFICA SE O JOGO TERMINOU

terminouJogo(T,Jogador) :- terminouJogoaux(T,1,1,0,Jogador).
terminouJogoaux(T,9,9,NPecas,Jogador):-!, NPecas < 3,desenha(T),nl,nl,nl,
	write('Terminou o jogo. O jogador '),
	troca(Jogador, Jogador2), write(Jogador2), write(' venceu.'),nl,nl,nl.

terminouJogoaux(T,X,Y,NPecas,Jogador) :-
       	if(verificaPeca(T, X, Y, Jogador), NPecasNovo is NPecas+1, NPecasNovo is NPecas),
	if(X == 9, (X1 is 1, Y1 is Y+1), (X1 is X+1, Y1 is Y)),
	terminouJogoaux(T,X1,Y1,NPecasNovo, Jogador).

