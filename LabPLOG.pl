:- discontiguous
        pertenece/2, no_pertenece/2,elegir/3,contenida/2,permutacion/2,
    	suma/2,sumaAcumulada/3,rango/2,rangoAcc/3,
    	tomar_n/4,columna/3,transpuesta/2.

%--------------------------------EJERCICIO 1 ------------------------------------------

%pertenece(?X,?L) <- El elemento X pertenece a la lista L.

pertenece(X,[X|L]).
pertenece(X,[H|L]) :- pertenece(X,L).


%no_pertenece(+X,+L) <- El elemento X no pertenece a la lista L.

no_pertenece(X,[]).
no_pertenece(X,[H|L]) :- no_pertenece(X,L),X\=H. 

%elegir(?X,?L,?R) ← La lista R resulta de eliminar el elemento X de la lista L.
elegir(X,[X|L],L).
elegir(X,[H|L],[H|S]) :- elegir(X,L,S).

%contenida(+L1,+L2) ← todos los elementos de L1 pertenecen a L2.

contenida([H|L1],L2) :- pertenece(H,L2) , contenida(L1,L2).
contenida([],L2).


%permutacion(+L1,?L2) ← La lista L2 es una permutación de la lista L1.
permutacion([],[]).
permutacion([H|T], L) :- 
	permutacion(T, T1), 
	elegir(H, L, T1).

%suma(+L,?S) ← S es la suma de los elementos de la lista L.
suma(L,S) :- sumaAcumulada(L,0,S).
%EL USO DEL ACUMULADOR PARA MAYOR EFICIENCIA.
sumaAcumulada([X|L1],AC,S) :- AUX is (AC + X),sumaAcumulada(L1,AUX,S).
sumaAcumulada([],S,S).

%rango(+N,?R) ← R es la lista que contiene los elementos de 1 a N.
rango(N,R) :- rangoAcc(N,[],R).
rangoAcc(0, Acc, Acc).
rangoAcc(N, Acc, R) :-
	N1 is N - 1,
	rangoAcc(N1, [N | Acc], R).

%tomar_n(+L,+N,?L1,?L2) ← L1 es una lista con los primeros N elementos de la lista L, 
%L2 es una lista con el resto de los elementos de la lista L.
tomar_n([],_,[],[]).
tomar_n(L,0,[],L).
tomar_n([X|L],N,[X|L1],L2) :- 
	AUX is (N-1), 
	tomar_n(L,AUX,L1,L2).

%columna(+M,?C,?R) ← M es una matriz representada como lista de listas de
%números, C es la primera columna de M en forma de lista y R es M sin la primera
%columna.
columna([[X|L]|M],[X|C],[L|R]):- columna(M,C,R).
columna([],[],[]).


%transpuesta(+M,?T) ← M es una matriz representada como lista de listas de
%números, T es la transpuesta de la matriz M.
transpuesta([F|M],T) :- columna(T,F,R) , transpuesta(M,R).
transpuesta([],[[]|_]).


%------------------------------FIN EJERCICIO 1---------------------------------

%------------------------------EJERCICIO 2-------------------------------------

columnas([],_).
columnas([X|Filas],N) :-
	length(X, N),
	columnas(Filas, N).

cuadro(C, N) :- length(C, N), columnas(C, N).

% Dado un K y una matriz M, F son los primeros K elementos de cada fila en una lista y R es la matriz M sin esos elementos.
primeros_elementos(K, M, [], M).
primeros_elementos(K, [FilaActual|M], Ret, [RestoActual|RestoRec]) :-
	tomar_n(FilaActual, K, ElemsActual, RestoActual),
	primeros_elementos(K, M, ElemsRec, RestoRec),
	append(ElemsActual, ElemsRec, Ret).

comparar_bloques(B, [[]|_], _).
comparar_bloques([Bloque | RestoBloques], KFilas, K) :- 
	primeros_elementos(K, KFilas, Bloque, RestoFilas),
	comparar_bloques(RestoBloques, RestoFilas, K).

chequear_solucion([], _, []).
chequear_solucion(M, K, B) :-
	tomar_n(M, K, KFilas, RestoFilas),
	tomar_n(B, K, KBloques, RestoBloques),
	comparar_bloques(KBloques, KFilas, K),
	chequear_solucion(RestoFilas, K, RestoBloques).

bloques(M, K, B) :-
	N is K*K,
	cuadro(B, N),
	cuadro(M, N), % Just for safety
	chequear_solucion(M, K, B).

filas_compatibles([],[]).
filas_compatibles([H|T], [H1,T1]) :-
	H \= H1,
	filas_compatibles(T, T1).

compatibles(_, []).
compatibles(Fila, [Fila2|RestoFilas]) :-
	filas_compatibles(Fila, Fila2),
	compatibles(Fila, RestoFilas).

verificar_sudoku(_, []). % Esto puede dar comportamientos inesperados.
verificar_sudoku(Rango, [Fila|RestoFilas]) :-
	permutacion(Fila, Rango),
	verificar_sudoku(Rango, RestoFilas).
	compatibles(Fila, RestoFilas).

sudoku(M, K) :-
	N is K*K,
	cuadro(M, N), % Just for safety
	rango(N, Rango),
	verificar_sudoku(Rango, M),
	bloques(M, K, B),
	verificar_sudoku(Rango, B),
	transpuesta(M, MTranspuesta),
	verificar_sudoku(Rango, MTranspuesta).


