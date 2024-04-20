%--------------------------------EJERCICIO 1 ------------------------------------------

% pertenece(?X,?L) <- El elemento X pertenece a la lista L.

pertenece(X,[X|_]).
pertenece(X,[_|L]) :- pertenece(X,L).


% no_pertenece(+X,+L) <- El elemento X no pertenece a la lista L.

no_pertenece(_, []).
no_pertenece(X,[H|L]) :- no_pertenece(X,L),X\=H. 

% elegir(?X,?L,?R) ← La lista R resulta de eliminar el elemento X de la lista L.
elegir(X,[X|L],L).
elegir(X,[H|L],[H|S]) :- elegir(X,L,S).

% contenida(+L1,+L2) ← todos los elementos de L1 pertenecen a L2.

contenida([H|L1],L2) :- pertenece(H,L2) , contenida(L1,L2).
contenida([], _).


% permutacion(+L1,?L2) ← La lista L2 es una permutación de la lista L1.
permutacion([],[]).
permutacion([H|T], L) :- 
	permutacion(T, T1), 
	elegir(H, L, T1).

% suma(+L,?S) ← S es la suma de los elementos de la lista L.
suma(L,S) :- sumaAcumulada(L,0,S).
% EL USO DEL ACUMULADOR PARA MAYOR EFICIENCIA.
sumaAcumulada([X|L1],AC,S) :- AUX is (AC + X),sumaAcumulada(L1,AUX,S).
sumaAcumulada([],S,S).

% rango(+N,?R) ← R es la lista que contiene los elementos de 1 a N.
rango(N,R) :- rangoAcumulado(N,1,R).
% EL USO DEL ACUMULADOR PARA MAYOR EFICIENCIA.
rangoAcumulado(N,AC,[AC|R]):- AC<N, AUX is (AC + 1),rangoAcumulado(N,AUX,R).
rangoAcumulado(N,N,[N]).

% tomar_n(+L,+N,?L1,?L2) ← L1 es una lista con los primeros N elementos de la lista L, 
% L2 es una lista con el resto de los elementos de la lista L.
tomar_n(L,0,[],L).
tomar_n([X|L],N,[X|L1],L2) :- 
	AUX is (N-1), 
	tomar_n(L,AUX,L1,L2).

% columna(+M,?C,?R) ← M es una matriz representada como lista de listas de
%números, C es la primera columna de M en forma de lista y R es M sin la primera
% columna.
columna([[X|L]|M],[X|C],[L|R]):- columna(M,C,R).
columna([],[],[]).


% transpuesta(+M,?T) ← M es una matriz representada como lista de listas de
%números, T es la transpuesta de la matriz M.
transpuesta([F|M],T) :- columna(T,F,R) , transpuesta(M,R).
transpuesta([],[[]|_]).


%--------------------------------FIN EJERCICIO 1---------------------------------

%---------------------------------EJERCICIO 2-------------------------------------

% columnas(+L,?N) <- Dada una Lista de Listas L, asegura que todas las Listas sean de largo N.
columnas([],_).
columnas([X|Filas],N) :-
	length(X, N),
	columnas(Filas, N).


% cuadro(?C,?N) <- Verifica que C sea una Lista de N Listas, cada una de largo N. Se debe instanciar al menos uno de los dos parametros.
cuadro(C, N) :- length(C, N), columnas(C, N).


% primeros_elementos(+K,+M,?F,?R) <- Dado un K y una matriz M, F es una lista con los primeros K elementos de cada fila y R es la matriz M sin esos elementos.
primeros_elementos(_, [], [], []).
primeros_elementos(K, [FilaActual|M], Ret, [RestoActual|RestoRec]) :-
    tomar_n(FilaActual, K, ElemsActual, RestoActual),
	primeros_elementos(K, M, ElemsRec, RestoRec),
	append(ElemsActual, ElemsRec, Ret).

% comparar_bloques(+B,+F,+K) <- Verifica que B sea la lista de Bloques de tamano K, de las K filas (F) seleccionadas.
comparar_bloques([], [[]|_], _).
comparar_bloques([Bloque | RestoBloques], KFilas, K) :- 
	primeros_elementos(K, KFilas, Bloque, RestoFilas),
	comparar_bloques(RestoBloques, RestoFilas, K).


% chequear_solucion(+M,+K,+B) <- Dada una matriz M de orden K, se verifica que la lista B sean los bloques de M.
chequear_solucion([], _, []).
chequear_solucion(M, K, B) :-
	tomar_n(M, K, KFilas, RestoFilas),
	tomar_n(B, K, KBloques, RestoBloques),
	comparar_bloques(KBloques, KFilas, K),
	chequear_solucion(RestoFilas, K, RestoBloques).


% bloques(+M,+K,?B) <- B es una lista de Bloques de una matriz M de orden K.
bloques(M, K, B) :-
	N is K*K,
	cuadro(B, N),
	chequear_solucion(M, K, B).

% filas_compatibles(+L1,+L2) <- Verifica que las listas L1 y L2 no tengan el mismo elemento en ninguna posicion.
filas_compatibles([],[]).
filas_compatibles([H|T], [H1|T1]) :-
	H \= H1,
	filas_compatibles(T, T1).

% compatibles(+F,+L) <- Verifica que la fila F sea compatible con el resto de las Filas L de la Matriz. Se utiliza para descartar soluciones antes de tiempo.
compatibles(_, []).
compatibles(Fila, [Fila2|RestoFilas]) :-
	filas_compatibles(Fila, Fila2),
	compatibles(Fila, RestoFilas).

% verificar_fila(+Rango,?L) <- Verifica que una lista L sea una permutacion de un Rango en un tiempo menor al predicado permutacion.
verificar_fila(_, []).
verificar_fila(Rango, [H|T]) :-
elegir(H, Rango, RangoRestante),
verificar_fila(RangoRestante, T).

% verificar_filas(+Rango,+Filas) <- Verifica que las listas en Filas sean permutaciones compatibles de los elementos de Rango.
verificar_filas(_, []).
verificar_filas(Rango, [Fila|RestoFilas]) :-
	verificar_fila(Rango, Fila),
	verificar_filas(Rango, RestoFilas),
	compatibles(Fila, RestoFilas).
	

% verificar_bloques(+Rango,+B) <- Verifica que todos los bloques de B sean permutaciones de Rango.
verificar_bloques(_, []).
verificar_bloques(Rango, [Fila|RestoFilas]) :-
	verificar_fila(Rango, Fila),
	verificar_bloques(Rango, RestoFilas).

% sudoku(+M,+K) ← M es una matriz que representa un sudoku de orden K, el predicado es verdadero si M es un sudoku correcto resuelto.
sudoku(M, K) :-
	N is K*K,
	rango(N, Rango),
	verificar_filas(Rango, M),
	transpuesta(M, MTranspuesta),
	verificar_filas(Rango, MTranspuesta),
	bloques(M, K, B),
	verificar_bloques(Rango, B).


%--------------------------------FIN EJERCICIO 2---------------------------------