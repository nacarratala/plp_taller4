% Unidades
% Este predicado indica cuáles son las unidades militares disponibles.
unidad(lancero).
unidad(arquero).
unidad(jinete).
unidad(guerrillero).
% Edificios
% Este predicado indica cuáles son los edificios que se pueden construir.
edificio(cuartel).
edificio(arqueria).
edificio(establo).
% Entrenamiento
% Este predicado indica en qué edificio se entrena cada unidad.
entrena(lancero,      cuartel). %80+300 = 380
entrena(arquero,      arqueria). %90+330 = 420
entrena(guerrillero,  arqueria). %70+330 = 400
entrena(jinete,       establo). %120+400 = 640
% Costos
% Este predicado indica el costo de cada unidad y de cada edificio.
costo(lancero,      80).
costo(arquero,      90).
costo(guerrillero,  70).
costo(jinete,       120).
costo(cuartel,      300).
costo(arqueria,     330).
costo(establo,      400).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ej 1 : costo para listas (tanto de batallones como de edificios)
% costo( +L , -C )
costo([],0).
costo([(U,C)], R) :- costo_batallon((U,C), R), !.
costo([E], R) :-  costo_edificio(E, R), !.
costo([ELEM | TAIL], R) :- costo([ELEM], X), costo(TAIL, Y), R is X+Y.

% costo_batallon(+B,?R): tiene éxito si el costo de creación del batallón B es R.
costo_batallon((U,C),R) :- esBatallon((U,C)), costo(U,X),R is X*C.

% costo_edificio(+E,?R): tiene éxito si el costo de creación del edificio E es R.
costo_edificio(U,R) :- edificio(U), costo(U,R).

% esBatallon(+B): tiene éxito si B es un batallón válido.
esBatallon((U,C)) :- unidad(U), integer(C), C > 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Lo que hacemos al agregar "ANTERIOR is X-1, not(esEjercito(ANTERIOR, Y, E))" es lo siguiente:
%% A esta altura, E ya esta instanciado y al X le restamos 1. Nosotros le estamos pidiendo a not que se fije si con el valor ANTERIOR 
%%(que es X-1) y con el valor Y, puede generar el ejercito E ya instanciado. 
%% Si esEjercito(ANTERIOR, Y, E) da true significa que pudo generar el mencionado ejercito E, por lo tanto E ya habia sido generado
%% con X-1 en vez de X. En otras palabras, es un ejercito repetido. Logicamente, not falla.
%% Ahora bien, si esEjercito(ANTERIOR, Y, E) da false, significa que NO pudo generar el mencionado ejercito E, por lo tanto 
%% es un caso no repetido. Not da true, y el nuevo ejercito es agregado. 
%
% Ej 2 : instanciar un ejército arbitrario
% ejercito ( -E )
ejercito(E) :- nonvar(E), pairs(X,Y), esEjercito(X,Y,E), ANTERIOR is X-1, not(esEjercito(ANTERIOR, Y, E)), !.
ejercito(E) :-    var(E), pairs(X,Y), esEjercito(X,Y,E), ANTERIOR is X-1, not(esEjercito(ANTERIOR, Y, E)).

% Reversibilidad: El parametro E puede estar instanciado o no. Esto es posible gracias realizar al comienzo el chequeo de si E es una varaible libre.
%% De no existir este chequeo, si llamasemos a ejercito(E) con un ejercito E instanciado de forma incorrecta, el predicado se colgaria pues generaria 
%% los infinitos ejercitos hasta poder encontrar uno que unifique con el pasado por parametro. 
%% Observacion: De no exisitir el chequeo, el predicado NO se colgaria si el E pasado como parametro fuese efectivamente un ejercito bien
%% instanciado pues en algun momento el predicado sera capaz de generar ese mismo ejercito que unifique con el parametro.

% esEjercito(+K,+N,-E): tiene éxito si E es un ejército de N batallones con hasta K unidades cada uno.
esEjercito(_,0,[]) :- !.
esEjercito(K, N,[(U,C)|XS]) :- between(1,K,C), unidad(U), H is N-1, esEjercito(K,H,XS).

% pairs(-X,-Y): instanciador de pares arbitrarios X e Y tal que X > 0 e Y > 0.
pairs(X,Y) :- from(2, S), K is S-1, between(1, K, X), Y is S-X.

% from(+X,-Y): instanciador de sucesores tal que X =< Y.
from(X, X).
from(X, Y) :- N is X+1, from(N,Y).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%EJERCICIO 2 MEJORADO

% ejercito2 ( -E ): % instancia todos los ejercitos posibles
ejercito2(E) :- from(1, X), ejercitoN2(X,E).

% esEjercito2 ( +E ):  Duelve true si E es un ejercito
esEjercito2([(U,C)]) :- integer(C), unidad(U).
esEjercito2([(U,C)|XS]) :- unidad(U), integer(C), esEjercito2(XS).

% esEjercitoN ( +N, -E ): instancia todos los ejercitos de N unidades
ejercitoN2(0, []).
ejercitoN2(N, [(U,C)|XS]) :-  unidad(U), between(1,N,C), M is N-C, ejercitoN2(M, XS). 

%batallonN2 generador de batallones con N unidades
batallonN2(N, (U, N)) :- unidad(U).

%cantUnidades(+E, ?R): tiene éxito si R es la cantidad de unidades total del ejército E. (asume fuertemente que E es un ejercito)
cantUnidades((_,C), C).
cantUnidades([], 0).
cantUnidades([ (_,C) | L ], R) :-  cantUnidades(L, Y), R is C+Y.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ej 3 : instancia una lista de edificios necesarios para el ejército
% edificiosNecesarios ( +Ej , -Ed )
edificiosNecesarios([], []).
edificiosNecesarios([(U,_) | L], R) :- entrena(U,X), edificiosNecesarios(L, Y), sort([ X | Y ], R).

%(-EJ, -ED)
edificiosNecesarios2(EJ, ED) :- ejercito(EJ), edificiosNecesarios(EJ, ED).


% Reversibilidad: 
% (-EJ, -ED): En este caso en el que ambos esten sin instanciar, unificara con la primer linea, por lo tanto un resultado sera ([], [])
% Cuando haga un redo, entrando a la segunda linea, ejecutra entrena(U,X) con U,X sin instanciar. Lo cual siempre devuelve como primer resultado
% el par (lancero, cuartel). Posteriormente a eso hace una llamada recursiva, cuyo primer resultado ser nuevamente ([], []). En definitiva, si 
% continuamos pidiendole al predicado mas respuestas, nos quedaria algo de la pinta: 
% ([],[]) 
% ([(lancero, basura)], [cuartel])
% ([(lancero, basura), (lancero, basura)], [cuartel])
% ([(lancero, basura), (lancero, basura), (lancero, basura)], [cuartel])
% ...
% OBS: en edificiosNecesarios2 esto no ocurre gracias a que ejercito(EJ) se encarga de instanciar todos los ejercitos posibles


% (-EJ, +ED):
% En este caso, el predicado no puede unificar con ([], []), por lo tanto nunca consigue un primer posible resultado, ya que en la segunda linea
%% se llama recursivamente y de nuevo, no puede unificar con ([], []). El predicado se cuelga


% (+EJ, +ED): en este caso, el predicado no se colgara. Toma el ejercito definido y ejecuta como "el caso normal" donde termina instanciando en ED
% los edificios necesarios. Pero, por como esta definido sort, es posible que este caso devuelva false cuando en realidad deberia ser true. Esto 
% se debe a que discrimina en el orden de la lista resultante de edificios necesarios, donode, por ejemplo [establo, arqueria] != [arqueria, establo]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ej 4 : índice de superioridad para unidades
% ids ( +A , +B , -I )
ids(jinete,       arquero,      1.5) :- !.
ids(jinete,       guerrillero,  0.5) :- !.
ids(lancero,      jinete,       2) :- !.
ids(lancero,      arquero,      0.6) :- !.
ids(guerrillero,  lancero,      1.1) :- !.
ids(guerrillero,  arquero,      2) :- !.
ids(A, A, 1) :- unidad(A), !.
ids(A, B, I) :- unidad(A), unidad(B), ids(B, A, X), I is 1/X, !.

% Reversibilidad:
% Este predicado admite todas las posibles combinaciones. Esta a la vista que es un predicado muy "hardcodeado". Aunque, una observacion que vale 
% la pena aclarar es que, en caso de no instanciar A y/o B no devuelve todos los potenciales resultados. Por ejemplo, si corriesemos 
% ids(A, B, 2)), dos posibles pares de resultado podrian ser (lancero, jinete) y (guerrillero, arquero). NO obstante, el predicado,
% por como esta implemetnado con el !, devuelve solamente el primero que encuentra. En este caso, (lancero, jinete)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ej 5
% ids ( +A , +B , -I )
ids((UA,CA),(UB,CB),Ib) :- ids(UA,UB,Iu), Ib is Iu * (CA / CB).
% gana ( +A , +B )
gana(A,B) :- ids(A,B,I), I >= 1.
gana(_,[]) :- !.
gana([A|AS],[B|BS]) :- gana(A,B), gana([A|AS],BS), !.
gana([A|AS],[B|BS]) :- gana(B,A), gana(AS,[B|BS]), !.


% ganaA ( ?A , +B , ?N )
ganaA(A, B, N) :- esBatallon(B), nonvar(N), batallonN2(N, A), gana(A, B).  
ganaA(A, B, N) :- esBatallon(B), var(N), cantUnidades(B, N), between(1, N, K), batallonN2(K, A), gana(A, B).  
ganaA(A, B, N) :- not(esBatallon(B)), nonvar(N), ejercitoN2(N, A), gana(A,B).
ganaA(A, B, N) :- not(esBatallon(B)), var(N), cantUnidades(B, N), between(1,N,K), ejercitoN2(K, A), gana(A,B).

% ¿Usaron "ejercito"? ¿por qué?
%No, no la utilizamos porque genera infinitos ejercitos sin discriminar por cantidad de unidades. Por lo tanto, por mas que sea obvio
%que ya no encontrara un ejercito A que venza a un potencial ejercito B, seguira buscando hasta el infinito de los tiempos y moriremos
%de anguista. 

% produccionTotal(+A, -PT) : tiene éxito si PT es la producción de recursos que generan A cantidad de aldeanos.
produccionTotal(A,PT) :- PT is A*50.

% aldeanosTotal(-A,+PT) : tiene éxito si A es la menor cantidad de aldeanos que pueden producir PT recursos.
aldeanosTotal(A,PT) :- between(1,PT,A), A*50 >= PT, !.

% Ej 6 : instancia un pueblo para derrotar a un ejército enemigo
% puebloPara ( +En , ?A , -Ed , -Ej )
puebloPara(En, A, Ed, Ej) :- nonvar(A), costoTotal(En, COSTO_TOTAL),
    produccionTotal(A, PRODUCCION_TOTAL), COSTO_TOTAL =< PRODUCCION_TOTAL.

puebloPara(En, A, Ed, Ej) :- var(A), costoTotal(En, COSTO_TOTAL),
    from(1,A),
    produccionTotal(A, PRODUCCION_TOTAL), COSTO_TOTAL =< PRODUCCION_TOTAL.

%(+En)
costoTotal(En, COSTO_TOTAL) :- ganaA(Ej, En, _), edificiosNecesarios(Ej,Ed), costo(Ed, COSTO_EDIFICIOS), costo(Ej, COSTO_EJERCITO), COSTO_TOTAL is COSTO_EDIFICIOS+COSTO_EJERCITO

% Ej 7 : pueblo óptimo (en cantidad de aldenos necesarios)
% puebloOptimoPara( +En , ?A , -Ed , -Ej )
puebloOptimoPara(En, A, Ed, Ej) :- nonvar(A), puebloPara(En,A,Ed,Ej).
puebloOptimoPara(En, A, Ed, Ej) :- var(A), minimaCantidadDeAldeanos(En, A, _, _), puebloPara(En, A, Ed, Ej).

minimaCantidadDeAldeanos(En, A, Ed, Ej) :- var(A), costoTotal(En, COSTO_TOTAL),
    from(1,A),
    produccionTotal(A, PRODUCCION_TOTAL), COSTO_TOTAL =< PRODUCCION_TOTAL, !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cantidadTestsCosto(10).
testCosto(1) :- costo([(arquero, 2)], 180).
testCosto(2) :- costo([cuartel], 300).
testCosto(3) :- costo([establo, cuartel], 700).
testCosto(4) :- costo([establo, cuartel, arqueria], 1030).
testCosto(5) :- costo([(lancero, 5), (arquero, 2)], 580).
testCosto(6) :- costo([(guerrillero, 7), (jinete, 1)], 610).
testCosto(7) :- costo([], 0).
testCosto(8) :- costo([cuartel, arqueria], 630).
testCosto(9) :- costo([(lancero, 1), (arquero, 77), (jinete, 2), (arquero, 8)], 7970).
testCosto(10) :- costo([(guerrillero, 2),(lancero, 3), (guerrillero, 4), (jinete, 5)], 1260).
cantidadTestsEjercito(5).
testEjercito(1) :- ejercito([(lancero, 1), (jinete, 3)]), !.
testEjercito(2) :- ejercito([(jinete, 5)]), !.
testEjercito(3) :- ejercito([(guerrillero, 4), (guerrillero, 2)]), !.
testEjercito(4) :- ejercito([(arquero, 1)]), !.
testEjercito(5) :- ejercito([(arquero, 4), (guerrillero, 3), (jinete, 12), (lancero, 5)]), !.
cantidadTestsEdificios(5).
testEdificios(1) :- edificiosNecesarios([(arquero, 2), (guerrillero, 2)], [arqueria]).
testEdificios(2) :- edificiosNecesarios([(arquero, 11)], [arqueria]).
testEdificios(3) :- edificiosNecesarios([(guerrillero, 3), (lancero, 3)], Ed), mismos(Ed,[arqueria, cuartel]).
testEdificios(4) :- edificiosNecesarios([(guerrillero, 3), (lancero, 3)], Ed), mismos(Ed, [cuartel, arqueria]).
testEdificios(5) :- edificiosNecesarios([(lancero, 1), (jinete, 10)], Ed), mismos(Ed, [establo, cuartel]).
% Auxiliar para chequear si tienen los mismos elementos
mismos(A,B) :- inc(A,B), inc(B,A).
inc([],_).
inc([A|As],Bs) :- member(A,Bs), inc(As,Bs).
cantidadTestsIdS(8).
testIdS(1) :- ids(jinete, jinete, X), X=:=1.
testIdS(2) :- ids(jinete, lancero, X), X=:=0.5.
testIdS(3) :- ids(lancero, jinete, X), X=:=2.
testIdS(4) :- ids(guerrillero, guerrillero, X), X=:=1.
testIdS(5) :- ids(lancero, guerrillero, X), X=:=0.9090909090909091.
testIdS(6) :- ids(arquero, lancero, X), X=:=1.6666666666666667.
testIdS(7) :- ids(arquero, guerrillero, X), X=:=0.5.
testIdS(8) :- ids(lancero, lancero, X), X=:=1.
cantidadTestsGanaA(5).
testGanaA(1) :- ganaA(E, (jinete, 3), 3), gana(E, (jinete, 3)), !.
testGanaA(2) :- not(ganaA(_, (guerrillero, 7), 6)).
testGanaA(3) :- ganaA(E, [(arquero, 1), (jinete, 1), (lancero, 1)], 2), gana(E, [(arquero, 1), (jinete, 1), (lancero, 1)]), !.
testGanaA(4) :- not(ganaA((guerrillero, 2),[(arquero, 2), (lancero, 4), (jinete, 6)], 2)).
testGanaA(5) :- not(ganaA([(arquero, 2), (jinete, 2), (guerrillero, 2)], [(lancero, 6)], 6)).
cantidadTestsPueblo(4).
testPueblo(1) :- En=[(jinete, 3)],
  puebloPara(En, A, Ed, Ej),
  costo(Ej, Ced), costo(Ej, Cej), C is Ced+Cej, A*50 >= C,
  edificiosNecesarios(Ej, Ed), gana(Ej, En), !.
testPueblo(2) :- En=[(arquero, 1), (lancero, 4)],
  puebloPara(En, A, Ed, Ej),
  costo(Ej, Ced), costo(Ej, Cej), C is Ced+Cej, A*50 >= C,
  edificiosNecesarios(Ej, Ed), gana(Ej,En), !.
testPueblo(3) :- En=[(guerrillero, 5)],
  puebloPara(En, A, Ed, Ej),
  costo(Ej, Ced), costo(Ej, Cej), C is Ced+Cej, A*50 >= C,
  edificiosNecesarios(Ej, Ed), gana(Ej,En), !.
testPueblo(4) :- En=[(jinete, 1), (lancero, 1), (guerrillero, 2), (arquero, 2)],
  puebloPara(En, A, Ed, Ej),
  costo(Ej, Ced), costo(Ej, Cej), C is Ced+Cej, A*50 >= C,
  edificiosNecesarios(Ej, Ed), gana(Ej,En), !.
cantidadTestsPuebloOptimo(5).
testPuebloOptimo(1) :- En=[(jinete,2)], puebloOptimoPara(En,8,[cuartel],[(lancero,1)]), !.
testPuebloOptimo(2) :- En=[(jinete,2)], puebloOptimoPara(En,8,[arqueria],[(guerrillero,1)]), !.%Si no optimizan recursos.
testPuebloOptimo(3) :- En=[(arquero,2)], puebloOptimoPara(En,8,[arqueria],[(guerrillero,1)]), !.
testPuebloOptimo(4) :- En=[(guerrillero, 2), (arquero, 3)], puebloOptimoPara(En, 10, [arqueria], [(guerrillero, 2)]), !.
testPuebloOptimo(5) :- En=[(arquero,4)], not(puebloOptimoPara(En,5,_,_)).
tests(costo) :- cantidadTestsCosto(M), forall(between(1,M,N), testCosto(N)).
tests(ejercito) :- cantidadTestsEjercito(M), forall(between(1,M,N), testEjercito(N)).
tests(edificios) :- cantidadTestsEdificios(M), forall(between(1,M,N), testEdificios(N)).
tests(ids) :- cantidadTestsIdS(M), forall(between(1,M,N), testIdS(N)).
tests(ganaA) :- cantidadTestsGanaA(M), forall(between(1,M,N), testGanaA(N)).
tests(pueblo) :- cantidadTestsPueblo(M), forall(between(1,M,N), testPueblo(N)).
tests(puebloOptimo) :- cantidadTestsPuebloOptimo(M), forall(between(1,M,N), testPuebloOptimo(N)).
tests(todos) :-
  tests(costo),
  tests(ejercito),
  tests(edificios),
  tests(ids),
  tests(ganaA),
  tests(pueblo),
  tests(puebloOptimo).
tests :- tests(todos).