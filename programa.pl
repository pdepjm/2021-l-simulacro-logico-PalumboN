% Aquí va el código.

% Individuos: Jugadores y Elementos

% tiene(Jugador, Elemento)
% requiere(ElementoCompuesto, ElementosRequeridos)

% Ana tiene agua, vapor, tierra y hierro. 
elementos(ana, [agua, vapor, tierra, hierro]).
% Beto tiene lo mismo que Ana.
elementos(beto, Elementos) :- elementos(ana, Elementos).
% Cata tiene fuego, tierra, agua y aire, pero no tiene vapor.
elementos(cata, [fuego, tierra, agua, aire]).

% Este es el predicado de posta, el que relaciona una persona con un elemento.
tiene(Persona, Elemento) :-
  elementos(Persona, ElementosQueTiene),
  member(Elemento, ElementosQueTiene).

jugador(Jugador) :- tiene(Jugador, _).


%Para construir pasto hace falta agua y tierra, 
requiere(pasto, [agua, tierra]).
% para construir hierro hace falta fuego, agua y tierra, 
requiere(hierro, [fuego, agua, tierra]).
% y para hacer huesos hace falta pasto y agua. 
requiere(huesos, [pasto, agua]).
% Para hacer presión hace falta hierro y vapor (que se construye con agua y fuego).
requiere(presion, [hierro, vapor]).
requiere(vapor, [agua, fuego]).
% Por último, para hacer una play station hace falta silicio (que se construye sólo con tierra), hierro y plástico (que se construye con huesos y presión).
requiere(playstation, [silicio, hierro, plastico]).
requiere(silicio, [tierra]).
requiere(plastico, [huesos, presion]).

requiereElemento(ElementoCompuesto, Material) :-
  requiere(ElementoCompuesto, Materiales),
  member(Material, Materiales).

cantidadDeIngredientes(ElementoCompuesto, Cantidad) :-
  requiere(ElementoCompuesto, Materiales),
  length(Materiales, Cantidad).
  

elemento(Elemento) :- tiene(_, Elemento).
elemento(Elemento) :- requiereElemento(Elemento, _).
elemento(Elemento) :- requiereElemento(_, Elemento).

% Saber si un jugador tieneIngredientesPara construir un elemento, que es cuando tiene ahora en su inventario todo lo que hace falta.

tieneIngredientesPara(Jugador, ElementoCompuesto) :-
  jugador(Jugador),
  requiere(ElementoCompuesto, _),
  forall(requiereElemento(ElementoCompuesto, Material), tiene(Jugador, Material)).


% Por ejemplo, ana tiene los ingredientes para el pasto, pero no para el vapor. 

:-begin_tests(tiene_ingredientes_para).
test(ana_tiene_pasto, nondet) :- tieneIngredientesPara(ana, pasto).
test(ana_no_tiene_pasto, fail) :- tieneIngredientesPara(ana, vapor).
test(inversible_para_persona, set(Persona == [ana, beto, cata])) :- 
  tieneIngredientesPara(Persona, pasto).
test(inversible_para_elemento, set(Elemento == [pasto,presion,silicio])) :- 
  tieneIngredientesPara(ana, Elemento).
:-end_tests(tiene_ingredientes_para).



% Saber si un elemento estaVivo. Se sabe que el agua, el fuego y todo lo que fue construido a partir de alguno de ellos, están vivos. Debe funcionar para cualquier nivel.

% estaVivo(Elemento).
estaVivo(agua).
estaVivo(fuego).
estaVivo(ElementoCompuesto) :- requiereElemento(ElementoCompuesto, Material), estaVivo(Material).

% Por ejemplo, la play station y los huesos están vivos, pero el silicio no.
:- begin_tests(esta_vivo).
test(la_play_esta_viva, nondet) :- estaVivo(playstation).
test(los_huesos_estan_vivos, nondet) :- estaVivo(huesos).
test(silicio_no_esta_vivo, fail) :- estaVivo(silicio).
test(elementos_vivos, set(Elemento == [agua,fuego,hierro,huesos,pasto,plastico,playstation,presion,vapor])) :-
  estaVivo(Elemento).
:- end_tests(esta_vivo).


% Conocer las personas que puedeConstruir un elemento, para lo que se necesita tener los ingredientes ahora en el inventario y además contar con una o más herramientas que sirvan para construirlo. 
% Para los elementos vivos sirve el libro de la vida (y para los elementos no vivos el libro inerte). Además, las cucharas y círculos sirven cuando soportan la cantidad de ingredientes del elemento (las cucharas soportan tantos ingredientes como centímetros de longitud/10, y los círculos alquímicos soportan tantos ingredientes como metros de diámetro * cantidad de niveles).

herramienta(ana, circulo(50,3)).
herramienta(ana, cuchara(40)).
herramienta(beto, circulo(20,1)).
herramienta(beto, libro(inerte)).
herramienta(cata, libro(vida)).
herramienta(cata, circulo(100,5)).

% puedeConstruir(Elemento, Persona)
puedeConstruir(Persona, Elemento) :-
  tieneIngredientesPara(Persona, Elemento),
  tieneHerramientaParaConstruir(Persona, Elemento).

tieneHerramientaParaConstruir(Persona, Elemento) :-
  herramienta(Persona, Herramienta),
  sirveParaConstruir(Herramienta, Elemento).

sirveParaConstruir(libro(vida), Elemento) :- estaVivo(Elemento).
sirveParaConstruir(libro(inerte), Elemento) :- not(estaVivo(Elemento)).

sirveParaConstruir(Herramienta, Elemento) :- 
  cantidadSoportada(Herramienta, CantidadSoportada),
  cantidadDeIngredientes(Elemento, CantidadIngredientes),
  CantidadSoportada >= CantidadIngredientes.

% cantidadSoportada(Herramienta, Cantidad)
cantidadSoportada(cuchara(Longitud), Cantidad) :-
  Cantidad is Longitud / 10.
cantidadSoportada(circulo(Diametro, Niveles), Cantidad) :-
  Cantidad is Diametro / 100 * Niveles.

% Los libros no soportan ninguna cantidad -> Universo cerrado

% Por ejemplo, beto puede construir el silicio (porque tiene tierra y tiene el libro inerte, que le sirve para el silicio), pero no puede construir la presión (porque a pesar de tener hierro y vapor, no cuenta con herramientas que le sirvan para la presión). Ana, por otro lado, sí puede construir silicio y presión.



% Saber si alguien es todopoderoso, que es cuando tiene todos los elementos primitivos (los que no pueden construirse a partir de nada) y además cuenta con herramientas que sirven para construir cada elemento que no tenga.

% todopoderoso(Persona)
todopoderoso(Persona) :-
  jugador(Persona),
  tieneTodosLosPrimitivos(Persona),
  tieneHerramientasParaFaltantes(Persona).

tieneTodosLosPrimitivos(Persona) :-
  forall(primitivo(Elemento), tiene(Persona, Elemento)).

primitivo(Elemento) :- 
  elemento(Elemento),
  not(requiereElemento(Elemento, _)).

tieneHerramientasParaFaltantes(Persona) :-
  forall(leFalta(Persona, Elemento), tieneHerramientaParaConstruir(Persona, Elemento)).

leFalta(Persona, Elemento) :-
  elemento(Elemento),
  not(tiene(Persona, Elemento)).

% Por ejemplo, cata es todopoderosa, pero beto no.


% Conocer quienGana, que es quien puede construir más cosas.

% quienGana(Persona)
quienGana(Persona) :-
  jugador(Persona),
  forall(contrincante(Persona, Contrincante), contruyeMasCosas(Persona, Contrincante)).

contrincante(Persona, Contrincante) :-
  jugador(Persona), jugador(Contrincante),
  Persona \= Contrincante.

contruyeMasCosas(Ganador, Perdedor) :-
  cantidadDeElementosQuePuedeConstruir(Ganador, Mayor),
  cantidadDeElementosQuePuedeConstruir(Perdedor, Menor),
  Mayor > Menor.

cantidadDeElementosQuePuedeConstruir(Persona, Cantidad) :-
  findall(Elemento, puedeConstruir(Persona, Elemento), Elementos),
  list_to_set(Elementos, ElementosSinRepetidos),
  length(ElementosSinRepetidos, Cantidad).

% Por ejemplo, cata gana, pero beto no.



% Hacer una nueva versión del predicado puedeConstruir (se puede llamar puedeLlegarATener) para considerar todo lo que podría construir si va combinando todos los elementos que tiene (y siempre y cuando tenga alguna herramienta que le sirva para construir eso). 
% Un jugador puede llegar a tener un elemento si o bien lo tiene, o bien tiene alguna herramienta que le sirva para hacerlo y cada ingrediente necesario para construirlo puede llegar a tenerlo a su vez.

puedeLlegarATener(Persona, Elemento) :-
  tiene(Persona, Elemento).

puedeLlegarATener(Persona, Elemento) :-
  tieneHerramientaParaConstruir(Persona, Elemento),
  forall(requiereElemento(Elemento, Ingrediente), puedeLlegarATener(Persona, Ingrediente)).
  
%Por ejemplo, cata podría llegar a tener una play station, pero beto no.