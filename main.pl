:- prolog_load_context(directory, Dir), working_directory(_, Dir).

:- use_module(library(pce)).

:- dynamic
    ojos/1,
    pico/1,
    cuerpo/1,
    patas/1,
    tarsos/1,
    loras/1,
    alas/1,
    vientre/1,
    corona/1,
    espalda/1,
    habitat/1,
    alimentacion/1,
    ave/13.

% Funcion que se ejecuta al inicio del programa para cargar las aves
cargar_aves :-
    format('Iniciando la carga de aves desde el archivo...\n'),
    % Abre el archivo de la base de datos de aves en modo lectura
    open('avesColomos.pl', read, Stream),
    % Llama a la funci�n para leer y cargar las aves
    leer_aves(Stream),
    % Cierra el archivo
    close(Stream),
    format('Archivo de aves le�do.\n'),
    % Ahora mostramos todas las aves cargadas
    mostrar_aves_cargadas.
% Muestra todas las aves que han sido cargadas en la base de conocimientos
mostrar_aves_cargadas :-
    format('Aves cargadas en la base de conocimientos:\n'),
    (   % Comprobar si existe al menos una definici�n de ave con 13 atributos
        ave(_, _, _, _, _, _, _, _, _, _, _, _, _)
    ->  % Usar forall/2 para iterar sobre las aves si existen
        forall(
            ave(Nombre, Ojos, Pico, Cuerpo, Patas, Tarsos, Loras, Alas, Vientre, Corona, Espalda, Habitat, Alimentacion),
            format('Ave: ~w, Ojos: ~w, Pico: ~w, Cuerpo: ~w, Patas: ~w, Tarsos: ~w, Loras: ~w, Alas: ~w, Vientre: ~w, Corona: ~w, Espalda: ~w, Habitat: ~w, Alimentacion: ~w\n', [Nombre, Ojos, Pico, Cuerpo, Patas, Tarsos, Loras, Alas, Vientre, Corona, Espalda, Habitat, Alimentacion])
        )
    ;   % En caso de que no haya aves, mostrar un mensaje
        format('No hay aves registradas en la base de conocimientos.\n')
    ).

% Funci�n recursiva para leer el archivo de aves linea por linea
leer_aves(Stream) :-
    % Comprueba si hemos alcanzado el final del archivo
    at_end_of_stream(Stream),
    !,  % Si es asi, termina la recursi�n con corte (!) para prevenir backtracking
    format('Fin del archivo alcanzado.\n').
leer_aves(Stream) :-
    % No hemos alcanzado el final, lee la siguiente l�nea
    read(Stream, Ave),
    % Aseg�rate de que la l�nea le�da es una cl�usula v�lida
    (   is_ave(Ave)
    ->  % Si es una cl�usula de ave, �sala para hacer un assertz
        assertz(Ave),
        format('Ave cargada: ~w\n', [Ave])
    ;   % Si no, ignora la l�nea
        true
    ),
    % Contin�a con la siguiente l�nea
    leer_aves(Stream).

% Verifica si la cl�usula le�da corresponde a una definici�n de ave
is_ave(Ave) :-
    % Comprueba si Ave es una cl�usula que comienza con ave(...)
    Ave =.. [ave|_].





%Ejecucion de la interfaz principal
start_gui :-
    % Carga las aves de la BD
    format('Iniciando la interfaz gr�fica...\n'),
    cargar_aves,

    new(MiVentana, dialog('Sistema experto, Universo de Stephen King')),

    send(MiVentana, append, label(instructivo, 'IMPORTANTE: Si no tiene informacion de un atributo especifico, escriba \'desconocido\'.')),
    send(MiVentana, append, label(instructivo, 'NOTA: Si son multiples valores para un atributo especifico escriba por ejemplo: \'rojo,azul\'')),

    % Crear campos de texto para cada atributo relevante
    send(MiVentana, append, new(Ojos, text_item('Libro'))),
    send(MiVentana, append, new(Pico, text_item('Edad'))),
    send(MiVentana, append, new(Cuerpo, text_item('Genero'))),
    send(MiVentana, append, new(Patas, text_item('Ocupacion'))),
    send(MiVentana, append, new(Tarsos, text_item('Estado'))),

    % Los siguientes campos son opcionales, ya que no aparecen en todas las definiciones de aves
    send(MiVentana, append, new(Loras, text_item('Fecha de nacimiento'))), % Si existe el atributo loras
    send(MiVentana, append, new(Alas, text_item('Fecha de fallecimiento'))), % Si existe el atributo alas
    send(MiVentana, append, new(Vientre, text_item('Ciudad de origen'))), % Si existe el atributo vientre
    send(MiVentana, append, new(Corona, text_item('Habilidad especial'))), % Si existe el atributo corona
    send(MiVentana, append, new(Espalda, text_item('Complexion'))), % Si existe el atributo espalda

    % Campos para h�bitat y alimentaci�n
    send(MiVentana, append, new(Habitat, text_item('Color de piel'))),
    send(MiVentana, append, new(Alimentacion, text_item('Altura'))),

    % Crear botones
    % Bot�n para identificar ave por sus atributos
    send(MiVentana, append, button('Identificar', message(@prolog, buscar_ave,
    Ojos?selection, Pico?selection, Cuerpo?selection, Patas?selection, Tarsos?selection,
    Loras?selection, Alas?selection, Vientre?selection, Corona?selection, Espalda?selection,
    Habitat?selection, Alimentacion?selection))),

    % Bot�n para identificar ave por s�lo su nombre
    send(MiVentana, append, button('Identificar por nombre', message(@prolog, buscar_ave_nombre))),

    % Bot�n para agregar ave
    send(MiVentana, append, button('Agregar', message(@prolog, gui_agregar_ave,
    Ojos?selection, Pico?selection, Cuerpo?selection, Patas?selection, Tarsos?selection,
    Loras?selection, Alas?selection, Vientre?selection, Corona?selection, Espalda?selection,
    Habitat?selection, Alimentacion?selection))),

    % Boton para mostrar todas las aves de la base de conocimientos
    send(MiVentana, append, button('Mostrar todos', message(@prolog, mostrar_aves))),

    % Boton para salir de la interfaz y limpiar la base de conocimientos de las aves
    send(MiVentana, append, button('Salir', message(@prolog, limpiar_datos_y_salir, MiVentana))),
    send(MiVentana, open).






% Se recogen los valores de las aves y su nombre
gui_agregar_ave(Ojos, Pico, Cuerpo, Patas, Tarsos, Loras, Alas, Vientre, Corona, Espalda, Habitat, Alimentacion) :-

    % Imprime los valores recibidos
    format('Recibido - Ojos: ~w, Pico: ~w, Cuerpo: ~w, Patas: ~w, Tarsos: ~w, Loras: ~w, Alas: ~w, Vientre: ~w, Corona: ~w, Espalda: ~w, Habitat: ~w, Alimentacion: ~w\n',
        [Ojos, Pico, Cuerpo, Patas, Tarsos, Loras, Alas, Vientre, Corona, Espalda, Habitat, Alimentacion]),


    new(Diag, dialog('Agregar personaje')),
    send(Diag, append, new(Nombre, text_item(nombre))),
    send(Diag, append, button('Aceptar', message(@prolog, nuevaAve,
        Nombre?selection, Ojos, Pico, Cuerpo, Patas, Tarsos, Loras, Alas, Vientre, Corona, Espalda, Habitat, Alimentacion, Diag))),
    send(Diag, open).

nuevaAve(NombreAveInput, Ojos, Pico, Cuerpo, Patas, Tarsos, Loras, Alas, Vientre, Corona, Espalda, Habitat, Alimentacion, Diag) :-
    % Realiza el assertz con los valores recolectados
    replace_spaces_with_underscores(NombreAveInput, NombreAve),
    assertz(ave(NombreAve, Ojos, Pico, Cuerpo, Patas, Tarsos, Loras, Alas, Vientre, Corona, Espalda, Habitat, Alimentacion)),

    % Abre el archivo en modo de anexi�n
    open('AvesColomos.pl', append, File),

    % Escribe la informaci�n del ave en el archivo
    maplist(ensure_value, [Ojos, Pico, Cuerpo, Patas, Tarsos, Loras, Alas, Vientre, Corona, Espalda, Habitat, Alimentacion], FormattedValues),
    format(File, 'ave(~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w).\n', [NombreAve|FormattedValues]),

    % Cierra el archivo
    close(File),

    % Muestra el mensaje de confirmaci�n
    send(@display, inform, 'La ave ha sido agregada en la base de datos'),

    % Cierra la ventana de di�logo despu�s de agregar la ave
    send(Diag, destroy).

% Asegurarse de que cada valor est� presente, de lo contrario utilizar 'null'
% Adem�s, si el valor contiene comas, lo convierte en una lista de Prolog.
ensure_value(Value, Result) :-
    (   nonvar(Value), Value \= '' ->
        (   split_string(Value, ",", " ", Parts),
            Parts = [_|_] -> % Verifica que haya m�s de un elemento
            % Convertir las partes en una lista de �tomos
            maplist(atom_string, AtomParts, Parts),
            % Crear una representaci�n de lista Prolog en forma de cadena
            atomics_to_string(AtomParts, ',', AtomList),
            format(atom(Result), '[~w]', [AtomList])
        ;   % Solo hay una parte, as� que usa el valor como est�
            Result = Value
        )
    ;   Result = 'null'
    ).

% Predicado auxiliar para reemplazar espacios con guiones bajos
replace_spaces_with_underscores(Input, Output) :-
    atom_codes(Input, Codes),
    maplist(replace_space, Codes, ReplacedCodes),
    atom_codes(Output, ReplacedCodes).

% Reemplaza el c�digo ASCII del espacio (32) por el del gui�n bajo (95)
replace_space(32, 95) :- !.
replace_space(Code, Code).







% Mostrar todas las aves en la base de conocimientos
mostrar_aves :-
    % Crear una ventana y un widget de label
    new(Dialogo, dialog('Personajes Registrados')),
    send(Dialogo, scrollbars, both),
    new(T, label(texto, '')),
    send(T, font, font(times, roman, 12)),
    send(Dialogo, append, T),

    % Verificar si hay alguna ave en la base de conocimientos
    (   % Comprobar si existe al menos una definici�n de ave con 13 atributos
        ave(_, _, _, _, _, _, _, _, _, _, _, _, _)
    ->  % Usar forall/2 para iterar sobre las aves si existen
        forall(
            ave(Nombre, Ojos, Pico, Cuerpo, Patas, Tarsos, Loras, Alas, Vientre, Corona, Espalda, Habitat, Alimentacion),
            (   % Construir la cadena de texto para cada ave con separadores
                with_output_to(string(Str),
                    format('Nombre: ~w\nLibro: ~w\nEdad: ~w\nGenero: ~w\nOcupacion: ~w\nEstado: ~w\nFecha de nacimiento: ~w\nFecha de fallecimiento: ~w\nCiudad de origen: ~w\nHabilidad Especial: ~w\nComplexion: ~w\nColor de piel: ~w\nAltura: ~w\n: ---------------------------------------\n',
                           [Nombre, Ojos, Pico, Cuerpo, Patas, Tarsos, Loras, Alas, Vientre, Corona, Espalda, Habitat, Alimentacion])),
                % Agregar la cadena de texto al label
                send(T, append, Str)
            )
        )
    ;   % En caso de que no haya aves, mostrar un mensaje
        send(T, value, 'No hay aves registradas en la base de conocimientos.')
    ),

    % Mostrar la ventana
    send(Dialogo, open).





% Predicado para limpiar los datos de aves y cerrar la ventana
limpiar_datos_y_salir(Ventana) :-
    retractall(ave(_, _, _, _, _, _, _, _, _, _, _, _, _)),
    send(Ventana, destroy).





% Identificar aves por s�lo su nombre
buscar_ave_nombre :-
    new(DialogoBuscar, dialog('Buscar por Nombre')),
    send(DialogoBuscar, append, label(info, 'Ingresa el nombre del personaje:')),
    send(DialogoBuscar, append, new(NombreAve, text_item(nombre))),
    send(DialogoBuscar, append, button('Buscar', message(@prolog, identificar_ave_por_nombre, NombreAve?selection, DialogoBuscar))),
    send(DialogoBuscar, default_button, 'Buscar'),  % Establece el bot�n por defecto
    send(DialogoBuscar, open).

% Funci�n para buscar un ave por nombre y mostrar la informaci�n
identificar_ave_por_nombre(NombreAveInput, DialogoBuscar) :-
    % Reemplaza espacios en el nombre del ave con guiones bajos
    replace(NombreAveInput, NombreAve),
    (   ave(NombreAve, Ojos, Pico, Cuerpo, Patas, Tarsos, Loras, Alas, Vientre, Corona, Espalda, Habitat, Alimentacion)
    ->  send(DialogoBuscar, destroy), % Cierra el di�logo de b�squeda
        mostrar_informacion_ave(NombreAve, Ojos, Pico, Cuerpo, Patas, Tarsos, Loras, Alas, Vientre, Corona, Espalda, Habitat, Alimentacion)
    ;   send(@display, inform, 'Personje no encontrado en la base de datos.'),
        fail
    ).

% Funci�n para mostrar la informaci�n de la ave encontrada
mostrar_informacion_ave(NombreAve, Ojos, Pico, Cuerpo, Patas, Tarsos, Loras, Alas, Vientre, Corona, Espalda, Habitat, Alimentacion) :-
    new(DialogoResultado, dialog('Informacion del Personaje')),

    % Convertir los atributos a cadenas si son listas para su visualizaci�n
    lista_a_cadena(NombreAve, NombreString),
    lista_a_cadena(Ojos, OjosString),
    lista_a_cadena(Pico, PicoString),
    lista_a_cadena(Cuerpo, CuerpoString),
    lista_a_cadena(Patas, PatasString),
    lista_a_cadena(Tarsos, TarsosString),
    lista_a_cadena(Loras, LorasString),
    lista_a_cadena(Alas, AlasString),
    lista_a_cadena(Vientre, VientreString),
    lista_a_cadena(Corona, CoronaString),
    lista_a_cadena(Espalda, EspaldaString),
    lista_a_cadena(Habitat, HabitatString),
    lista_a_cadena(Alimentacion, AlimentacionString),


    send(DialogoResultado, append, label(nombre, string('Nombre: %s', NombreString))),
    send(DialogoResultado, append, label(ojos, string('Libro: %s', OjosString))),
    send(DialogoResultado, append, label(pico, string('Edad: %s', PicoString))),
    send(DialogoResultado, append, label(cuerpo, string('Genero: %s', CuerpoString))),
    send(DialogoResultado, append, label(patas, string('Ocupacion: %s', PatasString))),
    send(DialogoResultado, append, label(tarsos, string('Estado: %s', TarsosString))),
    send(DialogoResultado, append, label(loras, string('Fecha de nacimiento: %s', LorasString))),
    send(DialogoResultado, append, label(alas, string('Fecha de fallecimiento: %s', AlasString))),
    send(DialogoResultado, append, label(vientre, string('Ciudad de origen: %s', VientreString))),
    send(DialogoResultado, append, label(corona, string('Habilidad especial: %s', CoronaString))),
    send(DialogoResultado, append, label(espalda, string('Complexion: %s', EspaldaString))),
    send(DialogoResultado, append, label(habitat, string('Color de piel: %s', HabitatString))),
    send(DialogoResultado, append, label(alimentacion, string('Altura: %s', AlimentacionString))),

    replace_spaces(NombreString, URLFriendlyNombre),

    % Crear el recuadro modificable con el enlace de b�squeda
    SearchURLBase = 'https://www.google.com/search?q=stephen%20king+',
    atom_concat(SearchURLBase, URLFriendlyNombre, SearchURL),
    send(DialogoResultado, append, label(mas_info, 'M�s informaci�n en:')),
    send(DialogoResultado, append, text_item(search_url, SearchURL)),

    % Bot�n para cerrar la ventana de di�logo
    send(DialogoResultado, append, button('Cerrar', message(DialogoResultado, destroy))),

    % Ajustar el tama�o de la ventana al contenido y mostrarla
    send(DialogoResultado, open).

    % Auxiliar para convertir una lista a una cadena, manteniendo �tomos individuales como est�n
lista_a_cadena(Attributo, Cadena) :-
    (   is_list(Attributo) -> atomic_list_concat(Attributo, ', ', Cadena)
    ;   Attributo == '' -> Cadena = 'Desconocido'
    ;   Cadena = Attributo
    ).

% Reemplaza espacios en el nombre del ave con signos m�s (+) para la URL
replace_spaces(String, URLFriendlyString) :-
    split_string(String, " ", "+", SubStrings),
    atomic_list_concat(SubStrings, "+", URLFriendlyString).

% Reemplaza espacios en una cadena con guiones bajos
replace(Input, Output) :-
    split_string(Input, " ", "", Parts),
    atomic_list_concat(Parts, '_', Output).






% Identificar ave por sus atributos con el mayor n�mero de coincidencias
buscar_ave(Ojos, Pico, Cuerpo, Patas, Tarsos, Loras, Alas, Vientre, Corona, Espalda, Habitat, Alimentacion) :-
    % Encuentra todas las aves con el conteo de coincidencias
    findall(Nombre-Coincidencias, (
        ave(Nombre, OjosAve, PicoAve, CuerpoAve, PatasAve, TarsosAve, LorasAve, AlasAve, VientreAve, CoronaAve, EspaldaAve, HabitatAve, AlimentacionAve),
        coincidencias([Ojos, Pico, Cuerpo, Patas, Tarsos, Loras, Alas, Vientre, Corona, Espalda, Habitat, Alimentacion],
                      [OjosAve, PicoAve, CuerpoAve, PatasAve, TarsosAve, LorasAve, AlasAve, VientreAve, CoronaAve, EspaldaAve, HabitatAve, AlimentacionAve],
                      0, Coincidencias)
    ), AvesConCoincidencias),

    % Encuentra el mayor n�mero de coincidencias
    max_coincidencias(AvesConCoincidencias, MaxCoincidencias),

    % Crear el di�logo y mostrar los resultados o un mensaje si no hay coincidencias
    new(DialogoResultado, dialog('Resultados')),
    (   MaxCoincidencias == 0 ->
    send(DialogoResultado, append, label('', 'Coincidencias no encontradas.'))
    ;   % Filtra las aves que tienen el mayor n�mero de coincidencias
    include(has_max_coincidencias(MaxCoincidencias), AvesConCoincidencias, AvesFiltradas),
    forall(member(Ave-Coincidencias, AvesFiltradas), (
        % Sustituye espacios por '+', ya que en las URLs los espacios se representan con '+'
        replace_spaces(Ave, URLFriendlyNombre),

        % Concatena la base de la URL con el nombre de la ave formateado para URL
        SearchURLBase = 'https://www.google.com/search?q=stephen%20king+',
        atom_concat(SearchURLBase, URLFriendlyNombre, SearchURL),

        % A�ade el nombre de la ave y el n�mero de coincidencias al di�logo
        send(DialogoResultado, append, label('', string('Personaje coincidente: %s (Coincidencias: %d)', Ave, Coincidencias))),

        % A�ade la etiqueta 'M�s informaci�n en:' y el enlace de b�squeda al di�logo
        send(DialogoResultado, append, label(mas_info, 'M�s informaci�n en:')),
        send(DialogoResultado, append, text_item(search_url, SearchURL)),

        % A�ade un separador visual
        send(DialogoResultado, append, label('', '-----------------'))
    ))
    ),

    % Bot�n para cerrar la ventana de di�logo
    send(DialogoResultado, append, button('Cerrar', message(DialogoResultado, destroy))),

    % Ajustar el tama�o de la ventana al contenido y mostrarla
    send(DialogoResultado, open).


max_coincidencias(AvesConCoincidencias, MaxCoincidencias) :-
    maplist(second, AvesConCoincidencias, ListaCoincidencias),
    max_list(ListaCoincidencias, MaxCoincidencias).

second(_-X, X).

has_max_coincidencias(MaxCoincidencias, _-Coincidencias) :-
    Coincidencias = MaxCoincidencias.


% Auxiliar para contar coincidencias
coincidencias([], [], Contador, Contador).
coincidencias([Attr|RestoAttrs], [AveAttr|RestoAveAttrs], Contador, Coincidencias) :-
    (   nonvar(Attr),
        (   is_list(AveAttr) % Verifica si el atributo en la base de conocimientos es una lista
        ->  split_string(Attr, ",", " ", AttrList),
            maplist(trim, AttrList, TrimmedList),
            maplist(atom_string, AtomList, TrimmedList), % Convierte la lista de strings a �tomos si es necesario
            (   subset(AtomList, AveAttr) % Verifica si todos los elementos de AtomList est�n en AveAttr
            ->  NuevoContador is Contador + 1
            ;   NuevoContador = Contador
            )
        ;   Attr = AveAttr
        ->  NuevoContador is Contador + 1
        ;   NuevoContador = Contador
        )
    ),
    coincidencias(RestoAttrs, RestoAveAttrs, NuevoContador, Coincidencias).

% Recorta espacios de un string
trim(S, T) :-
    string_codes(S, SC),
    trim_helper(SC, TC),
    string_codes(T, TC).

trim_helper(S, T) :-
    % Remove leading spaces
    append(Leading, Rem, S),
    \+ (member(C, Leading), \+ char_type(C, space)),
    % Remove trailing spaces
    append(T, Trailing, Rem),
    \+ (member(C, Trailing), \+ char_type(C, space)).
    
:-start_gui.
