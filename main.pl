:- prolog_load_context(directory, Dir), working_directory(_, Dir).

:- use_module(library(pce)).

:- dynamic
    nombrelibro/1,
    edad/1,
    genero/1,
    ocupacion/1,
    estado/1,
    fechanacimiento/1,
    fechafallecimiento/1,
    ciudadorigen/1,
    habilidadespecial/1,
    complexion/1,
    colorpiel/1,
    altura/1,
    libro/13.

% Funcion que se ejecuta al inicio del programa para cargar los libros
cargar_libros :-
    format('Iniciando la carga de libros desde el archivo...\n'),
    % Abre el archivo de la base de datos de libros en modo lectura
    open('libros.pl', read, Stream),
    % Llama a la funcion para leer y cargar los libros
    leer_libros(Stream),
    % Cierra el archivo
    close(Stream),
    format('Archivo de libros leido.\n'),
    % Ahora mostramos todas los libros cargadas
    mostrar_libros_cargadas.
% Muestra todas los libros que han sido cargadas en la base de conocimientos
mostrar_libros_cargadas :-
    format('Libros cargados en la base de conocimientos:\n'),
    (   % Comprobar si existe al menos una definicion de libro con 13 atributos
        libro(_, _, _, _, _, _, _, _, _, _, _, _, _)
    ->  % Usar forall/2 para iterar sobre las libros si existen
        forall(
            libro(Nombre, NombreDelLibro, Edad, Genero, Ocupacion, Estado, Fechanacimiento, Fechafallecimiento, Ciudadorigen, Habilidadespecial, Complexion, Colorpiel, Altura),
            format('Personaje: ~w, NombreDelLibro: ~w, Edad: ~w, Genero: ~w, Ocupacion: ~w, Estado: ~w, Fechanacimiento: ~w, Fechafallecimiento: ~w, Ciudadorigen: ~w, Habilidadespecial: ~w, Complexion: ~w, Colorpiel: ~w, Altura: ~w\n', [Nombre, NombreDelLibro, Edad, Genero, Ocupacion, Estado, Fechanacimiento, Fechafallecimiento, Ciudadorigen, Habilidadespecial, Complexion, Colorpiel, Altura])
        )
    ;   % En caso de que no haya libros, mostrar un mensaje
        format('No hay libros registrados en la base de conocimientos.\n')
    ).

% Funcion recursiva para leer el archivo de libros linea por linea
leer_libros(Stream) :-
    % Comprueba si hemos alcanzado el final del archivo
    at_end_of_stream(Stream),
    !,  % Si es asi, termina la recursion con corte (!) para prevenir backtracking
    format('Fin del archivo alcanzado.\n').
leer_libros(Stream) :-
    % No hemos alcanzado el final, lee la siguiente linea
    read(Stream, Libro),
    % Asegurate de que la linea leida es una clausula valida
    (   is_libro(Libro)
    ->  % Si es una clausula de libro, usala para hacer un assertz
        assertz(Libro),
        format('Libro cargado: ~w\n', [Libro])
    ;   % Si no, ignora la linea
        true
    ),
    % Continua con la siguiente linea
    leer_libros(Stream).

% Verifica si la clausula leida corresponde a una definicion de libro
is_libro(Libro) :-
    % Comprueba si Libro es una clausula que comienza con libro(...)
    Libro =.. [libro|_].





%Ejecucion de la interfaz principal
start_gui :-
    % Carga las libros de la BD
    format('Iniciando la interfaz grafica...\n'),
    cargar_libros,

    new(MiVentana, dialog('Sistema experto, Universo de Stephen King')),

    send(MiVentana, append, label(instructivo, 'IMPORTANTE: Si no tiene informacion de un atributo especifico, escriba \'desconocido\'.')),
    send(MiVentana, append, label(instructivo, 'NOTA: Si son multiples valores para un atributo especifico escriba por ejemplo: \'rojo,azul\'')),

    % Crear campos de texto para cada atributo relevante
    send(MiVentana, append, new(NombreDelLibro, text_item('Libro'))),
    send(MiVentana, append, new(Edad, text_item('Edad'))),
    send(MiVentana, append, new(Genero, text_item('Genero'))),
    send(MiVentana, append, new(Ocupacion, text_item('Ocupacion'))),
    send(MiVentana, append, new(Estado, text_item('Estado'))),

    % Los siguientes campos son opcionales
    send(MiVentana, append, new(Fechanacimiento, text_item('Fecha de nacimiento'))),
    send(MiVentana, append, new(Fechafallecimiento, text_item('Fecha de fallecimiento'))),
    send(MiVentana, append, new(Ciudadorigen, text_item('Ciudad de origen'))),
    send(MiVentana, append, new(Habilidadespecial, text_item('Habilidad especial'))),
    send(MiVentana, append, new(Complexion, text_item('Complexion'))),
    send(MiVentana, append, new(Colorpiel, text_item('Color de piel'))),
    send(MiVentana, append, new(Altura, text_item('Altura'))),

    % Crear botones
    % Boton para identificar libro por sus atributos
    send(MiVentana, append, button('Identificar', message(@prolog, buscar_libro,
    NombreDelLibro?selection, Edad?selection, Genero?selection, Ocupacion?selection, Estado?selection,
    Fechanacimiento?selection, Fechafallecimiento?selection, Ciudadorigen?selection, Habilidadespecial?selection, Complexion?selection,
    Colorpiel?selection, Altura?selection))),

    % Boton para identificar libro por solo su nombre
    send(MiVentana, append, button('Identificar por nombre', message(@prolog, buscar_libro_nombre))),

    % Boton para agregar libro
    send(MiVentana, append, button('Agregar', message(@prolog, gui_agregar_libro,
    NombreDelLibro?selection, Edad?selection, Genero?selection, Ocupacion?selection, Estado?selection,
    Fechanacimiento?selection, Fechafallecimiento?selection, Ciudadorigen?selection, Habilidadespecial?selection, Complexion?selection,
    Colorpiel?selection, Altura?selection))),

    % Boton para mostrar todos los libros de la base de conocimientos
    send(MiVentana, append, button('Mostrar todos', message(@prolog, mostrar_libros))),

    % Boton para salir de la interfaz y limpiar la base de conocimientos de las libros
    send(MiVentana, append, button('Salir', message(@prolog, limpiar_datos_y_salir, MiVentana))),
    send(MiVentana, open).






% Se recogen los valores de los libros y su nombre
gui_agregar_libro(NombreDelLibro, Edad, Genero, Ocupacion, Estado, Fechanacimiento, Fechafallecimiento, Ciudadorigen, Habilidadespecial, Complexion, Colorpiel, Altura) :-

    % Imprime los valores recibidos
    format('Recibido - NombreDelLibro: ~w, Edad: ~w, Genero: ~w, Ocupacion: ~w, Estado: ~w, Fechanacimiento: ~w, Fechafallecimiento: ~w, Ciudadorigen: ~w, Habilidadespecial: ~w, Complexion: ~w, Colorpiel: ~w, Altura: ~w\n',
        [NombreDelLibro, Edad, Genero, Ocupacion, Estado, Fechanacimiento, Fechafallecimiento, Ciudadorigen, Habilidadespecial, Complexion, Colorpiel, Altura]),


    new(Diag, dialog('Agregar personaje')),
    send(Diag, append, new(Nombre, text_item(nombre))),
    send(Diag, append, button('Aceptar', message(@prolog, nuevaLibro,
        Nombre?selection, NombreDelLibro, Edad, Genero, Ocupacion, Estado, Fechanacimiento, Fechafallecimiento, Ciudadorigen, Habilidadespecial, Complexion, Colorpiel, Altura, Diag))),
    send(Diag, open).

nuevaLibro(NombreLibroInput, NombreDelLibro, Edad, Genero, Ocupacion, Estado, Fechanacimiento, Fechafallecimiento, Ciudadorigen, Habilidadespecial, Complexion, Colorpiel, Altura, Diag) :-
    % Realiza el assertz con los valores recolectados
    replace_spaces_with_underscores(NombreLibroInput, NombreLibro),
    assertz(libro(NombreLibro, NombreDelLibro, Edad, Genero, Ocupacion, Estado, Fechanacimiento, Fechafallecimiento, Ciudadorigen, Habilidadespecial, Complexion, Colorpiel, Altura)),

    % Abre el archivo en modo de anexion
    open('libros.pl', append, File),

    % Escribe la informacion del libro en el archivo
    maplist(ensure_value, [NombreDelLibro, Edad, Genero, Ocupacion, Estado, Fechanacimiento, Fechafallecimiento, Ciudadorigen, Habilidadespecial, Complexion, Colorpiel, Altura], FormattedValues),
    format(File, 'libro(~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w).\n', [NombreLibro|FormattedValues]),

    % Cierra el archivo
    close(File),

    % Muestra el mensaje de confirmacion
    send(@display, inform, 'El libro ha sido agregada en la base de datos'),

    % Cierra la ventana de dialogo despues de agregar el libro
    send(Diag, destroy).

% Asegurarse de que cada valor esta presente, de lo contrario utilizar 'null'
% Ademas, si el valor contiene comas, lo convierte en una lista de Prolog.
ensure_value(Value, Result) :-
    (   nonvar(Value), Value \= '' ->
        (   split_string(Value, ",", " ", Parts),
            Parts = [_|_] -> % Verifica que haya mas de un elemento
            % Convertir las partes en una lista de atomos
            maplist(atom_string, AtomParts, Parts),
            % Crear una representacion de lista Prolog en forma de cadena
            atomics_to_string(AtomParts, ',', AtomList),
            format(atom(Result), '[~w]', [AtomList])
        ;   % Solo hay una parte, asi que usa el valor como esta
            Result = Value
        )
    ;   Result = 'null'
    ).

% Predicado auxiliar para reemplazar espacios con guiones bajos
replace_spaces_with_underscores(Input, Output) :-
    atom_codes(Input, Codes),
    maplist(replace_space, Codes, ReplacedCodes),
    atom_codes(Output, ReplacedCodes).

% Reemplaza el codigo ASCII del espacio (32) por el del guion bajo (95)
replace_space(32, 95) :- !.
replace_space(Code, Code).







% Mostrar todas los libros en la base de conocimientos
mostrar_libros :-
    % Crear una ventana y un widget de label
    new(Dialogo, dialog('Personajes Registrados')),
    send(Dialogo, scrollbars, both),
    new(T, label(texto, '')),
    send(T, font, font(times, roman, 12)),
    send(Dialogo, append, T),

    % Verificar si hay alguna libro en la base de conocimientos
    (   % Comprobar si existe al menos una definicion de libro con 13 atributos
        libro(_, _, _, _, _, _, _, _, _, _, _, _, _)
    ->  % Usar forall/2 para iterar sobre las libros si existen
        forall(
            libro(Nombre, NombreDelLibro, Edad, Genero, Ocupacion, Estado, Fechanacimiento, Fechafallecimiento, Ciudadorigen, Habilidadespecial, Complexion, Colorpiel, Altura),
            (   % Construir la cadena de texto para cada libro con separadores
                with_output_to(string(Str),
                    format('Nombre: ~w\nLibro: ~w\nEdad: ~w\nGenero: ~w\nOcupacion: ~w\nEstado: ~w\nFecha de nacimiento: ~w\nFecha de fallecimiento: ~w\nCiudad de origen: ~w\nHabilidad Especial: ~w\nComplexion: ~w\nColor de piel: ~w\nAltura: ~w\n: ---------------------------------------\n',
                           [Nombre, NombreDelLibro, Edad, Genero, Ocupacion, Estado, Fechanacimiento, Fechafallecimiento, Ciudadorigen, Habilidadespecial, Complexion, Colorpiel, Altura])),
                % Agregar la cadena de texto al label
                send(T, append, Str)
            )
        )
    ;   % En caso de que no haya libros, mostrar un mensaje
        send(T, value, 'No hay libros registradas en la base de conocimientos.')
    ),

    % Mostrar la ventana
    send(Dialogo, open).





% Predicado para limpiar los datos de libros y cerrar la ventana
limpiar_datos_y_salir(Ventana) :-
    retractall(libro(_, _, _, _, _, _, _, _, _, _, _, _, _)),
    send(Ventana, destroy).





% Identificar libros por solo su nombre
buscar_libro_nombre :-
    new(DialogoBuscar, dialog('Buscar por Nombre')),
    send(DialogoBuscar, append, label(info, 'Ingresa el nombre del personaje:')),
    send(DialogoBuscar, append, new(NombreLibro, text_item(nombre))),
    send(DialogoBuscar, append, button('Buscar', message(@prolog, identificar_libro_por_nombre, NombreLibro?selection, DialogoBuscar))),
    send(DialogoBuscar, default_button, 'Buscar'),  % Establece el bot�n por defecto
    send(DialogoBuscar, open).

% Funcion para buscar un libro por nombre y mostrar la informacion
identificar_libro_por_nombre(NombreLibroInput, DialogoBuscar) :-
    % Reemplaza espacios en el nombre del libro con guiones bajos
    replace(NombreLibroInput, NombreLibro),
    (   libro(NombreLibro, NombreDelLibro, Edad, Genero, Ocupacion, Estado, Fechanacimiento, Fechafallecimiento, Ciudadorigen, Habilidadespecial, Complexion, Colorpiel, Altura)
    ->  send(DialogoBuscar, destroy), % Cierra el dialogo de busqueda
        mostrar_informacion_libro(NombreLibro, NombreDelLibro, Edad, Genero, Ocupacion, Estado, Fechanacimiento, Fechafallecimiento, Ciudadorigen, Habilidadespecial, Complexion, Colorpiel, Altura)
    ;   send(@display, inform, 'Personje no encontrado en la base de datos.'),
        fail
    ).

% Funcion para mostrar la informacion del libro encontrado
mostrar_informacion_libro(NombreLibro, NombreDelLibro, Edad, Genero, Ocupacion, Estado, Fechanacimiento, Fechafallecimiento, Ciudadorigen, Habilidadespecial, Complexion, Colorpiel, Altura) :-
    new(DialogoResultado, dialog('Informacion del Personaje')),

    % Convertir los atributos a cadenas si son listas para su visualizacion
    lista_a_cadena(NombreLibro, NombreString),
    lista_a_cadena(NombreDelLibro, NombreDelLibroString),
    lista_a_cadena(Edad, EdadString),
    lista_a_cadena(Genero, GeneroString),
    lista_a_cadena(Ocupacion, OcupacionString),
    lista_a_cadena(Estado, EstadoString),
    lista_a_cadena(Fechanacimiento, FechanacimientoString),
    lista_a_cadena(Fechafallecimiento, FechafallecimientoString),
    lista_a_cadena(Ciudadorigen, CiudadorigenString),
    lista_a_cadena(Habilidadespecial, HabilidadespecialString),
    lista_a_cadena(Complexion, ComplexionString),
    lista_a_cadena(Colorpiel, ColorpielString),
    lista_a_cadena(Altura, AlturaString),


    send(DialogoResultado, append, label(nombre, string('Nombre: %s', NombreString))),
    send(DialogoResultado, append, label(nombrelibro, string('Libro: %s', NombreDelLibroString))),
    send(DialogoResultado, append, label(edad, string('Edad: %s', EdadString))),
    send(DialogoResultado, append, label(genero, string('Genero: %s', GeneroString))),
    send(DialogoResultado, append, label(ocupacion, string('Ocupacion: %s', OcupacionString))),
    send(DialogoResultado, append, label(fechanacimiento, string('Fecha de nacimiento: %s', FechanacimientoString))),
    send(DialogoResultado, append, label(fechafallecimiento, string('Fecha de fallecimiento: %s', FechafallecimientoString))),
    send(DialogoResultado, append, label(ciudadorigen, string('Ciudad de origen: %s', CiudadorigenString))),
    send(DialogoResultado, append, label(habilidadespecial, string('Habilidad especial: %s', HabilidadespecialString))),
    send(DialogoResultado, append, label(complexion, string('Complexion: %s', ComplexionString))),
    send(DialogoResultado, append, label(colorpiel, string('Color de piel: %s', ColorpielString))),
    send(DialogoResultado, append, label(altura, string('Altura: %s', AlturaString))),

    replace_spaces(NombreString, URLFriendlyNombre),

    % Crear el recuadro modificable con el enlace de busqueda
    SearchURLBase = 'https://www.google.com/search?q=stephen%20king+',
    atom_concat(SearchURLBase, URLFriendlyNombre, SearchURL),
    send(DialogoResultado, append, label(mas_info, 'M�s informaci�n en:')),
    send(DialogoResultado, append, text_item(search_url, SearchURL)),

    % Boton para cerrar la ventana de dialogo
    send(DialogoResultado, append, button('Cerrar', message(DialogoResultado, destroy))),

    % Ajustar el tamanio de la ventana al contenido y mostrarla
    send(DialogoResultado, open).

    % Auxiliar para convertir una lista a una cadena, manteniendo atomos individuales como estan
lista_a_cadena(Attributo, Cadena) :-
    (   is_list(Attributo) -> atomic_list_concat(Attributo, ', ', Cadena)
    ;   Attributo == '' -> Cadena = 'Desconocido'
    ;   Cadena = Attributo
    ).

% Reemplaza espacios en el nombre del libro con signos mas (+) para la URL
replace_spaces(String, URLFriendlyString) :-
    split_string(String, " ", "+", SubStrings),
    atomic_list_concat(SubStrings, "+", URLFriendlyString).

% Reemplaza espacios en una cadena con guiones bajos
replace(Input, Output) :-
    split_string(Input, " ", "", Parts),
    atomic_list_concat(Parts, '_', Output).






% Identificar libro por sus atributos con el mayor numero de coincidencias
buscar_libro(NombreDelLibro, Edad, Genero, Ocupacion, Estado, Fechanacimiento, Fechafallecimiento, Ciudadorigen, Habilidadespecial, Complexion, Colorpiel, Altura) :-
    % Encuentra todas las libros con el conteo de coincidencias
    findall(Nombre-Coincidencias, (
        libro(Nombre, NombreDelLibroLibro, EdadLibro, GeneroLibro, OcupacionLibro, EstadoLibro, FechanacimientoLibro, FechafallecimientoLibro, CiudadorigenLibro, HabilidadespecialLibro, ComplexionLibro, ColorpielLibro, AlturaLibro),
        coincidencias([NombreDelLibro, Edad, Genero, Ocupacion, Estado, Fechanacimiento, Fechafallecimiento, Ciudadorigen, Habilidadespecial, Complexion, Colorpiel, Altura],
                      [NombreDelLibroLibro, EdadLibro, GeneroLibro, OcupacionLibro, EstadoLibro, FechanacimientoLibro, FechafallecimientoLibro, CiudadorigenLibro, HabilidadespecialLibro, ComplexionLibro, ColorpielLibro, AlturaLibro],
                      0, Coincidencias)
    ), LibrosConCoincidencias),

    % Encuentra el mayor numero de coincidencias
    max_coincidencias(LibrosConCoincidencias, MaxCoincidencias),

    % Crear el dialogo y mostrar los resultados o un mensaje si no hay coincidencias
    new(DialogoResultado, dialog('Resultados')),
    (   MaxCoincidencias == 0 ->
    send(DialogoResultado, append, label('', 'Coincidencias no encontradas.'))
    ;   % Filtra los libros que tienen el mayor numero de coincidencias
    include(has_max_coincidencias(MaxCoincidencias), LibrosConCoincidencias, LibrosFiltradas),
    forall(member(Libro-Coincidencias, LibrosFiltradas), (
        % Sustituye espacios por '+', ya que en las URLs los espacios se representan con '+'
        replace_spaces(Libro, URLFriendlyNombre),

        % Concatena la base de la URL con el nombre del libro formateado para URL
        SearchURLBase = 'https://www.google.com/search?q=stephen%20king+',
        atom_concat(SearchURLBase, URLFriendlyNombre, SearchURL),

        % Anade el nombre del libro y el numero de coincidencias al dialogo
        send(DialogoResultado, append, label('', string('Personaje coincidente: %s (Coincidencias: %d)', Libro, Coincidencias))),

        % Anade la etiqueta 'Mas informacion en:' y el enlace de busqueda al dialogo
        send(DialogoResultado, append, label(mas_info, 'Mas informacion en:')),
        send(DialogoResultado, append, text_item(search_url, SearchURL)),

        % Anade un separador visual
        send(DialogoResultado, append, label('', '-----------------'))
    ))
    ),

    % Boton para cerrar la ventana de dialogo
    send(DialogoResultado, append, button('Cerrar', message(DialogoResultado, destroy))),

    % Ajustar el tamano de la ventana al contenido y mostrarla
    send(DialogoResultado, open).


max_coincidencias(LibrosConCoincidencias, MaxCoincidencias) :-
    maplist(second, LibrosConCoincidencias, ListaCoincidencias),
    max_list(ListaCoincidencias, MaxCoincidencias).

second(_-X, X).

has_max_coincidencias(MaxCoincidencias, _-Coincidencias) :-
    Coincidencias = MaxCoincidencias.


% Auxiliar para contar coincidencias
coincidencias([], [], Contador, Contador).
coincidencias([Attr|RestoAttrs], [LibroAttr|RestoLibroAttrs], Contador, Coincidencias) :-
    (   nonvar(Attr),
        (   is_list(LibroAttr) % Verifica si el atributo en la base de conocimientos es una lista
        ->  split_string(Attr, ",", " ", AttrList),
            maplist(trim, AttrList, TrimmedList),
            maplist(atom_string, AtomList, TrimmedList), % Convierte la lista de strings a atomos si es necesario
            (   subset(AtomList, LibroAttr) % Verifica si todos los elementos de AtomList estan en LibroAttr
            ->  NuevoContador is Contador + 1
            ;   NuevoContador = Contador
            )
        ;   Attr = LibroAttr
        ->  NuevoContador is Contador + 1
        ;   NuevoContador = Contador
        )
    ),
    coincidencias(RestoAttrs, RestoLibroAttrs, NuevoContador, Coincidencias).

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
