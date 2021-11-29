% Pedro Pereira ist 196905
% Projeto de LP, solucionador de palvras cruzadas

:- use_module(library(clpfd)).


%-------------------------------------------------------------------------------------------------------------------
% memberchk_eq(X, Y) predicado retirado da biblioteca de SWI Prolog, true se X for membro da lista Y (nao unifica X)
%-------------------------------------------------------------------------------------------------------------------
memberchk_eq(X, [Y|Ys]) :-
        (   X == Y
        ->  true
        ;   memberchk_eq(X, Ys)
        ).

%-----------------------------------------------------------------------------------------------
% intersect_eq() predicado retirado da biblioteca de SWI Prolog, faz a intersecao de duas listas
%-----------------------------------------------------------------------------------------------
intersect_eq([], _, []).
   intersect_eq([X|Xs], Ys, L) :-
    	(   memberchk_eq(X, Ys)
    	->  L = [X|T],
    	    intersect_eq(Xs, Ys, T)
     	;   intersect_eq(Xs, Ys, L)
        ).

%-------------------------------------------------------------------------------------------------------------------------------
% obtem_letras_palavras(Lista_Pals, Res) Res unifica com a lista de lista de palavras ordenada em que cada elemento de uma lista
%										 contem uma letra da palavra 
%-------------------------------------------------------------------------------------------------------------------------------
obtem_letras_palavras(Lista_Pal, Res) :- 
	aux_obtem_letras_palavras(Lista_Pal, [], Aux),
	sort(Aux, Res).

%------------------------------------------------------------------------------------------------------------------------------------------
% aux_obtem_letras_palavras(Lista_Pal, Aux, Res) Vai juntando a Aux a lista atomica de cada palavra e unifica Res no fim com esse resultado
%------------------------------------------------------------------------------------------------------------------------------------------
aux_obtem_letras_palavras([], Res, Res).

aux_obtem_letras_palavras([Pal|Lista_Pal], Aux, Res) :- 
	atom_chars(Pal, Pal_atom),
	aux_obtem_letras_palavras(Lista_Pal, [Pal_atom|Aux], Res).

%-------------------------------------------------
% espacos_fila(Fila, Esps) Lista um espaco da Fila
%-------------------------------------------------
espaco_fila(Fila, Esp) :- 
    aux_espaco_fila(Fila, [], Aux),
    reverse(Aux, Esp).

%------------------------------------------------------------------------------------------------------------------------
% aux_espaco_fila(Fila, Aux, Esp) Procura um espaco em Fila e unifica com Esp quando encontra uma parede se for um espaco
%------------------------------------------------------------------------------------------------------------------------ 
aux_espaco_fila([], Esp, Esp) :-
    length(Esp, N), 
    N > 2.

aux_espaco_fila([X|Fila], Aux, Esp) :- 
	X \== #, 
	aux_espaco_fila(Fila, [X|Aux], Esp).   % adiciona o elemento ao auxiliar e ve o proximo elemento da lista

aux_espaco_fila([X|_], Esp, Esp) :- 
	X == #, 
	length(Esp, N), 
	N > 2.             % encontrou uma parede, verifica se e um espaco e unifica caso seja
aux_espaco_fila([X|Fila], _, Esp) :-
	X == #, 
	aux_espaco_fila(Fila, [], Esp). 

%-----------------------------------------------------------------------------
% espaco_fila(Fila, Esps) Esps unifica com a lista de todos os espacos de Fila
%-----------------------------------------------------------------------------
espacos_fila(Fila, Esps) :- 
    bagof(Aux, espaco_fila(Fila, Aux), Esps).

%-------------------------------------------------------------------------------
% mat_transposta(Matriz, Transp) significa que Transp e' a transposta de Matriz
%-------------------------------------------------------------------------------
mat_transposta(Matriz, Transp) :-
    transpose(Matriz, Transp).

%-------------------------------------------------------------------------------------
% espacos_puzzle(Grelha, Espacos) Lista todos os espacos da grelha em linhas e colunas
%-------------------------------------------------------------------------------------
espacos_puzzle(Grelha, Espacos) :- 
    retirar_filas(Grelha, [], Linhas),
    mat_transposta(Grelha, Transposta),
    retirar_filas(Transposta, [], Colunas),
    append(Linhas, Colunas, Espacos).

%-------------------------------------------------------------------------------------
% retirar_filas(Grelha, Aux, Esp) Lista todos os espacos em linha de uma grelha
%-------------------------------------------------------------------------------------
retirar_filas([], Esp, Esp).

retirar_filas([X|Grelha], Aux, Esp) :-
    espacos_fila(X, Fila), 
    append(Aux, Fila, Aux2), !, 
    retirar_filas(Grelha, Aux2, Esp). % juntar os espacos anteriores ao novos 

retirar_filas([_|Grelha], Aux, Esp) :- 
	!, retirar_filas(Grelha, Aux, Esp).              % nao ha espacos novos entao ver fila seguinte

%---------------------------------------------------------------------------------------------------------
% espacos_com_posicoes_comuns(L_Espacos, Esp, Esp_Comuns) Esp_comuns resulta dos espacos que Esp interseta
%---------------------------------------------------------------------------------------------------------
espacos_com_posicoes_comuns(L_Espacos, Esp, Esp_comuns) :-
	ver_membros(L_Espacos, Esp, [], Aux), 
	reverse(Aux, Esp_comuns).

%---------------------------------------------------------------------------------------------------------------------
% ver_membros(L_Espacos, Esp, Aux, Comuns) Procura intersecoes de cada sublista da lista de espacos com o Esp de input
%---------------------------------------------------------------------------------------------------------------------
ver_membros([], _, Comuns, Comuns).

ver_membros([X | L_Espacos], Esp, Aux, Comuns) :-
	intersect_eq(X, Esp, Res), 
	length(Res, T_Res),   % depois da intersecao verificar se a lista resultante nao e vazia
    T_Res > 0, 
    length(X, T_original), 
    T_Res \== T_original, % excluir proprio Esp com a comparacao de tamanhos 
    !, ver_membros(L_Espacos, Esp, [X | Aux], Comuns).

ver_membros([_ | L_Espacos], Esp, Aux, Comuns) :- 
	!, ver_membros(L_Espacos, Esp, Aux, Comuns).         % sem intersecao
	

%-----------------------------------------------------------------------------------------------------------------
% palavra_possivel_esp(Pal, Esp, Espacos, Letras) verifica se Pal consegue unificar com Esp
%                                                 sem que impeca futuras palavras de unificar com outros espacos
%-----------------------------------------------------------------------------------------------------------------
palavra_possivel_esp(Pal, Esp, Espacos, Letras) :-
    espacos_com_posicoes_comuns(Espacos, Esp, Esp_comuns),
    cria_copia(Esp, Cop_Esp),                             % tem que se criar uma copia para nao unificar a grelha original
    cria_copia(Esp_comuns, Cop_Esp_comuns),
	Pal = Cop_Esp, 
    substitui(Esp, Pal, Esp_comuns, Cop_Esp_comuns),
    ver_unificacoes_de_comuns(Esp_comuns, Cop_Esp_comuns, Letras, _), !.  % verificar se a unificacao nao impossibilita unificacoes futuras com os espacos adjacentes

%------------------------------------------------------------------------------------------------------
% substitui(Esp, Pal, Esp_comuns, Cop_Esp_comuns) em vez de unificar as letras no original, 
%                                                 unifica as letras na copia nos sitios correspondentes
%------------------------------------------------------------------------------------------------------
substitui([], [], _, _).

substitui([X | Esp], [Letra | Pal], Esp_comuns, Copia) :-
    var(X),                                                % so se pode unificar as letras se X for uma variavel
    substitui_letra(X, Letra, 0, Esp_comuns, Copia), !,    % unificar letra nos devidos espacos da copia
	substitui(Esp, Pal, Esp_comuns, Copia).

substitui([_ | Esp], [_ | Pal], Esp_comuns, Copia) :-      % se for uma constante passa a proxima letra e variavel
    !, substitui(Esp, Pal, Esp_comuns, Copia).    

%-------------------------------------------------------------------------------------------------
% substitui_letra(Esp, Pal, Esp_comuns, Copia) unifica a letra nos sitios correspondentes na copia
%-------------------------------------------------------------------------------------------------
substitui_letra(_ , _, _, [], []).

substitui_letra(Esp, Letra, Index, [X | Esp_comuns], [Y | Copia]) :-
	length(X, Tamanho),
	Tamanho > Index,           % Index nao pode ser superior ao tamanho da lista a ser corrida
	nth0(Index, X, Sujeito),
	Sujeito == Esp,            % variavel de Esp e igual a variavel na lista dos espacos em comum
	nth0(Index, Y, Letra),     % unifica letra correspondente na copia
    substitui_letra(Esp, Letra, 0, Esp_comuns, Copia).

substitui_letra(Esp, Letra, Index, [X | Esp_comuns], [Y | Copia]) :-
	length(X, Tamanho),
	Tamanho > Index,
	nth0(Index, X, Sujeito),
	Sujeito \== Esp,
	plus(Index, 1, N_Index), 
	substitui_letra(Esp, Letra, N_Index, [X | Esp_comuns], [Y |Copia]).

substitui_letra(Esp, Letra, Index, [X | Esp_comuns], [_ | Copia]) :-
    length(X, Index),
    substitui_letra(Esp, Letra, 0, Esp_comuns, Copia).

%---------------------------------------------------------------------------------------------------------------------------------------------------
% ver_unificacoes_de_comuns(Esp_Comuns, Copia, Aux, Letras) ve se existe alguma palavra em Letras q consiga unificar com o 
%                                                           espaco de lista de Espacos, Aux serve para ir pondo as palavras testadas num certo espaco
%---------------------------------------------------------------------------------------------------------------------------------------------------
ver_unificacoes_de_comuns([], [], Letras, Letras).

ver_unificacoes_de_comuns([X | Espacos], [Y | Copia], [Possivel | Letras], Aux) :-
	Possivel \= Y, !,
	ver_unificacoes_de_comuns([X | Espacos], [Y | Copia], Letras, [Possivel | Aux]).  % se nao e possivel unificar passa a proxima palavra 

ver_unificacoes_de_comuns([X | Espacos], [Y | Copia], [Possivel | Letras], Aux) :-
    Possivel = Y,
    substitui(X, Possivel, [X | Espacos], [Y | Copia]),
    reverse(Aux, Pal_visitadas),
    append(Pal_visitadas, [Possivel | Letras], Letras_Restantes), !,         % juntar palavras anteriormente testadas com as de sobra
	ver_unificacoes_de_comuns(Espacos, Copia, Letras_Restantes, _). 

%-----------------------------------------------------------------------------------------------------------------
% cria_copia(L1, L2) L2 fica uma lista com os mesmos elementos de L1 mas com enderecos de memoria todos diferentes
% Nota: elementos de L1 que sao iguais, em L2 ficam distintos
%-----------------------------------------------------------------------------------------------------------------
cria_copia(L1, L2) :-
	findall(X, member(X, L1), L2).

%---------------------------------------------------------------------------------------------------------------------------
% palavras_possiveis_esp(Letras, Espacos, Esp, Pals_Possiveis) lista todas as palavras possiveis relativas a um certo espaco
%---------------------------------------------------------------------------------------------------------------------------
palavras_possiveis_esp(Letras, Espacos, Esp, Pals_Possiveis) :-
    auxiliar_pals_possiveis_esp(Letras, Espacos, Esp, 0, [], Aux), 
    reverse(Aux, Pals_Possiveis), !.

%-------------------------------------------------------------------------------------------------------------------------------------------------
% auxiliar_pals_possiveis_esp(Letras, Espacos, Esp, Indice, Aux, Pals_Possiveis) procura palavras possiveis para um certo espaco e junta-as em Aux
%-------------------------------------------------------------------------------------------------------------------------------------------------
auxiliar_pals_possiveis_esp(Letras, _, _, Index, Pals_Possiveis, Pals_Possiveis) :- 
    length(Letras, Index).

auxiliar_pals_possiveis_esp(Letras, Espacos, Esp, Index, Aux, Pals_Possiveis) :- 
    nth0(Index, Letras, Pal),
    palavra_possivel_esp(Pal, Esp, Espacos, Letras),
    plus(Index, 1, N_Index),
    auxiliar_pals_possiveis_esp(Letras, Espacos, Esp, N_Index, [Pal | Aux], Pals_Possiveis).

auxiliar_pals_possiveis_esp(Letras, Espacos, Esp, Index, Aux, Pals_Possiveis) :-
    plus(Index, 1, N_Index),
    auxiliar_pals_possiveis_esp(Letras, Espacos, Esp, N_Index, Aux, Pals_Possiveis).

%------------------------------------------------------------------------------------------------------------------------------------------
% palavras_possiveis(Letras, Espacos, Pals_Possiveis) Pals_Possiveis unifica de forma a listar todas as palavras possiveis para cada espaco
%------------------------------------------------------------------------------------------------------------------------------------------
palavras_possiveis(Letras, Espacos, Pals_Possiveis) :-
	aux_pals_possiveis(Letras, Espacos, 0, [], Aux), 
	reverse(Aux, Pals_Possiveis), !.

%----------------------------------------------------------------------------------------------------------------------------------------------
% aux_pals_possiveis(Letras, Espacos, Index, Aux, Pals_Possiveis) procura todas as palavras possiveis para cada espaco em Espacos e junta a Aux
%----------------------------------------------------------------------------------------------------------------------------------------------
aux_pals_possiveis(_, Espacos, Index, Pals_Possiveis, Pals_Possiveis) :- 
    length(Espacos, Index).

aux_pals_possiveis(Letras, Espacos, Index, Aux, Pals_Possiveis) :-
	nth0(Index, Espacos, Esp),                               % ver o espaco para atribuir palavras possiveis
	palavras_possiveis_esp(Letras, Espacos, Esp, Lista_Pos),
	plus(Index, 1, N_Index),
	N_Aux = [Esp, Lista_Pos],
	aux_pals_possiveis(Letras, Espacos, N_Index, [N_Aux | Aux], Pals_Possiveis).


%---------------------------------------------------------------------------------------------------------------------------------------------
% letras_comuns(Lst_Pals, Letras_comuns) Letras_comuns unifica com a lista de letras comuns tendo sendo o primeiro elemento o indice da letra,
%                 						 e o segundo a letra
%---------------------------------------------------------------------------------------------------------------------------------------------
letras_comuns(Lst_Pals, Letras_comuns) :- 
	ver_comuns(Lst_Pals, 1, [], Aux), reverse(Aux, Letras_comuns), !.

%---------------------------------------------------------------------------------------------------------------
% ver_comuns(Lst_Pals, Index, Aux, Letras_comuns) chama o predicado ver_letra_comum() tantas vezes como o tamanho das palavras, determinando se no
%               								  indice a ser trabalhado consegue haver uma unificacao
%---------------------------------------------------------------------------------------------------------------
ver_comuns([X | _], Index, Letras_comuns, Letras_comuns) :- 
    length(X, Tamanho), Index > Tamanho.

ver_comuns([X | Lst_Pals], Index, Aux, Letras_comuns) :-
	nth1(Index, X, Letra),                       % descobrir letra 
	ver_letra_comum(Lst_Pals, Index, Letra),     % ver se unifica
	plus(Index, 1, N_Index),
	Etiqueta = (Index, Letra),
	ver_comuns([X | Lst_Pals], N_Index, [Etiqueta | Aux], Letras_comuns).

ver_comuns(Lst_Pals, Index, Aux, Letras_comuns) :-
	plus(Index, 1, N_Index),
	ver_comuns(Lst_Pals, N_Index, Aux, Letras_comuns).

%--------------------------------------------------------------------------------------------------------------------------------
% ver_letra_comum(Lst_Pals, Index, Letra) X unifica com a letra se em todos os indices da lista de palavras estiver a mesma letra
%--------------------------------------------------------------------------------------------------------------------------------
ver_letra_comum([], _, _).

ver_letra_comum([X | Lst_Pals], Index, Letra) :-
	nth1(Index, X, Letra),
	ver_letra_comum(Lst_Pals, Index, Letra).

%------------------------------------------------------------------------------------------------------------------------
% atribui_comuns(Pals_Possiveis) unifica as letras comuns das Pals_Possiveis com as variaveis dos espacos correspondentes
%------------------------------------------------------------------------------------------------------------------------
atribui_comuns([]).

atribui_comuns([[Esp | Letras] | Pals_Possiveis]) :-
	nth0(1, [Esp | Letras], L),   % [[[Letra]]] passa a [[Letra]] = L
    letras_comuns(L, Letras_comuns), 
	unifica_comuns(Esp, Letras_comuns), !, % unificar variaveis de Esp
    atribui_comuns(Pals_Possiveis).

%-------------------------------------------------------------------------------------
% unifica_comuns(Esp, Letras_comuns) unifica a letras as variaveis do indice do espaco
%------------------------------------------------------------------------------------- 
unifica_comuns(_, []).

unifica_comuns(Esp, [(Indice, Letra) | Letras_comuns]) :-
    nth1(Indice, Esp, Letra),               % unificar letra a variavel
	unifica_comuns(Esp, Letras_comuns).

%-----------------------------------------------------------------------------------------------------------------------------------
% retira_impossiveis(Pals_Possiveis, Novas_Pals_Possiveis) Novas_Pals_Possiveis unifica com a lista de Pals_Possiveis sem as palvras 
%														   impossiveis de unificarem em cada espaco
%-----------------------------------------------------------------------------------------------------------------------------------
retira_impossiveis(Pals_Possiveis, Novas_Pals_Possiveis) :-
	aux_retira_impossivel(Pals_Possiveis, [], Aux), reverse(Aux, Novas_Pals_Possiveis), !.

%--------------------------------------------------------------------------------------------------------------------------------------------
% aux_retira_impossivel(Pals_Possiveis, Aux, Novas_Pals_Possiveis) divide as Pals_Possiveis em Blocos (= [Esp | Pals]) e direciona os para um 
%																   predicado que vai verificar a unicidade de cada palavra em relacao ao Esp
%--------------------------------------------------------------------------------------------------------------------------------------------
aux_retira_impossivel([], Novas_Pals_Possiveis, Novas_Pals_Possiveis).

aux_retira_impossivel([[Esp | Pals] | Pals_Possiveis], Aux, Novas_Pals_Possiveis) :-
	retira_impossivel_bloco([Esp | Pals], [], Novas_Pals, 0),  % direciona para um predicado que testa a unicidade de cada palavra
	reverse(Novas_Pals, Res_Pals),
	Novo_Bloco = [Esp | [Res_Pals]],       % bloco resultante 
	aux_retira_impossivel(Pals_Possiveis, [Novo_Bloco | Aux], Novas_Pals_Possiveis).

%----------------------------------------------------------------------------------------------------------------------------------
% retira_impossivel_bloco(Bloco, Aux, Novas_Pals, Index) direciona cada palavra do bloco para um predicado que testa a unicidade da 
%														 palavra com o espaco
%----------------------------------------------------------------------------------------------------------------------------------
retira_impossivel_bloco([_ | Pals_Possiveis], Novo_Bloco, Novo_Bloco, Index) :-
    nth0(0, Pals_Possiveis, L_Pals_Possiveis), % retira o primeiro par de parentices retos ficando uma lista de listas
    length(L_Pals_Possiveis, Index).

retira_impossivel_bloco([Esp | Pals_Possiveis], Aux, Novo_Bloco, Index) :-
	nth0(0, Pals_Possiveis, L_Pals_Possiveis), % retira o primeiro par de parentices retos ficando uma lista de listas
    nth0(Index, L_Pals_Possiveis, Palavra),
	espaco_compativel(Esp, Palavra),         % verificar unicidade da palavra com o espaco
	plus(Index, 1, N_Index),
	retira_impossivel_bloco([Esp | Pals_Possiveis], [Palavra | Aux], Novo_Bloco, N_Index).

retira_impossivel_bloco(Bloco, Aux, Novo_Bloco, Index) :- % se a palavra nao for unificavel, passa para a proxima palavra do bloco
	plus(Index, 1, N_Index),
	retira_impossivel_bloco(Bloco, Aux, Novo_Bloco, N_Index).

%----------------------------------------------------------------------------------------
% espaco_compativel(Esp, Palavra) testa unicidade de cada letra com o seu lugar no espaco
%----------------------------------------------------------------------------------------
espaco_compativel([], []).

espaco_compativel([X | Esp], [_ | Letras]) :-
    var(X),                                  % se for variavel, qualquer Char consegue unificar
    espaco_compativel(Esp, Letras).

espaco_compativel([X | Esp], [Char | Letras]) :-
    X == Char,
    espaco_compativel(Esp, Letras).

%------------------------------------------------------------------------------------------------------------------------
% obtem_unicas(Pals_Possiveis, Unicas) 'Unicas' unifica com uma lista de palavras possiveis que sao unicas para um espaco
%------------------------------------------------------------------------------------------------------------------------
obtem_unicas(Pals_Possiveis, Unicas) :- 
	aux_obtem_unicas(Pals_Possiveis, [], Aux), reverse(Aux, Unicas), !.

%----------------------------------------------------------------------------------------------------------------------------------------
% aux_obtem_unicas(Pals_Possiveis, Aux, Unicas) Para cada bloco de pals_possiveis ve se ha mais que uma palavra possivel para cada espaco
%----------------------------------------------------------------------------------------------------------------------------------------  
aux_obtem_unicas([], Unicas, Unicas).

aux_obtem_unicas([[_ | [Pals]] | Pals_Possiveis], Aux, Unicas) :- 
	length(Pals, 1),
    nth0(0, Pals, Pal),
	aux_obtem_unicas(Pals_Possiveis, [Pal | Aux], Unicas).

aux_obtem_unicas([_ | Pals_Possiveis], Aux, Unicas) :- % se houver mais que uma palavra possivel passa para o proximo bloco 
	aux_obtem_unicas(Pals_Possiveis, Aux, Unicas).

%---------------------------------------------------------------------------------------------------------
% retira_unicas(Pals_Possiveis, Novas_Pals_Possiveis) Novas_Pals_Possiveis unifica com a Pals_Possiveis,
%												      mas com as palavras possiveis ja sem palavras unicas
%--------------------------------------------------------------------------------------------------------- 
retira_unicas(Pals_Possiveis, Novas_Pals_Possiveis) :-
	obtem_unicas(Pals_Possiveis, Unicas),
	aux_retira_unicas(Unicas, Pals_Possiveis, [], Aux), reverse(Aux, Novas_Pals_Possiveis), !.

%-----------------------------------------------------------------------------------------------------------------------
% aux_retira_unicas(Unicas, Pals_Possiveis, Aux, Novas_Pals_Possiveis) para cada palavra unica chama o predicado que vai 
%																	   retirar as palavras unicas das palavras possiveis
%-----------------------------------------------------------------------------------------------------------------------
aux_retira_unicas(_, [], Novas_Pals_Possiveis, Novas_Pals_Possiveis).

aux_retira_unicas(Unicas, [[Esp | [Pals]] | Pals_Possiveis], Aux, Novas_Pals_Possiveis) :-
	length(Pals, Tamanho),
	Tamanho > 1,
	subtract(Pals, Unicas, Novas_Pals),
	Novo_Bloco = [Esp | [Novas_Pals]],
	aux_retira_unicas(Unicas, Pals_Possiveis, [Novo_Bloco | Aux], Novas_Pals_Possiveis).

aux_retira_unicas(Unicas, [[Esp| [Pals]] | Pals_Possiveis], Aux, Novas_Pals_Possiveis) :-
	length(Pals, 1),
    Bloco = [Esp | [Pals]],
	aux_retira_unicas(Unicas, Pals_Possiveis, [Bloco | Aux], Novas_Pals_Possiveis).


%-----------------------------------------------------------------------------------------------------------------------
% simplifica(Pals_Possiveis, Novas_Pals_Possiveis) Novas_Pals_Possiveis unifica com a solucao da atribuicao das palavras
%                                                  de Pals_Possiveis nos seu espacos
%----------------------------------------------------------------------------------------------------------------------- 
simplifica(Pals_Possiveis, Novas_Pals_Possiveis) :-
	atribui_comuns(Pals_Possiveis),
	retira_impossiveis(Pals_Possiveis, Sem_Impossiveis),
	retira_unicas(Sem_Impossiveis, New_List),
	Pals_Possiveis \== New_List, !,            % a recursao acaba quando a Nova lista for igual a antiga
	simplifica(New_List, Novas_Pals_Possiveis).

simplifica(Novas_Pals_Possiveis, Novas_Pals_Possiveis) :- 
    !, atribui_comuns(Novas_Pals_Possiveis).


%------------------------------------------------------------------------------------------------------------------------
% inicializa(Puzzle, Pals_Possiveis) Pals_Possiveis unifica com a possivel resolucao do puzzle, mas pode nao ser completa
%------------------------------------------------------------------------------------------------------------------------
inicializa([Lst_Pals | Puzzle], Pals_Possiveis) :-
	obtem_letras_palavras(Lst_Pals, Letras),
    nth0(0, Puzzle, Grelha), % retira do puzzle a Grelha retirando o primeiro par de parentices
	espacos_puzzle(Grelha, Espacos),
	palavras_possiveis(Letras, Espacos, Primeiras_Pals_Possiveis),
	simplifica(Primeiras_Pals_Possiveis, Pals_Possiveis), !.

%------------------------------------------------------------------------------------------------------------------
% escolhe_menos_alternativas(Pals_Possiveis, Escolha) Escolha unifica com o Bloco (Bloco | Pals_Posiveis) que tiver 
%                                                     menos palavras e com mais que uma dentro do mesmo
%-------------------------------------------------------------------------------------------------------------------
escolhe_menos_alternativas(Pals_Possiveis, [Esp, Pals]) :-
    aux_escolhe_menos_alternativas(Pals_Possiveis, [], [Esp, Pals]),
    length(Pals, Tamanho),
    Tamanho > 1.

%----------------------------------------------------------------------------------------------------------------------------
% aux_escolhe_menos_alternativas(Pals_Possiveis, Aux, Escolha) procura uma Escolha que consiga unificar segundo as restricoes
%----------------------------------------------------------------------------------------------------------------------------
aux_escolhe_menos_alternativas([], Escolha, Escolha).

aux_escolhe_menos_alternativas([[Esp, Pals]| Pals_Possiveis], Aux, Escolha) :-
    length(Aux, 0),         % caso em que Aux nao tem nada
    length(Pals, Numero_Pals),
    Numero_Pals > 1, !, 
    aux_escolhe_menos_alternativas(Pals_Possiveis, [Esp, Pals], Escolha).

aux_escolhe_menos_alternativas([[Esp, Pals] | PalsPossiveis], [ , Escolha_Pals_Possiveis], Escolha) :-
    length(Pals, Numero_Pals),
    Numero_Pals > 1,
    length(Escolha_Pals_Possiveis, Tamanho),
    Numero_Pals < Tamanho, !, 
    aux_escolhe_menos_alternativas(Pals_Possiveis, [Esp, Pals], Escolha).

aux_escolhe_menosalternativas([ | Pals_Possiveis], Aux, Escolha) :- % casos em que no bloco so ha uma palavra
    aux_escolhe_menos_alternativas(Pals_Possiveis, Aux, Escolha).


%-----------------------------------------------------------------------------------------------------------------
% experimenta_pal(Escolha, Pals_Possiveis, Novas_Pals_Possiveis) unifica o espaco de Escolha ([Esp | [[Pal | _]]]) 
%																 com Pal e renova as Pals_Possiveis
%-----------------------------------------------------------------------------------------------------------------
experimenta_pal([Esp | [Pals]], Pals_Possiveis, Novas_Pals_Possiveis) :-
	member(Esp, Pals),
	retirar_pals_bloco(Esp, Pals_Possiveis, [], Aux),
	reverse(Aux, Novas_Pals_Possiveis).

%-----------------------------------------------------------------------------------------------------------------------------------
% retirar_pals_bloco(Esp, Pals_Possiveis, Aux, Novas_Pals_Possiveis) Novas_Pals_Possiveis unifica com Pals_Possiveis mas com o bloco
%																	 correspondente a Esp so com o proprio Esp no lugar das palavras
%																	 possiveis
%-----------------------------------------------------------------------------------------------------------------------------------
retirar_pals_bloco(_, [], Novas_Pals_Possiveis, Novas_Pals_Possiveis).

retirar_pals_bloco(Esp, [[Espaco | _] | Pals_Possiveis], Aux, Novas_Pals_Possiveis) :-
	Esp == Espaco,
	Novo_Bloco = [Espaco | [[Espaco]]], !,
	retirar_pals_bloco(Esp, Pals_Possiveis, [Novo_Bloco | Aux], Novas_Pals_Possiveis).

retirar_pals_bloco(Esp, [Bloco | Pals_Possiveis], Aux, Novas_Pals_Possiveis) :-
	!, retirar_pals_bloco(Esp, Pals_Possiveis, [Bloco | Aux], Novas_Pals_Possiveis).

%-----------------------------------------------------------------------------------------------------------------------------------------
% resolve_aux(Pals_Possiveis, Novas_Pals_Possiveis) Novas_Pals_Possiveis unifica de forma a todos os espacos unificarem com alguma palavra
%-----------------------------------------------------------------------------------------------------------------------------------------
resolve_aux(Novas_Pals_Possiveis, Novas_Pals_Possiveis) :-  % chega a este caso quando escolhe_menos_alternativas da falso, que e quando todos espacos unificarem com uma palavra
	 verifica_preenchido(Novas_Pals_Possiveis), !.

resolve_aux(Pals_Possiveis, Novas_Pals_Possiveis) :-
	escolhe_menos_alternativas(Pals_Possiveis, Escolha),
	experimenta_pal(Escolha, Pals_Possiveis, Novas_Pals),
	simplifica(Novas_Pals, Novas_Pals_Possiveis_Incompletas),
    resolve_aux(Novas_Pals_Possiveis_Incompletas, Novas_Pals_Possiveis).

%------------------------------------------------------------------------------------------------------
% verifica_preenchido (Pals_Possiveis) true se todos os espacos de pals possiveis nao tiverem variaveis
%------------------------------------------------------------------------------------------------------
verifica_preenchido([]).

verifica_preenchido([[Esp | _] | Pals_Possiveis]):-
    verifica_var(Esp),
    verifica_preenchido(Pals_Possiveis).

%--------------------------------------------------
% verifica_var(Esp) true se Esp nao tiver variaveis
%--------------------------------------------------
verifica_var([]).

verifica_var([X | Esp]) :- 
    not(var(X)),
    verifica_var(Esp).

%-------------------------------------------------------------------------------
%                escreve_Grelha(Grelha)
%-------------------------------------------------------------------------------
escreve_Grelha(Grelha) :-
    maplist(escreve_Linha, Grelha).

escreve_Linha([]) :- nl, !.

escreve_Linha([P | R]) :-
    (var(P) -> write('- ')
            ;
     write(P), write(' ')),
     escreve_Linha(R).

%-----------------------------
% resolve(Puz) resolve o puzle
%-----------------------------
resolve([Lst_Pals | [Grelha]]) :-
	inicializa([Lst_Pals| [Grelha]], Pals_Possiveis),
	resolve_aux(Pals_Possiveis, _).