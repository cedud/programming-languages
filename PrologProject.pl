count([], _, 0).
count([H|T], Element, Counter) :-
    H =:= Element,
    count(T, Element, TempCounter),
    Counter is TempCounter + 1.
count([H|T], Element, Counter) :-
    H =\= Element,
    count(T, Element, Counter).

build_list(_, 0, []).
build_list(Element, Counter, [Element|T]) :-
    Counter > 0,
    NewCounter is Counter - 1,
    build_list(Element, NewCounter, T).

counting_sort(List, Sorted) :-
    max_list(List, Max),
    sort_from(Max, List, [], Sorted).

sort_from(-1, _, Acc, Acc).
sort_from(Element, List, Acc, Sorted) :-
    Element >= 0,
    count(List, Element, Counter),
    build_list(Element, Counter, Elements),
    append(Acc, Elements, NewAcc),
    NewElement is Element - 1,
    sort_from(NewElement, List, NewAcc, Sorted).

all_zero([]).
all_zero([0|T]) :-
    all_zero(T).

trim(List, 0, List).
trim([H|T], N, [H1|T1]) :-
    H1 is H - 1,
    N1 is N - 1,
    trim(T, N1, T1).

is_graph(List) :-
    counting_sort(List, Sorted),
    write('Is the list graphical?: '),
    (is_graph_alt(Sorted) ->  true ; false ).

is_graph_alt([]).
is_graph_alt([0|T]) :-
    all_zero(T).
is_graph_alt([H|T]) :-
    length(T, Length),
    H =< Length,
    counting_sort(T, NewT),
    trim(NewT, H, NewList),
    is_graph_alt(NewList).

is_connected(List, Answer) :-
    ( is_graph(List) ->  
        ( is_connected_by_degrees(List) -> 
            (write('Yes'), nl, Answer = 'Yes')  
        ;  
            (write('Yes'), nl, Answer = 'No') 
        ) 
    ; 
        write('No'), nl,
        Answer = 'No' 
    ),
    write('Is the graph connected?: '), write(Answer), nl.

is_connected_by_degrees(List) :-
    length(List, N),
    sum_list(List, S),
    S >= 2*(N - 1), 
    ( N = 1 ->  true  ; \+ member(0, List) ).

:- initialization(main).
main :-
    % Test cases with expected correct answers
    Tests = [
        [1,3,4,2,4,2,2,2],   % Yes, Yes
        [3,1,4,3,2,5,5,0,2], % No, No
        [1,0,1,10],           % No, No
        [1,1,1],              % No, No
        [1,1,1,1],            % Yes, No
        [2,2,2,1,1],          % Yes, Yes
        [2,2,2,1],            % No, No
        [0, 0],               % Yes, No
        [0]                   % Yes, Yes
    ],
    test_connectivity(Tests).

test_connectivity([]).
test_connectivity([List|Rest]) :-
    write('Input list: '), write(List), nl,
    is_connected(List, Answer), nl, nl, 
    test_connectivity(Rest).

test_graphical([]).
test_graphical([List|Rest]) :-
    is_graph(List),
    test_graphical(Rest).