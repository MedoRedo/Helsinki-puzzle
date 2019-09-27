grid_build(N,M):- helper1(N,0,M).
helper1(N,N,[]).
helper1(N,M,[H|T]):- M<N,length(H,N),M1 is M+1,helper1(N,M1,T).

num_gen(F,F,[F]).
num_gen(F,L,[F|T]):- F<L,F1 is F+1,num_gen(F1,L,T).

grid_gen(N,M):- grid_build(N,M),num_gen(1,N,Z),assignt(M,Z).

grid_gen2(N,M):- grid_build(N,M),row_col_match(M),num_gen(1,N,Z),assign(M,Z).

assign([],_).
assign([H|T],Z):- assign2(H,Z),\+ all_equal(H),	\+ member(H , T),assign(T,Z).

assignt([],_).
assignt([H|T],Z):- assign2(H,Z),assignt(T,Z).

assign2([],_).
assign2([H|T] , Z):- member(H,Z), assign2(T,Z).


all_equal([_]).
all_equal([H,H|T]):- all_equal([H|T]).


check_num_grid(G):- length(G,N),flatten(G,R),max_list(R,X),N>=X,num_gen(1,X,M),intersection(M,R,M).

column(I , G , []):-
	length(G , N),
	N1 is N*N,
	I > N1.
	
column(Index , G , [H|T]):-
	length(G,N),
	N1 is N*N,
	Index =< N1,
	flatten(G,R),
	nth1(Index , R , H),
	I is Index +N,
	column(I , G , T).

trans([],[]).
trans([H],[H]).

trans(M , R):-
	length(M,N),N>1,
	trans(1 , M , R).

trans(A , M , []):-
	length(M , N),
	A > N.	
trans(A , M , [H|T]):-
	length(M , N),
	A =< N,
	column(A , M , H),
	A1 is A+1,
	trans(A1 , M , T).


acceptable_distribution(G):-
	trans(G , M),
	check(G , M).

check([] ,[]).	
check([H1 | T1] , [H2 | T2]):-
	H1 \== H2,
	check(T1 ,T2).
		
distinct_rows([_]).
distinct_rows([H|T]):-
	\+ member(H , T),
	distinct_rows(T).
	
distinct_columns(M):-
	trans(M , G),
	distinct_rows(G).
	
	
	
row_col_match(M):-
	trans(M , G),
	acceptable_permutation(M , G).

match([] , _).	
match([H|T] , G):-
	member(H ,G),
	match(T , G).

helsinki(N,G):-
	grid_gen2(N,G),
	check_num_grid(G).

acceptable_permutation(L,P):-
	permutation(L,P),check(L,P).
