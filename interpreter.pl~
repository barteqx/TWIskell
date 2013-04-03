:- compile(lexer).
:- compile(parser).

interpreter(X, Y, R):-
	atom_codes(X1, X), read_file_to_codes(X1, Codes, []),
	lexer(Z, Codes, []), program(T, Z, []), lexer(Tk, Y, []), expression(E, Tk, []),  preprocessing(T, T1), eval(E, T1, _, R, _), !.

%globalizing local definitions
preprocessing([(head(variable(X), Y), Expr, Local)|T], [(head(variable(X), Y), Expr1)|T1]):-
	globalize(Local, X, Global), globalize_tree(Expr, Local, X, Expr1),
	preprocessing(T, T2), append(Global, T2, T1), !.
	
preprocessing([X|T], [X|T1]):-
	preprocessing(T, T1).
	
preprocessing([],[]).

globalize(Local, X, Global):-
	globalize(Local, Local, X, Global).

globalize([(head(variable(Id), Args), Expr)|T], Local, X, [(head(variable(NewId), Args1), Expr1)|T1]):-
	append(X, "!", X1), append(X1, Id, NewId), globalize_tree(Expr, Local, X, Expr1), globalize_const(Args, Local, X, Args1), globalize(T, Local, X, T1).

globalize([],_,_,[]).

globalize_tree(variable(X), _, Name, variable(NewId)):-
	append(Name, "!", X1), append(X1, X, NewId).

globalize_tree(constructor(X, Y), Local, Name, constructor(X, Y1)):-
	globalize_const(Y, Local, Name, Y1).	
		
globalize_tree(X, Local, Name, Y):-
	X =.. [A, B, C], globalize_tree(B, Local, Name, B1), globalize_tree(C, Local, Name, C1), Y =.. [A, B1, C1].

globalize_tree(X, _, _, X).

globalize_const([H|T], Local, Name, [H1|T1]):-
	globalize_tree(H, Local, Name, H1),
	globalize_const(T, Local, Name, T1).
	
globalize_const([],_, _,[]).

%evaluate application
eval(app(H,Y), Env, Args, Result, Rest):-
	getExpList(app(H,Y), [H1|L]),
	append(L, Args, Args1),
	eval(H1, Env, Args1, Result, Rest).

%evaluate lambdas
eval(lambda(variable(Id), Expr), Env, Args, Result, Rest2):-
	pattern_matching([variable(Id)], Args, Env, Var, Rest),
	transform(Expr, Var, Expr1, Env),
	eval(Expr1, Env, Rest, Result, Rest2).
	
%evaluate function
eval(variable(Id), Env, Args, Result, Rest1):-
	member((head(variable(Id), X), Expression), Env),
	pattern_matching(X, Args, Env, Vars, Rest),
	(transform(Expression, Vars, Expr1, Env),
	eval(Expr1, Env, Rest, Result, Rest1); Expression \= constructor(_,_),
	eval(Expression, Env, Rest, Result, Rest1)).

eval(variable(Id), Env, Args, Result, []):-
	member((head(variable(Id), X), _), Env),
	checklength(Args, X), Result = (variable(Id), Args).

%evaluate arithmetics and relations
eval(SExp, Env, Args, constructor(Result, []), Rest2):-
	SExp =..[Op, X, Y], member(Op, [=,+,-,*,div,mod,\=, >, <, >=, =<]),
	eval(X, Env, Args, constructor(R1, []), Rest1), eval(Y, Env, Rest1, constructor(R2, []), Rest2), 
	Res =.. [Op, R1, R2], (\+ member(Op, [=, >=, =<, >, <, \=]), !, 
	Result is Res; call(Res), !, Result = "True"; Result = "False").

%evaluate costructors	
eval(constructor(X, Y), Env, Args, constructor(X, Y1), Args):- 
	eval_const(Y, Env, Y1).

eval(constructor(X, []), _, Args, constructor(X, []), Args).
	
eval_const([H|T], Env, [H1|T1]):-
	eval(H, Env, [], H1, _),
	eval_const(T, Env, T1).

eval_const([],_,[]).

%transfrorm from absctract tree to concrete tree	
transform(variable(X), Variables, Y, _):-
	member((variable(X), Y), Variables);
	Y = variable(X).

transform(constructor(X, Y), Variables, constructor(X, Y1), Env):-
	transform_const(Y, Variables, Y1, Env).

transform(X, Variables, Y, Env):-
	X =.. [A, B, C], transform(B, Variables, B1, Env), transform(C, Variables, C1, Env), Y =.. [A, B1, C1].
	
transform_const([H|T], Variables, [H1|T1], Env):-
	transform(H, Variables, H1, Env),
	transform_const(T, Variables, T1, Env).

transform_const([], _, [], _).

%flatten applications	
getExpList(X, L1):-
	getExpList(X, L, []),
	reverse(L, L1).

getExpList(app(X,Y), [Y|L2], L):-
	getExpList(X, L2, L).

getExpList(X,[X|L],L):-
	X \= app(_,_).

pattern_matching(A, B, Env, L2, Rest):-
	A = [constructor(X, Y)|T], B = [Const|T1],
	eval(Const, Env, [], constructor(X, Y1), _),
	pattern_matching(Y, Y1, Env, L, _), pattern_matching(T, T1, Env, L1, Rest), append(L,L1,L2).

pattern_matching([variable(X)|T], [constructor(Y, Z)|T1], Env, [(variable(X), constructor(Y, Z))|Vars], Rest):-
	pattern_matching(T, T1, Env, Vars, Rest).
	
pattern_matching([variable(Y)|T], [variable(X)|T1], Env, [(variable(Y), variable(X))|Vars], Rest):-
	pattern_matching(T, T1, Env, Vars, Rest).
	
pattern_matching([variable(Y)|T], [X|T1], Env, [(variable(Y), X)|Vars], Rest):-
	pattern_matching(T, T1, Env, Vars, Rest).

pattern_matching([], Rest, _, [], Rest).

checklength([_|T], [_|T1]):-
	checklength(T, T1).

checklength([], _).
