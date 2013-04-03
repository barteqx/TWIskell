%grammar
program(AbstractST) --> 
	clausee(Clause), !, program(Rest), { AbstractST = [Clause | Rest] }.
program([]) --> [].
	
clausee(Clause) -->
	head(Head), !, [tokEq], expression(Expression), local_declarations(Local), 
	{ Local \= ";", !, Clause = (Head, Expression, Local); Clause = (Head, Expression)}.
	
clauses(Clauses) --> clausee(Clause), !, { Clauses = Clause }.
clauses(Clauses) --> [tokBlockOpen], program(Clauses), [tokBlockClose].

local_declarations(";") --> [tokSColon], !.
local_declarations(Local) --> [tokWhere], clauses(Local).

head(Head) -->
	[tokVariable(Id)], !, parameters(Parameters), { Head = head(variable(Id), Parameters) }.
	
parameters(Parameters) --> parameter(Param), !, parameters(Params), { Parameters = [Param|Params] }.
parameters([]) --> [].

parameter(Param) -->
	[tokVariable(Id)], !, {Param = variable(Id)};
	[tokConstructor(Id)], !, {Param = constructor(Id, [])};
	[tokLParen], expression(Expression), !, [tokRParen], {Param = (Expression)};
	[tokLParen], pattern(Pattern), [tokRParen], {Param = (Pattern)}.

pattern(Pattern) -->
	[tokVariable(Id)], !, {Pattern = variable(Id)};
	[tokConstructor(Id)], !, {Pattern = constructor(Id, [])};
	[tokLParen], [tokConstructor(Id)], parameters(Params), [tokRParen], {Pattern = constructor(Id, Params) }.

expression(Expression) -->
	[tokLambda], !,[tokVariable(Id)], [tokArrow], expression(Expr), {Expression = lambda(variable(Id), Expr)}.
	
expression(Expression) -->
	rel_expression(Expression).

rel_expression(RExp) -->
	arith_expression(AExp1), rel_operator(Operator), !, arith_expression(AExp2),
	{ operators(Operator, Op), RExp =.. [Op,AExp1,AExp2] }; arith_expression(RExp).

arith_expression(AExp) -->
	simple_expression(SExp), arith_expression(SExp,AExp).
arith_expression(SExp,AExp) -->
	add_operator(Operator), !, simple_expression(SExp2), 
	{ operators(Operator, Op), SExp1 =.. [Op,SExp,SExp2] }, arith_expression(SExp1,AExp).
arith_expression(AExp,AExp) -->
	[].
	
simple_expression(SExp) -->
	application(App),simple_expression(App,SExp).
simple_expression(Exp1,Exp2) -->
	mult_operator(Operator), !, application(App),
	{ operators(Operator, Op),  Exp=..[Op,Exp1,App] }, simple_expression(Exp,Exp2).
simple_expression(Exp,Exp) -->
	[].

application(Application) -->
	atomic_expression(AtExpression), application(AtExpression, Application).
application(Application1, Application2) -->
	atomic_expression(AtExpression), ! 
	,{Application = app(Application1, AtExpression)}, application(Application, Application2).
application(Application, Application) -->
	[].

atomic_expression(AtExpression) -->
	[tokLParen], !, expression(Expression), [tokRParen], {AtExpression = (Expression)};
	[tokVariable(Id)], !, { AtExpression = variable(Id)};
	[tokConstructor(N)], { number(N), !, AtExpression = constructor(N, [])};
	[tokConstructor(Id)], parameters(Params), { AtExpression = constructor(Id, Params)}.
	
mult_operator(X) --> 
	[tokTimes], !, { X = '*' }; 
	[tokMod], !, { X = 'mod' }; 
	[tokDiv], { X = 'div' }.

add_operator(X) -->	
	[tokPlus], !, { X = '+' }; 
	[tokMinus], { X = '-' }.

rel_operator(X) -->
	[tokEq], !, { X = '=' };
	[tokLeq], !, { X = '<=' }; 
	[tokLt], !, { X = '<' };
	[tokGeq], !, { X = '>=' }; 
	[tokGt], !, { X = '>' }; 
	[tokNeq], !, { X = '/=' }.
	
operators(I, O):-
	X = [('+', +), ('-', -), ('*', *), ('div', div), ('mod', mod), ('=', =), ('>=', >=), ('<=', =<), ('>', >), ('<', <), ('/=', \=)],
	member((I,O), X).
