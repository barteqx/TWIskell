lexer(Tokens) -->
   white_space,
   (
   		"--",		!, line, lexer(Tokens) 
   		;  "-", digit(D),  !, number(D, N), { N1 is N * -1, Token = tokConstructor(N1) } 
   		;(
         ";",       !, { Token = tokSColon }
      ;  "(",       !, { Token = tokLParen }
      ;  ")",       !, { Token = tokRParen }
      ;  "{",		!, { Token = tokBlockOpen }
      ;  "}",		!, { Token = tokBlockClose }
      ;  "->",		!, { Token = tokArrow }
      ;  "\\",		!, { Token = tokLambda }
      ;  "+",       !, { Token = tokPlus }
      ;  "-",       !, { Token = tokMinus }
      ;  "*",       !, { Token = tokTimes }
      ;  "=",       !, { Token = tokEq }
      ;  "/=",      !, { Token = tokNeq }
      ;  "<=",      !, { Token = tokLeq }
      ;  "<",       !, { Token = tokLt }
      ;  ">=",      !, { Token = tokGeq }
      ;  ">",       !, { Token = tokGt }
      ;  "div",		!, { Token = tokDiv }
      ;  "mod",		!, { Token = tokMod }
      ;  "where",	!, { Token = tokWhere }
      ;  digit(D),  !, number(D, N), { Token = tokConstructor(N) }
      ;  letter(L), !, (constructor(L, Id), { Token = tokConstructor(Id)};
      				variable(L, Id), { Token = tokVariable(Id)})
      ;  [_], !, { Token = tokUnknown }), !,
         { Tokens = [Token | TokList] }, lexer(TokList)
   ;  [],
         { Tokens = [] }
   ).

white_space -->
   [Char], { code_type(Char, space) }, !, white_space.
white_space -->
   [].
   
digit(D) -->
   [D],
      { code_type(D, digit) }.

digits([D|T]) -->
   digit(D),
   !,
   digits(T).
digits([]) -->
   [].

number(D, N) -->
   digits(Ds),
      { number_chars(N, [D|Ds]) }.

letter(L) -->
   [L], { code_type(L, alpha) }.

constructor(L, Id) -->
	{upcase(L)}, alphanum(X), {Id = [L|X]}.

variable(L, Id) -->
	alphanum(X), {downcase(L), Id = [L|X]}.

alphanum([A|T]) -->
   [A], { code_type(A, alnum) }, !, alphanum(T).
alphanum([]) -->
   [].
   
line -->
	"\n", !.	
	
line -->
	[_], line.
   
upcase(L):-
	L >= 65, L =< 90.

downcase(L):-
	L >= 97, L =< 122.
	

