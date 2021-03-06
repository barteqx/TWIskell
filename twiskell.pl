:- compile(interpreter).

command_shell(X, Env):-
	append(":l ", A, X), (atom_codes(X1, A), read_file_to_codes(X1, Codes, []), !,
	lexer(Tk, Codes, []), program(T, Tk, []), preprocessing(T, T1), append(Env, T1, Env1); 
	writef("Invalid file or file path\n> ")),
	read_line_to_codes(user_input, Code),
	command_shell(Code, Env1).
	
command_shell(X, Env):-
	append(":e ", A, X), lexer(Tk, A, []), expression(T, Tk, []), eval(T, Env, [], Result),
	output_string(Result, String), writef(String), writef("\n"),
	writef("\n> "),
	read_line_to_codes(user_input, Code),
	command_shell(Code, Env).
	
command_shell(X, _):-
	append(":q", _, X), halt.
	
command_shell("", Env):-
	writef("\n> "),
	read_line_to_codes(user_input, Code),
	command_shell(Code, Env).
	
command_shell(_, Env):-
	writef("Invalid command\n> "),
	read_line_to_codes(user_input, Code),
	command_shell(Code, Env).
	
output_string(constructor(X, _), String):-
	number(X), number_codes(X, String), !.

output_string(constructor(X, Y), String):-
	append("(", X, X1), build_string(Y, S), append(X1, " ", X2), append(X2, S, X3), append(X3, ")", String).

output_string((variable(_), _), "<function>").

build_string([H|T], String):-
	output_string(H, S), append(S, " ", S1), build_string(T, S3), append(S1, S3, String).

build_string([], "").

:- command_shell("", []).
