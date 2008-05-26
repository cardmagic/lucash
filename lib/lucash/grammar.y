class LucashGrammar
	options no_result_var
	prechigh
	    nonassoc UMINUS ';'
	    left '*' '/' '%' '|'
	    left '+' '-'
	preclow
rule
	program: 
		  line program { [:program, [val[0], *val[1][1]]] }
    | '{' program '}' { [:block, val[1]] }
		| line { [:program, [val[0]]] }
		| '\n' { [:program, []] }
		| ';' { [:program, []] }
	  | '(' line ')' { [:line, val[1]] }
    | '(' ')' { [:empty_parens] }
	line: 
	    expr { [:line, val[0]] } 
		| expr ';' { [:line, val[0]] } 
		| expr '\n' { [:line, val[0]] } 
		| 'if' line program 'end' { [:if, val[1], val[2]] }
		| 'if' line program 'else' program 'end' { [:if_else, val[1], val[2], val[4]] }
	  | expr '&&' line { [:and, val[0], val[2]] }
		| expr '|' line { [:pipe, [:line, val[0]], val[2]] }
	  | expr '||' line { [:or, val[0], val[2]] }
		| line '.' method_call { [:method, val[0], val[2]] }
		| expr '=' line { [:assignment, val[0], val[2]] }
		| expr '<-' line { [:functional_assignment, val[0], val[2]] }
	method_call:
	    atom { [:method_call, val[0]] }
	  | atom '(' ')' { [:method_call, val[0]] }
	  | atom '(' basic_result ')' { [:method_call, val[0], val[2]] }
	  | '(' expr ')' { [:method_call, val[1]] }
	  | '(' expr ')' '(' ')' { [:method_call, val[1]] }
	  | '(' expr ')' '(' basic_result ')' { [:method_call, val[1], val[4]] }
	expr: 
		  line '+' line { [:add, val[0], val[2]] }
		| line '-' line { [:subtract, val[0], val[2]] }
		| line '*' line { [:multiply, val[0], val[2]] }
		| line '/' line { [:divide, val[0], val[2]] }
		| line '%' line { [:mod, val[0], val[2]] }
		| '[' basic_result ']'	{ [:array, val[1]] }
    | '[' ']'		 		        { [:empty_array] }
		| atom	{ val[0] }
	basic_result:
		  program { [:splat, [val[0]]] }
		| program ',' basic_result { [:splat, [val[0], *val[2][1]]] }
	atom:
	    NUMBER { [:number, val[0]] }
		| IDENT { [:value, [val[0]]] }
  	| IDENT atom { [:value, [val[0], *val[1][1]]] }
end

---- inner

require 'lucash/parser'

include Lucash::Parser

def next_token
  @q.shift
end

---- footer