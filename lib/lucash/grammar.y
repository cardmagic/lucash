class LucashGrammar
	options no_result_var
	prechigh
	    nonassoc UMINUS ';'
	    left '*' '/' '%' '|'
	    left '+' '-'
	preclow
rule
	program: 
		  program program { [:program, val[0][1] + val[1][1]] }
    | '{' program '}' { [:block, val[1]] }
		| line { [:program, [val[0]]] }
		| '\n' { [:program, []] }
		| ';' { [:program, []] }
	line: 
	    expr { val[0] } 
		| expr ';' { val[0] } 
		| expr '\n' { val[0] } 
		| expr '&' { [:background, val[0]] } 
		| 'if' line program 'end' { [:if, val[1], val[2]] }
		| 'if' line program 'else' program 'end' { [:if, val[1], val[2], val[4]] }
	  | expr '&&' line { [:and, val[0], val[2]] }
		| expr '|' line { [:pipe, val[0], val[2]] }
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
	  | method_call '{' program '}' { [:yield, val[0], [:block, val[2]]] }
	expr: 
		  line '+' line { [:add, val[0], val[2]] }
		| line '-' line { [:subtract, val[0], val[2]] }
	  | line '*' line { [:multiply, val[0], val[2]] }
		| line '/' line { [:divide, val[0], val[2]] }
		| line '%' line { [:mod, val[0], val[2]] }
		| '[' basic_result ']'	{ [:array, val[1]] }
    | '[' ']'		 		        { [:empty_array] }
		| parens { val[0] }
	basic_result:
		  program { [:splat, [val[0]]] }
		| program ',' basic_result { [:splat, [val[0], *val[2][1]]] }
	parens: 
  	  '(' program ')' { val[1] }
    | '(' ')' { [:empty_parens] }
    | atom { val[0] }
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