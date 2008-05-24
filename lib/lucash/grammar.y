class Lucash
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
	line: 
	    expr { [:line, val[0]] } 
		| expr ';' { [:line, val[0]] } 
		| expr '\n' { [:line, val[0]] } 
		| '(' line ')' { [:line, val[1]] }
    | '(' ')' { [:empty_parens] }
		| 'if' line program 'end' { [:if, val[1], val[2]] }
		| 'if' line program 'else' program 'end' { [:if_else, val[1], val[2], val[4]] }
	  | expr '&&' line { [:and, val[0], val[2]] }
		| command '|' line { [:pipe, [:line, val[0]], val[2]] }
		| line '.' line { [:method, val[0], val[2]] }
		| line '.' IDENT { [:method, val[0], val[2]] }
		| line '.' IDENT expr { [:method_with_args, val[0], val[2], val[3]] }
		| line '.' IDENT '(' expr ')' { [:method_with_args, val[0], val[2], val[4]] }
		| IDENT '=' line { [:assignment, val[0], val[2]] }
		| IDENT '<-' line { [:functional_assignment, val[0], val[2]] }
	expr: 
		  expr '+' atom { [:add, val[0], val[2]] }
		| expr '-' atom { [:subtract, val[0], val[2]] }
		| expr '*' atom { [:multiply, val[0], val[2]] }
		| expr '/' atom { [:divide, val[0], val[2]] }
		| expr '%' atom { [:mod, val[0], val[2]] }
		| array { val[0] }
	array:
		  '[' basic_result ']'	{ [:array, val[1]] }
    | '[' ']'		 		        { [:empty_array] }
		| atom					        { val[0] }
	basic_result:
		  program { [:splat, [val[0]]] }
		| program ',' basic_result { [:splat, [val[0], *val[2][1]]] }
	atom:
	    NUMBER { [:number, val[0]] }
    | command { val[0] }
	command:
		  IDENT { [:value, [val[0]]] }
  	| IDENT command { [:value, [val[0], *val[1][1]]] }
end

---- inner

require 'lucash/parser'

include Lucash::Parser

def next_token
  @q.shift
end

---- footer