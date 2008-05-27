class LucashGrammar
	options no_result_var
	expect 88
	prechigh
	    nonassoc UMINUS ';'
	    left '*' '/' '%' '|'
	    left '+' '-'
	preclow
rule
	program: 
		  program program { [:program, val[0][1] + val[1][1]] }
    | '{' program '}' { val[1] }
		| line { [:program, [val[0]]] }
		| endline { val[0] }
	line: 
	    expr { val[0] } 
		| expr ';' { val[0] } 
		| expr '\n' { val[0] } 
		| expr '&' { [:background, val[0]] } 
		| '"' IDENT '"' { [:embedded_string, val[1]] } 
		| "'" IDENT "'" { [:string, val[1]] } 
		| 'if' line program 'end' { [:if, val[1], val[2]] }
		| 'if' line program 'else' program 'end' { [:if, val[1], val[2], val[4]] }
	  | line '&&' line { [:and, val[0], val[2]] }
		| line '|' line { [:pipe, val[0], val[2]] }
	  | line '||' line { [:or, val[0], val[2]] }
		| line '.' method_call { [:method, val[0], val[2]] }
		| line '==' line { [:==, val[0], val[2]] }
		| expr '=' line { [:assignment, val[0], val[2]] }
		| 'defn' atom '(' splat ')' program 'end' { [:assignment, val[1], [:lambda, val[3], val[5]]] }
		| 'def' atom program 'end' { [:assignment, val[1], [:lambda, nil, val[2]]] }
		| '->' '{' program '}' { [:lambda, nil, val[2]] }
		| '->(' splat ')' '{' program '}' { [:lambda, val[1], val[4]] }
		| '->(' splat ')' '{' program '}' '(' splat ')' { [:args, [:lambda, val[1], val[4]], val[7]] }
	endline:
	    '\n' { [:newline, []] }
	  | ';' { [:newline, []] }
	expr: 
		  line '+' line { [:add, val[0], val[2]] }
		| line '-' line { [:subtract, val[0], val[2]] }
	  | line '*' line { [:multiply, val[0], val[2]] }
		| line '/' line { [:divide, val[0], val[2]] }
		| line '%' line { [:mod, val[0], val[2]] }
		| '[' splat ']'	{ [:array, val[1]] }
    | '[' ']'		 		        { [:empty_array] }
		| method_call { val[0] }
	method_call:
	    parens { val[0] }
	  | parens '(' ')' { val[0] }
	  | parens '(' splat ')' { [:args, val[0], val[2]] }
	  | method_call '{' program '}' { [:yield, val[0], val[2]] }
	splat:
		  program { [:splat, [val[0]]] }
		| program ',' splat { [:splat, [val[0], *val[2][1]]] }
	parens: 
  	  '(' program ')' { val[1] }
    | '(' ')' { [:empty_parens] }
    | atom { val[0] }
	atom:
	    NUMBER { [:number, val[0]] }
		| IDENT { [:value, val[0]] }
  	| IDENT atom { [:value, val[0], *val[1][1]] }
end

---- inner

require 'lucash/parser'

include Lucash::Parser

def next_token
  @q.shift
end

---- footer