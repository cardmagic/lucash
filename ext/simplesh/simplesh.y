# $Id: simplesh.y 2112 2008-05-14 13:29:32Z cardmagic $
#
# Very simple shell.

class Simplesh
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
		| program '\n' { [:program, [val[0]]] }
		| program ';' { [:program, [val[0]]] }
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
  
	def parse(str)
	  @q = []
	  until str.empty?
	    case str
	    when /\A\s(\.\.*)/
          @q.push [:IDENT, $1]
	    when /\A[ \t\r]+/
		  when /\A(if|else|end)/i
		    @q.push [$&, $&]
		  when /\A\n/
		    @q.push ['\n', '\n']
	    when /\A&&/
		    @q.push [$&, $&]
	    when /\A<-/
		    @q.push [$&, $&]
	    when /\A\-?\d+\.\d+/
	      @q.push [:NUMBER, $&.to_f]
	    when /\A\-?[\d]+/
	      @q.push [:NUMBER, $&.to_i]
	    when /\A\:([\w\-]+)/
	      @q.push [:IDENT, $1.intern]
	    when /\A[\w\-][\w\-\=]*/
        @q.push [:IDENT, $&]
      when /\A\/([^\/]+)\//
        @q.push [:IDENT, Regexp.new($1)]
	    when /\A.|\n/o
	      s = $&
	      @q.push [s, s]
	    end
	    str = $'
	  end
	  @q.push [false, '$end']
	  puts @q.inspect if ENV['DEBUG']
	  do_parse
	end

	def next_token
	  @q.shift
	end
	
---- footer

require 'rubygems'
gem 'open4'
require 'open4'
require 'pp'

$vals = {}

def simplesh_eval(ast)
  case ast[0]
  when :program
    ast[1].map {|stmt| simplesh_eval(stmt)}.last
  when :line
    simplesh_eval(ast[1])
  when :and
    if simplesh_eval(ast[1])
      simplesh_eval(ast[2])
    end
  when :if
    if simplesh_eval(ast[1])
      simplesh_eval(ast[2])
    end
  when :if_else
    if simplesh_eval(ast[1])
      simplesh_eval(ast[2])
    else
      simplesh_eval(ast[3])
    end
  when :for
    for i in (simplesh_eval(ast.from)..simplesh_eval(ast.to))
      $vals[ast.ident.lexeme] = i
      simplesh_eval(ast.statements)
    end
  when :assignment
    $vals[ast[1]] = simplesh_eval(ast[2])
  when :functional_assignment
    $vals[ast[1]] = ast[2]
  when :add
    simplesh_eval(ast[1]) + simplesh_eval(ast[2])
  when :block
    simplesh_eval(ast[1])
  when :subtract
    simplesh_eval(ast[1]) - simplesh_eval(ast[2])
  when :multiply
    simplesh_eval(ast[1]) * simplesh_eval(ast[2])
  when :divide
    simplesh_eval(ast[1]) / simplesh_eval(ast[2])
  when :mod
    simplesh_eval(ast[1]) % simplesh_eval(ast[2])
  when :array
    simplesh_eval(ast[1])
  when :splat
    ast[1].map{|a| simplesh_eval(a)}
  when :string
    ast[1]
  when :value
    case ast[1][0]
    when "false"
      false
    when "true"
      true
    else
      ast[1]
    end
  when :number
    ast[1]
  end
end

def do_command(command)
	case command
	when "true", "false"
		return command == "true"
	when /^cd (.*)$/
		return Dir.chdir($1)
	end

	r = []
	er_t = nil
	in_t = nil
	o_t = nil
	p = Open4::popen4(command) do |pid, stdin, stdout, stderr|
		er_t = Thread.new do
			loop do
				$stderr.print stderr.read(stderr.stat.size)
				$stderr.flush
			end
		end

		in_t = Thread.new do
			loop do
				data = gets
				stdin.write(data)
			end
		end
	
		o_t = Thread.new do
			loop do
				r << stdout.read(stdout.stat.size)
			end
		end
	end
	er_t.kill
	in_t.kill
	o_t.kill
	r.join("")
rescue Errno::ENOENT, TypeError
	command
end

parser = Simplesh.new
puts
puts 'type "Q" to quit.'
puts
while true
  puts
  print '? '
  if str = gets
    break if /q/i =~ str
    begin
      p = parser.parse(str)
      r = simplesh_eval(p)
      puts p.inspect if ENV['DEBUG']
      puts r.inspect
    rescue ParseError
      puts $!
    end
  else
    exit
  end
end

