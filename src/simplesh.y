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
		line program 
		| line 
	line: 
		expr ';' { val[0] } 
		| expr '\n' { val[0] } 
		| expr { val[0] }
		| '(' line ')' { val[1] }
        | '(' ')' { nil }
	    | expr '&&' line { val[2] }
		| command '|' line { do_command("#{val[0]} #{val[2]}") }
		| line '.' line { val[0].send(val[2]) }
		| line '.' line { val[0].send(val[2]) }
		| line '.' IDENT { val[0].send(val[2]) }
		| line '.' IDENT expr { val[0].send(val[2], *val[4]) }
		| line '.' IDENT '(' expr ')' { val[0].send(val[2], *val[4]) }
		| IDENT '=' expr { $vals[val[0]] = val[2] }
	expr: 
		expr '+' mulex { val[0] + val[2] }
		| expr '-' mulex { val[0] - val[2] }
		| mulex { val[0] } 
	mulex:
		mulex '*' term { val[0] * val[2] }
		| mulex '/' term { val[0] / val[2] }
		| mulex '%' term { val[0] % val[2] }
		| mulex '&' term { val[0] & val[2] }
		| array { val[0] }
	array:
		'[' basic_result ']'	{ val[1] }
        | '[' ']'		 		{ [] }
		| term					{ val[0] }
	basic_result:
		line { [val[0]] }
		| basic_result ',' line { val[0] + [val[2]] }
	term:
	    | NUMBER { val[0] }
        | command {
			if $vals[val[0]]
				return $vals[val[0]]
			else
				do_command(val[0])
			end
		}
	command:
		IDENT { val[0] }
		| IDENT command { "#{val[0]} #{val[1]}" }
end

---- inner
  
	def parse(str)
	  @q = []
	  until str.empty?
	    case str
	    when /\A\s(\.\.*)/
              @q.push [:IDENT, $1]
	    when /\A\s+/
	    when /\A&&/
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

$vals = {}

def do_command(command)
	case command
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
    str.chop!
    break if /q/i =~ str
    begin
      puts parser.parse(str)
    rescue ParseError
      puts $!
    end
  else
    exit
  end
end

