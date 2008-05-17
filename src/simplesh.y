# $Id: simplesh.y 2112 2008-05-14 13:29:32Z cardmagic $
#
# Very simple shell.

class Simplesh
	options no_result_var
rule
	atom  : array { val[0] }
	
	array   : '[' basic_result ']'	{ val[1] }
	        | '[' ']'		 		{ [] }
			| result				{ val[0] }
	
	basic_result : atom { [val[0]] }
				 | basic_result ',' atom { 
				 	val[0] + [val[2]]
				 }
	
	result    : '(' result ')'   { val[1] }
	          | '(' ')'            { nil }
			  | result '&&' atom { val[2] }
	          | contents           { 
					r = []
					er_t = nil
					in_t = nil
					o_t = nil
					p = Open4::popen4(val[0]) do |pid, stdin, stdout, stderr|
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
				}
    
	contents: IDENT              { val[0] }
			  | IDENT contents	 { "#{val[0]} #{val[1]}" }
end

---- inner
  
  def parse(str)
    @str = str
    yyparse self, :scan
  end

  private

  def scan
    str = @str
    until str.empty?
      case str
      when /\A\s+/
        str = $'
      when /\A[\w\.\-]+/
        yield :IDENT, $&
        str = $'
      when /\A&&/
        yield '&&', '&&'
        str = $'
      else
        c = str[0,1]
        yield c, c
        str = str[1..-1]
      end
    end
    yield false, '$'   # is optional from Racc 1.3.7
  end

---- footer

require 'rubygems'
gem 'open4'
require 'open4'
require 'termios'

parser = Simplesh.new
puts
puts 'type "Q" to quit.'
puts
while true
  puts
  print '? '
  str = gets.chop!
  break if /q/i =~ str
  begin
    puts parser.parse(str)
  rescue ParseError
    puts $!
  end
end

