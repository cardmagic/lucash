# $Id: calc.y 2112 2005-11-20 13:29:32Z aamine $
#
# Very simple calculater.

class Simplesh
  options no_result_var
rule
	result    : '(' result ')'   { val[1] }
	          | '(' ')'            { nil }
	          | contents           { `#{val[0]}` }
         
	                # Racc can handle string over 2 bytes.
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
      when /\A[\w\.]+/
        yield :IDENT, $&
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

