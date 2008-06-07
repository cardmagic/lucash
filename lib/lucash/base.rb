class Lucash
  class InvalidAST < StandardError; end

  def initialize
    @grammar = LucashGrammar.new
  end
  
  def parse(str)
    ast = @grammar.parse(str)
    puts ast.inspect if ENV['DEBUG']
    Lucash::AST.eval(ast)
  end
  
  def start
    loop do
      puts
      print '? '
      if str = gets
        break if /q/i =~ str
        begin
          r = parse(str)
          case r
          when String
            puts r
          else
            puts r.inspect
          end
        rescue ParseError
          puts $!
        rescue InvalidAST
          puts "Invalid AST (#{$!.message})"
        end
      else
        exit
      end
    end
  end
end

require 'lucash/grammar'
require 'lucash/executable'
require 'lucash/paths'
require 'lucash/lambda'
require 'lucash/variable'
require 'lucash/ast'