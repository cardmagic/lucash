class Lucash
  class InvalidAST < StandardError; end

  def initialize
    @grammar = LucashGrammar.new
  end
  
  def parse(str)
    ast = @grammar.parse(str)
    puts ast.inspect if ENV['DEBUG']
    
    result = Lucash::AST.new(ast)
    result.eval
  end
  
  def start
    loop do
      puts
      print '? '
      if str = gets
        break if /q/i =~ str
        begin
          puts parse(str).inspect
        rescue ParseError
          puts $!
        rescue InvalidAST
          puts "Invalid AST (#{$!.message.inspect})"
        end
      else
        exit
      end
    end
  end
end

require 'lucash/grammar'
require 'lucash/shell'
require 'lucash/ast'