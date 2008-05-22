require 'rockit/rockit'

def run_interactive_evaluator(parser, evaluator, prompt = "poly")
  while true
    print "<#{prompt}> "
    expr = ""
    expr = STDIN.gets.chomp
    exit if expr == "exit"
    begin
      evaluator.eval(parser.parse expr)
    rescue Exception => e
      puts e.inspect
    end
  end
end

def polynomials_parser
  # Note! The grammar is not exactly as the one in the ANTLR tutorial but
  # the differences are minor.
  Parse.generate_parser <<-'END_OF_GRAMMAR'
   Grammar Polynomials
   Tokens
    BLANK      = /\s+/               [:Skip]
    FLOAT      = /\d+(\.\d+)?/ 
    ID         = /[a-z]/
   Productions
    Assignment -> ID '=' Polynom          [Assignment: var, _, polynom]
    Polynom -> list(Term, '+')            [Polynom: terms]
    Term -> FLOAT                         [^: val]
         |  FLOAT? ID Exponent?           [Term: coefficient, var, exponent]
         |  ID                            [^: var]
    Exponent -> '^' FLOAT                 [^: _, exponent]
             |  '^' ID                    [^: _, exponent]
  END_OF_GRAMMAR
end

class Evaluator
  def initialize
    @vars = Hash.new(0.0)
  end

  def eval(ast)
    case ast.name
    when "Assignment"
      @vars[ast.var.lexeme] = eval(ast.polynom)
      puts "storing #{@vars[ast.var.lexeme]} in #{ast.var.lexeme}"
    when "Polynom"
      sum = 0.0
      ast.terms.each {|term| sum += eval(term)}
      sum
    when "Term"
      val = eval(ast.var)
      val **= eval(ast.exponent) if ast.exponent
      val *= ast.coefficient.lexeme.to_f if ast.coefficient
      val
    when "FLOAT"
      ast.lexeme.to_f
    when "ID"
      @vars[ast.lexeme]
    else
      raise "Unknown AST node: #{ast.inspect}"
    end
  end
end

if $0 == __FILE__
  parser, evaluator = polynomials_parser, Evaluator.new
  if ARGV[0]
    puts evaluator.eval(parser.parse ARGV[0]).inspect
  else
    run_interactive_evaluator(parser, evaluator)
  end
end
