require 'rockit/rockit'

# NOTE! There are bugs in here. Its simply a proof-of-concept...

def run_interactive_evaluator(parser, evaluator, prompt = "diff")
  while true
    print "<#{prompt}> "
    expr = ""
    expr = STDIN.gets.chomp
    exit if expr == "exit"
    evaluator.eval(parser.parse expr)
  end
end

def differentiate_parser
  Parse.generate_parser <<-'END_OF_GRAMMAR'
   Grammar DiffPolynomials
   Tokens
    BLANK      = /\s+/               [:Skip]
    FLOAT      = /\d+(\.\d+)?/ 
    ID         = /[a-z]/
   Productions
    Input -> Assignment                   [^]
          |  Differentiation              [^]
    Differentiation -> 'd' ID '/d' ID     [Differentiate: _, func, _, var]
    Assignment -> ID '=' Polynom          [Assignment: func, _, polynom]
    Polynom -> list(Term, '+')            [Polynom: terms]
    Term -> FLOAT                         [^: val]
         |  FLOAT? ID Exponent?           [Term: coefficient, var, exponent]
         |  ID                            [^: var]
    Exponent -> '^' FLOAT                 [^: _, exponent]
  END_OF_GRAMMAR
end

class Evaluator
  def initialize
    @funcs = Hash.new
  end

  def inspect_polynom(polynomAst)
    return "" unless polynomAst.name == "Polynom"
    str = ""
    polynomAst.terms.each do |term|
      str += " + " if str.length > 0
      case term.name
      when "Term"
	str += term.coefficient.lexeme if term.coefficient
	str += term.var.lexeme
	str += ("^" + term.exponent.lexeme) if term.exponent and term.exponent.lexeme.to_f != 1.0
      else
	str += term.lexeme
      end
    end
    str
  end

  def update_lexeme(node, newValue)
    # Ugly!!
    node.instance_eval("@childrens[0] = " + newValue.inspect)
  end

  def differentiate(functionAst, var)
    functionAst.terms.collect! do |term|
      if term.name == "Term" and term.var.lexeme == var.lexeme
	if term.coefficient and term.exponent
	  update_lexeme(term.coefficient, 
			(eval(term.coefficient) * eval(term.exponent)).inspect)
	end
	if term.exponent
	  update_lexeme(term.exponent,
			(eval(term.exponent) - 1).inspect)
	else
	  term = term.coefficient
	end
      else
	term = nil
      end
      term
    end.compact!
    functionAst
  end

  def eval(ast)
    case ast.name
    when "Assignment"
      @funcs[ast.func.lexeme] = eval(ast.polynom)
      puts "storing #{inspect_polynom(@funcs[ast.func.lexeme])} in #{ast.func.lexeme}"
    when "Differentiate"
      p = differentiate(eval(ast.func), ast.var)
      puts "#{inspect_polynom(p)}"
    when "Polynom"
      ast
    when "Term"
      val = eval(ast.var)
      val **= eval(ast.exponent) if ast.exponent
      val *= ast.coefficient.lexeme.to_f if ast.coefficient
      val
    when "FLOAT"
      ast.lexeme.to_f
    when "ID"
      @funcs[ast.lexeme]
    else
      raise "Unknown AST node: #{ast.inspect}"
    end
  end
end

if $0 == __FILE__
  parser, evaluator = differentiate_parser, Evaluator.new
  if ARGV[0]
    puts evaluator.eval(parser.parse ARGV[0]).inspect
  else
    run_interactive_evaluator(parser, evaluator)
  end
end
