require 'calculator'

class AdvancedCalculator < Calculator
  def eval(ast)
    case ast.name
    when "Pow"
      eval(ast.base) ** eval(ast.exponent)
    when "UnaryMinus"
      - eval(ast.num)
    else
      super(ast)
    end
  end
end

def rpcalc_parser
  Parse.generate_parser <<-'END_OF_GRAMMAR'
   Grammar ReversePolishNotationExpressions
   Tokens
    Blank      = /\s+/               [:Skip]
    Num        = /\d+/
   Productions
    Exp -> Num                       [Constant: num]
        |  Exp Exp '+'               [Plus: left,right,_]
        |  Exp Exp '-'               [Minus: left,right,_]
        |  Exp Exp '*'               [Mul: left,right,_]
        |  Exp Exp '/'               [Div: left,right,_]
        |  Exp Exp '^'               [Pow: base,exponent,_]
        |  Exp 'n'                   [UnaryMinus: num,_]
  END_OF_GRAMMAR
end

if $0 == __FILE__
  parser, calc = rpcalc_parser, AdvancedCalculator.new
  if ARGV[0]
    puts calc.eval(parser.parse ARGV[0]).inspect
  else
    run_interactive_calculator(parser, calc, "rpcalc")
  end
end
