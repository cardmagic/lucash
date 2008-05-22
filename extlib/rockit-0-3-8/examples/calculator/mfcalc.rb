require 'calculator'

class MultiFuncCalculator < Calculator
  def initialize
    @vars = Hash.new(0)
    @function_map = {"ln" => "log"}
  end

  def eval(ast)
    case ast.name
    when "Exponentiation"
      eval(ast.left) ** eval(ast.right)
    when "Variable"
      @vars[ast.var.lexeme]
    when "Assignment"
      @vars[ast.var.lexeme] = eval(ast.expr)
    when "UnaryMinus"
      -1.0 * eval(ast.expr)
    when "Constant"
      ast.num.lexeme.to_f
    when "FunCall"
      fun = ast.function.lexeme
      fun = @function_map[fun] if @function_map.keys.include?(fun)
      begin
	Kernel.eval("Math.#{fun}(#{self.eval(ast.param).inspect})")
      rescue NameError
	"No function named #{fun} available"
      end
    else
      super(ast)
    end
  end
end

def mfcalc_parser
  Parse.generate_parser <<-'END_OF_GRAMMAR'
   Grammar MultiFunctionCalculator
     Tokens
       WS         = /\s+/               [:Skip]
       NUM        = /\d+(\.\d+)?/
       ID         = /[A-Za-z]\w*/
   Productions
       Expr -> NUM                      [Constant: num]
            |  ID                       [Variable: var]
            |  ID '=' Expr              [Assignment: var, _, expr]
            |  ID '(' Expr ')'          [FunCall: function, _, param, _]
            |  '(' Expr ')'             [^: _,expr,_]
            |  '-' Expr                 [UnaryMinus: _,expr]
            |  Expr '^' Expr            [Exponentiation: left,_,right]
            |  Expr '/' Expr            [Div: left,_,right]
            |  Expr '*' Expr            [Mul: left,_,right]
            |  Expr '+' Expr            [Plus: left,_,right]
            |  Expr '-' Expr            [Minus: left,_,right]
   Priorities
       right(Exponentiation), left(Mul), left(Plus), left(Minus)
       left(Plus, Minus)
       FunCall > Assignment > UnaryMinus > Exponentiation > Div = Mul > Plus 
       Plus = Minus
  END_OF_GRAMMAR
end

if $0 == __FILE__
  parser, calc = mfcalc_parser, MultiFuncCalculator.new
  if ARGV[0]
    puts calc.eval(parser.parse ARGV[0]).inspect
  else
    run_interactive_calculator(parser, calc, "mfcalc")
  end
end
