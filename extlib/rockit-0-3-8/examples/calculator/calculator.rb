require 'rockit/rockit'

class Calculator
  def eval(ast)
    case ast.name
    when "Plus"
      eval(ast.left) + eval(ast.right)
    when "Minus"
      eval(ast.left) - eval(ast.right)
    when "Mul", "Mult"
      eval(ast.left) * eval(ast.right)
    when "Div"
      eval(ast.left) / eval(ast.right)
    when "Constant"
      ast.num.lexeme.to_i
    else
      raise "Unknown AST node: #{ast.inspect}"
    end
  end
end

def run_interactive_calculator(parser, calculator, prompt = "calc")
  while true
    print "<#{prompt}> "
    expr = STDIN.gets.chomp
    exit if expr == "exit"
    begin
      puts "#{calculator.eval(parser.parse expr)}"
    rescue Exception => e
      puts e.inspect
    end
  end
end

def calculator_parser
  Parse.generate_parser <<-'END_OF_GRAMMAR'
  Grammar Expressions
     Tokens
       WS         = /\s+/               [:Skip]
       NUM        = /\d+/
     Productions
       Expr -> NUM                    [Constant: num]
            |  Expr '+' Expr          [Plus: left,_,right]
            |  Expr '-' Expr          [Minus: left,_,right]
            |  Expr '*' Expr          [Mul: left,_,right]
            |  Expr '/' Expr          [Div: left,_,right]
            |  '(' Expr ')'           [^: _,expr,_]
  END_OF_GRAMMAR
end

if $0 == __FILE__
  parser, calc = calculator_parser, Calculator.new
  if ARGV[0]
    puts calc.eval(parser.parse ARGV[0]).inspect
  else
    run_interactive_calculator(parser, calc)
  end
end
