require 'runit/testcase'
require 'runit/testsuite'
require 'runit/cui/testrunner'
require 'runit/topublic'
include RUNIT::ToPublic

require 'rockit/rockit' 

class Calculator
  def eval(ast)
    case ast.name
    when "UnaryMinus"
      -1.0 * eval(ast.e)
    when "Exp"
      eval(ast.base) ** eval(ast.exponent)
    when "Plus"
      eval(ast.left) + eval(ast.right)
    when "Minus"
      eval(ast.left) - eval(ast.right)
    when "Mul", "Mult"
      eval(ast.left) * eval(ast.right)
    when "Div"
      eval(ast.left) / eval(ast.right)
    when "Constant"
      ast.num.lexeme.to_f
    else
      raise "Unknown AST node: #{ast.inspect}"
    end
  end
end

class PrettyPrinter
  def print(ast)
    case ast.name
    when "Plus"
      "(#{print(ast.left)} + #{print(ast.right)})"
    when "Minus"
      "(#{print(ast.left)} - #{print(ast.right)})"
    when "Mul", "Mult"
      "(#{print(ast.left)} * #{print(ast.right)})"
    when "Div"
      "(#{print(ast.left)} / #{print(ast.right)})"
    when "Constant"
      ast.num.lexeme.to_f
    else
      raise "Unknown AST node: #{ast.inspect}"
    end
  end
end

class AcceptanceTestMediumGrammarsInText < RUNIT::TestCase
  def test_calculator
    p = Parse.generate_parser <<-'EOG'
      Grammar CalculatorExpressions
       Tokens
        Blank = /\s+/                     [:Skip]
        Int   = /\d+/
        Float = /\d+\.\d+/
       Productions
        Expr -> (Float | Int)             [Constant: num]
             |  Expr '+' Expr             [Plus: left, _, right]
             |  Expr '-' Expr             [Minus: left, _, right]
             |  Expr '*' Expr             [Mult: left, _, right]
             |  Expr '/' Expr             [Div: left, _, right]
             |  '(' Expr ')'              [^]
    EOG

    calc = Calculator.new
    ast = nil
    assert_no_exception {ast = p.parse "4 +3.1"}
    assert_equals(3, ast.childrens.length)
    assert_equals(4, ast.left.num.lexeme.to_i)
    assert_equals(3.1, ast.right.num.lexeme.to_f)
    assert_equals("+", ast[1].lexeme)
    ast.compact!
    assert_equals(2, ast.childrens.length)
    assert_equals(7.1, calc.eval(ast))

    assert_no_exception {ast = p.parse "4+(3*(7-1))"}
    ast.compact!
    assert_equals(22, calc.eval(ast))

    exception = false
    begin
      ast = p.parse "3+4*5"
    rescue AmbigousParseException => ex
      exception = true
      puts ex.inspect(PrettyPrinter.new)
    end
    assert(exception)
  end

  def test_unambiguated_calculator
    # Add some priorities to the Calculator grammar. This should resolve some
    # ambiguities. 
    p = Parse.generate_parser <<-'EOG'
      Grammar CalculatorExpressions
       Tokens
        Blank = /\s+/                     [:Skip]
        Int   = /\d+/
        Float = /\d+\.\d+/
       Productions
        Expr -> (Float | Int)             [Constant: num]
             |  Expr '+' Expr             [Plus: left, _, right]
             |  Expr '-' Expr             [Minus: left, _, right]
             |  Expr '*' Expr             [Mult: left, _, right]
             |  Expr '/' Expr             [Div: left, _, right]
             |  '(' Expr ')'              [^]
       Priorities
        left(Mult), left(Div), left(Plus), left(Minus)
        left(Plus, Minus), left(Div, Mult)
        Div = Mult > Plus = Minus
    EOG

    calc = Calculator.new
    ast = nil
    assert_no_exception {ast = p.parse "3+4*5"}
    assert_equals(23, calc.eval(ast))

    assert_no_exception {ast = p.parse "5+10/2"}
    assert_equals(10, calc.eval(ast))

    assert_no_exception {ast = p.parse "3+4+5"}
    assert_equals(12, calc.eval(ast))

    assert_no_exception {ast = p.parse "3-4+5"}
    assert_equals(4, calc.eval(ast))

    assert_no_exception {ast = p.parse "3+4-5"}
    assert_equals(2, calc.eval(ast))

    assert_no_exception {ast = p.parse "(3+4)*5"}
    assert_equals(35, calc.eval(ast))

    assert_no_exception {ast = p.parse "(3+4)*(5-2)"}
    assert_equals(21, calc.eval(ast))
  end

  def test_eelco_vissers_thesis_page_158
    p = Parse.generate_parser <<-'EOG'
      Grammar EelcoVissersThesisPage158
       Tokens
        Blank = /\s+/                     [:Skip]
        Int   = /\d+/
        Float = /\d+\.\d+/
       Productions
        E -> "(" E ")"     [^]
          |  "-" E         [UnaryMinus: _, e]
          |  E "^" E       [Exp: base, _, exponent]
          |  E "*" E       [Mult: left, _, right]
          |  E "+" E       [Plus: left, _, right]
          |  E "-" E       [Minus: left, _, right]
          |  (Int | Float) [Constant: num]
       Priorities
        right(Exp), left(Mult), left(Plus), left(Minus)
        left(Plus, Minus)
        UnaryMinus > Exp > Mult > Plus = Minus
    EOG

    ast = nil
    # Example from page 158 (with numbers inserted)
    assert_no_exception {ast = p.parse "1--2*3+4-5^2"}
    assert_equals(-14, Calculator.new.eval(ast))
  end
end

# Run if we were directly called
if $0 == __FILE__
  testrunner = RUNIT::CUI::TestRunner.new
  if ARGV.size == 0
    suite = AcceptanceTestMediumGrammarsInText.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(AcceptanceTestMediumGrammarsInText.new(testmethod))
    end
  end
  testrunner.run(suite)
end
