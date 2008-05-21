require 'runit/testcase'
require 'runit/testsuite'
require 'runit/cui/testrunner'
require 'runit/topublic'
include RUNIT::ToPublic

require 'rockit/glr_parser' 
require 'rockit/lalr_parsetable_generator'
include Parse

class TestToken < RUNIT::TestCase
  def setup
    @tokens = [
      @tc = StringToken.new("c"),
      @td = StringToken.new("d")
    ]
    @prods = [
      Production.new(:S, [:C, :C], stb("Pair", [:left, :right])),
      Production.new(:C, [@tc, :C], stb("C", [:leaf, :more])),
      Production.new(:C, [@td], stb("D", [:leaf]))
    ]
    @g = Grammar.new("Example_4_42_Dragon_book", @prods, @tokens)
    lptg = LaLr1ParseTableGenerator.new(@g)
    @pt = lptg.generate_parse_table
    @p = GeneralizedLrParser.new(@pt)
  end

  def test_01_parser
    assert_kind_of(GeneralizedLrParser, @p)
  end

  def test_02_parse
    ast = @p.parse("ccdcd")
    assert_kind_of(SyntaxTree, ast)

    ast = @p.parse("cccccccccccccccccdcccccccccccd")
    assert_kind_of(SyntaxTree, ast)
  end

  def test_only_implicit_tokens
    g = Grammar.new("TG", [prod(:S, ["a"])])
    pt = LaLr1ParseTableGenerator.new(g).generate_parse_table
    p = GeneralizedLrParser.new(pt)
    assert_kind_of(GeneralizedLrParser, p)
    ast = p.parse "a"
    assert_kind_of(SyntaxTree, ast)
  end
end

# Run if we were directly called
if $0 == __FILE__
  testrunner = RUNIT::CUI::TestRunner.new
  if ARGV.size == 0
    suite = TestToken.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestToken.new(testmethod))
    end
  end
  testrunner.run(suite)
end

