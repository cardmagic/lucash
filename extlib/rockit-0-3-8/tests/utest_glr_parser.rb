require 'runit/testcase'
require 'runit/testsuite'
require 'runit/cui/testrunner'
require 'runit/topublic'
include RUNIT::ToPublic

require 'rockit/glr_parser'
require 'rockit/conflict_resolution'
require 'rockit/token'

# Some fake classes for testing
class FakeParseData
  attr_reader :start_state, :priorities
  def initialize(actionTable, gotoTable, productions, terminals, nonterminals)
    @action_table, @goto_table = actionTable, gotoTable
    @productions = productions
    @start_state = 0
    @nonterminal_index = nonterminals
    @terminal_index = Hash.new
    @priorities = ProductionPriorities.new
    terminals.push :EOF
    terminals.each_index {|i| @terminal_index[terminals[i]] = i}
  end
  def actions(state, tokenType)
    @action_table[state][@terminal_index[tokenType]]
  end
  def goto(state, productionNumber)
    @goto_table[state][@nonterminal_index[productionNumber]]
  end
  def production(number)
    @productions[number-1]
  end
end

class TreeNode
  attr_reader :name
  def initialize(name, children)
    @name, @children = name, children
  end
  def [](index)
    @children[index]
  end
  def each_node
  end
end

class FakeProductions
  attr_reader :length
  def initialize(name, rightHandSide)
    @name, @length = name, rightHandSide.length
  end
  def create_tree(childTrees)
    TreeNode.new(@name, childTrees)
  end
end

class FakeLexer
  attr_accessor :hash

  def initialize(hash, position = 0)
    @hash, @position = hash, position
  end
  def init(s)
    self
  end
  def peek
    v = @hash[@position]
    v = v.map do |s,t,lnum| 
      LexerToken.new(s,t,(lnum ? FakeLexer.new(@hash,lnum) : nil))
    end if v.kind_of?(Array)
    v
  end
  def inspect
    "L(#{@position})"
  end
end

# Short hands for defining action tables and productions
def r(productionNumber); [:REDUCE, productionNumber]; end
def s(state); [:SHIFT, state]; end
def a; [:ACCEPT]; end
def p(name, len); FakeProductions.new(name, len); end

class TestGlrParser < RUNIT::TestCase
  def setup
    @it = t("Int", /\d+/)
    @ft = t("Float", /\d+\.\d+/)
    @dt = t("Dot", /\./)
  end    
  
  def test_01_multiple_token_streams
    # Fake setup corresponding to grammar:
    #  Tokens
    #   Int   = /\d+/
    #   Float = /\d\.\d+/
    #   Dot   = /\./
    #  Productions
    #   E -> N Dot
    #   N -> Int
    #     |  Float
    #
    action_table =
      [[[   ],  [s(3)], [s(4)], [   ]],
       [[s(5)], [   ],  [   ],  [   ]],
       [[   ],  [   ],  [   ],  [a]  ],
       [[r(2)], [   ],  [   ], [r(2)]],
       [[r(3)], [   ],  [   ], [r(3)]],
       [[   ],  [   ],  [   ], [r(1)]]]
    goto_table = [[1,2],[nil,nil],[nil,nil],[nil,nil],[nil,nil],[nil,nil]]
    productions = [p("E", ["N", "Dot"]), p("N", ["Int"]), p("N", ["Float"])]
    pd = FakeParseData.new(action_table, goto_table, productions,
			   [@dt,@it,@ft], {1=>1, 2=>0, 3=>0} )

    # Fake ForkingLexer corresponding to the input "1.23." which can be
    # scanned in two different ways:
    #
    #   Int("1"), Dot, Int("23"), Dot
    #   Float("1.23"), Dot
    #
    # and only the latter is valid according to the grammar. To handle this
    # in a LALR(1) system based on traditional lexers we would have to specify
    # that the "prefer longest match" rule should be used in the lexer. Thus
    # we would loose flexibility and expressiveness. Now, we can list tokens
    # in any order => we can add tokens from different grammar modules
    # without worrying about the correctness. However, there might be 
    # performance issues, ie. the parse speed depends on the order of the
    # tokens.
    #
    l = FakeLexer.new({0 => [["1", @it, 1], ["1.23", @ft, 2]],
			1 => [[".", @dt, 3]], 2 => [[".", @dt, 4]],
			3 => [["23", @it, 5]], 4 => [[nil, :EOF, nil]], 
			5 => [[".", @dt, 4]]})
    ifp = GeneralizedLrParser.new(pd, l)
    ast = ifp.parse "1.23."
    assert_equals(TreeNode, ast.type)
    assert_equals("E", ast.name)
    assert_equals(TreeNode, ast[0].type)
    assert_equals("N", ast[0].name)
    assert_equals(SyntaxTree, ast[1].type)
    assert_equals("Dot", ast[1].name)
    assert_equals(SyntaxTree, ast[0][0].type)
    assert_equals("Float", ast[0][0].name)
    assert_equals("1.23", ast[0][0].value)

    l.hash = {0 => [["1", @it, 1]], 1 => [[".", @dt, 2]], 2 => [[nil, :EOF, nil]]}
    ast = ifp.parse "1."
    assert_equals(TreeNode, ast.type)
    assert_equals("E", ast.name)
    assert_equals(TreeNode, ast[0].type)
    assert_equals("N", ast[0].name)
    assert_equals(SyntaxTree, ast[1].type)
    assert_equals("Dot", ast[1].name)
    assert_equals(SyntaxTree, ast[0][0].type)
    assert_equals("Int", ast[0][0].name)
    assert_equals("1", ast[0][0].value)
  end
end



# Run if we were directly called
if $0 == __FILE__
  testrunner = RUNIT::CUI::TestRunner.new
  if ARGV.size == 0
    suite = TestGlrParser.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestGlrParser.new(testmethod))
    end
  end
  testrunner.run(suite)
end

