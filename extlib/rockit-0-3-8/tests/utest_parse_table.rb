require 'runit/testcase'
require 'runit/testsuite'
require 'runit/cui/testrunner'
require 'runit/topublic'
include RUNIT::ToPublic

require 'rockit/parse_table' 

PublicParseTable = to_public(ParseTable)

class TestParseTable < RUNIT::TestCase
  def setup
    @ps = [
      Production.new(:E, [:N, ".", :N], stb("E", [:left, :_, :right])),
      Production.new(:E, [:N], stb("E", [:left, :_, :right], [:right]))
    ]
    @ts = [
      Token.new("Int", /\d+/),
      Token.new("Float", /\d+\.\d+/)
    ]
    @ns = @ps.map {|p| p.nonterminal}.uniq
    @pt = ParseTable.new(@ps, @ts) 
    @ppt = PublicParseTable.new(@ps, @ts) 
  end

  def test_initialize
    assert_kind_of(ParseTable, @pt)
    assert_equals(@ts, @pt.tokens) # Works since @ts has been updated!
    assert_equals(0, @pt.start_state)
    assert_equals("UNNAMED_LANGUAGE", @pt.language)
  end

  def test_new_from_grammar
    g = Grammar.new("TestGrammar", @ps, @ts)
    pt = ParseTable.new_from_grammar(g)
    assert_kind_of(ParseTable, pt)
    assert_equals(0, pt.start_state)
    assert_equals("TestGrammar", pt.language)
    assert(@pt != pt) # Since stringtoken "." has been added to tokens
    assert(pt.priorities.respond_to?("conflict?"))
  end

  def test_production
    @ps.each_with_index {|p,i| assert_equals(p, @pt.production(i))}
  end

  def test_to_src
    assert_equals(@pt, eval(@pt.to_src))
  end

  def test_add_action
    @pt.add_action(0, @ts[0], a1 = [:SHIFT, 2])
    assert_equals([a1], @pt.actions(0, @ts[0]))
    @pt.add_action(0, @ts[0], a2 = [:REDUCE, 1])
    assert_equals([a1,a2], @pt.actions(0, @ts[0]))
    @pt.add_action(0, @ts[1], a3 = [:REDUCE, 3])
    assert_equals([a3], @pt.actions(0, @ts[1]))
  end

  def test_num_states
    @pt.add_action(0, @ts[0], a2 = [:REDUCE, 1])
    assert_equals(1, @pt.num_states)
    @pt.add_action(1, @ts[0], [:REDUCE, 1])
    @pt.add_action(1, @ts[0], [:SHIFT, 1])
    assert_equals(2, @pt.num_states)
    @pt.add_action(2, @ts[0], [:SHIFT, 1])
    assert_equals(3, @pt.num_states)
  end

  def test_add_goto
    @pt.add_goto(0, @ps[0].nonterminal, 1)
    assert_equals(1, @pt.goto(0, 0))
  end

  def test_goto_bug
    assert_no_exception {@pt.goto(0,0)}
  end

  def test_nonterminals_with_equal_name
    # Two different NonTerminals with the same name will be created below
    pt = ParseTable.new([p1 = prod(:S, [:L]), p2 = prod(:S, [:R])], [])
    # Make sure only one of them makes it into the terminals
    assert_equals(1, pt.instance_eval("@nonterminals").length)

    # Add some gotos and try it out
    pt.add_goto(0, p2.nonterminal, 2)
    assert_equals(2, pt.goto(0, 0))
    assert_equals(2, pt.goto(0, 1))
    pt.add_goto(0, p1.nonterminal, 1)
    assert_equals(1, pt.goto(0, 0))
    assert_equals(1, pt.goto(0, 1))
  end

  def test_compact!
    @pt.add_action(0, @ts[0], a1 = [:SHIFT, 2])
    @pt.add_action(0, @ts[0], a1 = [:SHIFT, 2])
    @pt.compact!
    assert_equals([[:SHIFT, 2]], @pt.actions(0, @ts[0]))
  end

  def test_actionnum_to_action
    assert_equals([:REDUCE, 10], @ppt.actionnum_to_action((10 << 2) + 0))
    assert_equals([:SHIFT, 20], @ppt.actionnum_to_action((20 << 2) + 1))
    assert_equals([:ACCEPT, 30], @ppt.actionnum_to_action((30 << 2) + 2))
  end

  def test_action_to_actionnum
    assert_equals((10 << 2) + 0, @ppt.action_to_actionnum([:REDUCE, 10]))
    assert_equals((20 << 2) + 1, @ppt.action_to_actionnum([:SHIFT, 20]))
    assert_equals((30 << 2) + 2, @ppt.action_to_actionnum([:ACCEPT, 30]))
  end

  def test_valid_tokens
    @pt.add_action(0, @ts[0], a1 = [:SHIFT, 2])
    @pt.add_action(0, @ts[0], a2 = [:REDUCE, 1])
    assert_equals([@ts[0]], @pt.valid_tokens(0))
  end
end

# Run if we were directly called
if $0 == __FILE__
  testrunner = RUNIT::CUI::TestRunner.new
  if ARGV.size == 0
    suite = TestParseTable.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestParseTable.new(testmethod))
    end
  end
  testrunner.run(suite)
end

