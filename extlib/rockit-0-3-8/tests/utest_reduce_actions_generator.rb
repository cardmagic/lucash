require 'runit/testcase'
require 'runit/testsuite'
require 'runit/cui/testrunner'
require 'runit/topublic'
include RUNIT::ToPublic

require 'rockit/reduce_actions_generator'
require 'tests/utest_lalr_parsetable_generator'

def gen_state_graph_parsetable_and_items(grammar)
  gen = PubGen.new(grammar, FakeL)
  gen.generate_parse_table(pt = FakePT.new) 
  l = FakeL.instance
  return l.state_graph, pt, gen.instance_eval("@item_factory").instances
end

class TestReduceActionsGenerator < RUNIT::TestCase
  def setup
    @tokens = [
      blank = t("Blank", /\s+/, :Skip),
      @id = t("Id", /[A-Za-z]+/)
    ]
    l = NonTerminal.new("L")
    r = NonTerminal.new("R")
    s = NonTerminal.new("S")
    @prods = [
      prod(s, [l, "=", r]),
      prod(s, [r]),
      prod(l, ["*", r]),
      prod(l, [@id]),
      prod(r, [l])
    ]
    @g = Grammar.new("Dragon_book_4_46", @prods, @tokens)
    @sg, @pt, @items = gen_state_graph_parsetable_and_items(@g)
    @rag = ReduceActionsGenerator.new(@sg, @g, @pt, @items)
  end

  def test_initialize
    assert_kind_of(ReduceActionsGenerator, @rag)
  end

  def test_add_reduce_actions
    @rag.add_reduce_actions
    as = @pt.actions
    assert_equals(7, as.select {|a| a[0][0] == :SHIFT}.length)
    assert_equals(7, as.select {|a| a[0][0] == :GOTO}.length)
    assert_equals(1, as.select {|a| a[0][0] == :ACCEPT}.length)
    assert_equals(9, as.select {|a| a[0][0] == :REDUCE}.length)
    assert_equals(24, as.length)
  end

  def test_dragonbook_grammar_4_21
    @g4_21 = Grammar.new("Dragon_book_4_21", [
			   prod(:S, [:C,:C]),
			   prod(:C, ["c", :C]),
			   prod(:C, ["d"])
			 ])
    @sg4_21, @pt4_21, @i4_21 = gen_state_graph_parsetable_and_items(@g4_21)
    @l4_21 = ReduceActionsGenerator.new(@sg4_21, @g4_21, @pt4_21, @i4_21)
    @l4_21.add_reduce_actions
    as = @pt4_21.actions
    assert_equals(6, as.select {|a| a[0][0] == :SHIFT}.length)
    assert_equals(4, as.select {|a| a[0][0] == :GOTO}.length)
    assert_equals(1, as.select {|a| a[0][0] == :ACCEPT}.length)
    assert_equals(7, as.select {|a| a[0][0] == :REDUCE}.length)
    assert_equals(18, as.length)
  end
end

# Run if we were directly called
if $0 == __FILE__
  testrunner = RUNIT::CUI::TestRunner.new
  if ARGV.size == 0
    suite = TestReduceActionsGenerator.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestReduceActionsGenerator.new(testmethod))
    end
  end
  testrunner.run(suite)
end

