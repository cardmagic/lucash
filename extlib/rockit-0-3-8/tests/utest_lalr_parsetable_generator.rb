require 'runit/testcase'
require 'runit/testsuite'
require 'runit/cui/testrunner'
require 'runit/topublic'
include RUNIT::ToPublic

require 'rockit/lalr_parsetable_generator'
include Parse

PubGen = RUNIT::ToPublic.to_public(LaLr1ParseTableGenerator)

class FakePT
  attr_reader :actions
  def initialize
    @actions = Array.new
  end
  def new_from_grammar(*args)
    self
  end
  def add_shift(src, symbol, dest)
      @actions.push [:SHIFT, src, symbol, dest]
  end
  def add_goto(src, symbol, dest)
    @actions.push [[:GOTO], src, symbol, dest]
  end
  def add_action(src, terminal, action)
    t = [action, src, terminal]
    @actions.push t unless @actions.include?(t)
  end
  def add_action_for_terminalset(src, action, terminalSet)
    terminalSet.each {|t| add_action(src, t, action)}
  end
  def compact!
  end
end

class FakeL
  attr_reader :state_graph, :parse_table
  @@instance = nil
  def initialize(stateGraph, *args)
    @state_graph = stateGraph
  end
  def add_reduce_actions
    @@instance = self
  end
  def FakeL.instance
    @@instance
  end
end

def gen_state_graph_and_parsetable(grammar)
  gen = PubGen.new(grammar, FakeL)
  gen.generate_parse_table(pt = FakePT.new) 
  l = FakeL.instance
  return l.state_graph, pt
end

class TestLaLr1ParsetableGenerator < RUNIT::TestCase
  def setup
    # Symbols common to several of the example grammars
    @t_id = StringToken.new("id")
    @t_mult = StringToken.new("mult", "*")
    @t_eq = StringToken.new("eq", "=")
    @t_plus = StringToken.new("plus", "+")
    @t_lpar = StringToken.new("lpar", "(")
    @t_rpar = StringToken.new("rpar", ")")
    @t_c = StringToken.new("c", "c")
    @t_d = StringToken.new("d", "d")

    @nt_s = NonTerminal.new("S")
    @nt_sp = NonTerminal.new("S'")
    @nt_l = NonTerminal.new("L")
    @nt_r = NonTerminal.new("R")
    @nt_e = NonTerminal.new("E")
    @nt_ep = NonTerminal.new("E'")
    @nt_t = NonTerminal.new("T")
    @nt_tp = NonTerminal.new("T'")
    @nt_f = NonTerminal.new("F")
    @nt_c = NonTerminal.new("C")

    # Setup the grammar (w/o augmentation) from example 4.46 in the Dragon book
    @g1prods = [
      Production.new(@nt_s, [@nt_l, @t_eq, @nt_r]),
      Production.new(@nt_s, [@nt_r]),
      Production.new(@nt_l, [@t_mult, @nt_r]),
      Production.new(@nt_l, [@t_id]),
      Production.new(@nt_r, [@nt_l])]
    @g1 = Grammar.new("Example4_46_Dragon_book", @g1prods, 
		      [@t_id,@t_mult,@t_eq])

    # Setup the grammar from example 4.11 in the Dragon book
    @g2 = Grammar.new("Example4_11_Dragon_book", [], 
		      [@t_id,@t_plus,@t_mult,@t_lpar,@t_rpar])
    @g2.add_production(sp = Production.new(@nt_e, [@nt_t, @nt_ep]))
    @g2.add_production Production.new(@nt_ep, [@t_plus, @nt_t, @nt_ep])
    @g2.add_production Production.new(@nt_ep, [Grammar.epsilon])
    @g2.add_production Production.new(@nt_t, [@nt_f, @nt_tp])
    @g2.add_production Production.new(@nt_tp, [@t_mult, @nt_f, @nt_tp])
    @g2.add_production Production.new(@nt_tp, [Grammar.epsilon])
    @g2.add_production Production.new(@nt_f, [@t_lpar, @nt_e, @t_rpar])
    @g2.add_production Production.new(@nt_f, [@t_id])

    # Setup grammar from example 4.34
    @g3prods = [
      Production.new(@nt_e, [@nt_e,@t_plus,@nt_t]),
      Production.new(@nt_e, [@nt_t]),
      Production.new(@nt_t, [@nt_t,@t_mult,@nt_f]),
      Production.new(@nt_t, [@nt_f]),
      Production.new(@nt_f, [@t_lpar,@nt_e,@t_rpar]),
      Production.new(@nt_f, [@t_id])]
    @g3 = Grammar.new("Example4_34_Dragon_book", @g3prods, 
		      [@t_plus,@t_mult,@t_lpar,@t_rpar,@t_id])

    # Setup grammar from example 4.42
    @g4prods = [
      Production.new(@nt_s, [@nt_c, @nt_c]),
      Production.new(@nt_c, [@t_c, @nt_c]),
      Production.new(@nt_c, [@t_d])]
    @g4 = Grammar.new("Example_4_42_Dragon_book", @g4prods, [@t_c, @t_d])
  end

  def test_initialize
    t1 = LaLr1ParseTableGenerator.new(@g1)
    assert_kind_of(LaLr1ParseTableGenerator, t1)
  end

  def test_calculate_state_graph
    state_graph, pt = gen_state_graph_and_parsetable(@g1)

    assert_kind_of(DirectedGraph, state_graph)
    assert_equals(10, state_graph.nodes.length)
    assert_equals(14, state_graph.links.length)
    assert_equals(6, state_graph.leafs.length)
    assert_equals(1, state_graph.roots.length)

    # Number of outgoing edges
    outgoing_counts = Array.new
    state_graph.nodes.each {|n| 
      outgoing_counts.push state_graph.links_from(n).length
    }
    assert_equals([5,0,1,0,4,0,4,0,0,0].sort, outgoing_counts.sort)

    # One action for each link
    assert_equals(14, pt.actions.length)
  end
end

# Run if we were directly called
if $0 == __FILE__
  testrunner = RUNIT::CUI::TestRunner.new
  if ARGV.size == 0
    suite = TestLaLr1ParsetableGenerator.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestLaLr1ParsetableGenerator.new(testmethod))
    end
  end
  testrunner.run(suite)
end

