require 'runit/testcase'
require 'runit/testsuite'
require 'runit/cui/testrunner'
require 'runit/topublic'
include RUNIT::ToPublic

require 'rockit/parsetable_generation'

class TestLrState < RUNIT::TestCase
  def setup
    @t_id = Token.new("id", /\w+/)
    @g1prods = [
      @p0 = prod("S'", [:S]),
      p1 = prod(:S, [:L, "=", :R]),
      p2 = prod(:S, [:R]),
      p3 = prod(:L, ["*", :R]),
      p4 = prod(:L, [@t_id]),
      p5 = prod(:R, [:L]),
    ]
    @g1 = Grammar.new("Example4_46_Dragon_book", @g1prods, 
		      [@t_id])
    @if = IndexableFactory.new(Item, 0)
    @g1prods.each {|p| p.elements.each {|e| 
	e.calc_nonkernel_items(@g1,@if) if e.nonterminal?
      }
    }
    @s = LrState.new([Item.new(@p0,0)])
    @s2 = LrState.new([Item.new(p1,0)])
    @s3 = LrState.new([Item.new(@p0, 1)])
    @s4 = LrState.new([Item.new(p1, 3), Item.new(p2,0)])
  end

  def test_initialize
    assert_kind_of(LrState, @s)
  end

  def test_closure
    assert_equals(6, @s.closure.length)
    assert_equals(3, @s2.closure.length)
  end

  def test_closure_bug
    g = Grammar.new("T", [p0 = prod(:S, [".", mult(/\d+/)])])
    g.augment; g.normalize; ifactory = IndexableFactory.new(Item, 0)
    g.nonterminals.each {|nt| nt.calc_nonkernel_items(g, ifactory)}
    i0, state = ifactory.make(g.productions[0],0), nil
    assert_no_exception {state = LrState.new([i0])}

    dest_sets = DefaultInitHash.new {|k| Array.new}
    state.closure.each do |item|
      s = item.symbol
      dest_sets[s].push(item.next_item) if s and item.next_item
    end
    dest_sets.each do |symbol, kernel_item_set|
      assert_no_exception {state = LrState.new(kernel_item_set)}
    end
  end

  def test_reduce_state?
    assert_equals(false, @s.reduce_state?)
    assert_equals(false, @s2.reduce_state?)
    assert_equals(true, @s3.reduce_state?)
    assert_equals(true, @s4.reduce_state?)
  end

  def test_final_items
    assert_equals([], @s.final_items)
    assert_equals([], @s2.final_items)
    assert_equals(1, @s3.final_items.length)
    assert_equals(1, @s4.final_items.length)
  end

  def test_consistent?
    assert_equals(true, @s.consistent?)
    assert_equals(true, @s2.consistent?)
    assert_equals(true, @s3.consistent?)
    assert_equals(false, @s4.consistent?)
  end
end

# Run if we were directly called
if $0 == __FILE__
  testrunner = RUNIT::CUI::TestRunner.new
  if ARGV.size == 0
    suite = TestLrState.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestLrState.new(testmethod))
    end
  end
  testrunner.run(suite)
end

