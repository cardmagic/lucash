require 'runit/testcase'
require 'runit/testsuite'
require 'runit/cui/testrunner'
require 'runit/topublic'
include RUNIT::ToPublic

require 'rockit/directed_graph'

class TestBackLinkedDirectedGraph < RUNIT::TestCase
  def setup
    @dg = BackLinkedDirectedGraph.new
    [[1,2],[2,3],[3,2],[2,4]].each_with_index do |ft,i| 
      @dg.add_link(ft[0],ft[1],i)
    end
  end

  def test_initialize
    assert_kind_of(BackLinkedDirectedGraph, @dg)
  end

  def test_back_transition
    assert_equals(1, @dg.back_transition(2, 0))
    assert_equals(2, @dg.back_transition(3, 1))
    assert_equals(3, @dg.back_transition(2, 2))
    assert_equals(2, @dg.back_transition(4, 3))
    assert_exception(GraphTraversalException) {@dg.back_transition(2,-1)}
  end

  def test_back_traverse
    assert_equals(1, @dg.back_traverse(4,[0,3]))
    assert_equals(1, @dg.back_traverse(3,[0,1]))
    assert_equals(1, @dg.back_traverse(2,[0,1,2]))
    assert_equals(1, @dg.back_traverse(4,a = [0,1,2,3]))
    assert_equals([0,1,2,3], a)
    assert_equals(4, @dg.back_traverse(4,[]))
    assert_exception(GraphTraversalException) {@dg.traverse(1,[0,1,2,-1])}
  end

  def test_incoming_links_info
    assert_equals([], @dg.incoming_links_info(1))
    assert_equals([0,2], @dg.incoming_links_info(2).sort)
    assert_equals([1], @dg.incoming_links_info(3).sort)
    assert_equals([3], @dg.incoming_links_info(4).sort)
  end
end

# Run if we were directly called
if $0 == __FILE__
  testrunner = RUNIT::CUI::TestRunner.new
  if ARGV.size == 0
    suite = TestBackLinkedDirectedGraph.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestBackLinkedDirectedGraph.new(testmethod))
    end
  end
  testrunner.run(suite)
end

