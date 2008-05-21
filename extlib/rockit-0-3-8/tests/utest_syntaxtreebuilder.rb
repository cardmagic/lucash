require 'runit/testcase'
require 'runit/testsuite'
require 'runit/cui/testrunner'
require 'runit/topublic'
include RUNIT::ToPublic

require 'rockit/syntax_tree'

class TestSyntaxTree < RUNIT::TestCase
  def test_initialize
    stb = SyntaxTreeBuilder.new("Expression", ["left", "right"])
    assert_kind_of(SyntaxTreeBuilder, stb)
    assert_equals("Expression", stb.node_name)
    assert_equals(["left", "right"].sort, stb.active_childrens.sort)
    assert_equals(["left", "right"].sort, stb.children_names.sort)
  end

  def test_initialize_w_inactive
    stb = SyntaxTreeBuilder.new(:Expression, ["left", "op", :right], [1])
    assert_kind_of(SyntaxTreeBuilder, stb)
    assert_equals("Expression", stb.node_name)
    assert_equals(["left", "right"], stb.active_childrens)
    assert_equals(["left", "op", "right"].sort, stb.children_names.sort)
  end

  def test_initialize_duplicate_childrennames
    assert_exception(ArgumentError) {
      SyntaxTreeBuilder.new(:E, [:left, :left])
    }
    assert_no_exception {
      SyntaxTreeBuilder.new(:E, [:_, "_"])
    }
  end

  def test_initialize_w_semi_inactive_children
    stb = SyntaxTreeBuilder.new(:Expression, ["_", "op", :right], [1])
    assert_equals(["_", "right"].sort, stb.active_childrens.sort)

    stb = SyntaxTreeBuilder.new(:Expression, ["_", "op", "_"])
    assert_equals(["_", "op", "_"], stb.active_childrens)
  end

  def test_equality
    stb1 = SyntaxTreeBuilder.new(:Expression, ["_", "op", :right], [1])
    stb2 = SyntaxTreeBuilder.new(:Expression, ["_", "op", :right], [1])
    assert(stb1 == stb2)
    stb1.inactivate_child("right")
    stb2.inactivate_child("right")
    assert(stb1 == stb2)
  end

  def test_copy
    s = SyntaxTreeBuilder.new("Expression", [:left, :right])
    s2 = s.copy
    s2.inactivate_child("right")
    assert_equals(["left"], s2.active_childrens)
    assert_equals(["left", "right"], s.active_childrens)

    s = SyntaxTreeBuilder.new("Expression", [:left, :right, :d, :e], [2])
    s2 = s.copy
    s2.inactivate_child("right")
    assert_equals(["left", "e"].sort, s2.active_childrens.sort)
    assert_equals(["left", "right", "e"].sort, s.active_childrens.sort)
  end

  def test_activation_and_inactivation
    stb = SyntaxTreeBuilder.new("Expression", ["left", "op", "right"])
    assert_equals(["left", "op", "right"].sort, stb.active_childrens.sort)

    stb.inactivate_child("op")
    assert_equals(["left", "right"].sort, stb.active_childrens.sort)

    stb.inactivate_child("left")
    assert_equals(["right"], stb.active_childrens)

    stb.activate_child("left")
    assert_equals(["left", "right"].sort, stb.active_childrens.sort)

    stb.activate_child("op")
    assert_equals(["left", "op", "right"].sort, stb.active_childrens.sort)

    stb.inactivate_child(1)
    assert_equals(["left", "right"].sort, stb.active_childrens.sort)

    stb.inactivate_child(1)
    assert_equals(["left"], stb.active_childrens)

    stb = SyntaxTreeBuilder.new("Expression", ["left", "op", "right"], [1])
    stb.inactivate_child(1)
    assert_equals(["left"], stb.active_childrens)
  end

  def test_to_src
    stb = SyntaxTreeBuilder.new("Expression", ["left", "right"])
    assert_equals(stb, eval(stb.to_src))

    stb = SyntaxTreeBuilder.new("Expression", [:left, "op", "right"], [1])
    assert_equals(stb, eval(stb.to_src))
  end

  def test_create_tree
    stb = SyntaxTreeBuilder.new("Expression", ["left", "op", "right"], [1])
    t = stb.create_tree([1,2])
    assert_kind_of(SyntaxTree, t)
    assert_equals(3, t.childrens.length)
    assert_equals([1,nil,2], t.childrens)
    assert_equals(1, t.left)
    assert_equals(2, t.right)
    assert_equals(nil, t.op)
  end
end

# Run if we were directly called
if $0 == __FILE__
  testrunner = RUNIT::CUI::TestRunner.new
  if ARGV.size == 0
    suite = TestSyntaxTree.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestSyntaxTree.new(testmethod))
    end
  end
  testrunner.run(suite)
end

