require 'runit/testcase'
require 'runit/testsuite'
require 'runit/cui/testrunner'
require 'runit/topublic'
include RUNIT::ToPublic

require 'rockit/syntax_tree'

class TestSyntaxTree < RUNIT::TestCase
  def test_arraynodebuilder_copy
    s = SyntaxTreeBuilder.new("Expression", [:left, :right])
    a = ArrayNodeBuilder.new([], nil, s, 0)
    a.inactivate_child(1)
    a2 = a.copy
    assert_equals(["left"], a2.active_childrens)
    assert_equals(["left"], a.active_childrens)
  end

  def test_syntaxtree_initialize
    t = SyntaxTree.new("TestNode", ["c1", "c2"], [1,2])
    assert_kind_of(SyntaxTree, t)
    assert_equals("TestNode", t.name)
    assert_kind_of(Hash, t.attributes)
    assert_equals(0, t.attributes.length)
    assert_equals([1,2], t.childrens)

    assert_exception(ArgumentError) {SyntaxTree.new("T", ["c1"], [1,2])}
    assert_exception(ArgumentError) {SyntaxTree.new("T", ["c1","c2"], [1])}
  end

  def test_syntaxtree_aref
    t = SyntaxTree.new("TestNode", ["c1", "c2", "c3"], [1,2,3])
    assert_equals(1, t[0])
    assert_equals(2, t[1])
    assert_equals(3, t[2])
    assert_equals(nil, t[3])
    assert_equals(1, t["c1"])
    assert_equals(2, t["c2"])
    assert_equals(3, t["c3"])
    assert_exception(ArgumentError) {t["c4"]}
  end

  def test_syntaxtree_method_access
    t = SyntaxTree.new("TestNode", ["c1", "c2", "c3"], [1,2,3])
    assert_equals(1, t.c1)
    assert_equals(2, t.c2)
    assert_equals(3, t.c3)
    exception = RUBY_VERSION >= "1.7" ? NoMethodError : NameError
    assert_exception(exception) {t.c4}
  end

  def test_syntaxtree_compact!
    t0 = SyntaxTree.new("TestNode", ["_", "c2", nil], [1,2,3])
    t = SyntaxTree.new("TestNode", ["_", "c2", "_"], [1,t0,2])
    t.compact!
    assert_equals(1, t.childrens.length)
    assert_equals(1, t.c2.childrens.length)
    assert_equals(t0, t.c2)
    assert_equals(2, t.c2.c2)
  end

  def test_syntaxtree_inspect
    t0 = SyntaxTree.new("TN0", ["_", "c2", nil], [1,2,3])
    t = SyntaxTree.new("TN1", ["_", "c1", "c2"], [1,t0,2]).compact!
    assert_equals("TN1:[TN0:[2],2]", t.inspect)
    #assert_equals("TN1\n c1: TN0\n  c2: 2\n c2: 2", t.inspect)
  end

  def test_syntaxtree_attributes
    t = SyntaxTree.new("TN1", ["c1", "c2", "c3"], [1,2,3])
    t.attributes[:position] = 10
    assert_equals(1, t.attributes.length)
    assert_equals(10, t.attributes[:position])
  end

  def test_liftingsyntaxtreebuilder_initialize
    stb = LiftingSyntaxTreeBuilder.new([:left, "right"])
    assert_kind_of(LiftingSyntaxTreeBuilder, stb)
    assert_equals("^", stb.node_name)
    assert_equals(["left", "right"], stb.active_childrens)

    stb = LiftingSyntaxTreeBuilder.new([:left, :op, "right"], [1])
    assert_kind_of(LiftingSyntaxTreeBuilder, stb)
    assert_equals("^", stb.node_name)
    assert_equals(["left", "right"], stb.active_childrens)
  end

  def test_liftingsyntaxtreebuilder_create_tree
    stb = LiftingSyntaxTreeBuilder.new(["_", "right"])
    t = stb.create_tree([1,2])
    assert_equals(2, t)
  end

  def test_stb_shorthand_creator
    b = stb("TN", ["c1", "c2"])
    assert_kind_of(SyntaxTreeBuilder, b)
    assert_equals(2, b.active_childrens.length)

    b = stb("TN", ["c1", "c2"], [1])
    assert_kind_of(SyntaxTreeBuilder, b)
    assert_equals(["c1"], b.active_childrens)

    b = stb("^", ["_", "c2"])
    assert_kind_of(LiftingSyntaxTreeBuilder, b)
    assert_equals(["_", "c2"], b.active_childrens)
    t = b.create_tree([10,20])
    assert_equals(20, t)

    b = stb(:^, [:_, :c2])
    assert_kind_of(LiftingSyntaxTreeBuilder, b)
    assert_equals(2, b.active_childrens.length)
    t = b.create_tree([10,20])
    assert_equals(20, t)
  end

  def test_arraynodebuilder
    anb = ArrayNodeBuilder.new([1,2])
    assert_kind_of(ArrayNodeBuilder, anb)
    t = anb.create_tree([0,1,2,3,4])
    assert_equals(1, t[0][0])
    assert_equals(2, t[0][1])

    anb = ArrayNodeBuilder.new([1,3],0)
    assert_kind_of(ArrayNodeBuilder, anb)
    an = ArrayNode.new([[0,1]])
    t = anb.create_tree([an,1,2,3,4])
    assert_equals(0, t[0][0])
    assert_equals(1, t[0][1])
    assert_equals(1, t[1][0])
    assert_equals(3, t[1][1])
    assert_equals(["c1", "c2"], t.children_names)
    assert_equals(1, an.length)

    a1 = ArrayNodeBuilder.new([1], 0)
    a2 = ArrayNodeBuilder.new([0])
    t = a1.create_tree([a2.create_tree([1]), 2])
    assert_equals(1, t[0])
    assert_equals(2, t[1])

    a1 = ArrayNodeBuilder.new([1,2], 0)
    a2 = ArrayNodeBuilder.new([0,1])
    t = a2.create_tree([1,2])
    assert_equals([1,2], t[0])
    assert_equals([[1,2],[3,4]], a1.create_tree([t, 3, 4]).as_a)

    a1 = ArrayNodeBuilder.new
    t = a1.create_tree([])
    assert_equals(0, t.length)
  end

  class IdStb < SyntaxTreeBuilder
    def create_tree(a)
      a
    end
  end

  def test_arraynodebuilder_chaining
    anb = ArrayNodeBuilder.new([1,3])
    s = IdStb.new("T", ["c1","c2","c3","c4","c5"])
    anb.chain_treebuilder(s, 0, [2])
    t = anb.create_tree([0,1,2,3])
    assert_equals(ArrayNode, t[0].type)
    assert_equals([1,3], t[0][0])
    assert_equals(1, t[1])
    assert_equals(3, t[2])

    anb = ArrayNodeBuilder.new([1,3])
    s = IdStb.new("T", ["c1","c2","c3","c4","c5"])
    anb.shifting_insert = true
    anb.chain_treebuilder(s, 0, [1,3])
    t = anb.create_tree([0,1,2,3])
    assert_equals(ArrayNode, t[0].type)
    assert_equals([1,3], t[0][0])
    assert_equals(1, t[1])
    assert_equals(3, t[2])

    anb = ArrayNodeBuilder.new([1,3])
    s = IdStb.new("T", ["c1","c2","c3","c4","c5"])
    anb.chain_treebuilder(s, 2, [1,3])
    t = anb.create_tree([0,1,2,3])
    assert_equals(ArrayNode, t[1].type)
    assert_equals([1,3], t[1][0])
    assert_equals(0, t[0])

    assert_equals(["c1","c2","c3","c4","c5"], anb.active_childrens)
    anb.inactivate_child("c2")
    assert_equals(["c1","c3","c4","c5"], anb.active_childrens)
    assert_equals("T", anb.node_name)
  end

  def test_groupingsyntaxtreebuilder
    stb = stb("TN", ["c0", "c1", "c2"])
    gstb = GroupingSyntaxTreeBuilder.new(1,2,stb)
    assert_kind_of(GroupingSyntaxTreeBuilder, gstb)
    t = gstb.create_tree([0,1,2,3])
    assert_equals(0, t.c0)
    assert_equals(3, t.c2)
    assert_equals([1,2], t.c1)
  end

  def test_liftingsyntaxtreebuilder_to_src
    stb = LiftingSyntaxTreeBuilder.new([:left, "right"])
    assert_equals(stb, eval(stb.to_src))

    stb = LiftingSyntaxTreeBuilder.new([:left, "right"])
    assert_equals(stb, eval(stb.to_src))
  end

  def test_arraynodebuilder_to_src
    anb = ArrayNodeBuilder.new([1,3],0)
    assert_equals(anb, eval(anb.to_src))

    anb2 = ArrayNodeBuilder.new([0],0, anb, 1, [])
    assert_equals(anb2, eval(anb2.to_src))

    anb3 = ArrayNodeBuilder.new([0],0, anb, 1, [])
    anb3.append_element = false
    assert_equals(anb3, eval(anb3.to_src))

    anb4 = ArrayNodeBuilder.new([0],0, anb, 1, [2,3], false)
    assert_equals(anb4, eval(anb4.to_src))
  end

  def test_groupedsyntaxtreebuilder
    stb = stb("TN", ["c0", "c1", "c2"])
    gstb = GroupingSyntaxTreeBuilder.new(1,2,stb)
    assert_kind_of(GroupingSyntaxTreeBuilder, gstb)

    gstb = GroupingSyntaxTreeBuilder.new(1,1,stb)
    assert_equals(stb, gstb)
  end

  def test_groupedsyntaxtreebuilder_to_src
    stb = stb("TN", ["c0", "c1", "c2"])
    gstb = GroupingSyntaxTreeBuilder.new(1,2,stb)
    assert_equals(gstb, eval(gstb.to_src))
  end

  def test_syntaxtree_node_visiting
    stb = stb("TN", ["c0"])
    stb2 = stb("TN", ["c0", "c1", "c2"])
    c1 = stb.create_tree([1])
    c2 = stb.create_tree([2])
    c3 = stb.create_tree([3])
    t = stb2.create_tree([c1,c2,c3])
    
    nodes = []
    t.each_depth_first {|treenode| nodes.push treenode}
    assert_equals(4, nodes.length)
    assert_equals([c1,c2,c3,t], nodes)

    nodes = []
    t.each_breadth_first {|treenode| nodes.push treenode}
    assert_equals(4, nodes.length)
    assert_equals([t,c1,c2,c3], nodes)
  end

  def substring_in_tree(tree, fullString, &getPositions)
    min, max = fullString.length, -1
    tree.each do |node| 
      begin
	start, stop = getPositions.call(node)
	min = start if start < min
	max = stop if stop > max
      rescue Exception
      end
    end
    fullString[min..max]
  end

  def not_yet_test_syntaxtree_substring_in_tree
    string = "other stuff not in the substring 1 + 2 more stuff not in there"
    i1 = SyntaxTree.new("i1", ["val", "lexeme"], [1, "1"])
    i1.attributes[:position] = 33
    i2 = SyntaxTree.new("i2", ["val", "lexeme"], [2, "2"])
    i2.attributes[:position] = 37
    pl = SyntaxTree.new("Plus", ["lexeme"], ["+"])
    pl.attributes[:position] = 35
    t = SyntaxTree.new("E", [:c1, :c2, :c3], [i1, pl, i2])
    assert_equals("1+2", substring_in_tree(t, string) {|n| s = n.attributes[:position], s + n.lexeme.length - 1})
  end

  def test_chained_arraynodebuilders_bug
    anb = ArrayNodeBuilder.new([],nil,inner_anb = ArrayNodeBuilder.new([0],1, inner = LiftingSyntaxTreeBuilder.new(["statements", "_"],[]),0,[1],true),2,[],true)
    assert_equals(inner.node_name, inner_anb.node_name)
    assert_equals(inner.node_name, anb.node_name)
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

