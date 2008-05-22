require 'runit/testcase'
require 'runit/testsuite'
require 'runit/cui/testrunner'
require 'runit/topublic'
include RUNIT::ToPublic

require 'rockit/conflict_resolution' 

class FakeProduction
  include SourceCodeDumpable
  attr_reader :elements, :name
  
  def initialize(name, length)
    @name, @elements = name, (0...length).to_a
  end

  def length
    @elements.length
  end

  def ==(other)
    other.type == self.type and
      other.name == self.name and
      other.elements == self.elements
  end

  def inspect
    "#{@name}"
  end

  def to_src(name = nil, nameHash = {})
    assign_to(name, 
	      new_of_my_type(@name, @elements.length))
  end
end

PublicProdPrio = RUNIT::ToPublic.to_public(ProductionPriorities)

class TestConflictResolution < RUNIT::TestCase
  def setup
    # Faking grammar:
    # Grammar Expressions
    #
    #  Productions
    #   E -> "-" E                [UnaryMinus]
    #     |  E "^" E     :right:  [Exp]
    #     |  E "*" E     :left:   [Mult]
    #     |  E "+" E     :left:   [Plus]
    #     |  E "-" E     :left:   [Minus]
    #  Priorities
    #   left(Plus, Minus)
    #   UnaryMinus > Exp > Mult > Plus == Minus
    #
    @um = FakeProduction.new("UnaryMinus", 2)
    @e = FakeProduction.new("Exp", 3)
    @mu = FakeProduction.new("Mult", 3)
    @pl = FakeProduction.new("Plus", 3)
    @mi = FakeProduction.new("Minus", 3)
    
    @p = PublicProdPrio.new([Relation.new(@pl, :LEFT, @pl),
			      Relation.new(@mu, :LEFT, @mu),
			      Relation.new(@mi, :LEFT, @mi),
			      Relation.new(@e, :RIGHT, @e),
			      Relation.new(@pl, :LEFT, @mi),
			      Relation.new(@um, :HIGHER, @e),
			      Relation.new(@e, :HIGHER, @mu),
			      Relation.new(@mu, :HIGHER, @pl),
			      Relation.new(@pl, :EQUAL, @mi),
			    ])

    @dp = FakeProduction.new("Dummy", 3)
  end

  def test_00_initialize
    assert_kind_of(ProductionPriorities, ProductionPriorities.new)
    
    assert_kind_of(PublicProdPrio, @p)
  end

  def test_01_precedence_conflict?
    # UnaryMinus dominates all
    assert_equals(true, @p.precedence_conflict?(@e, @um))
    assert_equals(true, @p.precedence_conflict?(@mu, @um))
    assert_equals(true, @p.precedence_conflict?(@pl, @um))
    assert_equals(true, @p.precedence_conflict?(@mi, @um))

    # Exp dominates all but UnaryMinus
    assert_equals(false, @p.precedence_conflict?(@um, @e))
    assert_equals(true, @p.precedence_conflict?(@mu, @e))
    assert_equals(true, @p.precedence_conflict?(@pl, @e))
    assert_equals(true, @p.precedence_conflict?(@mi, @e))

    # Mult dominates Plus and Minus
    assert_equals(false, @p.precedence_conflict?(@um, @mu))
    assert_equals(false, @p.precedence_conflict?(@e, @mu))
    assert_equals(true, @p.precedence_conflict?(@pl, @mu))
    assert_equals(true, @p.precedence_conflict?(@mi, @mu))
    
    # Plus dominates nothing
    assert_equals(false, @p.precedence_conflict?(@um, @pl))
    assert_equals(false, @p.precedence_conflict?(@e, @pl))
    assert_equals(false, @p.precedence_conflict?(@mu, @pl))
    assert_equals(false, @p.precedence_conflict?(@mi, @pl))

    # Minus dominates nothing
    assert_equals(false, @p.precedence_conflict?(@um, @mi))
    assert_equals(false, @p.precedence_conflict?(@e, @mi))
    assert_equals(false, @p.precedence_conflict?(@mu, @mi))
    assert_equals(false, @p.precedence_conflict?(@pl, @mi))
  end

  def test_set_precedence_lower
    @p.set_precedence(@pl, @dp, :LOWER)
    assert_equals(false, @p.higher_precedence?(@pl, @dp))
    assert_equals(true, @p.higher_precedence?(@dp, @pl))
    assert_equals(false, @p.higher_precedence?(@mi, @dp))
    assert_equals(true, @p.higher_precedence?(@dp, @mi))

    assert_equals(false, @p.higher_precedence?(@dp, @mu))
    assert_equals(false, @p.higher_precedence?(@dp, @e))
    assert_equals(false, @p.higher_precedence?(@dp, @um))
    assert_equals(false, @p.higher_precedence?(@mu, @dp))
    assert_equals(false, @p.higher_precedence?(@e, @dp))
    assert_equals(false, @p.higher_precedence?(@um, @dp))
  end

  def test_set_precedence_conflict
    [@um, @e, @mu, @pl].each do |p|
      assert_exception(RelationCircularityException) do 
	@p.set_precedence(@mi, p, :HIGHER)
      end
    end

    [@um, @e, @mu, @pl].each do |p|
      assert_exception(RelationCircularityException) do 
	@p.set_precedence(p, @mi, :LOWER)
      end
    end

    [@um, @e, @mu].each do |p|
      assert_exception(RelationCircularityException) do 
	@p.set_precedence(@mi, p, :EQUAL)
      end
    end
  end

  def test_associativity_conflict?
    assert_equals(false, @p.left_or_non_associativity_conflict?(@pl, 0, @pl))
    assert_equals(true, @p.left_or_non_associativity_conflict?(@pl, 2, @pl))
    assert_equals(false, @p.left_or_non_associativity_conflict?(@mu, 0, @mu))
    assert_equals(true, @p.left_or_non_associativity_conflict?(@mu, 2, @mu))
    assert_equals(false, @p.left_or_non_associativity_conflict?(@mi, 0, @mi))
    assert_equals(true, @p.left_or_non_associativity_conflict?(@mi, 2, @mi))

    assert_equals(false, @p.left_or_non_associativity_conflict?(@mi, 0, @pl))
    assert_equals(false, @p.left_or_non_associativity_conflict?(@pl, 0, @mi))
    assert_equals(true, @p.left_or_non_associativity_conflict?(@mi, 2, @pl))
    assert_equals(true, @p.left_or_non_associativity_conflict?(@pl, 2, @mi))

    assert_equals(true, @p.right_or_non_associativity_conflict?(@e, 0, @e))
    assert_equals(false, @p.right_or_non_associativity_conflict?(@e, 2, @e))
  end

  def test_conflict?
    # Associativity conflicts
    assert_equals(true, @p.conflict?(@pl, 2, @pl))
    assert_equals(false, @p.conflict?(@pl, 0, @pl))
    assert_equals(true, @p.conflict?(@mi, 2, @mi))
    assert_equals(false, @p.conflict?(@mi, 0, @mi))
    assert_equals(true, @p.conflict?(@mu, 2, @mu))
    assert_equals(false, @p.conflict?(@mu, 0, @mu))
    assert_equals(false, @p.conflict?(@e, 2, @e))
    assert_equals(true, @p.conflict?(@e, 0, @e))
    assert_equals(true, @p.conflict?(@mi, 2, @pl))
    assert_equals(false, @p.conflict?(@mi, 0, @pl))
    assert_equals(true, @p.conflict?(@pl, 2, @mi))
    assert_equals(false, @p.conflict?(@pl, 0, @mi))
    
    # Precedence conflicts (some...)
    assert_equals(true, @p.conflict?(@e, 0, @um))
    assert_equals(false, @p.conflict?(@um, 0, @e))
    assert_equals(true, @p.conflict?(@mu, 0, @e))
    assert_equals(false, @p.conflict?(@e, 0, @mu))
    assert_equals(true, @p.conflict?(@pl, 0, @mu))
    assert_equals(false, @p.conflict?(@mu, 0, @pl))
    assert_equals(false, @p.conflict?(@e, 0, @pl))
    assert_equals(false, @p.conflict?(@um, 0, @mi))
  end

  def test_shorthand_left
    r = left(@pl)[0]
    assert_kind_of(Relation, r)
    assert_equals(@pl, r.left)
    assert_equals(@pl, r.right)
    assert_equals(:LEFT, r.relation)

    r = left(@pl, @mi)[0]
    assert_kind_of(Relation, r)
    assert_equals(@pl, r.left)
    assert_equals(@mi, r.right)
    assert_equals(:LEFT, r.relation)

    r = left(@pl, @mi, @um).flatten
    assert_kind_of(Array, r)
    assert_equals(@pl, r[0].left)
    assert_equals(@mi, r[0].right)
    assert_equals(:LEFT, r[0].relation)
  end

  def test_shorthand_right
    r = right(@pl)[0]
    assert_kind_of(Relation, r)
    assert_equals(@pl, r.left)
    assert_equals(@pl, r.right)
    assert_equals(:RIGHT, r.relation)

    r = right(@pl, @mi)[0]
    assert_kind_of(Relation, r)
    assert_equals(@pl, r.left)
    assert_equals(@mi, r.right)
    assert_equals(:RIGHT, r.relation)

    r = right(@pl, @mi, @um).flatten
    assert_kind_of(Array, r)
    assert_equals(3, r.length)
    assert_equals(@pl, r[0].left)
    assert_equals(@mi, r[0].right)
    assert_equals(:RIGHT, r[0].relation)
  end

  def test_shorthand_decreasing_precedence
    r = decreasing_precedence(@um, @e, @mu)
    assert_kind_of(Array, r)
    assert_equals(2, r.length)
    assert_equals(@um, r[0].left)
    assert_equals(@e, r[0].right)
    assert_equals(:HIGHER, r[0].relation)
    assert_equals(@e, r[1].left)
    assert_equals(@mu, r[1].right)
    assert_equals(:HIGHER, r[1].relation)
  end

  def test_shorthand_priorities
    p = priorities(left(@um), right(@e), left(@pl, @mi),
		   decreasing_precedence(@um, @e, @mu, @pl),
		   equal_precedence(@pl, @mi))
    assert_kind_of(ProductionPriorities, p)
    assert_equals(true, p.higher_precedence?(@um, @e))
    assert_equals(true, p.higher_precedence?(@e, @mu))
    assert_equals(true, p.higher_precedence?(@mu, @pl))
    assert_equals(true, p.higher_precedence?(@mu, @mi))
    assert_equals(true, p.equal_precedence?(@pl, @mi))
    assert_equals(true, p.conflict?(@pl, 2, @mi))
    assert_equals(true, p.conflict?(@e, 0, @e))
    assert_equals(true, p.conflict?(@um, 1, @um))
  end

  def test_in_some_conflict?
    assert_equals(true, @p.in_some_conflict?(@um))
    assert_equals(true, @p.in_some_conflict?(@e))
    assert_equals(true, @p.in_some_conflict?(@mu))
    assert_equals(true, @p.in_some_conflict?(@pl))
    assert_equals(true, @p.in_some_conflict?(@mi))
    assert_equals(false, @p.in_some_conflict?(@dp))
  end

  def test_circularity_exception_bug1
    di = FakeProduction.new("Div", 3)
    assert_no_exception do
      priorities([decreasing_precedence(@mu, @pl),
		   equal_precedence(di, @mu),
		   equal_precedence(@pl, @mi)])
    end
  end

  def test_equal_precedence
    pr = ProductionPriorities.new
    pr.set_precedence(2,0,:HIGHER)
    pr.set_precedence(1,2,:EQUAL)
    assert_equals(true, pr.higher_precedence?(1,0))

    pr = ProductionPriorities.new
    pr.set_precedence(1,2,:EQUAL)
    pr.set_precedence(2,0,:HIGHER)
    assert_equals(true, pr.higher_precedence?(1,0))
  end

  def test_circularity_exception_bug2
    di, pr = FakeProduction.new("Div", 3), nil
    assert_no_exception do
      pr = priorities([equal_precedence(di, @mu),
			decreasing_precedence(@mu, @pl),
			equal_precedence(@pl, @mi)])
    end
    assert_equals(true, pr.equal_precedence?(di, @mu))
    assert_equals(true, pr.higher_precedence?(di, @pl))
    assert_equals(true, pr.higher_precedence?(di, @mi))
  end

  def test_relation_to_src
    r = Relation.new(1, :HIGHER, 2)
    assert_equals("Relation.new(1,:HIGHER,2)", r.to_src)
    assert_equals("a = Relation.new(1,:HIGHER,2)", r.to_src("a"))
  end

  def test_productionpriotities_to_src
    str = @p.to_src("priorities", 
		    nh = {@um => "unary_minus", 
		      @e => "exp", @mu => "mult", 
		      @pl => "plus_prod", @mi => "minus_prod"})
    assert_kind_of(String, str)
    defs = nh.invert.map {|key, val| val.to_src(key)}.join("\n") + "\n"
    p2 = eval(defs + str )
    assert_kind_of(PublicProdPrio, p2)
    assert_equals(@p.relations.length, p2.relations.length)
    @p.relations.each {|r| assert(p2.relations.include?(r))}
    p2.relations.each {|r| assert(@p.relations.include?(r))}
  end
end

# Run if we were directly called
if $0 == __FILE__
  testrunner = RUNIT::CUI::TestRunner.new
  if ARGV.size == 0
    suite = TestConflictResolution.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestConflictResolution.new(testmethod))
    end
  end
  testrunner.run(suite)
end

