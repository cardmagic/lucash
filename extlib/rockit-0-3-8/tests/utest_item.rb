require 'runit/testcase'
require 'runit/testsuite'
require 'runit/cui/testrunner'
require 'runit/topublic'
include RUNIT::ToPublic

require 'rockit/parsetable_generation'

class TestItem < RUNIT::TestCase
  def setup
    @c = NonTerminal.new("C")
    @c.derives_epsilon = false
    @p = Production.new(:S, ["s", @c])
    @i = Item.new(@p, 0)

    @c2 = NonTerminal.new("C")
    @c2.derives_epsilon = true
    @d = NonTerminal.new("D")
    @d.derives_epsilon = true
    @e = NonTerminal.new("E")
    @p2 = Production.new(:S, ["s", @c2, @d, @e])
    @i2 = Item.new(@p2, 0)
  end

  def test_initialize
    assert_kind_of(Item, @i)
    assert_equals(0, @i.position)
    assert_equals(@p, @i.production)
    s = @i.symbol
    assert_kind_of(Token, s)
    assert_equals("\"s\"", s.inspect)
    assert_equals(nil, @i.lookahead)
  end

  def test_suffix
    su1 = @i.suffix
    assert_equals(1, su1.length)
    assert_equals(NonTerminal.new("C"), su1[0])
  end

  def test_next_item_no_factory
    ni = @i.next_item
    assert_kind_of(Item, ni)
    assert_equals(NonTerminal.new("C"), ni.symbol)
    assert_equals(1, ni.position)
    assert_equals(@p, ni.production)
    assert_equals(nil, ni.lookahead)
  end

  def test_next_item_factory
    item_factory = IndexableFactory.new(Item, 0)
    i1 = item_factory.make(@p, 0)
    assert_equals(0, i1.index_number)
    assert_equals(item_factory, i1.factory)
    ni = i1.next_item
    assert_equals(1, ni.index_number)
  end

  def test_final
    assert_equals(false, @i.final?)
    i2 = @i.next_item
    assert_equals(false, i2.final?)
    assert_equals(true, i2.next_item.final?)
  end

  def test_direct_following_symbols
    assert_equals([@c], @i.direct_following_symbols)
    assert_equals([@c2, @d, @e], @i2.direct_following_symbols)
  end
end

# Run if we were directly called
if $0 == __FILE__
  testrunner = RUNIT::CUI::TestRunner.new
  if ARGV.size == 0
    suite = TestItem.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestItem.new(testmethod))
    end
  end
  testrunner.run(suite)
end
