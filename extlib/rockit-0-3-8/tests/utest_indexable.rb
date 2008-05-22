require 'runit/testcase'
require 'runit/testsuite'
require 'runit/cui/testrunner'
require 'runit/topublic'
include RUNIT::ToPublic

require 'rockit/indexable'

class Dummy
  include Indexable

  def initialize(num)
  end
end

class TestIndexable < RUNIT::TestCase
  def setup
    @f = IndexableFactory.new(Dummy, 0)
    @f2 = IndexableFactory.new(Dummy)
    @f3 = IndexableFactory.new(Dummy, -10)
    @d = [10,20].map {|v| @f.make(v)}
  end

  def test_indexablefactory_initialize
    assert_kind_of(IndexableFactory, @f)
  end

  def test_indexablefactory_initialize_with_start_index
    assert_kind_of(IndexableFactory, @f2)
    assert_equals(0, @f2.start_index)
    assert_kind_of(IndexableFactory, @f3)
    assert_equals(-10, @f3.start_index)
  end

  def test_indexablefactory_make
    assert_equals(0, @d[0].index_number)
    assert_equals(1, @d[1].index_number)
    assert(@d[1] != @d[0])
    d = @f.make(10)
    assert(d == @d[0])
    d = @f.make(20)
    assert(d == @d[1])
  end

  def test_indexablefactory_make_in_different_factories
    f2d, f3d = @f2.make(10), @f3.make(10)
    assert(f2d != f3d)
  end

  def test_indexablefactory_make_unless_exists
    d, new_obj = @f.make_unless_exists(10)
    assert_equals(@d[0], d)
    assert(!new_obj)

    d, new_obj = @f.make_unless_exists(30)
    assert_equals(2, d.index_number)
    assert(new_obj)
  end

  def test_indexablefactory_instances
    assert_equals(@d, @f.instances)
  end

  def test_indexablefactory_get_instance
    assert_equals(@d[0], @f.get_instance(10))
    assert_equals(@d[1], @f.get_instance(20))
    assert_equals(nil, @f.get_instance(3000))
  end

  def test_multiple_factories
  end
end

# Run if we were directly called
if $0 == __FILE__
  testrunner = RUNIT::CUI::TestRunner.new
  if ARGV.size == 0
    suite = TestIndexable.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestIndexable.new(testmethod))
    end
  end
  testrunner.run(suite)
end

