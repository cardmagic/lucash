require 'runit/testcase'
require 'runit/testsuite'
require 'runit/cui/testrunner'
require 'runit/topublic'
include RUNIT::ToPublic

require 'rockit/reduce_actions_generator'

class Integer
  def index_number
    self
  end
end

class TestTerminalSet < RUNIT::TestCase
  def setup
    @terms = (1..10).to_a
    @t1 = TerminalSet.new(@terms)
    @t2 = TerminalSet.new(@terms, [1,4,9])
    @t3 = TerminalSet.new(@terms, [4,5,7,10])
    @t4 = TerminalSet.new(@terms, (1..10).to_a)
  end

  def test_initialize
    assert_kind_of(TerminalSet, @t1)
  end

  def test_initialize_with_members
    t = TerminalSet.new(@terms, [1,4,9])
    assert_kind_of(TerminalSet, t)
    assert_equals([1,4,9], t.terminals.sort)
  end

  def test_add
    @t1.add(2)
    assert_equals([2,], @t1.terminals)

    assert_exception(ArgumentError) {@t1.add(11)}
  end

  def test_update
    @t2.update(@t3)
    assert_equals([1,4,5,7,9,10], @t2.terminals.sort)
  end

  def test_include?
    [1,4,9].each {|t| assert(@t2.include?(t))}
    [2,3,5,6,7,8,10].each {|t| assert(!@t2.include?(t))}
  end

  def test_subtract
    t = @t4 - @t3
    assert_kind_of(TerminalSet, t)
    assert_equals([1,2,3,6,8,9], t.terminals.sort)
    assert(t.id != @t4.id)
    assert(t.id != @t3.id)

    t = t - @t2
    assert_equals([2,3,6,8], t.terminals.sort)
  end

  def test_empty?
    assert(@t1.empty?)
    assert(!@t2.empty?)
    assert(!@t3.empty?)
    assert(!@t4.empty?)
  end
end

# Run if we were directly called
if $0 == __FILE__
  testrunner = RUNIT::CUI::TestRunner.new
  if ARGV.size == 0
    suite = TestTerminalSet.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestTerminalSet.new(testmethod))
    end
  end
  testrunner.run(suite)
end

