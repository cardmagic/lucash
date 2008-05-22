require 'runit/testcase'
require 'runit/testsuite'
require 'runit/cui/testrunner'
require 'runit/topublic'
include RUNIT::ToPublic

require 'rockit/bounded_lru_cache'

class TestBoundedLruCache < RUNIT::TestCase
  def setup
    @n = 10
    @c = BoundedLruCache.new @n
    @n.times {|i| @c[i] = i}
  end

  def test_01
    assert_equals(@n, @c.max_size)
    assert_equals(@n, @c.length)
    temp = @c[0]    # Use 0 => 1 the last one used
    temp = @c[1]    # Use 1 => 2 the last one used
    @c[2*@n] = 2*@n # Add new => 2 is thrown out
    assert_equals(@n, @c.length)
    assert(@c[2] == nil)
    assert(@c[0] == 0)
    assert(@c[1] == 1)
    assert(@c[2*@n] == 2*@n)
  end

  def test_02
    c = BoundedLruCache.new(2)
    100.times {|i| temp = c[rand(2)]} # Some random uses
    10.times {|i| t = rand(100); c[t] = t}
    assert_equals(2, c.length)
  end
end

# Run if we were directly called
if $0 == __FILE__
  testrunner = RUNIT::CUI::TestRunner.new
  if ARGV.size == 0
    suite = TestBoundedLruCache.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestBoundedLruCache.new(testmethod))
    end
  end
  testrunner.run(suite)
end

