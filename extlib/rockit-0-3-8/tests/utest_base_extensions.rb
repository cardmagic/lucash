require 'runit/testcase'
require 'runit/testsuite'
require 'runit/cui/testrunner'
require 'runit/topublic'
include RUNIT::ToPublic

require 'rockit/base_extensions'

class TestSourceCodeDumpable < RUNIT::TestCase
  def test_delete_at_indices
    a = [0,1,2,3,4,5,6]
    assert_equals([1,2,4], a.delete_at_indices([0,3,5,6]))
    assert_equals([0,1,2,3,4,5,6], a)
  end

  def test_array_of_arrays
    a = ArrayOfArrays.new
    assert_equals(0, a.length)
    assert_equals(Array, a[2].type)
    assert_equals(0, a[2].length)
    assert(a[2].id != a[5].id)
    a[2].push 1
    assert_equals(1, a[2].first)
    a.clear
    assert_equals(0, a[2].length)
  end

  def test_array_of_hashes
    a = ArrayOfHashes.new
    assert_equals(0, a.length)
    assert_equals(Hash, a[2].type)
    assert_equals(nil, a[2][10])
    assert(a[2].id != a[5].id)
    a[2][10] = 1
    assert_equals(1, a[2][10])
    a.clear
    assert_equals(nil, a[2][10])
  end
end

# Run if we were directly called
if $0 == __FILE__
  testrunner = RUNIT::CUI::TestRunner.new
  if ARGV.size == 0
    suite = TestSourceCodeDumpable.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestSourceCodeDumpable.new(testmethod))
    end
  end
  testrunner.run(suite)
end

