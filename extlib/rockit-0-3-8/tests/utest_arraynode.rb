require 'runit/testcase'
require 'runit/testsuite'
require 'runit/cui/testrunner'
require 'runit/topublic'
include RUNIT::ToPublic

require 'rockit/syntax_tree'

class TestArrayNode < RUNIT::TestCase
  def test_arraynode_compact!
    a = ArrayNode.new([1,2,3])
    a.compact!
    assert_equals([1,2,3], a.childrens)
  end

  def test_arraynode_each
    a = ArrayNode.new([1,2,3])
    a2 = ArrayNode.new([10,a,30])
    cnt = 0
    a2.each {|e| cnt+=1}
    assert_equals(3, cnt)
  end
end

# Run if we were directly called
if $0 == __FILE__
  testrunner = RUNIT::CUI::TestRunner.new
  if ARGV.size == 0
    suite = TestArrayNode.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestArrayNode.new(testmethod))
    end
  end
  testrunner.run(suite)
end

