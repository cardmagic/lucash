require 'runit/testcase'
require 'runit/testsuite'
require 'runit/cui/testrunner'
require 'runit/topublic'
include RUNIT::ToPublic

require 'rockit/glr_parser' 

class TestParseStack < RUNIT::TestCase
  def setup
    @s = (0..7).to_a.map {|v| ParseStack.new(v,v)}
    cnt, @links = -1, []
    [[1,0],[2,1],[4,1],[3,2],[3,4],
     [5,4],[5,3],[6,5],[7,6],[7,3]].each do |from, to|
      @links.push(@s[from].add_link(@s[to],cnt+=1))
    end
  end

  def test_01_links_to_stack_in_state?
    assert(@s[3].links_to_stack_in_state?(4))
    assert(@s[3].links_to_stack_in_state?(2))
    assert(@s[7].links_to_stack_in_state?(6))
    assert(@s[7].links_to_stack_in_state?(3))
  end

  def test_02_paths_of_length_nolink
    assert_equals(1, @s[1].paths_of_length(1).length)
    assert_equals(1, @s[2].paths_of_length(2).length)
    assert_equals(2, @s[3].paths_of_length(3).length)
    assert_equals(2, @s[3].paths_of_length(3).length)
    assert_equals(5, @s[7].paths_of_length(4).length)
  end

  def test_03_paths_of_length_link
    assert_equals(2, @s[7].paths_of_length(4, @links.last).length)
  end

  def test_04_valid_paths_of_length_with_pruning
    paths = @s[7].valid_paths_of_length_with_pruning(4) do |link,pos|
      link.tree % 2 == (pos+1) % 2
    end    
    assert_equals(1, paths.length) 
    assert_equals(6, paths[0].links[0].stack.state)
    assert_equals(5, paths[0].links[1].stack.state)
    assert_equals(3, paths[0].links[2].stack.state)
    assert_equals(2, paths[0].links[3].stack.state)

    # Because of the pruning there is only one path left and its the one above
    assert_equals(1, @s[7].paths_of_length(1).length)
    assert_equals(1, @s[6].paths_of_length(1).length)
    assert_equals(1, @s[3].paths_of_length(1).length)
    assert_equals(1, @s[5].paths_of_length(1).length)
  end
end

# Run if we were directly called
if $0 == __FILE__
  testrunner = RUNIT::CUI::TestRunner.new
  if ARGV.size == 0
    suite = TestParseStack.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestParseStack.new(testmethod))
    end
  end
  testrunner.run(suite)
end

