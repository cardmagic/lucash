require 'runit/testcase'
require 'runit/testsuite'
require 'runit/cui/testrunner'
require 'runit/topublic'
include RUNIT::ToPublic

require 'rockit/token'

class TestEofToken < RUNIT::TestCase
  def setup
    @t = EofToken.new
  end

  def test_initialize
    assert_kind_of(EofToken, @t)
  end

  def test_match
    assert_equals(nil, @t.match("safs"))
  end

  def test_regexp
    re = @t.regexp
    assert_equals(nil, re.match("sfds"))
  end

  def test_regexp_bug
    ["sfs", "23q"].each do |aString|
      scanner = StringScanner.new(aString)
      assert_equals(nil, scanner.check(@t.regexp))
    end
  end

  def test_to_src
    assert_equals(@t, eval(@t.to_src))
  end
end

# Run if we were directly called
if $0 == __FILE__
  testrunner = RUNIT::CUI::TestRunner.new
  if ARGV.size == 0
    suite = TestEofToken.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestEofToken.new(testmethod))
    end
  end
  testrunner.run(suite)
end

