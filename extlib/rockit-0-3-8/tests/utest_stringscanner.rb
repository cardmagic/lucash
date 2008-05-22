require 'runit/testcase'
require 'runit/testsuite'
require 'runit/cui/testrunner'
require 'runit/topublic'
include RUNIT::ToPublic

$OLD_USING_STRSCAN = $USING_STRSCAN
$USING_STRSCAN = false
require 'rockit/stringscanner'

class TestStringScanner < RUNIT::TestCase
  def setup
    @str = "abcdefg1234980"
    @s = StringScanner.new @str
  end

  def test_initialize
    assert_kind_of(StringScanner, @s)
    assert_equals(0, @s.pointer)
  end

  def test_string
    assert_equals(@str, @s.string)
    @s.scan /abc/
    assert_equals(@str, @s.string)
  end

  def test_rest
    assert_equals(@str, @s.rest)
    @s.scan /abc/
    assert_equals(@str[3..-1], @s.rest)
  end

  def test_check
    m = @s.check /abc/
    assert_equals("abc", m)
    assert_equals(0, @s.pointer)
  end
end

# Run if we were directly called
if $0 == __FILE__
  testrunner = RUNIT::CUI::TestRunner.new
  if ARGV.size == 0
    suite = TestStringScanner.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestStringScanner.new(testmethod))
    end
  end
  testrunner.run(suite)
  $USING_STRSCAN = $OLD_USING_STRSCAN
end

