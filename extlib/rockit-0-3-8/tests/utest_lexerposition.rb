require 'runit/testcase'
require 'runit/testsuite'
require 'runit/cui/testrunner'
require 'runit/topublic'
include RUNIT::ToPublic

require 'rockit/token'

class TestLexerPosition < RUNIT::TestCase
  def test_01_basic_strings
    l = LexerPosition.new
    assert_equals(0, l.column)
    assert_equals(0, l.row)
    assert_equals(0, l.char_position)

    l += "a"
    assert_equals(1, l.column)
    assert_equals(0, l.row)
    assert_equals(1, l.char_position)

    l += " \tasd"
    assert_equals(6, l.column)
    assert_equals(0, l.row)
    assert_equals(6, l.char_position)
  end

  def test_02_newline_strings
    l = LexerPosition.new + "adf\nadf"
    assert_equals(3, l.column)
    assert_equals(1, l.row)
    assert_equals(7, l.char_position)

    l += "\n"
    assert_equals(0, l.column)
    assert_equals(2, l.row)
    assert_equals(8, l.char_position)

    l += "\n\n\na"
    assert_equals(1, l.column)
    assert_equals(5, l.row)
    assert_equals(12, l.char_position)
  end

  def test_03_special_newline_strings
    l = LexerPosition.new + "a\nd#\t\"f\n\ra\rdf\r\nde"
    assert_equals(2, l.column)
    assert_equals(6, l.row)
    assert_equals(17, l.char_position)
  end
end

# Run if we were directly called
if $0 == __FILE__
  testrunner = RUNIT::CUI::TestRunner.new
  if ARGV.size == 0
    suite = TestLexerPosition.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestLexerPosition.new(testmethod))
    end
  end
  testrunner.run(suite)
end

