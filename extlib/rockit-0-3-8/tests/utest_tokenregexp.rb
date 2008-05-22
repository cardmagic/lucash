require 'runit/testcase'
require 'runit/testsuite'
require 'runit/cui/testrunner'
require 'runit/topublic'
include RUNIT::ToPublic

require 'rockit/token' 

class TestTokenRegexp < RUNIT::TestCase

  def setup
  end

  def test_01_new_from_string
    assert_match("1", TokenRegexp.new("1"))
    assert_match("1", TokenRegexp.new("\\d"))
    assert_match("1", TokenRegexp.new('\d+'))
  end

  def test_02_new_from_regexp
    assert_match("1", TokenRegexp.new(/1/))
    assert_match("1", TokenRegexp.new(/\d+/))
  end

  def test_03_string
    assert_equals('\w+', TokenRegexp.new("\\w+").string)
    assert_equals('\w+', TokenRegexp.new("^(\\w+)").string)
    assert_equals('(\s*)', TokenRegexp.new("(\\s*)").string)
    assert_equals('\w+', TokenRegexp.new(/\w+/).string)
  end

  def test_acceptance_01_ruby_float_literal
    # Test with Ruby float literals.
    # In the grammar file this should be written as:
    #  FLOAT -> base decimal? exponent
    #        |  base? decimal exponent?
    #    where
    #      base     = /\d(_*\d+)*/
    #      decimal  = /\.\d(_*\d+)*/
    #      exponent = /(E|e)-?(_*\d+)+/
    #
    fbase = tr /\d(_*\d+)*/
    fdecimal = tr /\.\d(_*\d+)*/
    fexponent = tr /(e|E)-?(_*\d+)+/
    alt1 = rseq(fbase, r?(fdecimal), fexponent)
    alt2 = rseq(r?(fbase), fdecimal, r?(fexponent))
    float = ror(alt2, alt1)
    #float = ror(alt1, alt2) # Why doesn't this work?

    assert_match("1e1", alt1)
    assert_match("1.2e-1", alt1)
    assert_match("1.2e-1", alt2)
    assert_match(".2e1", alt2)
    assert_match(".2", alt2)

    assert_match("1.2", float)
    assert_match("1.2e-1", float)
    assert_match("353_2.2_2e_3", float)
    assert_match("1_223_4.2_22___3e-_1__1", float)
    assert_match(".2e-1", float)
    assert_match("1e1", float)
    assert_match("631e-1", float)
    assert_match("62_34e7", float)

    assert_not_match("1_223_4._2223e2", float)
    assert_not_match("_2234.2223e-2", float)
    assert_not_match("12234", float)
    assert_not_match("1", float)
    assert_not_match("1a", float)
    assert_not_match("A", float)
    assert_not_match("-", float)
  end
end

# Run if we were directly called
if $0 == __FILE__
  testrunner = RUNIT::CUI::TestRunner.new
  if ARGV.size == 0
    suite = TestTokenRegexp.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestTokenRegexp.new(testmethod))
    end
  end
  testrunner.run(suite)
end

