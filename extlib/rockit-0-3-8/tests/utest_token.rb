require 'runit/testcase'
require 'runit/testsuite'
require 'runit/cui/testrunner'
require 'runit/topublic'
include RUNIT::ToPublic

require 'rockit/token' 

class TestToken < RUNIT::TestCase
  def test_01_initialize
    t = Token.new("Integer", /\d+/, :SKIP)
    assert(t.skip)

    t = Token.new("Integer", '\d+', :sKIp)
    assert(t.skip)

    t = Token.new("Integer", '\d+')
    assert(!t.skip)

    t = Token.new("Integer", '\d+', "SkiP")
    assert(t.skip)
  end

  def test_02_match
    t = Token.new("Integer", '\d+')
    m = t.match "12344"
    assert_kind_of(MatchData, m)
    assert_equals("12344", m[0])

    assert_equals(nil, t.match("as"))
  end

  def test_03_shorthand_initialize
    assert(t("Integer", /\d+/, :SKIP).skip)
  end

  def test_04_stringtoken
    st = StringToken.new("st1", "aa")
    assert_kind_of(StringToken, st)
    assert_equals("\"aa\"", st.inspect)

    st = StringToken.new("st1", "+")
    assert_kind_of(StringToken, st)
    assert_equals("\"+\"", st.inspect)
  end

  def test_05_token_to_src
    t = Token.new("Integer", /\d+/, :SKIP)
    assert_equals(t, eval(t.to_src))

    t = Token.new("Float", /\d+\.\d+/)
    assert_equals(t, eval(t.to_src))

    t = Token.new("AlmostRegexp", /\\\//)
    assert_equals(t, eval(t.to_src))

    t = Token.new("Regexp", /\/((\\\/)|[^\/])*\/[iomx]*/)
    assert_equals(t, eval(t.to_src))

    t = Token.new("String", /('((\\')|[^'])*')|("((\\")|[^"])*")/)
    assert_equals(t, eval(t.to_src))
  end

  def test_06_stringtoken_to_src
    st = StringToken.new("st1", "aa")
    assert_equals(st, eval(st.to_src))

    st = StringToken.new("st1", "+")
    assert_equals(st, eval(st.to_src))
  end

  def test_07_token_equality
    assert(string_token("-"), string_token("-"))
  end

  def test_08_regexptoken
    retok = regexp_token(/:Skip/i)
    assert_kind_of(Token, retok)
    assert_match(":Skip", retok.regexp)
    assert_match(":SKIP", retok.regexp)
    assert_equals(true, retok.regexp.casefold?)
  end

  def test_09_token_with_ignorecase_regexp
    t = Token.new("A", /a/i)
    assert_equals(true, t.regexp.casefold?)
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
    ft = Token.new("Float", ror(rseq(r?(fbase), fdecimal, r?(fexponent)),
				rseq(fbase, r?(fdecimal), fexponent)))

    assert_match("1.2", ft.regexp)
    assert_match("1.2e-1", ft.regexp)
    assert_match("353_2.2_2e_3", ft.regexp)
    assert_match("1_223_4.2_22___3e-_1__1", ft.regexp)
    assert_match(".2e-1", ft.regexp)
    assert_match("1e1", ft.regexp)
    assert_match("631e-1", ft.regexp)
    assert_match("62_34e7", ft.regexp)

    assert_not_match("1_223_4._2223e2", ft.regexp)
    assert_not_match("_2234.2223e-2", ft.regexp)
    assert_not_match("12234", ft.regexp)
    assert_not_match("1", ft.regexp)
    assert_not_match("1a", ft.regexp)
    assert_not_match("A", ft.regexp)
    assert_not_match("-", ft.regexp)
  end
end

# Run if we were directly called
if $0 == __FILE__
  testrunner = RUNIT::CUI::TestRunner.new
  if ARGV.size == 0
    suite = TestToken.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestToken.new(testmethod))
    end
  end
  testrunner.run(suite)
end

