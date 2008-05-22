require 'runit/testcase'
require 'runit/testsuite'
require 'runit/cui/testrunner'
require 'runit/topublic'
include RUNIT::ToPublic

require 'rockit/token' 

class TestForkingRegexpLexer < RUNIT::TestCase
  def test_acceptance_01_multi_nums
    tokens = 
      [i = t("Integer", /\d+/), 
       f = t("Float", /\d+\.\d+/),
       d = t("Dot", /\./), 
       t("Blank", /\s+/, :skip)]
    l = ForkingRegexpLexer.new tokens
    l.init " 1.23."
    r0 = l.peek.sort {|a,b| a.lexeme.length <=> b.lexeme.length}
    assert_equals(Array, r0.type)
    assert_equals(2, r0.length)
    assert_equals("1", r0[0].lexeme)
    assert_equals(i, r0[0].token_type)
    assert_equals("1.23", r0[1].lexeme)
    assert_equals(i, r0[0].token_type)
    assert_equals(1, r0[0].position.char_position)

    r1 = r0[0].lexer.peek.sort {|a,b| a.lexeme.length <=> b.lexeme.length}
    assert_equals(Array, r1.type)
    assert_equals(1, r1.length)
    assert_equals(".", r1[0].lexeme)
    assert_equals(d, r1[0].token_type)
    assert_equals(2, r1[0].position.char_position)

    r2 = r0[1].lexer.peek.sort {|a,b| a.lexeme.length <=> b.lexeme.length}
    assert_equals(Array, r2.type)
    assert_equals(1, r2.length)
    assert_equals(".", r2[0].lexeme)
    assert_equals(d, r2[0].token_type)
    assert_equals(5, r2[0].position.char_position)
    
    r3 = r1[0].lexer.peek.sort {|a,b| a.lexeme.length <=> b.lexeme.length}
    assert_equals(Array, r3.type)
    assert_equals(1, r3.length)
    assert_equals("23", r3[0].lexeme)
    assert_equals(i, r3[0].token_type)
    assert_equals(3, r3[0].position.char_position)

    r4 = r2[0].lexer.peek
    assert_equals(Array, r4.type)
    assert_equals(1, r4.length)
    assert_equals(nil, r4[0].lexeme)
    assert_kind_of(EofToken, r4[0].token_type)
    assert_equals(nil, r4[0].lexer)
    assert_equals(6, r4[0].position.char_position)

    r5 = r3[0].lexer.peek.sort {|a,b| a.lexeme.length <=> b.lexeme.length}
    assert_equals(Array, r5.type)
    assert_equals(1, r5.length)
    assert_equals(".", r5[0].lexeme)
    assert_equals(d, r5[0].token_type)
    assert_equals(5, r5[0].position.char_position)

    r6 = r5[0].lexer.peek
    assert_equals(Array, r6.type)
    assert_equals(1, r6.length)
    assert_equals(nil, r6[0].lexeme)
    assert_kind_of(EofToken, r6[0].token_type)
    assert_equals(nil, r6[0].lexer)
    assert_equals(6, r6[0].position.char_position)

    l.init "1"
    r0 = l.peek.sort {|a,b| a.lexeme.length <=> b.lexeme.length}
    assert_equals(Array, r0.type)
    assert_equals(1, r0.length)
    assert_equals("1", r0[0].lexeme)
    assert_equals(i, r0[0].token_type)
  end

  def test_02_skip_tokens
    tokens = 
      [i = t("Integer", /\d+/), 
       t("Blank", /\s+/, :skip),
       t("Plus", /\+/)]
    l = ForkingRegexpLexer.new tokens

    l.init " 1"
    r0 = l.peek
    assert_equals(1, r0.length)
    assert_equals("1", r0[0].lexeme)

    l.init " 1 "
    r0 = l.peek
    assert_equals(1, r0.length)
    assert_equals("1", r0[0].lexeme)

    l.init "1 + 2"
    r0 = l.peek
    assert_equals(1, r0.length)
    assert_equals("1", r0[0].lexeme)
    r1 = r0[0].lexer.peek
    assert_equals(1, r1.length)
    assert_equals("+", r1[0].lexeme)

    l.init "\t1  +  2 "
    r0 = l.peek
    assert_equals(1, r0.length)
    assert_equals("1", r0[0].lexeme)
  end

  def test_03_eof_when_no_match_bug
    tokens = [
      blank = t("Blank", /\s+/, :Skip),
      int = t("Int", /\d+/),
      float = t("Float", /\d+\.\d+/),
      t("Plus", /\+/),
      id = t("Id", /[A-Za-z]+/)
    ]
    l = ForkingRegexpLexer.new tokens

    l.init "4+3.1"
    r0 = l.peek.sort {|a,b| a.lexeme.length <=> b.lexeme.length}
    assert_equals("4", r0[0].lexeme)
    r1 = r0[0].lexer.peek.sort {|a,b| a.lexeme.length <=> b.lexeme.length}
    assert_equals("+", r1[0].lexeme)

    r2 = r1.first.lexer.peek.sort {|a,b| a.lexeme.length <=> b.lexeme.length}
    assert_equals(2, r2.length)
    assert_equals("3", r2[0].lexeme)
    assert_equals("3.1", r2[1].lexeme)

    assert_equals([], r2[0].lexer.peek)
    r3 = r2[1].lexer.peek
    assert_kind_of(EofToken, r3[0].token_type)
  end
end

# Run if we were directly called
if $0 == __FILE__
  testrunner = RUNIT::CUI::TestRunner.new
  if ARGV.size == 0
    suite = TestForkingRegexpLexer.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestForkingRegexpLexer.new(testmethod))
    end
  end
  testrunner.run(suite)
end

