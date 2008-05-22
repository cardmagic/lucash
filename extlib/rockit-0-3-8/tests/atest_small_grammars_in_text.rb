require 'runit/testcase'
require 'runit/testsuite'
require 'runit/cui/testrunner'
require 'runit/topublic'
include RUNIT::ToPublic

require 'rockit/rockit_grammars_parser' 

def count_tree_nodes(ast)
  count = 0
  ast.each_node {|n| count += 1}
  count
end

class AcceptanceTestSmallGrammarsInText < RUNIT::TestCase
  def test_dragonbook_4_41
    p = Parse.generate_parser <<-'EOG'
      Grammar Dragon_book_4_41
       Productions
        S -> B B
        B -> 'a' B
          |  'b'
    EOG

    ast = nil
    assert_no_exception {ast = p.parse("abab")}
    assert_kind_of(SyntaxTree, ast)
    assert_equals(9, count_tree_nodes(ast))

    assert_no_exception {ast = p.parse("aaabab")}
    assert_equals(13, count_tree_nodes(ast))

    assert_exception(ParseException) {p.parse("ab")}
  end

  def test_dragonbook_4_41_rockit
    p = Parse.generate_parser <<-'EOG'
      Grammar Dragon_book_4_41
       Productions
        S -> B B
        B -> /a/i+ 'b'
    EOG

    ast = nil
    assert_no_exception {ast = p.parse("abAb")}
    assert_equals(9, count_tree_nodes(ast)) # The arrays do not count
    
    assert_no_exception {ast = p.parse("AAAbab")}
    assert_equals(11, count_tree_nodes(ast)) # The arrays do not count
    assert_kind_of(ArrayNode, ast[0][0])
    assert_equals(3, ast[0][0].as_a.length)
    (0..2).each {|i| assert_equals("A", ast[0][0].as_a[i].lexeme)}

    assert_exception(ParseException) {p.parse("ab")}
  end

  def test_dragonbook_4_46
    p = Parse.generate_parser <<-'EOG'
      Grammar Dragon_book_4_46
       Productions
        S -> L '=' R
          |  R
        L -> '*' R
          |  /[A-Za-z]+/
        R -> L
    EOG

    ast = nil
    assert_no_exception {ast = p.parse "myid"}
    assert_kind_of(SyntaxTree, ast)
    assert_equals(4, count_tree_nodes(ast))
    assert_equals("myid", ast[0][0][0].lexeme)
   
    assert_no_exception {ast = p.parse "x=y"}
    assert_kind_of(SyntaxTree, ast)
    assert_equals(7, count_tree_nodes(ast))
    assert_equals("x", ast[0][0].lexeme)
    assert_equals("=", ast[1].lexeme)
    assert_equals("y", ast[2][0][0].lexeme)

    assert_no_exception {ast = p.parse "*x"}
    assert_equals(7, count_tree_nodes(ast))
    assert_equals("*", ast[0][0][0].lexeme)
    assert_equals("x", ast[0][0][1][0][0].lexeme)

    assert_no_exception {ast = p.parse "x=*y"}
    assert_equals(10, count_tree_nodes(ast))
    assert_equals("=", ast[1].lexeme)
    assert_equals("x", ast[0][0].lexeme)
    assert_equals("*", ast[2][0][0].lexeme)
    assert_equals("y", ast[2][0][1][0][0].lexeme)

    assert_no_exception {ast = p.parse "*x=***y"}
  end

  def test_dragonbook_4_46_rockit_simplified
    p = Parse.generate_parser <<-'EOG'
      Grammar Dragon_book_4_46
       Productions
        S -> L '=' L
          |  L
        L -> '*'* /[A-Za-z]+/
    EOG

    assert_no_exception {ast = p.parse "myid"}
    assert_no_exception {ast = p.parse "x=y"}
    assert_no_exception {ast = p.parse "*x"}
    assert_no_exception {ast = p.parse "x=*y"}
    assert_no_exception {ast = p.parse "*x=***y"}
  end

  def test_dragonbook_4_34
    p = Parse.generate_parser <<-'EOG'
      Grammar Dragon_book_4_46
       Productions
        E -> E '+' T
          |  T
        T -> T '*' F
          |  F
        F -> '(' E ')'
          |  /[A-Za-z]+/
    EOG

    assert_no_exception {ast = p.parse "a"}
    assert_no_exception {ast = p.parse "a+b"}
    assert_no_exception {ast = p.parse "a+(b*c)"}
    assert_no_exception {ast = p.parse "a*(b+(c*d))"}
    assert_no_exception {ast = p.parse "a*b*c"}
  end

  def test_ambigous
    p = Parse.generate_parser <<-'EOG'
      Grammar Ambigous_grammar
       Productions
        E -> E '+' E
          |  /\d+/
    EOG

    assert_no_exception {ast = p.parse "1+2"}
    assert_exception(AmbigousParseException) {p.parse "1+2+3"}
  end

  def test_skip_token
    p = Parse.generate_parser <<-'EOG'
      Grammar Ambigous_grammar
       Tokens
        Blank = /\s+/ [:Skip]
        Int   = /\d+/
       Productions
        E -> E '+' E
          |  Int
    EOG

    assert_no_exception {ast = p.parse "1 + 2"}
    assert_no_exception {ast = p.parse " 1  +  2 "}
  end

  def test_list_operator
    p = Parse.generate_parser <<-'EOG'
      Grammar Ambigous_grammar
       Tokens
        Blank = /\s+/ [:Skip]
        Int   = /\d+/
       Productions
        S -> list(Int, ',')
    EOG

    ast = nil
    assert_no_exception{ast = p.parse "4,5,6"} 
    a = ast[0].as_a.map {|t| t.lexeme.to_i}
    assert_equals([4,5,6], a)

    assert_no_exception{ast = p.parse "4"} 
    a = ast[0].as_a.map {|t| t.lexeme.to_i}
    assert_equals([4], a)

    p = Parse.generate_parser <<-'EOG'
      Grammar Ambigous_grammar
       Tokens
        Blank = /\s+/ [:Skip]
        Int   = /\d+/
        Float = /\d+\.\d+/
       Productions
        S -> list((Int Float), ";:")
    EOG

    assert_no_exception{ast = p.parse "4 4.5;: 5 5.6;: 6 6.7 ;:7 7.8;: 8 8.9"} 
    a = ast[0].as_a.map {|a| a.map {|v| v.lexeme.to_f}}
    assert_equals([[4.0,4.5],[5.0,5.6],[6.0,6.7],[7.0,7.8],[8.0,8.9]], a)
  end

  def test_mult_operator
    p = Parse.generate_parser <<-'EOG'
      Grammar Mult_grammar
       Tokens
        Blank = /\s+/ [:Skip]
        Int   = /\d+/
       Productions
        S -> "." Int* [S: dot, ints]
    EOG

    ast = nil
    assert_no_exception{ast = p.parse ". 4 5 6"} 
    a = ast.ints.map {|t| t.lexeme.to_i}
    assert_equals([4,5,6], a)

    assert_no_exception{ast = p.parse "."} 
    assert_equals(0, ast.ints.length)

    p = Parse.generate_parser <<-'EOG'
      Grammar Mult_grammar
       Tokens
        Blank = /\s+/ [:Skip]
        Int   = /\d+/
        Float = /\d+\.\d+/
       Productions
        S -> "." Int* Float+ 
    EOG

    assert_no_exception{ast = p.parse ". 5.4"} 
    assert_equals(0, ast[1].length)
    assert_equals(1, ast[2].length)
    assert_equals(5.4, ast[2][0].lexeme.to_f)
  end

  def test_maybe_operator
    p = Parse.generate_parser <<-'EOG'
      Grammar Maybe_grammar
       Tokens
        Blank = /\s+/        [:Skip]
        Int   = /\d+/
       Productions
        S -> ".." Int? "."   [S: d1, mint, d2]
    EOG

    ast = nil
    assert_no_exception{ast = p.parse ".. 4 ."}
    assert_equals(4, ast.mint.lexeme.to_i)
    assert_no_exception{ast = p.parse ".. ."} 
    assert_equals(nil, ast.mint)

    p = Parse.generate_parser <<-'EOG'
      Grammar Maybe_grammar2
       Tokens
        Blank = /\s+/                 [:Skip]
        Int   = /\d+/
        Float = /\d+\.\d+/
       Productions
        S -> ".." (Int Float)? "."    [S: d1, mint, d2]
    EOG

    assert_no_exception{ast = p.parse ".. 4 5.6 ."}
    assert_equals(2, ast.mint.length)
    assert_equals(4, ast.mint[0].lexeme.to_i)
    assert_equals(5.6, ast.mint[1].lexeme.to_f)
  end

  def test_multiple_operators
    p = Parse.generate_parser <<-'EOG'
      Grammar Multiple_operators
       Tokens
        Blank = /\s+/                 [:Skip]
        Int   = /\d+/
        Id    = /[A-Za-z]+/
       Productions
        S -> Int* Id+ Int? "." (Int | Id) list(Int, ",")
    EOG

    ast = nil
    assert_no_exception{ast = p.parse "1 a 2 . 3 4,5,6"} 
    assert_equals(1, ast[0][0].lexeme.to_i)
    assert_equals("a", ast[1][0].lexeme)
    assert_equals(2, ast[2].lexeme.to_i)
    assert_equals(3, ast[4].lexeme.to_i)
    assert_equals(4, ast[5][0].lexeme.to_i)
    assert_equals(5, ast[5][1].lexeme.to_i)
    assert_equals(6, ast[5][2].lexeme.to_i)

    assert_no_exception{ast = p.parse "a b . c 1"} 
    assert_equals(0, ast[0].length)
    assert_equals("a", ast[1][0].lexeme)
    assert_equals("b", ast[1][1].lexeme)
    assert_equals(nil, ast[2])
    assert_equals("c", ast[4].lexeme)
    assert_equals(1, ast[5][0].lexeme.to_i)
  end

  def test_dangling_else_ambiguity
    # The GLR parser resolves the dangling else ambiguity without special
    # treatment!
    p = Parse.generate_parser <<-'EOG'
      Grammar DanglingElse
       Tokens
        Blank = /\s+/ [:Skip]
       Productions
        S -> "if" E "then" S ("else" S)?   [If: _, expr, _, first, opt2nd]
          |  "s"
        E -> /\d+/
    EOG
    
    ast = nil
    assert_no_exception {ast = p.parse "if 2 then s else s"}
    ast.compact!
    assert_equals(2, ast.opt2nd.length)
    assert_no_exception {ast = p.parse "if 2 then s"}
    ast.compact!
    assert_equals(nil, ast.opt2nd)
  end

  def test_multiple_identical_string_tokens
    p = Parse.generate_parser <<-'EOG'
      Grammar MultipleIdenticalStringTokens
       Tokens
        A     = /a/
       Productions
        S -> "-" S
          |  S "-" S
          |  A
    EOG

    assert_no_exception {ast = p.parse "a--a"}
  end

  def test_lifting_first_production
    p = Parse.generate_parser <<-'EOG'
      Grammar LiftingFirst
       Tokens
        Tok     = /B/
       Productions
        A -> B+        [^: bs]
        B -> Tok       [^: b]
    EOG

    ast = nil
    assert_no_exception {ast = p.parse "BBB"}
    ast.compact!
    assert_equals(3, ast.length)
  end

  def test_bug_plus2
    p = Parse.generate_parser <<-'EOG'
      Grammar WasBuggy
       Tokens
        Blank = /\s+/ [:Skip]
        Ar    = /(->)|(::=)|(:)/
        Sn    = /[A-Z][A-Za-z]*/
       Productions
        A -> B+        [^]
        B -> Sn Ar C
        C -> Sn+
    EOG

    ast = nil
    assert_no_exception { ast = p.parse <<-'EOS'
      K -> L
      L -> M N
     EOS
    }
    ast.compact!
    assert_equals(2, ast.length)
  end

  def test_bug_many_maybes
    p = Parse.generate_parser <<-'EOG'
      Grammar WasBuggy
       Tokens
        A = /a/
       Productions
        S -> "." A? "b"? "c"?
    EOG

    ast = nil
    assert_no_exception {ast = p.parse ".abc"}
    assert_equals([".", "a", "b", "c"], ast.map{|n| n.lexeme})

    assert_no_exception {ast = p.parse ".ab"}
    assert_equals([".", "a", "b", nil], ast.map{|n| n ? n.lexeme : n})

    assert_no_exception {ast = p.parse ".a"}
    assert_equals([".", "a", nil, nil], ast.map{|n| n ? n.lexeme : n})

    assert_no_exception {ast = p.parse "."}
    assert_equals([".", nil, nil, nil], ast.map{|n| n ? n.lexeme : n})
  end

  def test_bug_maybe_on_sequence
    p = Parse.generate_parser <<-'EOG'
      Grammar WasBuggy
       Productions
        S -> "." (/\d+/ "b")?
    EOG

    ast = nil
    assert_no_exception {ast = p.parse ".12b"}
    assert_equals(12, ast[1][0].lexeme.to_i)
    assert_equals("b", ast[1][1].lexeme)
    assert_no_exception {ast = p.parse "."}
    assert_equals(nil, ast[1])
  end

  def test_bug_list_and_lift
    p = Parse.generate_parser <<-'EOG'
      Grammar SupposedToBeBuggyButDoesntExhibitTheSameBug
       Productions
        S -> A
        A -> list(B, C) ';'*                   [^: bs, _]
        C -> (';' | '\n' | '\n\r' | '\r')+ 
        B -> "b"
    EOG

    ast = nil
    assert_no_exception {ast = p.parse "b"}
    assert_no_exception {ast = p.parse "b;;"}
  end

  def notready_test_bug_balanced_string    
  end

  def atest_bug_szegedy_multiple_unnamned_maybes
    p = Parse.generate_parser <<-'EOG'
    Grammar SimpleGrammar
      Tokens
        A       = /a/
        B       = /b/
      Productions
        Expr ->  A? B A?  [Expression: _, val, _]
    EOG

   ast = nil
   assert_no_exception {ast = p.parse "b"}
   assert_no_exception {ast = p.parse "aba"}
  end

end

# Run if we were directly called
if $0 == __FILE__
  testrunner = RUNIT::CUI::TestRunner.new
  if ARGV.size == 0
    suite = AcceptanceTestSmallGrammarsInText.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(AcceptanceTestSmallGrammarsInText.new(testmethod))
    end
  end
  testrunner.run(suite)
end

