require 'runit/testcase'
require 'runit/testsuite'
require 'runit/cui/testrunner'
require 'runit/topublic'
include RUNIT::ToPublic

require 'rockit/rockit' 

def count_tree_nodes(ast)
  count = 0
  ast.each_node {|n| count += 1}
  count
end

class AcceptanceTestSmallGrammarsInCode < RUNIT::TestCase
  def setup
    @tokens = [
      blank = t("Blank", /\s+/, :Skip),
      @int = t("Int", /\d+/),
      @float = t("Float", /\d+\.\d+/),
      @id = t("Id", /[A-Za-z]+/)
    ]
  end

  def parser(name, prods, tokens = [])
    g = Grammar.new(name, prods, tokens)
    Parse.parser_from_grammar(g)
  end

  def test_dragonbook_4_41
    prods = [
      prod(:S, [:B, :B]),
      prod(:B, ["a", :B]),
      prod(:B, ["b"])
    ]
    p = parser("Dragon_book_4_41", prods)

    ast = nil
    assert_no_exception {ast = p.parse("abab")}
    assert_kind_of(SyntaxTree, ast)
    assert_equals(9, count_tree_nodes(ast))

    assert_no_exception {ast = p.parse("aaabab")}
    assert_equals(13, count_tree_nodes(ast))

    assert_exception(ParseException) {p.parse("ab")}
  end

  def test_dragonbook_4_41_rockit
    prods = [
      prod(:S, [:B, :B]),
      prod(:B, [plus(/a/i), "b"]),
    ]
    p = parser("Dragon_book_4_41_rockit_simplified", prods)

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
    prods = [
      prod(:S, [:L, "=", :R]),
      prod(:S, [:R]),
      prod(:L, ["*", :R]),
      prod(:L, [@id]),
      prod(:R, [:L])
    ]
    p = parser("Dragon_book_4_46", prods, @tokens)

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
    prods = [
      prod(:S, [:L, "=", :L]),
      prod(:S, [:L]),
      prod(:L, [mult("*"), @id]),
    ]
    p = parser("Dragon_book_4_46_rockit_simplified", prods, @tokens)

    assert_no_exception {ast = p.parse "myid"}
    assert_no_exception {ast = p.parse "x=y"}
    assert_no_exception {ast = p.parse "*x"}
    assert_no_exception {ast = p.parse "x=*y"}
    assert_no_exception {ast = p.parse "*x=***y"}
  end

  def test_dragonbook_4_34
    productions = [
      prod(:E, [:E, "+", :T]),
      prod(:E, [:T]),
      prod(:T, [:T, "*", :F]),
      prod(:T, [:F]),
      prod(:F, ["(", :E, ")"]),
      prod(:F, [@id])
    ]
    p = parser("Dragon_book_4_34", productions, @tokens)
    
    assert_no_exception {ast = p.parse "a"}
    assert_no_exception {ast = p.parse "a+b"}
    assert_no_exception {ast = p.parse "a+(b*c)"}
    assert_no_exception {ast = p.parse "a*(b+(c*d))"}
    assert_no_exception {ast = p.parse "a*b*c"}
  end

  def test_ambigous
    productions = [
      prod(:E, [:E, "+", :E]),
      prod(:E, [@int])
    ]
    p = parser("Ambigous_grammar", productions, @tokens)

    ast = nil
    assert_no_exception {ast = p.parse "1+2"}
    assert_exception(AmbigousParseException) {ast = p.parse "1+2+3"}
  end

  def test_skip_token
    productions = [
      prod(:E, [:E, "+", :E]),
      prod(:E, [@int])
    ]
    p = parser("Ambigous_grammar", productions, @tokens)

    assert_no_exception {ast = p.parse "1 + 2"}
    assert_no_exception {ast = p.parse " 1  +  2 "}
  end

  def test_list_operator
    productions = [
      prod(:S, [liste(@int, ",")])
    ]
    p = parser("List_grammar", productions, @tokens)

    ast = nil
    assert_no_exception{ast = p.parse "4,5,6"} 
    a = ast[0].as_a.map {|t| t.lexeme.to_i}
    assert_equals([4,5,6], a)

    assert_no_exception{ast = p.parse "4"} 
    a = ast[0].as_a.map {|t| t.lexeme.to_i}
    assert_equals([4], a)

    productions = [
      prod(:S, [liste([@int, @float], ";:")])
    ]
    p = parser("List_grammar", productions, @tokens)
    assert_no_exception{ast = p.parse "4 4.5;: 5 5.6;: 6 6.7 ;:7 7.8;: 8 8.9"} 
    a = ast[0].as_a.map {|a| a.map {|v| v.lexeme.to_f}}
    assert_equals([[4.0,4.5],[5.0,5.6],[6.0,6.7],[7.0,7.8],[8.0,8.9]], a)
  end

  def test_mult_operator
    productions = [
      prod(:S, [".", mult(@int)], stb("S", [:dot, :ints]))
    ]
    p = parser("Mult_grammar", productions, @tokens)

    ast = nil
    assert_no_exception{ast = p.parse ". 4 5 6"} 
    a = ast.ints.map {|t| t.lexeme.to_i}
    assert_equals([4,5,6], a)

    assert_no_exception{ast = p.parse "."} 
    assert_equals(0, ast.ints.length)

    productions = [
      prod(:S, [".", mult(@int), plus(@float)])
    ]
    p = parser("Mult_grammar", productions, @tokens)

    assert_no_exception{ast = p.parse ". 5.4"} 
    assert_equals(0, ast[1].length)
    assert_equals(1, ast[2].length)
    assert_equals(5.4, ast[2][0].lexeme.to_f)
  end

  def test_maybe_operator
    productions = [
      prod(:S, ["..", maybe(@int), "."], stb("S", [:d1, :mint, :d2]))
    ]
    p = parser("Ambigous_grammar", productions, @tokens)

    ast = nil
    assert_no_exception{ast = p.parse ".. 4 ."}
    assert_equals(4, ast.mint.lexeme.to_i)
    assert_no_exception{ast = p.parse ".. ."} 
    assert_equals(nil, ast.mint)

    productions = [
      prod(:S, ["..", maybe([@int, @float]), "."], stb("S", [:d1, :mint, :d2]))
    ]
    p = parser("Ambigous_grammar", productions, @tokens)

    assert_no_exception{ast = p.parse ".. 4 5.6 ."}
    assert_equals(2, ast.mint.length)
    assert_equals(4, ast.mint[0].lexeme.to_i)
    assert_equals(5.6, ast.mint[1].lexeme.to_f)
  end

  def test_multiple_operators
    productions = [
      prod(:S, [mult(@int), plus(@id), maybe(@int), ".", ore(@int, @id), liste(@int, ",")])
    ]
    p = parser("Ambigous_grammar", productions, @tokens)

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
    productions = [
      prod(:S, ["if", :E, "then", :S, maybe(["else", :S])], 
	   stb(:If, [:_, :expr, :_, :first, :opt_2nd])),
      prod(:S, "s"),
      prod(:E, @int)
    ]
    p = parser("DanglingElse", productions, @tokens)
    
    ast = nil
    assert_no_exception {ast = p.parse "if 2 then s else s"}
    ast.compact!
    assert_equals(2, ast.opt_2nd.length)
    assert_no_exception {ast = p.parse "if 2 then s"}
    ast.compact!
    assert_equals(nil, ast.opt_2nd)
  end

  def test_multiple_identical_string_tokens
    productions = [
      prod(:S, ["-", :S]),
      prod(:S, [:S, "-", :S]),
      prod(:S, ["a"])
    ]
    p = parser("MultipleIdenticalStringTokens", productions)
    assert_no_exception {ast = p.parse "a--a"}
  end

  def test_lifting_first_production
    prods = [
      pa = prod(:A, [plus(:B)], stb(:^, [:bs])),
      prod(:B, ["B"], stb(:^, [:b]))
    ]
    p = parser("LiftingFirst", prods)
    
    ast = nil
    assert_no_exception {ast = p.parse "BBB"}
    ast.compact!
    assert_equals(3, ast.length)
  end

  def test_bug_plus2
    tokens = [
      t("Blank", /\s+/, :Skip),
      arrow = t("Ar", /(->)|(::=)|(:)/),
      symbol_name = t("SN", /[A-Z][A-Za-z]*/)
    ]
    prods = [
      prod(:A, [plus(:B)], stb(:^)),
      prod(:B, [symbol_name, arrow, :C]),
      prod(:C, [plus(symbol_name)])
    ]
    p, ast = parser("WasBuggy", prods, tokens), nil

    assert_no_exception { ast = p.parse <<-'EOS'
      K -> L
      L -> M N
     EOS
    }
    ast.compact!
    assert_equals(2, ast.length)
  end
end

# Run if we were directly called
if $0 == __FILE__
  testrunner = RUNIT::CUI::TestRunner.new
  if ARGV.size == 0
    suite = AcceptanceTestSmallGrammarsInCode.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(AcceptanceTestSmallGrammarsInCode.new(testmethod))
    end
  end
  testrunner.run(suite)
end

