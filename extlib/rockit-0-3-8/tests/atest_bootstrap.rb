require 'runit/testcase'
require 'runit/testsuite'
require 'runit/cui/testrunner'
require 'runit/topublic'
include RUNIT::ToPublic

require 'rockit/bootstrap' 
include Parse

# We fake productions with Symbol objects so we add elements method
# returning an array whose length we can control
class Symbol
  def elements
    Array.new($l)
  end
end
$l = 3

class AcceptanceTestBootstrap < RUNIT::TestCase
  def atest_01_tokens
    p, ast = rockit_tokens_parser, nil
    assert_kind_of(GeneralizedLrParser, p)
    assert_match("/\/((\\\/)|[^\/])*\/[iomx]*/", RockitTokens[3].regexp)

    rockit_grammar_tokens_section = <<-'EOS'
      Tokens
       Blank		= /\s+/                          [:Skip]
       Comment	        = /#.*$/                         [:Skip]
       String	        = /('((\\')|[^'])*')|("((\\")|[^"])*")/
       Regexp           = /\/((\\\/)|[^\/])*\/[iomx]*/
       Arrow		= /(->)|(::=)|(:)/
       Int		= /\d+/
       SymbolName	= /[A-Z][A-Za-z]*/
    EOS

    assert_no_exception {ast = p.parse rockit_grammar_tokens_section}
    ast.compact!
    assert_equals(7, ast.length)

    tokens = rockit_tokens_eval(ast)
    assert_kind_of(Array, tokens)
    assert_equals(7, tokens.length)
    tokens.each {|token| assert_kind_of(Token, token)}

    # Create new parser from parsed tokens and make sure they can parse their
    # own definition!
    prods = [
      prod(:Tokens, ['Tokens', plus(:TokenSpec)], stb(:^, [:_, :tokens])),
      prod(:TokenSpec, 
	   [tokens[6], '=', ore(tokens[2], tokens[3]), maybe(:TokenOpts)],
	   stb(nil, [:token_name, :_, :regexp, :options])),
      prod(:TokenOpts, ['[', /:Skip/i, ']'], stb(:^, [:_, :options, :_]))
    ]
    bp = Parse.parser_from_grammar(Grammar.new("BootstrappedRockitTokens", 
					       prods, tokens))
    assert_no_exception {ast = bp.parse rockit_grammar_tokens_section}
    assert_equals(7, ast.length)
  end

  def assert_priorities(priorities, productions)
    productions.each do |prodref|
      assert_equals(true, priorities.in_some_conflict?(prodref))
    end
    um, e, mu, di, pl, mi = *productions

    # Check precedence relations
    assert_equals(true, priorities.higher_precedence?(um, e))
    assert_equals(true, priorities.higher_precedence?(um, mu))
    assert_equals(true, priorities.higher_precedence?(um, di))
    assert_equals(true, priorities.higher_precedence?(um, pl))
    assert_equals(true, priorities.higher_precedence?(um, mi))
    assert_equals(true, priorities.higher_precedence?(e, mu))
    assert_equals(true, priorities.higher_precedence?(e, di))
    assert_equals(true, priorities.higher_precedence?(e, pl))
    assert_equals(true, priorities.higher_precedence?(e, mi))
    assert_equals(true, priorities.higher_precedence?(mu, pl))
    assert_equals(true, priorities.higher_precedence?(mu, mi))
    assert_equals(true, priorities.higher_precedence?(di, pl))
    assert_equals(true, priorities.higher_precedence?(di, mi))
    assert_equals(true, priorities.equal_precedence?(mu, di))
    assert_equals(true, priorities.equal_precedence?(pl, mi))

    # Check associativity relations
    pr = priorities
    assert_equals(true, pr.left_or_non_associativity_conflict?(pl,2,pl))
    assert_equals(true, pr.left_or_non_associativity_conflict?(mi,2,mi))
    assert_equals(true, pr.left_or_non_associativity_conflict?(di,2,di))
    assert_equals(true, pr.left_or_non_associativity_conflict?(mu,2,mu))
    assert_equals(true, pr.right_or_non_associativity_conflict?(e,0,e))
    assert_equals(true, pr.left_or_non_associativity_conflict?(pl,2,mi))
    assert_equals(true, pr.left_or_non_associativity_conflict?(mi,2,pl))
    assert_equals(true, pr.left_or_non_associativity_conflict?(mu,2,di))
    assert_equals(true, pr.left_or_non_associativity_conflict?(di,2,mu))
  end

  def atest_02_priorities
    p, ast = rockit_priorities_parser, nil
    assert_kind_of(GeneralizedLrParser, p)

    example_priorities_section = <<-'EOS'
      Priorities
       right(Exp), left(Mult), left(Div), left(Plus), left(Minus)
       left(Plus, Minus), left(Mult, Div)
       UnaryMinus > Exp > Mult = Div > Plus = Minus
    EOS

    assert_no_exception {ast = p.parse example_priorities_section}
    ast.compact!
    assert_equals(8, ast.length)

    priorities = rockit_priorities_eval(ast)
    assert_kind_of(ProductionPriorities, priorities)
    assert_priorities(priorities, 
		      [:UnaryMinus, :Exp, :Mult, :Div, :Plus, :Minus])
    
    # Check mapping to productions
    prods = [
      um = prod(:E, ["-", :E], stb(:UnaryMinus)),
      e = prod(:E, [:E, "^", :E], stb(:Exp)),
      mu = prod(:E, [:E, "*", :E], stb(:Mult)),
      di = prod(:E, [:E, "/", :E], stb(:Div)),
      pl = prod(:E, [:E, "+", :E], stb(:Plus)),
      mi = prod(:E, [:E, "-", :E], stb(:Minus)),
    ]
    priorities = rockit_priorities_eval(ast, prods)
    assert_kind_of(ProductionPriorities, priorities)
    assert_priorities(priorities, [um, e, mu, di, pl, mi])
  end

  def test_03_production_reference_map
    prods = [
      um = prod(:E, ["-", :E], stb(:UnaryMinus)), # UnaryMinus
      e = prod(:E, [:E, "^", :E], stb(:Exp)),     # E2
      mu = prod(:E, [:E, "*", :E], stb(:Exp)),    # E3
      di = prod(:S, [:E, "/", :E], stb(:Exp)),    # S1
      pl = prod(:E, [:E, "+", :E], stb(:^)),      # E4
      mi = prod(:E, [:E, "-", :E], stb(:^)),      # E5
      d1 = prod(:S, [:E, ".", :E], stb(:^)),      # S2
      d2 = prod(:S, [:E, ":", :E], stb(:_)),      # S3
      d3 = prod(:S, [:E, ";", :E], stb(:_)),      # S4
    ]
    map = production_reference_map(prods)
    prod_to_name = map.invert
    assert_equals(:UnaryMinus, prod_to_name[um])
    assert_equals(:E2, prod_to_name[e])
    assert_equals(:E3, prod_to_name[mu])
    assert_equals(:S1, prod_to_name[di])
    assert_equals(:E4, prod_to_name[pl])
    assert_equals(:E5, prod_to_name[mi])
    assert_equals(:S2, prod_to_name[d1])
    assert_equals(:S3, prod_to_name[d2])
    assert_equals(:S4, prod_to_name[d3])
  end

  def atest_04_productions
    puts "\n  This test will take several seconds. Be patient..."
    num_tokens = RockitTokens.length
    p, ast = rockit_productions_parser, nil
    assert_kind_of(GeneralizedLrParser, p)
    assert_equals(num_tokens, RockitTokens.length)

    rockit_productions_section = <<-'EOS'
Productions
  Productions -> 'Productions' Prod+                [^: _,productions]
  Prod        -> SymbolName Arrow list(Alt, '|')    [nonterminal, _, alts]
  Alt         -> Element+ AstSpec?                  [elements, astspec]
  Element     -> SymbolName                         [^]
              |  (String | Regexp)                  [ImplicitToken: regexp]
              |  Element '?'                        [Maybe: element, _]
              |  Element '+'                        [Plus: element, _]
              |  Element '*'                        [Mult: element, _]
              |  '(' list(Element, '|') ')'         [Or: _, elements, _]
              |  '(' Element+ ')'                   [Sequence: _,elements,_]
              |  'list(' Element ',' Element ')'    [List: _, element, _, 
                                                           delimiter]
  AstSpec     -> '[' ProdSpec ': ' ElemSpecs ']'    [_,prodspec,_,elemspecs,_]
              |  '[' ProdSpec ']'                   [_,prodspec,_]
              |  '[' ElemSpecs ']'                  [_,elemspecs,_]
  ElemSpecs   -> list((/[a-z]+/ | '_'), ',')        [^: _, specs]
  ProdSpec    -> (SymbolName | '^')                 [^]
    EOS

    assert_no_exception {ast = p.parse rockit_productions_section}
    ast.compact!
    assert_equals(7, ast.productions.length)

    small_test = <<-'EOS'
     Productions
      A -> B C? D+ E* 
      B -> (F | G) list(H, I) [BNode: ore, liste]
    EOS
    assert_no_exception {ast = p.parse small_test}

    # Test substitutions
    tokens = [
      tC = t("C", /C/)
    ]
    prods = rockit_productions_eval(ast, tokens)
    assert_equals(2, prods.length)
    assert_equals("A", prods[0].nonterminal.inspect)
    assert_kind_of(MaybeElement, prods[0].elements[1])
    assert_kind_of(PlusElement, prods[0].elements[2])
    assert_kind_of(MultElement, prods[0].elements[3])
    assert_equals(tC, prods[0].elements[1].sub_elements[0])
    assert_equals("B", prods[1].nonterminal.inspect)
    assert_kind_of(OrElement, prods[1].elements[0])
    assert_kind_of(ListElement, prods[1].elements[1])
  end

  def assert_small_expression_grammar(ast)
    # Create the grammar
    seg_grammar = rockit_grammar_eval(ast)
    assert_kind_of(Grammar, seg_grammar)
    assert_kind_of(ProductionPriorities, seg_grammar.priorities)

    # Check a production
    plus_prod = seg_grammar.productions[2]
    assert_kind_of(Production, plus_prod)
    assert_equals("E", plus_prod.nonterminal.inspect)
    assert_equals(3, plus_prod.length)
    t = plus_prod.create_tree([1, :+, 2])
    assert_kind_of(SyntaxTree, t)
    assert_equals(["left", "_", "right"], t.children_names)
    assert_equals(1, t.left)
    assert_equals(2, t.right)
    assert_equals(:+, t[1])

    # Create the parser
    assert_equals("Calculator", seg_grammar.name)
    seg_parser = Parse.parser_from_grammar(seg_grammar)
    assert_kind_of(GeneralizedLrParser, seg_parser)

    # and try it on some simple sentences
    assert_no_exception {ast = seg_parser.parse "1+2+3"}
    assert_no_exception {ast = seg_parser.parse "1*2*3"}
    assert_no_exception {ast = seg_parser.parse "1+2*3"}
    assert_no_exception {ast = seg_parser.parse "1*2+3"}
    assert_no_exception {ast = seg_parser.parse "1+2/3"}
    assert_exception(ParseException) {seg_parser.parse "2*1.2"}
    assert_exception(ParseException) {seg_parser.parse "2+a"}
    assert_exception(AmbigousParseException) {ast = seg_parser.parse "1/2/3"}
  end

  def test_05_full_bootstrap_grammar
    puts "\n  This test will take a long time. Please be patient..."
    p, ast = rockit_grammars_bootstrap_parser, nil
    assert_kind_of(GeneralizedLrParser, p)

    small_example_grammar = <<-'EOS'
    Grammar Calculator
    Tokens
      Blank = /\s+/ [:Skip]
      Num   = /\d+/
    Productions
      E -> Num      [^]
        |  E '+' E  [Plus: left, _, right]
        |  E '*' E  [Mult: left, _, right]
        |  E '/' E  [Div: left, _, right]
    Priorities
      Div = Mult > Plus
      left(Plus), left(Mult)
    EOS
    assert_no_exception {ast = p.parse small_example_grammar}
    assert_equals(2, ast.tokens.length)
    assert_equals(1, ast.productions.productions.length)
    assert_equals(3, ast.priorities.length)
    ast.compact!
    # File.open("seg_result.graph", "w") {|f| f.write syntaxtree_as_dot_digraph(ast)}

    assert_small_expression_grammar(ast)

    # Lets parse the rockit grammar itself
    rg = nil
    File.open("lib/rockit-grammar.grammar", "r") {|f| rg = f.read}
    assert_no_exception {ast = p.parse rg}
    ast.compact!
    assert_equals(9, ast.tokens.length)
    assert_equals(15, ast.productions.productions.length)
    assert_equals(nil, ast.priorities)

    # generate the grammar and parser from it
    rockit_grammar = rockit_grammar_eval(ast)
    assert_kind_of(Grammar, rockit_grammar)
    assert_equals("RockitGrammar", rockit_grammar.name)
    rockit_parser = Parse.parser_from_grammar(rockit_grammar)
    assert_kind_of(GeneralizedLrParser, rockit_parser)

    # try it on the previous example
    assert_no_exception {ast = rockit_parser.parse small_example_grammar}
    # ast.compact!
    # File.open("bootstrapped_seg_result.graph", "w") {|f| f.write syntaxtree_as_dot_digraph(ast)}
    assert_small_expression_grammar(ast)

    # and now lets try the bootstrapping procedure on itself!
    assert_no_exception {ast = rockit_parser.parse rg}
    ast.compact!
    assert_equals(9, ast.tokens.length)
    assert_equals(15, ast.productions.productions.length)
    assert_equals(nil, ast.priorities)
  end
end

# Run if we were directly called
if $0 == __FILE__
  testrunner = RUNIT::CUI::TestRunner.new
  if ARGV.size == 0
    suite = AcceptanceTestBootstrap.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(AcceptanceTestBootstrap.new(testmethod))
    end
  end
  testrunner.run(suite)
end
