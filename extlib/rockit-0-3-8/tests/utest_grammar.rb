require 'runit/testcase'
require 'runit/testsuite'
require 'runit/cui/testrunner'
require 'runit/topublic'
include RUNIT::ToPublic

require 'rockit/grammar'

class TestProduction < RUNIT::TestCase
  def setup
    @e = NonTerminal.new "E"
    @n = NonTerminal.new :Num
    @i = NonTerminal.new :Int
    @dot = string_token(".")

    @tokens = [t("Int", /\d+/), t("Float", /(\d+\.\d+((e|E)-?\d+)?)/)]
    @ps = [
      @p1 = prod(:Numeric, [@tokens[0]]),
      @p2 = prod(:Numeric, [@tokens[1]])
    ]
    @pp = priorities(decreasing_precedence(@p1, @p2))
    @g1 = Grammar.new("Simplfied-Ruby-Numeric", @ps, @tokens, @pp)
  end

  def test_nonterminal_initialize
    assert_kind_of(NonTerminal, @e)
    assert_kind_of(NonTerminal, @n)
  end

  def test_nonterminal_inspect
    assert_equals("E", @e.inspect)
    assert_equals("Num", @n.inspect)
  end

  def test_nonterminal_equality
    assert_equals(NonTerminal.new("E"), @e) 
    assert_equals(NonTerminal.new("E"), @e)
  end
  
  def test_nonterminal_hash
    assert_equals(NonTerminal.new("E").hash, @e.hash)
    assert_equals("E".hash, @e.hash)
  end

  def test_make_element
    assert_kind_of(NonTerminal, make_element(@e))
    assert_kind_of(NonTerminal, make_element(:E))
    assert_kind_of(Token, make_element(@dot))
    assert_kind_of(Token, make_element("."))
  end

  def test_orelement_initialize
    assert_exception(ArgumentError) {OrElement.new([@e])}
    assert_no_exception {OrElement.new([@e, @n])}
  end

  def test_operatorelement_basic_inspect
    assert_equals("E+", PlusElement.new(@e).inspect)
    assert_equals("(E Num)+", PlusElement.new([@e,@n]).inspect)
    assert_equals("E*", MultElement.new(@e).inspect)
    assert_equals("(E Num)*", MultElement.new([@e,@n]).inspect)
    assert_equals("E?", MaybeElement.new(@e).inspect)
    assert_equals("(E Num)?", MaybeElement.new([@e,@n]).inspect)
    assert_equals("(E | Num)", OrElement.new([@e,@n]).inspect)
    assert_equals("list(E, \",\")", ListElement.new([@e]).inspect)
    assert_equals("list((E Num), \",\")", ListElement.new([@e,@n]).inspect)
  end

  def test_operatorelement_compound_inspect
    assert_equals("(E? | (E Num)*)+", 
		  plus(ore(maybe(:E),mult(:E,:Num))).inspect)
  end

  def test_nonterminal_to_src
    el = NonTerminal.new("E")
    assert_equals(el, eval(el.to_src))

    el = NonTerminal.new(:Expression)
    assert_equals(el, eval(el.to_src))
  end

  def test_multelement_initialize
    opel = MultElement.new(NonTerminal.new(:E))
    assert_kind_of(MultElement, opel)
    assert_equals([@e], opel.sub_elements)

    opel = MultElement.new([@e, @n])
    assert_kind_of(MultElement, opel)
    assert_equals([@e, @n], opel.sub_elements)

    opel = MultElement.new(@e, @n)
    assert_kind_of(MultElement, opel)
    assert_equals([@e, @n], opel.sub_elements)
  end

  def test_pluselement_normalization_denormalization
    p = Production.new(:E, [o = PlusElement.new([@n, @i]), ".", "a"])
    normalized, extra = o.normalize([p])
    assert_equals(1, normalized.length)
    assert_equals(2, extra.length)
    assert_match(normalized[0].elements[0].name, /Plus\d+/)
    assert_equals(normalized[0].elements[0], extra[0].nonterminal)
    assert_equals(normalized[0].elements[0], extra[1].nonterminal)
    assert_equals(normalized[0].elements[0], extra[0].elements[0])
    assert_equals(@n, extra[0].elements[1])
    assert_equals(@i, extra[0].elements[2])
    assert_equals(@n, extra[1].elements[0])
    assert_equals(@i, extra[1].elements[1])
    assert_kind_of(ArrayNodeBuilder, extra[0].tree_builder)
    assert_equals([1,2], extra[0].tree_builder.indices)
    assert_equals(0, extra[0].tree_builder.array_index)
    assert_kind_of(ArrayNodeBuilder, extra[1].tree_builder)
    assert_equals([0,1], extra[1].tree_builder.indices)
    assert_equals(nil, extra[1].tree_builder.array_index)

    t = extra[0].create_tree([extra[1].create_tree([1,2]), 3,4])
    ast = normalized[0].create_tree([t, ".", "a"])
    assert_kind_of(SyntaxTree, ast)
    assert_kind_of(ArrayNode, ast.plus)
    assert_equals(2, ast.plus.as_a.length)
    assert_equals([1,2], ast.plus[0])
    assert_equals([3,4], ast.plus[1])
  end

  def test_multelement_normalization
    p = Production.new(:E, [".", o = MultElement.new([@i]), "a"])
    normalized, extra = o.normalize([p])
    assert_equals(2, normalized.length)
    assert_equals(2, extra.length)
    assert_match(normalized[0].elements[1].name, /Mult\d+/)
    assert_equals(normalized[0].elements[1], extra[0].nonterminal)
    assert_equals(normalized[0].elements[1], extra[1].nonterminal)
    assert_equals(normalized[0].elements[1], extra[0].elements[0])
    assert_equals(p.elements.length-1, normalized[1].elements.length)
    assert_equals(@i, extra[0].elements[1])
    assert_equals(@i, extra[1].elements[0])
    assert_kind_of(ArrayNodeBuilder, extra[0].tree_builder)
    assert_equals([1], extra[0].tree_builder.indices)
    assert_equals(0, extra[0].tree_builder.array_index)
    assert_kind_of(ArrayNodeBuilder, extra[1].tree_builder)
    assert_equals([0], extra[1].tree_builder.indices)
    assert_equals(nil, extra[1].tree_builder.array_index)
  end

  def test_maybeelement_normalization
    p = Production.new(:E, [".", "a", o = MaybeElement.new([@i])],
		       SyntaxTreeBuilder.new("E", ["c0","c1","c2"]))
    normalized, extra = o.normalize([p])
    assert_equals(2, normalized.length)
    assert_equals(0, extra.length)
    assert_equals(@i, normalized[0].elements[2])
    assert_equals(p.elements.length-1, normalized[1].elements.length)
    assert_kind_of(SyntaxTreeBuilder, normalized[0].tree_builder)
    assert_kind_of(SyntaxTreeBuilder, normalized[1].tree_builder)
    assert_equals(["c0","c1"], normalized[1].tree_builder.active_childrens)

    p = Production.new(:E, [".", "a", o = MaybeElement.new([@i, @n])],
		       SyntaxTreeBuilder.new("E", ["c0","c1","c2"]))
    normalized, extra = o.normalize([p])
    assert_equals(2, normalized.length)
    assert_equals(0, extra.length)
    assert_equals(@i, normalized[0].elements[2])
    assert_equals(p.elements.length-1, normalized[1].elements.length)
    assert_kind_of(GroupingSyntaxTreeBuilder, normalized[0].tree_builder)
    assert_kind_of(SyntaxTreeBuilder, normalized[0].tree_builder)
    assert_equals(2..3, normalized[0].tree_builder.range)
    assert_equals(p.tree_builder, normalized[0].tree_builder.chained_builder)
    assert_kind_of(SyntaxTreeBuilder, normalized[1].tree_builder)
    assert_equals(["c0","c1"], normalized[1].tree_builder.active_childrens)

    e, f, g = NonTerminal.new(:E), NonTerminal.new(:F), NonTerminal.new(:G)
    p = Production.new(:D, [o = maybe(e), f, maybe(g)],
		       SyntaxTreeBuilder.new("D", ["c1","c2","c3"]))
    normalized, extra = o.normalize([p])
    assert_equals(4, normalized.length)
    assert_equals(0, extra.length)
    childrens = normalized.map {|p| p.tree_builder.active_childrens}.sort
    assert_equals([["c2"],["c1","c2"],["c2","c3"],["c1","c2","c3"]].sort,
		  childrens.sort)
  end

  def test_orelement_normalization
    p = Production.new(:E, [".", o = OrElement.new([@i, @n, @e]), "a"])
    normalized, extra = o.normalize([p])
    assert_equals(3, normalized.length)
    assert_equals(0, extra.length)
    assert_equals(@i, normalized[0].elements[1])
    assert_equals(@n, normalized[1].elements[1])
    assert_equals(@e, normalized[2].elements[1])
  end

  def test_listelement_normalization
    p = Production.new(:E, [o = ListElement.new([@i], @dot)])
    normalized, extra = o.normalize([p])
    assert_equals(2, normalized.length)
    assert_equals(2, extra.length)
    assert_match(normalized[0].elements[1].name, /List\d+/)
    assert_equals(normalized[0].elements[1], extra[0].nonterminal)
    assert_equals(normalized[0].elements[1], extra[1].nonterminal)
    assert_equals(normalized[0].elements[1], extra[0].elements[0])
    assert_equals(2, normalized[0].elements.length)
    assert_equals(1, normalized[1].elements.length)
    assert_equals(@dot, extra[0].elements[1])
    assert_equals(@i, extra[0].elements[2])
    assert_equals(@dot, extra[1].elements[0])
    assert_equals(@i, extra[1].elements[1])
    assert_kind_of(ArrayNodeBuilder, normalized[0].tree_builder)
    assert_kind_of(ArrayNodeBuilder, extra[0].tree_builder)
    assert_equals([2], extra[0].tree_builder.indices)
    assert_equals(0, extra[0].tree_builder.array_index)
    assert_kind_of(ArrayNodeBuilder, extra[1].tree_builder)
    assert_equals([1], extra[1].tree_builder.indices)
    assert_equals(nil, extra[1].tree_builder.array_index)

    p = Production.new(:E, [".", o = ListElement.new([@i, @n], @dot), "a"])
    normalized, extra = o.normalize([p])
    assert_equals(2, normalized.length)
    assert_equals(2, extra.length)
    assert_match(normalized[0].elements[3].name, /List\d+/)
    assert_equals(normalized[0].elements[3], extra[0].nonterminal)
    assert_equals(normalized[0].elements[3], extra[1].nonterminal)
    assert_equals(normalized[0].elements[3], extra[0].elements[0])
    assert_equals(4, normalized[1].elements.length)
    assert_equals(@dot, extra[0].elements[1])
    assert_equals(@i, extra[0].elements[2])
    assert_equals(@n, extra[0].elements[3])
    assert_equals(@dot, extra[1].elements[0])
    assert_equals(@i, extra[1].elements[1])
    assert_equals(@n, extra[1].elements[2])
    assert_kind_of(ArrayNodeBuilder, extra[0].tree_builder)
    assert_equals([2,3], extra[0].tree_builder.indices)
    assert_equals(0, extra[0].tree_builder.array_index)
    assert_kind_of(ArrayNodeBuilder, extra[1].tree_builder)
    assert_equals([1,2], extra[1].tree_builder.indices)
    assert_equals(nil, extra[1].tree_builder.array_index)
  end

  def test_pluselement_to_src
    pe = PlusElement.new(@i, "aa", @n)
    assert_equals(pe, eval(pe.to_src))
  end

  def test_multelement_to_src
    me = MultElement.new(@i, @n, "aa")
    assert_equals(me, eval(me.to_src))
  end

  def test_maybeelement_to_src
    me = MaybeElement.new(@i, @n, "aa")
    assert_equals(me, eval(me.to_src))
  end

  def test_orelement_to_src
    me = OrElement.new(@i, @n, "aa")
    assert_equals(me, eval(me.to_src))
  end

  def test_listelement_to_src
    le = ListElement.new([@i, @n, "aaa"], ".")
    assert_equals(le, eval(le.to_src))
  end

  def test_production_to_src
    pr = Production.new(@e, [@n, @dot])
    assert_equals(pr, eval(pr.to_src))
  end

  def test_autoassignment_of_children_names
    gvar = Token.new("GlobalVariable", /@@\w+/)
    p = Production.new(@e, [@n, gvar, gvar])
    assert_equals(["num", "globalvariable", "globalvariable2"],
		  p.tree_builder.children_names)
  end

  def test_production_initialize_with_nonterminals_and_terminals
    p = Production.new(@e, [@n, @dot])
    assert_equals(@e, p.nonterminal)
    assert_equals(@n, p.elements[0])
    assert_equals(@dot, p.elements[1])
    assert_equals(SyntaxTreeBuilder, p.tree_builder.type)
    assert_equals(["num", "c2"], p.tree_builder.children_names)

    p = Production.new(@e, [@n, @dot], stb("_", ["num", "dot"]))
    assert_equals(SyntaxTreeBuilder, p.tree_builder.type)
    assert_equals("E", p.tree_builder.node_name)
    assert_equals(["num", "dot"], p.tree_builder.children_names)

    p = Production.new(@e, [@n, @dot], stb(nil, ["num", "dot"]))
    assert_kind_of(SyntaxTreeBuilder, p.tree_builder)
    assert_equals("E", p.tree_builder.node_name)

    p = Production.new(@e, [@n, @dot], stb("", ["num", "dot"]))
    assert_kind_of(SyntaxTreeBuilder, p.tree_builder)
    assert_equals("E", p.tree_builder.node_name)

    p = Production.new(@e, [@dot, @n, @i, @dot])
    assert_equals(SyntaxTreeBuilder, p.tree_builder.type)
    assert_equals(["c1", "num", "int", "c4"], p.tree_builder.children_names)

    p = Production.new(@e, [@dot, @n, @i, @dot], :^)
    assert_equals(LiftingSyntaxTreeBuilder, p.tree_builder.type)
    assert_equals(["_", "num", "int", "_"], p.tree_builder.children_names)

    p = Production.new(@e, [@dot, @n, @i, @dot], "^")
    assert_equals(LiftingSyntaxTreeBuilder, p.tree_builder.type)
    assert_equals(["_", "num", "int", "_"], p.tree_builder.children_names)

    symbol_name = t("SymbolName", /[A-Z][A-Za-z]*/)
    arrow, p = t("Arrow", /(->)|(::=)|(:)/), nil
    assert_no_exception{ p = prod(:B, [symbol_name, arrow, liste(:C, '|')]) }
    assert_equals(["symbolname", "arrow", "list"], 
		  p.tree_builder.children_names)
  end

  def test_production_initialize_with_symbols
    p = Production.new(:E, [:Num, "."])
    assert_equals(@e, p.nonterminal)
    assert_equals(@n, p.elements[0])
    assert_equals(@dot, p.elements[1])
  end

  def test_production_initialize_with_only_tokens
    p = Production.new(:E, [StringToken.new("d")])
    assert_equals(@e, p.nonterminal)
    assert_equals(StringToken.new("d"), p.elements[0])
    assert_equals("c1", p.tree_builder.children_names[0])
  end

  def test_production_clone_and_substitute
    o = PlusElement.new([@n])
    p = Production.new(:E, [o, ".", "a"])
    p2 = p.clone_and_substitute(o, [@dot, @i])
    assert_equals(@e, p2.nonterminal)
    assert_equals(@dot, p2.elements[0])
    assert_equals(@i, p2.elements[1])
    assert_equals(@dot, p2.elements[2])

    p3 = p2.clone_and_substitute(@n, [@i])
    assert_equals(p2, p3)
  end

  def test_production_normalize
    p = Production.new(:E, [".", o = ListElement.new([@i, @n], @dot),
		            MaybeElement.new([@e]), PlusElement.new([@i]),
			    "aaa"])
    n = p.normalize
    # List = (2 norm + 2 extra), Maybe = (2 norm), Plus = (1 norm + 2 extra)
    # We expect 2*2*1 + 2 + 0 + 2 = 8
    assert_equals(8, n.length)
  end

  def test_production_create_tree
    p = Production.new(@e, [@dot, @n, @i, @dot], :^)
    t = p.create_tree([1,2,3,4])
    assert_equals(2, t)

    t = p.create_tree([1,2])
    assert_equals(2, t)

    # Hur bygga träd och speca namn när man har komplexa element med 
    # operatorer?
  end

  def test_element_normalize
    p = Production.new(:E, [".", o = ListElement.new([@i, @n], @dot),
		            MaybeElement.new([@e]), PlusElement.new([@i]),
			    "aaa"])
    normalized, extra = @e.normalize([p])
    assert_equals(p, normalized[0])
  end

  def test_grammar
    tokens = [it = t("Int", /\d+/), ft = t("Float", /\d+\.\d+/)]
    p = Production.new(:E, [".", o = ListElement.new([@i, @n], @dot),
		            MaybeElement.new([@e]), PlusElement.new([@i]),
			    "aaa"])
    g = 
      Grammar.new("TestGrammar1",
		  [prod(:E, [:Num, @dot]),
		   prod(:Num, [it]),
		   prod(:Num, [ft]), 
		   p],
		  tokens)

    g.normalize
    assert_equals(8+3, g.productions.length)

    # Normalizing a 2nd time has no effect...
    g.normalize
    assert_equals(8+3, g.productions.length)
  end

  def test_nested_operatorelements
    p = Production.new(:E, [o = ListElement.new(PlusElement.new([@i]), @dot),
		            MaybeElement.new([@e]), "aaa"])
    pn = p.normalize
    assert_equals(8, pn.length)
    pn.each do |production|
      production.elements.each {|e| assert(!e.kind_of?(OperatorElement))}
    end
  end

  def test_grammar_initialize
    g = Grammar.new
    assert_kind_of(Grammar, g)

    g = Grammar.new("", [])
    assert_kind_of(Grammar, g)

    g = Grammar.new("", [], [])
    assert_kind_of(Grammar, g)
  end

  def test_grammar_addition
    g2 = Grammar.new("Simplified-Ruby-Expression",
		     [prod(:Expr, [:Numeric, "+", :Numeric]),
		      prod(:Expr, [:Numeric, "*", :Numeric])], [], :Expr)

    g2 += @g1
  end

  def test_grammar_alternatives
    assert_equals(2, (a = @g1.alternatives(NonTerminal.new("Numeric"))).length)
    assert_equals(@p1, a[0])
    assert_equals(@p2, a[1])
  end

  def test_grammar_nonterminals
    assert_equals([NonTerminal.new("Numeric")], @g1.nonterminals)
    @g1.augment
    assert_equals([@g1.start_symbol, NonTerminal.new("Numeric")], 
		  @g1.nonterminals)
    assert_equals([NonTerminal.new("Numeric")], @g1.nonterminals(false))
  end

  def test_grammar_augment
    assert_equals(false, @g1.augmented?)
    old_start_symbol, plen = @g1.start_symbol, @g1.productions.length
    tlen = @g1.tokens.length
    @g1.augment
    assert_equals(true, @g1.augmented?)
    assert_equals(plen+1, @g1.productions.length)
    assert_equals(@g1.start_symbol, @g1.productions.first.nonterminal)
    @g1.unaugment
    assert_equals(false, @g1.augmented?)
    assert_equals(old_start_symbol, @g1.start_symbol)
    assert_equals(plen, @g1.productions.length)
    assert_equals(tlen, @g1.tokens.length)
  end

  def test_production_length
    p = Production.new(:E, [o = ListElement.new(PlusElement.new([@i]), @dot),
		            MaybeElement.new([@e]), "aaa"])
    assert_equals(3, p.length)
  end

  def test_prod_with_strings_as_nonterminal
    pr = prod("S", ["a"])
    assert_kind_of(Production, pr)
    assert_equals(NonTerminal.new("S"), pr.nonterminal)
  end

  def test_grammar_priorities
    assert_kind_of(ProductionPriorities, @g1.priorities)
  end

  def test_grammar_multiple_identical_string_tokens
    prods = [
      p1 = prod(:S, [string_token("-"), :S]), 
      p2 = prod(:S, [:S, string_token("-"), :S])
    ]
    g = Grammar.new("T", prods)
    assert_equals(2, g.tokens.length) # EOF has been added
    assert(p1.elements[0] == p2.elements[1])
    assert(p1.elements[0].id == p2.elements[1].id)
  end

  def test_grammar_multiple_identical_nonterminals
    c1 = NonTerminal.new("C")
    c2 = NonTerminal.new("C")
    prods = [
      p1 = prod(:S, [c1,c2]),
      p2 = prod(:C, ["c"])
    ]
    g = Grammar.new("T", prods)
    assert_equals(2, g.nonterminals.length)
    assert(p1.elements[0] == p1.elements[1])
    assert(p1.elements[0].id == p1.elements[1].id)
    assert(p1.elements[0] == p2.nonterminal)
    assert(p1.elements[0].id == p2.nonterminal.id)
  end    
end

# Run if we were directly called
if $0 == __FILE__
  testrunner = RUNIT::CUI::TestRunner.new
  if ARGV.size == 0
    suite = TestProduction.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestProduction.new(testmethod))
    end
  end
  testrunner.run(suite)
end

