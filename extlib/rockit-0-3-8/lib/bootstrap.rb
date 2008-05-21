# This is a version of the grammar for Rockit grammars in code. It is used
# to bootstrap the rockit grammar parser.
#
# NOTE: The handcoded grammar in this file may not be
#       fully in sync with the rockit-grammar.grammar. This is not
#       needed as long as it can be used to parse the file. However,
#       YOU SHOULD NOT USE THE HANDCODED GRAMMAR IN THIS FILE AS A REFERENCE
#       TO ROCKIT GRAMMARS.
#
# Overview of bootstrapping process:
#  1. Generate a parser (rp) for Rockit grammars from this in-code grammar.
#     The parser MAY NOT BE FULLY UP-TO-DATE but it must be able to parse
#     the rockit-grammar.grammar file.
#  2. Parse the rockit-grammar.grammar file with rp.
#  3. Generate source code for up-to-date rockit parser.
#
require 'rockit'
require 'base_extensions'

module Parse
  RockitTokens = [
    blank = t("Blank", /\s+/, :Skip),
    comment = t("Comment", /#.*$/, :Skip),
    string = t("String", /('((\\')|[^'])*')|("((\\")|[^"])*")/),
    regexp = t("Regexp", /\/((\\\/)|[^\/])*\/[iomx]*/),
    arrow = t("Arrow", /(->)|(::=)|(:)/),
    symbol_name = t("SymbolName", /[A-Z][A-Za-z]*/),
    production_reference = t("ProductionReference", /[A-Z][A-Za-z]*\d+/)
  ]
 
  RockitTokenProds = [
    prod(:Tokens, ['Tokens', plus(:TokenSpec)], stb(:^, [:_, :tokens])),
    prod(:TokenSpec, 
	 [symbol_name, '=', ore(string, regexp), maybe(:TokenOpts)],
	 stb(nil, [:tokenname, :_, :regexp, :options])),
    prod(:TokenOpts, ['[', /:Skip/i, ']'], stb(:^, [:_, :options, :_]))
  ]

  def rockit_tokens_parser
    Parse.parser_from_grammar(Grammar.new("RockitTokens", RockitTokenProds, 
					  RockitTokens))
  end

  RockitPrioritiesProds = [
    prod(:Priorities, ['Priorities', plus(:Priority)], stb(:^, [:_, :prios])),
    prod(:Priority, [ore('left(', 'right('), liste(:ProdRef, ','), ')'], 
	 stb(:Associativity, [:relation, :productionrefs, :_])),
    prod(:Priority, [:ProdRef, plus(ore('>', '='), :ProdRef)],
	 stb(:Precedence, [:first, :rest])),
    prod(:Priority, [:Priority, ','], stb(:^, [:prio, :_])),
    prod(:ProdRef, [ore(symbol_name, production_reference)], stb(:^))
  ]

  def rockit_priorities_parser
    Parse.parser_from_grammar(Grammar.new("RockitPriorities", 
					  RockitPrioritiesProds, 
					  RockitTokens))
  end

  RockitProductionsProds = [
    prod(:Productions, ['Productions', plus(:Prod)], 
	 stb(:Productions, [:_, :productions])),
    prod(:Prod, [symbol_name, arrow, liste(:Alt, '|')],
	 stb(nil, [:nonterminal, :_, :alts])),
    prod(:Alt, [plus(:Element), maybe(:AstSpec)], 
	 stb(nil, [:elements, :astspec])),
    prod(:Element, [symbol_name], stb(:^)),
    prod(:Element, [ore(string, regexp)], stb(:ImplicitToken, [:regexp])),
    prod(:Element, [:Element, '?'], stb(:Maybe, [:element, :_])),
    prod(:Element, [:Element, '+'], stb(:Plus, [:element, :_])),
    prod(:Element, [:Element, '*'], stb(:Mult, [:element, :_])),
    prod(:Element, ['(', liste(:Element, '|'), ')'], 
	 stb(:Or, [:_,:elements, :_])),
    prod(:Element, ['(', plus(:Element), ')'], 
	 stb(:Sequence, [:_, :elements, :_])),
    prod(:Element, ['list(', :Element, ',', :Element, ')'],
	 stb(:List, [:_, :element, :_, :delimiter])),
    prod(:AstSpec, ['[', maybe(:ProdSpec), maybe(:ElemSpecs), ']'],
	 stb(nil, [:_, :prodspec, :elemspecs, :_])),
    prod(:ElemSpecs, [': ', liste(:ElemSpec, ',')], stb(:^, [:_, :specs])),
    prod(:ElemSpec, [ore(/[a-z]+/, '_')], stb(:^)),
    prod(:ProdSpec, [ore(symbol_name, '^')], stb(:^, [:name])),
  ]

  def rockit_productions_parser
    Parse.parser_from_grammar(Grammar.new("RockitProductions", 
					  RockitProductionsProds, 
					  RockitTokens))
  end
  module_function :rockit_productions_parser

  RockitProds = [
    prod(:Grammar, 
	 ['Grammar', /[A-Za-z]+([-_]*[A-Za-z\d]+)*/, 
	   maybe(:Tokens), :Productions, maybe(:Priorities)], 
	 stb(:Grammar, [:_, :language, :tokens, :productions, :priorities]))
  ] + RockitTokenProds + RockitProductionsProds + RockitPrioritiesProds
  
  def rockit_grammars_bootstrap_parser
    Parse.parser_from_grammar(Grammar.new("RockitGrammar",
					  RockitProds, RockitTokens))
  end
  module_function :rockit_grammars_bootstrap_parser
end

if __FILE__ == $0
  $TIME_AND_PUTS_VERBOSE = true
  grammarfile = ARGV[0] || "rockit-grammar.grammar"
  parser_filename = ARGV[1] || "rockit_grammars_parser.rb"
  module_name = ARGV[3] || "Parse"
  parser_name = as_module_method_named(module_name, 
				       ARGV[2] || (ARGV[0] ? "parser" : 
						    "rockit_grammars_parser"))
  time_and_puts("Boostrapping by generating parser from handcoded grammar") {
    $bootstrap_parser = Parse.rockit_grammars_bootstrap_parser
  }
  Parse.generate_parser_from_file_to_file(grammarfile, parser_filename,
					  parser_name, module_name,
					  $bootstrap_parser)
end
