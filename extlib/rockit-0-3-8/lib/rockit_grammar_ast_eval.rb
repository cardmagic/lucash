
module Parse
  # Evaluate a Rockit tokens section and return an array with the tokens
  def rockit_tokens_eval(tokensAst)
    tokens = Array.new
    tokensAst.each do |node|
      if node.name == "TokenSpec"
	# VERY simple handling of options right now since only one is valid.
	# Needs to be generalized...
	if node.options
	  tokens.push t(node.tokenname.lexeme, 
			eval(node.regexp.lexeme), :Skip)
	else
	  tokens.push t(node.tokenname.lexeme, eval(node.regexp.lexeme))
	end
      end
    end
    tokens
  end
  module_function :rockit_tokens_eval

  # Evaluate priorities section. If given an array with productions
  # we try to map the given SymbolNames to productions; if not we simply use
  # the symbol names (as Ruby symbols) directly and the mapping must be done 
  # later.
  def rockit_priorities_eval(prioritiesAst, productions = nil)
    relations = priorities_as_relations(prioritiesAst).flatten
    relations = map_symbolnames_to_productions(relations, productions)
    ProductionPriorities.new(relations)
  end
  module_function :rockit_priorities_eval

  def priorities_as_relations(prioritiesAst)
    relations = Array.new
    prioritiesAst.each do |node|
      case node.name
      when "Associativity"
	# We use the left and right shorthand funcs defined in 
	# conflict_resolution.rb.
	prod_refs = node.productionrefs.map {|sn| ":" + sn.lexeme}
	relations << eval(node.relation.lexeme + prod_refs.join(',') + ')')
      when "Precedence"
	left = node.first
	node.rest.childrens.each do |relation, right|
	  if relation.lexeme == '>'
	    relations.push decreasing_precedence(left.lexeme.intern, 
						 right.lexeme.intern)
	  elsif relation.lexeme == '='
	    relations.push equal_precedence(left.lexeme.intern, 
					    right.lexeme.intern)
	  end
	  left = right
	end
      end
    end
    relations
  end
  module_function :priorities_as_relations

  # Maps symbolnames used in priorities section to the corresponding 
  # productions. The production reference is determined by the rules:
  #  1. It is the AST node name of the productions tree_builder if it is
  #       unique and not "^" or "_" or nil, or
  #  2. It is the nonterminal name + an index starting at 1 for each unique
  #       nonterminal.
  def map_symbolnames_to_productions(relationsWithStringsAsProductions,
				     productions)
    return relationsWithStringsAsProductions unless productions
    map = production_reference_map(productions)
    relationsWithStringsAsProductions.map do |relation|
      new_left, new_right = map[relation.left], map[relation.right]
      if nil == new_left or nil == new_right
	raise "Could not map #{relation.inspect} to the productions involved"
      end
      relation.left, relation.right = new_left, new_right
      relation
    end
  end
  module_function :map_symbolnames_to_productions

  def production_reference_map(productions)
    map, production_counts = Hash.new, Hash.new(0)
    ast_name_counts, nonterminal_counts = Hash.new(0), Hash.new(0)

    # Pass 1 to get the count of the AST names and assign numbers
    productions.each do |p| 
      a = ast_name_counts[p.tree_builder.node_name] += 1
      b = nonterminal_counts[p.nonterminal.name] += 1
      production_counts[p] = [a, b]
    end

    # Pass 2 to assign the names
    productions.each do |production|
      ast_name = production.tree_builder.node_name
      if ["^", "_", :_, :^, nil].include?(ast_name) or
	  ast_name_counts[ast_name] != 1
	ref = production.nonterminal.name + 
	  production_counts[production].last.inspect
	map[ref.intern] = production
      else
	map[ast_name.intern] = production
      end
    end

    map
  end
  module_function :production_reference_map

  # Return Array with the productions from a productionsAST.
  # If an Array with tokens are given the token with the correct name is
  # substituted for the name. If no matching token is found a Ruby Symbol
  # is inserted.
  def rockit_productions_eval(productionsAst, tokens = [])
    evaluator = RockitProductionsEvaluator.new(tokens)
    evaluator.eval_ast(productionsAst)
  end
  module_function :rockit_productions_eval

  class RockitProductionsEvaluator
    def initialize(tokens = [])
      @token_map = Hash.new
      tokens.each {|token| @token_map[token.name] = token}
    end  
    
    def eval_ast(ast)
      return nil unless ast
      case ast.name
      when "Productions"
	ast.productions.map {|prod| eval_ast(prod)}.flatten
      when "Prod"
	nonterminal = eval_ast(ast.nonterminal)
	ast.alts.map do |alt| 
	  elements, treespec = eval_ast(alt)
	  prod(nonterminal, elements, treespec)
	end
      when "Alt"
	[ast.elements.map {|e| eval_ast(e)}, 
	  eval_ast(ast.astspec)]
      when "SymbolName"
	symbol = ast.lexeme
	@token_map[symbol] || symbol.intern
      when "ImplicitToken"
	eval(ast.regexp.lexeme) # Eval to String or Regexp
      when "Maybe"
	maybe(eval_ast(ast.element))
      when "Plus"
	plus(eval_ast(ast.element))
      when "Mult"
	mult(eval_ast(ast.element))
      when "Or"
	ore(*(ast.elements.map {|e| eval_ast(e)}))
      when "Sequence"
	ast.elements.map {|e| eval_ast(e)}
      when "List"
	liste(eval_ast(ast.element), eval_ast(ast.delimiter))
      when "AstSpec"
	if ast.elemspecs
	  elemspecs = ast.elemspecs.map {|e| e.lexeme.intern}
	else
	  elemspecs = []
	end
	if ast.prodspec
	  prodspec = (ast.prodspec.lexeme == "^" ? :^ :eval_ast(ast.prodspec))
	else
	  prodspec = nil
	end
	stb(prodspec, elemspecs)
      when "Lift"
	:^
      end
    end
  end

  def rockit_grammar_eval(grammarAst)
    if grammarAst.tokens
      tokens = rockit_tokens_eval(grammarAst.tokens)
    else
      tokens = []
    end
    productions = rockit_productions_eval(grammarAst.productions, tokens)
    if grammarAst.priorities
      priorities = rockit_priorities_eval(grammarAst.priorities, productions)
    end
    Grammar.new(grammarAst.language.lexeme, productions, tokens, priorities)
  end
  module_function :rockit_grammar_eval
end
