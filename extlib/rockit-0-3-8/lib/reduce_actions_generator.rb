# Find the reductions in a parse table for a StateGraph.
# 
# There can be reductions in all states with kernel items that are final, ie.
# where the position cannot be advanced any further 
# (example: State(3, [S -> R.])).
#
# For states with only one kernel item (which is final which is always the
# case if its only one) we should reduce for
# all terminals in the follow set of the nonterminal (Follow(S) in the 
# example above). These states are called consistent reduce states.
#
# When there are multiple kernel items and one (or several) of them is a final
# item we should reduce for terminals in Follow(state, nonterminal). These
# states are called inconsistent reduce states. An example is
# State(5, [S -> L .= R, R -> L.]) where we dont know whether to reduce
# or shift. Here, we should reduce by R -> L for all terminals in Follow(5, R).
#
# To reduce the amount of computation needed we use a hybrid, lazy approach.
# We calculate all first and follow sets for all symbols in the grammar.
# With this we can find the reductions for consistent reduce states. For the
# inconsistent ones we build the first to follow relations "from the back"
# (ie. from the actual follow sets we need) up to the first sets. This way
# we don't need to calculate relations that are not relevant.
#
require 'directed_graph'
require 'base_extensions'
require 'parsetable_generation'

require 'profiler'

#$PROFILE = true

class ReduceActionsGenerator
  def initialize(stateGraph, grammar, parseTable, allItems)
    @state_graph, @grammar, @parsetable = stateGraph, grammar, parseTable
    set_index_numbers
    precalc_items_at_nonterminal(allItems)
    @lalr_pair_factory = IndexableFactory.new(LaLrPair, 0)
    init_traverse_cache
  end

  def add_reduce_actions
    Profiler.start if $PROFILE
    add_actions_for_consistent_states
    add_actions_for_inconsistent_states
    @parsetable.compact!
    puts Profiler.profile_summary(true, true) if $PROFILE
    @parsetable
  end

  protected

  def init_traverse_cache
    num_states = @state_graph.nodes.length
    @traverse_cache = Array.new(num_states)
    num_states.times {|i| @traverse_cache[i] = Hash.new}
  end

  def set_index_numbers
    cnt = -1
    @grammar.terminals.each {|t| t.index_number = (cnt += 1)}
    @grammar.nonterminals.each {|nt| nt.index_number = (cnt += 1)}
    @grammar.productions.each_with_index {|p,i| p.index_number = i}
  end

  def add_actions_for_consistent_states
    Profiler.__enter__(:add_actions_for_consistent_states) if $PROFILE
    crs = @state_graph.consistent_reduce_states
    build_follow_sets(crs).each do |state|
      state.final_items.each do |item|
	prod = item.production
	add_reduce_action_for_terminalset(state, 
					  follow_set(prod.nonterminal), prod)
      end
    end
    Profiler.__leave__(:add_actions_for_consistent_states) if $PROFILE
  end

  def build_follow_sets(states)
    create_first_sets
    precalc_epsilon_derivation
    create_follow_sets
    build_relations # Currently we build the full relations graphs
    update_first_sets
    update_follow_sets
    states
  end

  def epsilon
    return @epsilon if @epsilon
    @epsilon = EpsilonToken.new
    @epsilon.index_number = @grammar.terminals.map{|t| t.index_number}.max+1
    @epsilon
  end

  def all_terminals
    @all_terminals || (@all_terminals = @grammar.terminals + [epsilon])
  end

  def precalc_epsilon_derivation
    @i2i_relations = DirectedGraph.new
    @grammar.productions.each do |production|
      first_element = production.elements.first
      unless first_element
	i2i_relation(@epsilon, production.nonterminal)
      end
    end
    propagate_terminal_sets([@epsilon], @i2i_relations) {|n| first_set(n)}
    @grammar.nonterminals.each do |nt| 
      nt.derives_epsilon = first_set(nt).include?(@epsilon)
    end
  end

  def update_first_sets
    propagate_terminal_sets(@i2i_relations.roots, @i2i_relations) {|n| first_set(n)}
  end

  def update_follow_sets
    @i2o_relations.each do |src, dests|
      next unless dests
      source_set = first_set(src)
      dests.uniq.each {|d| follow_set(d).update source_set}
    end
    propagate_terminal_sets(@o2o_relations.roots, @o2o_relations) {|n| follow_set(n)}
  end

  def create_first_sets
    nonterms = @grammar.nonterminals
    ntfs = @nonterminal_first_sets = Array.new(nonterms.length)
    nonterms.each {|nt| ntfs[nt.index_number] = new_terminal_set}
    terms = @grammar.terminals
    tfs = @terminal_first_sets = Array.new(nonterms.length)
    terms.each {|t| tfs[t.index_number] = new_terminal_set(t)}
  end

  def create_follow_sets
    nonterms = @grammar.nonterminals
    fs = @follow_sets = Array.new(nonterms.length)
    nonterms.each {|nt| fs[nt.index_number] = new_terminal_set}
  end

  def build_relations
    @i2o_relations = Hash.new
    @o2o_relations = DirectedGraph.new
    @productions_ending_with = Array.new
    i = elements = nonterminal = last_nonterm = len = update_first = nil
    @grammar.productions.each do |production|
      elements = production.elements
      nonterminal = production.nonterminal
      i, updating, len, last_nonterm = 0, true, elements.length, nil
      while i < len
	x = elements[i]
	i2i_relation(x, nonterminal) if updating
	i2o_relation(x, last_nonterm) if last_nonterm
	last_nonterm = x.nonterminal? ? x : nil
	updating = updating and x.nonterminal? and x.derives_epsilon?
	i += 1
      end
      i, updating = len-1, true
      while updating and i >= 0
	x = elements[i]
	if x.nonterminal?
	  o2o_relation(nonterminal, x)
	  update_productions_ending_with(x, production, elements[0...i])
	end
	updating = x.nonterminal? and x.derives_epsilon?
	i -= 1
      end
    end
    # Should be @grammar.original_start_symbol but this will always work
    # since Follow(S) includes Follow(S'). It will take care of the accept.
    i2o_relation(@grammar.eof_terminal, @grammar.start_symbol)
  end

  def i2i_relation(src, dest)
    @i2i_relations.link_nodes(src, dest)
  end

  def o2o_relation(src, dest)
    @o2o_relations.link_nodes(src, dest)
  end

  def i2o_relation(src, dest)
    a = @i2o_relations[src]
    if a
      a.push dest
    else
      @i2o_relations[src] = [dest]
    end
  end

  def new_terminal_set(terminal = nil)
    if terminal
      TerminalSet.new(all_terminals, [terminal])
    else
      TerminalSet.new(all_terminals)
    end
  end

  def first_set(symbol)
    if symbol.nonterminal?
      @nonterminal_first_sets[symbol.index_number]
    else
      @terminal_first_sets[symbol.index_number]
    end
  end

  def add_actions_for_inconsistent_states
    Profiler.__enter__(:add_actions_for_inconsistent_states) if $PROFILE
    irs = @state_graph.inconsistent_reduce_states
    build_lalr_follow_sets(irs).each do |state, pair|
      state.final_items.each do |item|
	add_reduce_action_for_terminalset(state, lalr_follow_set(pair),
					  item.production)
      end
    end
    Profiler.__leave__(:add_actions_for_inconsistent_states) if $PROFILE
  end

  def add_reduce_action_for_terminalset(state, terminalSet, production)
    Profiler.__enter__(:add_reduce_action_for_terminalset, state, terminalSet, production) if $PROFILE
    if production.nonterminal == @grammar.start_symbol
      a = [:ACCEPT, 0]
    else
      a = [:REDUCE, production.index_number]
    end
    @parsetable.add_action_for_terminalset(state.index_number, a, terminalSet)
    Profiler.__leave__(:add_reduce_action_for_terminalset) if $PROFILE
  end

  def propagate_terminal_sets(roots, graph = @relations, &getTerminalSet)
    Profiler.__enter__(:propagate_terminal_sets, roots, graph) if $PROFILE
    # Logothetis-Bermudez-style propagation. Does NOT exploit the fact that
    # many propagations share a majority of paths. Exploit if speed needed!
    # Note that this is not garantueed to work if there is a strongly connected
    # component with more than two nodes. The intermediate nodes (only linked
    # within the component) will not be updated after the backlink.
    # Solve by doing a real reachability graph instead.
    roots.each do |root|
      graph.each_reachable_node_once_breadth_first(root) do |parent|
	parent_fset = getTerminalSet.call(parent)
	graph.children(parent).each do |child|
	  getTerminalSet.call(child).update parent_fset
	end
      end
    end
    Profiler.__leave__(:propagate_terminal_sets) if $PROFILE
  end

  def build_lalr_follow_sets(states)
    Profiler.__enter__(:build_lalr_follow_sets) if $PROFILE
    follow_sets_needed = Array.new
    states.each do |state|
      state.final_items.each do |item|
	production = item.production
	src_state = back_traverse(state, production.elements)
	follow_sets_needed.push [state, 
	                         lalr_pair(src_state, production.nonterminal)]
      end
    end
    build_o2o_lalr_relations
    create_lalr_follow_sets
    update_lalr_follow_sets_with_direct_followers
    propagate_terminal_sets(@o2o_lalr_relations.roots, @o2o_lalr_relations)  {|n| lalr_follow_set(n)}
    Profiler.__leave__(:build_lalr_follow_sets) if $PROFILE
    follow_sets_needed
  end

  def o2o_lalr_relation(src, dest)
    @o2o_lalr_relations.link_nodes(src, dest)
  end

  LaLrPair = Struct.new("LaLrPair", :state, :nonterminal)
  class LaLrPair
    include Indexable
    def inspect
      "(#{state.index_number.inspect}, #{nonterminal.inspect})"
    end
  end
  
  def lalr_pair(state, nonterminal)
    @lalr_pair_factory.instance_with_args(state, nonterminal)
  end
  
  def precalc_items_at_nonterminal(allItems) # O(I)
    @items_at_nonterminal = Array.new
    allItems.each do |item|
      symbol = item.symbol
      if symbol and symbol.nonterminal?
	update_items_at_nonterminal(symbol, item)
      end
    end
    @items_at_nonterminal
  end
    
  def update_items_at_nonterminal(nonterminal, item)
    i = nonterminal.index_number
    if (a = @items_at_nonterminal[i])
      a.push item
    else
      @items_at_nonterminal[i] = [item]
    end
  end
  
  def items_at_nonterminal(nonterminal)
    @items_at_nonterminal[nonterminal.index_number]
  end
  
  def create_lalr_follow_sets
    Profiler.__enter__(:create_lalr_follow_sets) if $PROFILE
    lalr_pairs = @lalr_pair_factory.instances 
    @lalr_follow_sets = Array.new(lalr_pairs.length)
    lalr_pairs.each do |pair| 
      @lalr_follow_sets[pair.index_number] = new_terminal_set
    end
    Profiler.__leave__(:create_lalr_follow_sets) if $PROFILE
  end

  def lalr_follow_set(lalrPair)
    @lalr_follow_sets[lalrPair.index_number]
  end

  def update_lalr_follow_sets_with_direct_followers
    Profiler.__enter__(:update_lalr_follow_sets_with_direct_followers) if $PROFILE
    @lalr_pair_factory.instances.each do |pair|
      if pair.nonterminal == @grammar.start_symbol
	lalr_follow_set(pair).update first_set(@grammar.eof_terminal)
      end
      items = items_at_nonterminal(pair.nonterminal)
      next unless items
      items.each do |item|
	item.direct_following_symbols.each do |symbol|
	  lalr_follow_set(pair).update first_set(symbol)
	end
      end
    end
    Profiler.__leave__(:update_lalr_follow_sets_with_direct_followers) if $PROFILE
  end

  def productions_ending_with2(nonterminal, withLastPrefix = nil)
    if withLastPrefix
      @productions_ending_with[nonterminal.index_number][withLastPrefix.index_number]
    else
      @productions_ending_with_and_having_empty_prefix
    end
  end

  def update_productions_ending_with2(nonterminal, production, prefix)
    ntindex = nonterminal.index_number
    pindex = prefix.last.index_number
    new_element = [production, prefix]
    if a = @productions_ending_with[ntindex]
      if a2 = @productions_ending_with[ntindex][pindex]
	a2.push(new_element) unless a2.include?(new_element)
      else
	@productions_ending_with[ntindex][pindex] = [new_element]
      end
    else
      @productions_ending_with[ntindex] = Array.new
      @productions_ending_with[ntindex][pindex] = [new_element]
    end
  end

  def productions_ending_with(nonterminal)
      @productions_ending_with[nonterminal.index_number]
  end

  def update_productions_ending_with(nonterminal, production, prefix)
    ntindex = nonterminal.index_number
    new_element = [production, prefix]
    if a = @productions_ending_with[ntindex]
      a.push(new_element) unless a.index(new_element)
    else
      @productions_ending_with[ntindex] = [new_element]
    end
  end

  @@empty_array = Array.new

  # We discard prefixes where the first symbol is not valid directly
  # to save time in back_traverse.
  def productions_with_valid_prefixes(pair)
    Profiler.__enter__(:productions_with_valid_prefixes) if $PROFILE
    prods = productions_ending_with(pair.nonterminal)
    unless prods
      Profiler.__leave__(:productions_with_valid_prefixes) if $PROFILE
      return @@empty_array 
    end
    incoming = @state_graph.incoming_links_info(pair.state)
    res = prods.select do |prod, prefix| 
      prefix.length == 0 or incoming.include?(prefix.last)
    end
    Profiler.__leave__(:productions_with_valid_prefixes) if $PROFILE
    res
  end

  def build_o2o_lalr_relations
    Profiler.__enter__(:build_o2o_lalr_relations) if $PROFILE
    @o2o_lalr_relations = DirectedGraph.new
    current = 0
    while current < @lalr_pair_factory.instances.length
      pair = @lalr_pair_factory.instances[current]
      productions = productions_with_valid_prefixes(pair)
      productions.each do |prod, prefix|
	q = back_traverse(pair.state, prefix)
	if q
	  src_pair = lalr_pair(q, prod.nonterminal)
	  o2o_lalr_relation(src_pair, pair)
	end
      end
      current += 1
    end
    #puts "Num LaLrPairs = #{current.inspect}"
    Profiler.__leave__(:build_o2o_lalr_relations) if $PROFILE
  end

  def back_traverse(state, elements)
    Profiler.__enter__(:back_traverse, state, elements) if $PROFILE
    index = state.index_number
    dest = @traverse_cache[index][elements]
    unless dest
      begin
	dest = @state_graph.back_traverse(state, elements)
	@traverse_cache[index][elements] = dest
      rescue GraphTraversalException => e
	dest = nil
      end
    end
    Profiler.__leave__(:back_traverse) if $PROFILE
    dest
  end
  
  def follow_set(symbol)
    @follow_sets[symbol.index_number]
  end
end

class TerminalSet
  # With fast union and add operations. Actually its actually a general
  # implementation and should probably be called IndexableObjectSet.

  def initialize(allPossibleMembers, members = [], all = nil, max = nil)
    @all_possible_members = allPossibleMembers
    @max_index = max || allPossibleMembers.map{|m| m.index_number}.max
    @all = all || (2**(@max_index+1))-1
    @included = 0 # We represent as bit vector in Integer.
    members.each {|t| add(t)}
  end

  @@empty_array = Array.new

  def TerminalSet.new_from_integer(allPossibleMembers, included, all = nil,
				   max = nil)
    ts = new(allPossibleMembers, @@empty_array, all, max)
    ts.set_include_vector(included)
    ts
  end

  def add(terminal)
    raise ArgumentError unless @all_possible_members.include?(terminal)
    begin
      @included |= mask(terminal.index_number)
    rescue Exception
      puts "TerminalSet: #{@all_possible_members.inspect} but was #{terminal.inspect}"
    end
  end

  def update(other)
    @included |= other.to_i
  end

  def -(other)
    # 1 in result if 1 in @included and 0 in other =>
    #     
    TerminalSet.new_from_integer(@all_possible_members, 
				 @included & (@all - other.to_i), @all,
				 @max_index)
  end

  def empty?
    @included == 0
  end

  def terminals
    @all_possible_members.select {|t| index_included?(t.index_number)}
  end

  def index_included?(index)
    @included & mask(index) > 0
  end

  def include?(terminal)
    index_included?(terminal.index_number)
  end

  def inspect
    terminals.inspect
  end

  def each
    Profiler.__enter__(:TerminalSet_each) if $PROFILE
    @all_possible_members.each do |t| 
      yield(t) if index_included?(t.index_number)
    end
    Profiler.__leave__(:TerminalSet_each) if $PROFILE
  end
    
  def set_include_vector(newVector)
    @included = newVector
  end

  def to_i
    @included
  end

  protected

  @@masks = Array.new

  def mask(index)
    @@masks[index] || (@@masks[index] = (1<<index))
  end
end
