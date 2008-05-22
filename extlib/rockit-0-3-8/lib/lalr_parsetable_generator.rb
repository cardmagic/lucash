require 'grammar'
require 'parse_table'
require 'memoize' # COMMENT OUT if you don't have Memoize
require 'parsetable_generation'
require 'reduce_actions_generator'

require 'profiler'

module Parse
  class StateGraph < BackLinkedDirectedGraph
    attr_reader :start_state
    attr_reader :consistent_reduce_states, :inconsistent_reduce_states

    def initialize(startState, *args)
      super(*args)
      @start_state = startState
      @consistent_reduce_states = Array.new
      @inconsistent_reduce_states = Array.new
    end

    def add_node(state)
      super(state)
      if state.reduce_state?
	if state.consistent?
	  a = @consistent_reduce_states
	else
	  a = @inconsistent_reduce_states
	end
	a.push state unless a.include?(state)
      end
    end

    @@node_labeler_with_kernels = proc{|state|
      "S" + state.index_number.inspect + ": " + state.kernel_items.inspect
    }

    @@node_labeler = proc{|state|
      "S" + state.index_number.inspect
    }

    def to_postscript_file(filename, withKernelItems = true)
      super(filename, nil, 
	    withKernelItems ? @@node_labeler_with_kernels : @@node_labeler )
    end
  end

  class LaLr1ParseTableGenerator
    def initialize(grammar, 
		   lookaheadCalculatorKlass = 
		     ReduceActionsGenerator)
      @grammar, @lookahead_calculator_klass = grammar, lookaheadCalculatorKlass
    end

    def generate_parse_table(parseTableKlass = ParseTable)
      @grammar.augment
      #puts "\n  NoN = #{@grammar.productions.map{|p| p.nonterminal}.uniq.length}"
      #puts "  NoP = #{@grammar.productions.length}"
      time_and_puts("\n  Normalizing grammar") {
	@grammar.normalize
      }
      #puts "  NoPN = #{@grammar.productions.length}"
      state_graph = nil
      time_and_puts("  Calculating states") {
	@item_factory = IndexableFactory.new(Item, 0)
	@state_factory = IndexableFactory.new(LrState, 0)
	precalc_nonkernel_items_for_nonterminals
	@parse_table = parseTableKlass.new_from_grammar(@grammar)
	state_graph = calculate_state_graph # also adds gotos and shift actions
      }
      time_and_puts("  Calculating lalr1_lookaheads") {      
	lookahead_alg = 
	  @lookahead_calculator_klass.new(state_graph, @grammar, @parse_table, 
					  @item_factory.instances)
	lookahead_alg.add_reduce_actions
      }
      @parse_table
    end

    def test_nonterminal_uniqueness
      nts = DefaultInitHash.new {|k| Array.new}
      @grammar.productions.each do |production|
	nts[production.nonterminal.name].push(production.nonterminal.id)
	nts[production.nonterminal.name].uniq!
	production.elements.each do |e| 
	  if e.nonterminal?
	    nts[e.name].push(e.id)
	    nts[e.name].uniq!
	  end
	end
      end
      puts nts.inspect
    end

    protected

    def precalc_nonkernel_items_for_nonterminals
      @grammar.nonterminals.each do |nt| 
	nt.calc_nonkernel_items(@grammar, @item_factory)
      end
    end

    def add_state_unless_exists(kernel_items)
      @state_factory.make_unless_exists(kernel_items)
    end

    # Calculate the state graph by constructing the sets-of-lr0-items 
    # collection.
    # See page 224 (basic algorithm) and 240 (representing the states by
    # their kernel items) in the Dragon book
    def calculate_state_graph
      Profiler.__enter__(:calculate_state_graph) if $PROFILE
      start_item = @item_factory.make(@grammar.productions[0], 0)
      state_graph = StateGraph.new(add_state_unless_exists([start_item]).first)
      states, current = [state_graph.start_state], 0
      dest_sets, next_item = DefaultInitHash.new {|k| Array.new}, nil
      while current < states.length
	state = states[current]
	dest_sets.clear
	state.closure.each do |item|
	  symbol = item.symbol
	  if symbol
	    next_item = item.next_item
	    dest_sets[symbol].push(next_item) if next_item
	  end
	end
	dest_sets.each do |symbol, kernel_item_set|
	  kernel_item_set.uniq! # Needed?
	  dest_state, new_state = add_state_unless_exists(kernel_item_set)
	  states.push(dest_state) if new_state
	  state_graph.add_link(state, dest_state, symbol)
	  if symbol.nonterminal?
	    @parse_table.add_goto(state.index_number, symbol, 
				  dest_state.index_number)
	  else
	    @parse_table.add_action(state.index_number, symbol, 
				    [:SHIFT, dest_state.index_number])
	  end
	end
	current += 1
      end
      Profiler.__leave__(:calculate_state_graph, state_graph) if $PROFILE
      state_graph
    end
  end
end
