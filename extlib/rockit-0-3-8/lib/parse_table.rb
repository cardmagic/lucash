require 'sourcecode_dumpable'
require 'grammar'
require 'base_extensions'

require 'profiler'

class ParseTable
  include SourceCodeDumpable
  attr_reader :start_state, :tokens, :priorities
  attr_accessor :language

  def ParseTable.new_from_grammar(aGrammar)
    pt = self.new(aGrammar.productions, aGrammar.tokens, aGrammar.priorities)
    pt.language = aGrammar.name || "UNNAMED_LANGUAGE"
    pt
  end

  # We save the actions in a compact numerical way to save space and time:
  #   The action table is an array of arrays. Each state number is an index
  #   into the array and its array contains an even number of integers.
  #   Each pair of integers represent one unique action. The first of the 
  #   integers is the action and the second is the number representing the
  #   terminals for which it apply. The least significant 'action_bits' bits
  #   of the action number determines the type of action by giving an index
  #   into the 'action_map'. Its default value is:
  #     [:REDUCE, :SHIFT, :ACCEPT]
  #   so that 
  @@default_action_map = [:REDUCE, :SHIFT, :ACCEPT]

  def initialize(productions, tokens, priorities = nil,
		 actionTable = ArrayOfArrays.new, gotoHash = Hash.new,
		 actionBits = 2, actionMap = @@default_action_map)
    @productions, @start_state, @language = productions, 0, "UNNAMED_LANGUAGE"
    @priorities = priorities
    @tokens, @nonterminals = tokens, nonterminals(productions)
    @action_table, @goto_hash = actionTable, gotoHash
    @action_cache = ArrayOfHashes.new
    @mask = Array.new
    @action_map, @action_bits, @action_mask = actionMap, actionBits, 0
    while actionBits > 0
      @action_mask = (@action_mask << 1) | 1
      actionBits -= 1
    end
    init_productionnum_to_nonterminal_number_hash
    init_tokentype_to_token_number_hash
  end

  def num_states
    @action_table.length
  end

  def ==(other)
    other.type == self.type and
      other.productions == @productions and
      other.tokens == @tokens and
      other.action_table == @action_table and
      other.goto_hash == @goto_hash and
      other.start_state == @start_state
  end

  def add_action(state, aTokenType, action)
    Profiler.__enter__(:ParseTable_add_action, state, aTokenType, action) if $PROFILE
    @action_cache[state].clear
    actionnum = action_to_actionnum(action)
    @action_table[state] << actionnum << token_to_terminalset(aTokenType)
    Profiler.__leave__(:ParseTable_add_action) if $PROFILE
  end

  def add_action_for_terminalset(state, action, terminalSet)
    Profiler.__enter__(:ParseTable_add_action_for_terminalset, state, action, terminalSet) if $PROFILE
    @action_table[state] << action_to_actionnum(action) << terminalSet.to_i
    Profiler.__leave__(:ParseTable_add_action_for_terminalset) if $PROFILE
  end

  # Unify terminal sets for identical actions
  def compact!
    Profiler.__enter__(:ParseTable_compact!) if $PROFILE
    actions, i, new_index = Hash.new, 0, 0
    @action_table.map! do |actionnums|
      actions.clear; 
      i, new_actionnums, new_index = 0, Array.new, 0
      while i < actionnums.length
	if (index = actions[actionnums[i]])
	  new_actionnums[index+1] |= actionnums[i+1]
	else
	  actions[actionnums[i]] = new_index
	  new_index += 2
	  new_actionnums << actionnums[i] << actionnums[i+1]
	end
	i += 2
      end
      new_actionnums
    end
    Profiler.__leave__(:ParseTable_compact!) if $PROFILE
  end

  def token_to_terminalset(aTokenType)
    mask(@token_to_number[aTokenType])
  end

  def mask(index)
    @mask[index] || (@mask[index] = (1 << index))
  end

  def add_goto(state, aNonTerminal, newState)
    begin
      @goto_hash[state][@nonterminals.index(aNonTerminal)] = newState
    rescue NameError
      @goto_hash[state] = Hash.new
      retry
    end
  end

  def actions(state, tokenType)
    actions = @action_cache[state][tokenType]
    unless actions
      actions = Array.new
      actionnums = @action_table[state]
      token_mask = mask(@token_to_number[tokenType])
      i = 0
      while i < actionnums.length
	if actionnums[i+1] & token_mask > 0
	  actions.push actionnum_to_action(actionnums[i])
	end
	i += 2
      end
      @action_cache[state][tokenType] = actions
    end
    actions
  end

  def valid_tokens(state)
    terminal_set = 0
    each_terminalset(state) {|ts| terminal_set |= ts}
    terminalset_to_terminals(terminal_set)
  end

  def each_terminalset(state)
    @action_table[state].each_with_index {|e,i| yield(e) if i % 2 == 1}
  end

  def terminalset_to_terminals(terminalSet)
    @tokens.select {|t| terminalSet & mask(@token_to_number[t]) > 0}
  end

  def actionnum_to_action(actionNumber)
    [@action_map[actionNumber & @action_mask], actionNumber >> @action_bits]
  end

  def action_to_actionnum(action)
    Profiler.__enter__(:ParseTable_action_to_actionnum, action) if $PROFILE
    res = @action_map.index(action[0]) + (action[1] << @action_bits)
    Profiler.__leave__(:ParseTable_action_to_actionnum) if $PROFILE
    res
  end

  def goto(state, productionNumber)
    begin
      @goto_hash[state][@productionnum_to_nonterminal_num[productionNumber]]
    rescue Exception
      nil
    end
  end

  def production(number)
    @productions[number]
  end

  def to_src(name = "parse_table", nameHash = {})
    names = name_hash(@tokens) {|t| "t"}
    str = @tokens.to_src("tokens", names) + "\n"
    names.update(name_hash(@productions) {|p| "p"})
    str << @productions.to_src("productions", names) + "\n"
    str << @priorities.to_src("priorities", names) + "\n"
    #str << "r = :REDUCE\n"
    #str << "s = :SHIFT\n"
    str << @action_table.to_compact_src("action_table") + "\n"
    str << @goto_hash.to_compact_src("goto_hash") + "\n"
    str << assign_to(name, 
		     new_of_my_type(as_code("productions"), 
				    as_code("tokens"),
				    as_code("priorities"),
				    as_code("action_table"), 
				    as_code("goto_hash"),
				    @action_bits,
				    @action_map))
    str
  end

  def inspect
    str = "ParseTable\n"
    str += "Tokens: #{@tokens.inspect}\n"
    str += "NonTerminals: #{@nonterminals.inspect}\n"
    str += "Productions:\n#{productions_inspect}\n"
    str += "Actions: \n"
    max_state = @action_table.length-1
    (max_state+1).times do |state|
      str += "#{state}:\t"
      @tokens.each do |t|
	str += inspect_actions(actions(state, t)) + ","
      end
      str += "| "
      @nonterminals.each do |nt|
	i = @productions.index(@productions.detect {|p| p.nonterminal == nt})
	str += ((ns=goto(state, i)) ? "#{ns}" : " ") + ","
      end
      str += "\n"
    end
    str
  end

  protected

  def productions_inspect
    str = ""
    @productions.each_with_index do |production, i|
      str += "  #{i}: #{production.inspect}\n"
    end
    str
  end

  def inspect_actions(actions)
    unless actions
      "   "
    else
      return "   " if actions.length == 0
      if actions.length > 1
	"[" + actions.map {|a| inspect_actions([a])}.join(',') + "]"
      else
	case actions[0][0]
	when :ACCEPT
	  " a "
	when :SHIFT
	  "s#{actions[0][1]} "
	when :REDUCE
	  "r#{actions[0][1]} "
	end
      end
    end
  end

  attr_reader :productions, :action_table, :goto_hash

  def nonterminals(anArrayOfProductions)
    anArrayOfProductions.map {|p| p.nonterminal}.equality_uniq
  end

  def init_nonterminal_index(nonterminals, productions)
    @nonterminal_index = Hash.new
    productions.each_with_index do |prod, i|
      @nonterminal_index[i] = nonterminals.index(prod.nonterminal)
    end
    @nonterminal_index
  end

  def init_token_index(tokens)
    @token_index = Hash.new
    tokens.each_with_index {|t,i| @token_index[t] = i}
    @token_index
  end

  def init_productionnum_to_nonterminal_number_hash
    @productionnum_to_nonterminal_num = Hash.new
    @productions.each_with_index do |p, n| 
      @productionnum_to_nonterminal_num[n] = @nonterminals.index(p.nonterminal)
    end
  end

  def init_tokentype_to_token_number_hash
    @token_to_number = Hash.new
    @tokens.each_with_index {|t,i| @token_to_number[t] = i}
  end
end
