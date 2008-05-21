# Common classes used in many of the parse table generation classes.
#
require 'indexable'
require 'token'
require 'grammar'

class Item
  include Indexable
  attr_reader :symbol, :production, :position, :lookahead
  
  def initialize(production, position, lookahead = nil, nextItem = nil)
    @production, @position, @lookahead = production, position, lookahead
    @symbol = production.elements[position]
    @hash_value = [production, position, lookahead].hash
  end

  def direct_following_symbols
    @direct_following_symbols || (@direct_following_symbols = calc_followers)
  end

  def calc_followers
    return @@empty_array if position >= production.elements.length-1
    elements = production.elements[position+1..-1]
    followers = Array.new
    elements.each do |e|
      followers.push e
      return followers unless e.derives_epsilon?
    end
    followers
  end

  @@empty_array = Array.new

  def suffix
    if position < production.elements.length-1
      production.elements[position+1..-1]
    else
      nil
    end
  end

  def final?
    next_item == nil
  end

  def next_item
    @next_item || (@next_item = calc_next_item)
  end

  def lookahead_item(lookaheadSymbol)
    make_new_item(production, position, lookaheadSymbol, next_item)
  end
  
  def inspect
    endpos = ((@position >= production.elements.length) or 
	      (@position == 0))
    production.nonterminal.inspect + "->" +
      inspect_elements(production.elements[0...@position]) +
      (endpos ? "." : " .") +
      inspect_elements(production.elements[@position..-1])
  end
  
  protected 
  
  def inspect_elements(elements)
    elements ? elements.map {|e| e.inspect}.join(" ") : ""
  end

  def make_new_item(*args)
    @factory ? @factory.make(*args) : Item.new(*args)
  end

  def calc_next_item
    if position < production.elements.length
      make_new_item(production, position+1)
    else
      nil
    end
  end
end

class LrState
  include Indexable
  attr_reader :kernel_items, :closure, :final_items
 
  def initialize(kernelItems)
    @kernel_items = kernelItems
    calc_closure(kernelItems) # Do it lazily instead?
    @final_items = kernelItems.select {|i| i.final?}
    @reduce_state = @final_items.length > 0
    @consistent = kernelItems.length == 1
  end
  
  def reduce_state?
    @reduce_state
  end

  def consistent?
    @consistent
  end

  def length
    @kernel_items.length
  end
  
  def inspect
    "State(#{@kernel_items.inspect})"
  end
  
  protected
  
  @@empty_array = Array.new
  
  def calc_closure(itemset)
    @closure, checked = itemset.clone, Hash.new
    itemset.each {|i| checked = recursive_calc_closure(i.symbol, checked)}
  end

  def recursive_calc_closure(symbol, checked)
    return checked if !symbol or checked[symbol]
    checked[symbol] = true
    @closure.concat(new_items = symbol.nonkernel_items)
    new_items.each {|i| checked = recursive_calc_closure(i.symbol, checked)}
    checked
  end
end

# Decorate the grammar symbols with some additional info we're gonna need
class NonTerminal
  include Indexable
  attr_reader :nonkernel_items
  def calc_nonkernel_items(grammar, itemFactory)
    @nonkernel_items = Array.new
    grammar.alternatives(self).each do |production|
      @nonkernel_items.push itemFactory.make(production, 0)
    end
  end
  
  def nonterminal?; true; end
  def terminal?; false; end

  attr_writer :derives_epsilon

  def derives_epsilon?; @derives_epsilon; end
end

class Token
  include Indexable
  def nonterminal?; false; end
  def terminal?; true; end

  @@empty_array = []

  def nonkernel_items
    @@empty_array
  end

  def derives_epsilon?; false; end
end

# Decorate Productions
class Production
  include Indexable
end
