require 'token'
require 'base_extensions'
require 'conflict_resolution'

require 'profiler'

# A Grammar has a name, a set of tokens, a start symbol and a set of 
# productions. Grammars are modular and can be merged. To help in resolving
# conflicts when grammars are merged the symbols that are exported from
# a grammar can be specified. By default all the nonterminals (left hand sides
# of the productions) are exported.
#
# Productions map a NonTerminal to a sequence of Element's.
# Elements can be either symbols (NonTerminals or terminals represented by
# their string name or symbol) or OperatorElements.
#
# OperatorElements are one of:
#   Plus - corresponds to the EBNF operator '+', ie. one or several
#   Mult - corresponds to the EBNF operator '*', ie. zero or several
#   Maybe - corresponds to the EBNF operator '?', ie. zero or one
#   List(Elements, separator) - A list of Elements separated by separator
#   Or - One of a sequence (at least 2) elements   
#
# A grammar is in normal form when no OperatorElements are in its productions.
# Converting a grammar to normal form is called normalization. Converting
# a syntax tree back to the unnormalized form of its grammar is called
# denormalization. Normalization is part of this file while denormalization
# is in a file of its own.
#
# Productions include a syntax tree specification describing how to build a
# (sub)tree for the syntax tree of the production when it is been matched.
# The SyntaxTreeSpecification needs to be known at this level since it
# is affected by normalization.
#
class Element
  include SourceCodeDumpable
  attr_accessor :sub_elements, :tree_specification

  def initialize(subElements, treeSpecification = nil)
    @sub_elements, @tree_specification = subElements, treeSpecification
  end

  # Normalize the element in the context of a production. The context is
  # needed to give information for naming of extra productions that (may)
  # need to be created. Returns two arrays, the former with the normalization
  # of the element in the existing production and the latter with additional
  # productions needed.
  def normalize(productions)
    # Default is that no normalization is needed and no extra productions
    # are added
    [productions, []] 
  end

  protected

  def clone_and_substitute_productions(productions, substitute, 
				       &updater)
    prods = productions.map do |prod| 
      p = prod.clone_and_substitute(self, substitute)
      index = prod.elements.index(self)
      p = updater.call(p, index) if updater and index
      p
    end
    (prods.map {|p| p.normalize}).flatten
  end
end

# To make it simple to enter productions some standard Ruby objects can be
# used in place of the "proper" objects. This function converts to the right
# type or raises an exception if object cannot be made into an element.
def make_element(anObject)
  if anObject.kind_of?(String)
    string_token(anObject)
  elsif anObject.kind_of?(Regexp)
    regexp_token(anObject)
  elsif anObject.kind_of?(Symbol)
    NonTerminal.new(anObject.to_s)
  elsif anObject.kind_of?(NonTerminal) or anObject.kind_of?(Token) or
      anObject.kind_of?(OperatorElement)
    anObject
  else
    raise ArgumentError, "cannot make an element from #{anObject.inspect}"
  end
end

def make_elements(anArrayOrElement, anArray = [])
  if anArrayOrElement.kind_of?(Array)
    anArrayOrElement += anArray
  else
    anArrayOrElement = [anArrayOrElement] + anArray
  end
  anArrayOrElement.map {|e| make_element(e)}
end

class OperatorElement < Element
  attr_reader :sub_elements

  def initialize(subElements, *rest)
    @sub_elements = make_elements(subElements, rest)
  end

  def name
    type_to_src.split("Element").first
  end

  def to_src(name = nil, nameHash = {})
    assign_to(name, 
	      new_of_my_type(as_code(@sub_elements.to_src(nil,nameHash))))
  end

  def ==(other)
    other.type == self.type and other.sub_elements == @sub_elements
  end

  protected

  def inspect_sub_elements(separator = " ")
    str = @sub_elements.map {|e| e.inspect}.join(separator)
    str = "(" + str + ")" if @sub_elements.length > 1
    str
  end

  def temp_nonterminal(name, production)
    NonTerminal.new("#{name}#{id.inspect}")
  end
end

class PlusElement < OperatorElement
  def inspect
    "#{inspect_sub_elements}+"
  end

  # NT -> a b (se1 ... sen)+ c d
  #
  # is normalized to
  #
  # NT -> a b NT-Plus-X c d
  # NT-Plus-X -> NT-Plus-X se1 ... sen
  #           |  se1 ... sen
  def normalize(productions)
    temp_nonterm = temp_nonterminal("Plus", productions.first)
    num_sub_elements = @sub_elements.length
    temp_production1 = 
      Production.new(temp_nonterm, [temp_nonterm].concat(@sub_elements),
		     ArrayNodeBuilder.new((1..num_sub_elements).to_a, 0))
    temp_production2 = 
      Production.new(temp_nonterm, @sub_elements,
		     ArrayNodeBuilder.new((0...num_sub_elements).to_a))
    [clone_and_substitute_productions(productions, [temp_nonterm]),
     temp_production1.normalize + temp_production2.normalize]
  end
end

class MultElement < OperatorElement
  def inspect
    "#{inspect_sub_elements}*"
  end

  # NT -> a b (se1 ... sen)* c d
  #
  # is normalized to
  #
  # NT -> a b NT-Mult-X c d
  #    |  a b c d
  # NT-Mult-X -> NT-Mult-X se1 ... sen
  #           |  se1 ... sen
  def normalize(productions)
    temp_nonterm = temp_nonterminal("Mult", productions.first)
    num_sub_elements = @sub_elements.length
    temp_production1 = 
      Production.new(temp_nonterm, [temp_nonterm].concat(@sub_elements),
		     ArrayNodeBuilder.new((1..num_sub_elements).to_a, 0))
    temp_production2 = 
      Production.new(temp_nonterm, @sub_elements,
		     ArrayNodeBuilder.new((0...num_sub_elements).to_a))
    ps2 = clone_and_substitute_productions(productions, []) do |prod, i|
      # Will insert empty ArrayNode
      prod.tree_builder = 
	ArrayNodeBuilder.new([], nil, prod.tree_builder, i)
      prod.tree_builder.shifting_insert = true
      prod
    end
    [clone_and_substitute_productions(productions, [temp_nonterm]) + ps2,
     temp_production1.normalize + temp_production2.normalize]
  end
end

class MaybeElement < OperatorElement
  def inspect
    "#{inspect_sub_elements}?"
  end

  # NT -> a b (se1 ... sen)? c d
  #
  # is normalized to
  #
  # NT -> a b se1 ... sen c d
  #    |  a b c d
  def normalize(productions)
    p1 = clone_and_substitute_productions(productions, 
					  @sub_elements) do |prod, elemindex|
      endindex = elemindex+@sub_elements.length-1
      prod.tree_builder = 
	GroupingSyntaxTreeBuilder.new(elemindex, 
				      endindex,
				      prod.tree_builder)
      prod
    end
    p2 = clone_and_substitute_productions(productions, []) do |p,ei|
      p.tree_builder.inactivate_child(ei)
      p
    end 
    [p1 + p2, []]
  end
end

class ListElement < OperatorElement
  def initialize(subElements, separatorElement = ",")
    super(subElements)
    @separator = make_element(separatorElement)
  end

  def to_src(name = nil, nameHash = {})
    assign_to(name, 
	      new_of_my_type(as_code(@sub_elements.to_src(nil,nameHash)), 
				     @separator))
  end

  # NT -> a b list(se1 ... sen, sep) c d
  #
  # is normalized to
  #
  # NT -> a b se1 ... sen NT-List-X c d
  #    |  a b se1 ... sen c d
  # NT-List-X -> NT-List-X sep se1 ... sen
  #           |  sep se1 ... sen
  def normalize(productions)
    temp_nonterminal = temp_nonterminal("List", productions.first)
    num_sub_elements = @sub_elements.length
    ps1 = 
      clone_and_substitute_productions(productions, @sub_elements + 
				       [temp_nonterminal]) do |prod, elemindex|
      endindex = elemindex+@sub_elements.length-1
      prod.tree_builder = 
	ArrayNodeBuilder.new((elemindex..endindex).to_a,
			     endindex+1, prod.tree_builder,
			     elemindex, ((elemindex+1)..(endindex+1)).to_a)
      prod.tree_builder.append_element = false
      prod
    end
    ps2 = clone_and_substitute_productions(productions, 
					   @sub_elements) do |prod, elemindex|
      endindex = elemindex+@sub_elements.length-1
      prod.tree_builder = 
	ArrayNodeBuilder.new((elemindex..endindex).to_a,
			     nil, prod.tree_builder,
			     elemindex, ((elemindex+1)..endindex).to_a)
      prod
    end
    temp_production1 =
      Production.new(temp_nonterminal, 
		     [temp_nonterminal, @separator] + @sub_elements,
		     ArrayNodeBuilder.new((2...(2+num_sub_elements)).to_a, 0))
    temp_production2 = 
      Production.new(temp_nonterminal, [@separator] + @sub_elements,
		     ArrayNodeBuilder.new((1...(1+num_sub_elements)).to_a))
    [ps1 + ps2,
     temp_production1.normalize + temp_production2.normalize]
  end

  def inspect
    "list(#{inspect_sub_elements}, #{@separator.inspect})"
  end
end

class OrElement < OperatorElement
  def initialize(*args)
    super(*args)
    raise ArgumentError, "At least two sub-elements needed" unless @sub_elements.length > 1
  end

  # NT -> a b (se1 | ... | sen) c d
  #
  # is normalized to
  #
  # NT -> a b se1 c d
  #    |  a b ... c d
  #    |  a b sen c d
  def normalize(productions)
    normalized_productions = @sub_elements.map do |subelement|
      clone_and_substitute_productions(productions, [subelement])
    end
    [normalized_productions.flatten, []]
  end

  def inspect
    inspect_sub_elements(" | ")
  end
end

# Short hand funcs
def as_array(subElements)
  if subElements.length == 1 and subElements[0].kind_of?(Array)
    subElements[0]
  else
    subElements
  end
end

def plus(*subElements); PlusElement.new(as_array(subElements)); end
def mult(*subElements); MultElement.new(as_array(subElements)); end
def maybe(*subElements); MaybeElement.new(as_array(subElements)); end
def ore(*subElements); OrElement.new(as_array(subElements)); end
def liste(subElements, separator); ListElement.new(subElements, separator); end

class GrammarSymbol < Element
  attr_reader :name
  def initialize(name)
    super(nil, nil) # Symbols have no sub-elements or tree-specification
    @name = name.to_s
  end

  def ==(other)
    #Profiler.__enter__("GrammarSymbol#==".intern, other)
    res = other.kind_of?(GrammarSymbol) and name == other.name
    #Profiler.__leave__("GrammarSymbol#==".intern, res)
  end

  def hash
    name.hash
  end

  def eql?(other)
    other.hash == hash
  end

  def inspect
    name
  end

  def to_src(assignToName = nil, nameHash = {})
    assign_to(assignToName, new_of_my_type(name))
  end
end

class NonTerminal < GrammarSymbol
end

# When init:ing productions nonterminals can be specified with symbols both
# as nonterminal or as elements. String tokens can be specified as strings.
#
class Production
  include SourceCodeDumpable
  attr_accessor :elements    # Right hand side
  attr_accessor :nonterminal # Left hand side
  attr_accessor :tree_builder

  def initialize(nonterminal, elements, treeBuilder = nil)
    nonterminal = NonTerminal.new(nonterminal) if nonterminal.type == String
    @nonterminal = make_element(nonterminal)
    @elements = make_elements(elements)
    init_tree_builder(treeBuilder)
  end

=begin
  def clone
    Production.new(@nonterminal.clone, @elements.clone, @tree_builder.clone)
  end
=end

  def normalize
    maybe_normalize, extra_productions = [self], []
    @elements.each do |element|
      if element.kind_of?(Element)
	maybe_normalize, new_extra = element.normalize(maybe_normalize)
	extra_productions += new_extra
      end
    end
    (maybe_normalize + extra_productions).equality_uniq
  end

  def length
    elements.length
  end

  def ==(other)
    #Profiler.__enter__("Production#==".intern, other)
    res = other.type == self.type and
      nonterminal == other.nonterminal and
      elements == other.elements and
      tree_builder == other.tree_builder
    #Profiler.__leave__("Production#==".intern, res)
  end

  def clone_and_substitute(element, substitute)
    index = elements.index element
    substitute = [substitute] unless substitute.kind_of?(Array)
    if index
      Production.new(nonterminal, 
		     elements[0...index] + substitute + 
		     elements[(index+1)..-1], tree_builder.copy)
    else
      self
    end
  end

  def inspect
    elements_inspect = @elements.map {|e| e.inspect}.join(" ")
    "#{nonterminal.inspect} -> #{elements_inspect}"
  end

  def create_tree(childrenValues)
    tree_builder.create_tree(childrenValues)
  end

  def to_src(name = nil, nameHash = {})
    assign_to(name,
	      new_of_my_type(nonterm_to_symbol(nonterminal),
			     as_code(elements_to_src(nameHash)), tree_builder))
  end

  protected

  def elements_to_src(nameHash)
    "[" + elements.map do |element|
      if element.kind_of?(Token) and nameHash[element]
	nameHash[element]
      elsif element.kind_of?(NonTerminal)
	nonterm_to_symbol(element).to_src
      else
	element.to_compact_src(nil,nameHash)
      end
    end.join(", ") + "]"
  end

  def nonterm_to_symbol(o)
    return o unless o.type == NonTerminal
    if o.name.include? "'"
      as_code('"' + o.name + '".intern')
    else
      o.name.intern
    end
  end

  def init_tree_builder(treeBuilder)
    if treeBuilder == nil
      @tree_builder = stb(nonterminal.name, assign_element_names)
    elsif [:^, "^"].include?(treeBuilder)
      @tree_builder = stb("^", assign_element_names(true))
    else
      if ["_", nil, ""].include?(treeBuilder.node_name)
	treeBuilder.node_name = nonterminal.name
      end
      @tree_builder = treeBuilder
    end
  end

  def assign_element_names(inactivateStringTokens = false)
    at_least_one_active, count = false, 0
    name_count = Hash.new(0)
    element_names = elements.map do |element|
      count += 1
      if element.kind_of?(StringToken)
	inactivateStringTokens ? "_" : "c#{count}"
      elsif element.kind_of?(Token)
	name = element.name || "c#{count}"
	name_count[name] += 1
	name += "#{name_count[name]}" if name_count[name] > 1
	inactivateStringTokens ? "_" : name.downcase
      elsif element.kind_of?(NonTerminal) or element.kind_of?(OperatorElement)
	at_least_one_active = true
	n = element.name.downcase
	name_count[n] += 1
	n += "#{name_count[n]}" if name_count[n] > 1
	n
      end
    end
    element_names[0] = "c1" unless at_least_one_active
    element_names
  end
end

def prod(nonterminal, elements, treeSpec = nil)
  Production.new(nonterminal, elements, treeSpec)
end

class Grammar
  attr_reader :tokens, :productions, :exports, :start_symbol, :priorities
  attr_reader :eof_terminal, :original_start_symbol
  attr_accessor :name
  alias_method :terminals, :tokens

  def initialize(name = nil, productions = [], tokens = [], 
		 priorities = nil,
		 startSymbol = nil, exports = [])
    # NOTE! Beware that the EofToken below wont get a unique index number!!
    @eof_terminal = EofToken.new
    @name, @tokens, @alternatives = name, [@eof_terminal], Hash.new
    @priorities = priorities || ProductionPriorities.new
    @nonterminals = Array.new
    add_tokens(tokens)
    clear_productions
    add_productions(productions)
    init_start_symbol(startSymbol)
    @exports = exports
  end

  def alternatives(nonterminal)
    @alternatives[nonterminal.name]
  end

  def normalize
    old_productions = @productions
    clear_productions
    old_productions.each do |production|
      add_productions(production.normalize)
    end
    self
  end

  def add_token(token)
    @tokens.push token unless @tokens.include?(token)
  end

  def add_unique_symbols(production)
    production.elements = production.elements.map {|e| unique_symbol(e)}
    production.nonterminal = unique_symbol(production.nonterminal)
  end

  def unique_symbol(symbol)
    existing_symbol = (@tokens + @nonterminals).detect {|e| e==symbol}
    if not existing_symbol
      add_token(symbol) if symbol.kind_of?(Token)
      @nonterminals.push(symbol) if symbol.kind_of?(NonTerminal)
      return symbol
    else
      return existing_symbol
    end
  end

  def add_production(production)
    unless @productions.include?(production)
      add_unique_symbols(production)
      @productions.push production
      nt = production.nonterminal
      unless @alternatives[nt.name]
	@alternatives[nt.name] = [production]
      else
	@alternatives[nt.name].push production
      end
    end
    @start_symbol = production.nonterminal unless @start_symbol
  end

  def +(otherGrammar)
    # NOTE: Maybe check for and/or handle naming conflicts?
    add_tokens(otherGrammar.tokens)
    add_productions(otherGrammar.productions)
    otherGrammar.exports.each {|e| @exports.push(e)}
    self
  end

  def nonterminals(includeStartSymbol = true)
    nts = (@productions.map {|p| p.nonterminal}).equality_uniq
    nts.delete(@start_symbol) unless includeStartSymbol
    nts
  end

  def augmented?
    @augmented ? true : false
  end

  def inspect
    str = "Grammar #{@name}\n"
    str += "Start symbol: #{@start_symbol.inspect}\n" if @start_symbol
    str += "Tokens:\n" + @tokens.map{|t| "  " + t.inspect}.join("\n")
    str += "\nProductions:\n"
    @productions.each do |prod|
      str += "  " + prod.inspect + "\n"
    end
    str
  end

  class EpsilonTokenType < Token
    def initialize
    end
    def ==(other)
      other.type == self.type
    end
  end
  EpsilonToken = EpsilonTokenType.new

  def Grammar.epsilon
    EpsilonToken
  end

  # Augment the grammar by adding new start symbol and production from it
  # to previous start symbol. The added production gets index 0.
  def augment
    return true if augmented?
    @original_start_symbol = self.start_symbol
    # Add prim's until unique nonterminal name
    new_name = @original_start_symbol.name + "'"
    while nonterminals.collect{|nt| nt.name}.include?(new_name)
      new_name += "'"
    end
    @start_symbol = NonTerminal.new(new_name)
    add_production(p = Production.new(@start_symbol, [@original_start_symbol]))
    # Make sure its on top
    @productions.delete p
    @productions.unshift p
    @augmented = true
  end

  def unaugment
    return false unless augmented?
    @start_symbol = @original_start_symbol
    @productions.shift
    @augmented = false
  end

  private

  def init_start_symbol(startSymbol)
    if startSymbol
      @start_symbol = make_element(startSymbol)
    elsif @productions.length > 0
      @start_symbol = @productions.first.nonterminal
    end
  end

  def add_tokens(tokens)
    tokens.each {|token| add_token(token)}
  end

  def add_productions(productions)
    productions.each {|production| add_production(production)}
  end

  def clear_productions
    @productions = Array.new
    @alternatives = Hash.new
  end
end
