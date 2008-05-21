require 'sourcecode_dumpable'
# A ProductionPriorities object knows the relative priorities between
# productions and can resolve which subTreeProducitons are valid at
# certain positions in a production. In essence it is a partial ordering
# among the productions of a grammar.
# 
# For more info about this kind of tree filtering conflict resolution
# see Eelco Visser's thesis from 1997.
#
# IDEA: Change to a more general handling of associativity using associativity
# vectors à la Ziemovit Laski's Bertha parser gen. Each production/rule has
# an assoc. vector with ones in the positions were subtrees having the same
# precedence is allowed. Left associativity is thus expressed as an assoc. 
# vector with a one in the first position and zeros in the rest. Right 
# associativity is zeros in all but the last position etc. See the papers
# on Bertha on www.ziemovitlaski.net.
#
Relation = Struct.new("Relation", :left, :relation, :right)
class Relation
  include SourceCodeDumpable

  def inspect
    "#{relation.inspect}(#{left.inspect}, #{right.inspect})"
  end

  def to_src(name = nil, nameHash = {})
    left_src = nameHash[left] ? nameHash[left] : left.to_src(nil, nameHash)
    right_src = nameHash[right] ? nameHash[right] : right.to_src(nil, nameHash)
    assign_to(name, 
	      new_of_my_type(as_code(left_src), 
			     as_code(relation.inspect), 
			     as_code(right_src))) 
  end
end
class RelationCircularityException < Exception; end

class ProductionPriorities
  include SourceCodeDumpable

  def initialize(relations = [])
    @left, @right, @non_associativity = Hash.new, Hash.new, Hash.new
    @higher, @equal = Hash.new(nil), Hash.new(nil)
    @in_conflict, @transitivity_stack = Hash.new(false), []
    init_relations(relations)
  end

  # Precedence relations can be :HIGHER, :EQUAL or :LOWER. When not set the 
  # (default) relation is a non-relation, ie. the productions are not related 
  # and can thus not have a (precedence) conflict.
  def set_precedence(p1, p2, precedenceRelation = :HIGHER)
    return if p1 == p2
    @in_conflict[p1] = true; @in_conflict[p2] = true
    if precedenceRelation == :LOWER
      precedenceRelation = :HIGHER
      p1, p2 = p2, p1
    end
    if (precedenceRelation == :HIGHER and 
	(higher_precedence?(p2,p1) or equal_precedence?(p2,p1))) or
	(precedenceRelation == :EQUAL and 
	 (higher_precedence?(p2,p1) or higher_precedence?(p1,p2)))
      raise RelationCircularityException
    end 
    transitivity(p1, p2, precedenceRelation)
    set_basic_precedence(p1, p2, precedence_hash(precedenceRelation))
    if :EQUAL == precedenceRelation
      set_basic_precedence(p2, p1, precedence_hash(precedenceRelation)) 
    end
  end

  def higher_precedence(a)
    @higher[a] || []
  end

  def higher_precedence?(a,b)
    higher_precedence(a).include?(b)
  end

  def equal_precedence(a)
    @equal[a] || []
  end

  def equal_precedence?(a,b)
    equal_precedence(a).include?(b) or equal_precedence(b).include?(a)
  end

  def set_associativity(production1, production2, associativityRelation)
    @in_conflict[production1] = true; @in_conflict[production2] = true
    if associativityRelation == :LEFT
      associativity_hash = @left
    elsif associativityRelation == :RIGHT
      associativity_hash = @right
    else
      associativity_hash = @non_associativity
    end
    begin
      associativity_hash[production1].push production2
    rescue NameError
      associativity_hash[production1] = [production2]
    end
  end

  def in_some_conflict?(production)
    @in_conflict[production]
  end

  # Is there a conflict with having a child derived from 'subTreeProduction'
  # at position 'position' in 'production'?
  def conflict?(subTreeProduction, position, production)
    # If production yielding subtree has higher precedence
    # than production yielding root node or left or right associativity 
    # conflict
    precedence_conflict?(subTreeProduction, production) or
      left_or_non_associativity_conflict?(subTreeProduction, position, 
					  production) or
      right_or_non_associativity_conflict?(subTreeProduction, position, 
					   production)
  end

  def precedence_conflict?(subTreeProduction, production)
    higher_precedence?(production, subTreeProduction)
  end

  def left_or_non_associativity_conflict?(subTreeProduction, position, production)
    if position == 0 or
	position != production.elements.length-1
      # No conflict unless the position is rightmost and there is something
      # preceding it
      false 
    else
      left_associative?(subTreeProduction, production) or
	non_associative?(subTreeProduction, production)
    end
  end

  def right_or_non_associativity_conflict?(subTreeProduction, position, 
					   production)
    if position != 0 or
	position == production.elements.length-1
      # No conflict unless the position is leftmost and there is something
      # after it
      false 
    else
      right_associative?(subTreeProduction, production) or
	non_associative?(subTreeProduction, production)
    end
  end

  def each(&block)
    each_production_pair(@left, :LEFT, &block)
    each_production_pair(@right, :RIGHT, &block)
    each_production_pair(@non_associativity, :NONASSOC, &block)
    each_production_pair(@higher, :HIGHER, &block)
    each_production_pair(@equal, :EQUAL, &block)
  end

  def each_production_pair(hash, relation, &block)
    hash.each do |p1, ps|
      ps.each do |p2|
	block.call(Relation.new(p1, relation, p2))
      end
    end
  end

  def to_src(name = nil, nameHash = {})
    str = relations.to_src("relations", nameHash) + "\n"
    str + assign_to(name, new_of_my_type(as_code("relations")))
  end

  def relations
    relations = Array.new
    self.each {|relation| relations.push relation}
    relations
  end

  protected

  def set_basic_precedence(a, b, hash)
    begin
      hash[a].push(b) unless hash[a].include?(b)
    rescue NameError
      hash[a] = [b]
    end
  end

  def precedence_hash(relation)
    case relation
    when :HIGHER
      @higher
    when :EQUAL
      @equal
    else
      raise ArgumentError, "Invalid relation #{relation.inspect}"
    end
  end

  def transitivity(a, b, relation)
    return if @transitivity_stack.include?([a,b,relation])
    @transitivity_stack.push [a,b,relation]
    higher_precedence(b).each {|c| set_precedence(a,c, :HIGHER)}
    one_step_higher_than(a).each {|h| set_precedence(h,b, :HIGHER)}
    equal_precedence(a).each {|c| set_precedence(c,b, relation)}
    equal_precedence(b).each {|c| set_precedence(a,c, relation)}
    if relation == :EQUAL
      higher_precedence(a).each {|c| set_precedence(b,c, :HIGHER)}
    end
    @transitivity_stack.pop
  end

  def one_step_higher_than(a)
    @higher.keys.select {|p| p != a and higher_precedence?(p, a)}
  end

  def init_relations(relations)
    relations.each do |relation|
      case relation.relation
      when :LEFT, :RIGHT, :NONASSOC
	set_associativity(relation.left, relation.right, relation.relation)
      when :HIGHER, :LOWER, :EQUAL
	set_precedence(relation.left, relation.right, relation.relation)
      else
	raise ArgumentError, "Unknown relation #{relation.inspect}"
      end
    end
  end

  def left_associative?(production1, production2)
    check_associativity(production1, production2, @left) or
      check_associativity(production2, production1, @left)
  end

  def right_associative?(production1, production2)
    check_associativity(production1, production2, @right) or
      check_associativity(production2, production1, @right)
  end

  def non_associative?(production1, production2)
    check_associativity(production1, production2, @non_associativity) or
      check_associativity(production2, production1, @non_associativity)
  end

  def check_associativity(production1, production2, associativityHash)
    begin
      associativityHash[production1].include?(production2)
    rescue NameError
      false
    end
  end

  def precedence(production1, production2)
    return :EQUAL if production1 == production2
    begin
      @precedence[production1][production2]
    rescue NameError
      nil # For the case when production1 is not related to anyone
    end
  end
end

# Short hand funcs for specifying priorities
def priorities(relations = [], *rest)
  relations += rest
  ProductionPriorities.new(relations.flatten.flatten)
end

def left(p1, *rest)
  assocs(:LEFT, true, p1, *rest)
end

def right(p1, *rest)
  assocs(:RIGHT, true, p1, *rest)
end

def nonassoc(p1, *rest)
  assocs(:NONASSOC, true, p1, *rest)
end

def assocs(rel, allPairs, p1, *rest)
  return assocs(rel, allPairs, *(p1+rest)) if p1.kind_of?(Array)
  if rest.length == 0
    assocs(rel, allPairs, p1, p1)
  elsif rest.length == 1
    [Relation.new(p1, rel, rest[0])]
  else
    if allPairs
      rest.map {|prod| assocs(rel, allPairs, p1, prod)} + 
	assocs(rel, allPairs, rest.shift, *rest)
    else
      rest.unshift p1
      (0...(rest.length-1)).map do |i|
	assocs(rel, allPairs, rest[i], rest[i+1])
      end.flatten
    end
  end
end

def decreasing_precedence(p1, p2, *rest)
  assocs(:HIGHER, false, p1, p2, *rest)
end

def equal_precedence(p1, p2, *rest)
  assocs(:EQUAL, false, p1, p2, *rest)
end
