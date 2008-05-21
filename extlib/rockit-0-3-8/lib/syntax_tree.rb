require 'sourcecode_dumpable'
require 'graphdrawing'
require 'base_extensions'

# Base class for objects that build the syntax tree. Each has a name of
# the node they build, a list of names of the children and a list of the 
# childrens that are inactive. No value will be specified for inactive
# childrens even though their names are still available for the built node.
# Children named "_" are semi-inactive; values for the need to be specified
# when creating trees but the will be deleted when the tree is compacted.
# If a tree is not compacted their value can be accessed via their index
# number.
#
class SyntaxTreeBuilder
  include SourceCodeDumpable
  attr_accessor :node_name
  attr_reader :children_names, :inactive_children_indices

  def initialize(nodeName, childrenNames, inactiveChildrenIndices = [])
    @node_name = nodeName.to_s
    @inactive_children_indices = inactiveChildrenIndices.sort
    init_children_names(childrenNames)
  end

  def init_children_names(childrenNames)
    @children_names = Array.new
    childrenNames.each do |name|
      name = name.to_s
      if @children_names.include?(name) and name != "_"
	raise ArgumentError, 
	  "there are duplicates in the children names #{childrenNames.inspect}"
      else
	@children_names.push name
      end
    end
  end
  protected :init_children_names

  def copy
    SyntaxTreeBuilder.new(node_name, children_names, inactive_children_indices)
  end

  def active_childrens
    inactive = inactive_childrens
    children_names.select {|c| not inactive.include?(c)}
  end

  def inactive_childrens
    children_names.indices(*@inactive_children_indices)
  end

  def inactivate_child(anIntegerOrString)
    position, child = position_and_child(anIntegerOrString)
    @inactive_children_indices.push(position)
    @inactive_children_indices.uniq!
    @inactive_children_indices.sort!
  end

  def position_and_child(anIntegerOrString)
    if anIntegerOrString.kind_of?(Integer)
      # Position is among currently active childrens
      child = active_childrens[anIntegerOrString]
      position = children_names.index(child)
      return position, child
    else
      return children_names.index(anIntegerOrString), anIntegerOrString
    end
  end
  protected :position_and_child

  def activate_child(anIntegerOrString)
    position, child = position_and_child(anIntegerOrString)
    @inactive_children_indices.delete position
  end

  def ==(other)
    other.type == self.type and
      other.node_name == node_name and
      other.children_names == children_names and
      other.inactive_children_indices == inactive_children_indices
  end

  def create_tree(childrenValues)
    childrenValues = insert_nil_for_inactive_children(childrenValues)
    SyntaxTree.new(node_name, children_names, childrenValues)
  end

  def insert_nil_for_inactive_children(children)
    new_children, count = Array.new, 0
    @children_names.each_with_index do |child_name, i|
      if @inactive_children_indices.include?(i)
	new_children.push nil
      else
	new_children.push children[count]
	count += 1
      end
    end
    new_children
  end
  protected :insert_nil_for_inactive_children

  def to_src(name = nil, nameHash = {})
    assign_to(name,
	      new_of_my_type(node_name, 
			     as_code(children_names.to_compact_src), 
			     as_code(inactive_children_indices.to_compact_src)))
  end
end

class LiftingSyntaxTreeBuilder < SyntaxTreeBuilder
  def initialize(childrenNames, inactiveChildrenIndices = [])
    super("^", childrenNames, inactiveChildrenIndices)
    @child_to_lift_index = 
      @children_names.index(@children_names.detect {|n| n != "_"})
  end

  def create_tree(childrenValues)
    if @child_to_lift_index
      return childrenValues[@child_to_lift_index]
    else
      # Use first children value which does not have a "lexeme" children
      # ie. which represents a non-terminal
      val = childrenValues.detect {|c| !c.children_names.include?("lexeme")}
      val = childrenValues[0] unless val
      return val
    end
  end

  def copy
    LiftingSyntaxTreeBuilder.new(children_names, inactive_children_indices)
  end

  def to_src(name = nil, nameHash = {})
    assign_to(name,
	      new_of_my_type(as_code(children_names.to_compact_src), 
			     as_code(inactive_children_indices.to_compact_src)))
  end
end

# Short hand SyntaxTreeBuilder creator
def stb(nodeName, childrenNames = [], inactiveChildren = [])
  nodeName = nodeName.to_s
  if nodeName == "^"
    LiftingSyntaxTreeBuilder.new(childrenNames, inactiveChildren)
  else
    SyntaxTreeBuilder.new(nodeName, childrenNames, inactiveChildren)
  end
end

# Builder for arrays. You specify the indices for the children values
# that should be collected into an Array element and (optionally) 
# give the index to an Array to append the element to.
class ArrayNodeBuilder
  include SourceCodeDumpable
  attr_reader :indices, :array_index
  attr_accessor :append_element, :shifting_insert

  def initialize(indices = [], arrayIndex = nil,
		 chainedTreeBuilder = nil, insertAtIndex = nil,
		 deleteIndices = [], append_element = true)
    @indices, @array_index = indices, arrayIndex
    @append_element, @shifting_insert = append_element, false
    chainedTreeBuilder = SyntaxTreeBuilder.new("ArrayNode", []) unless chainedTreeBuilder
    chain_treebuilder(chainedTreeBuilder, insertAtIndex, deleteIndices)
  end

  def copy
    n = ArrayNodeBuilder.new(@indices, @array_index, @chained_treebuilder.copy,
			 @insert_at_index, @delete_indices)
    n.shifting_insert = @shifting_insert
    n
  end

  def create_tree(childrenValues)
    tree = create_tree_basic(childrenValues)
    if @chained_treebuilder and @insert_at_index
      if @shifting_insert
	childrenValues[@insert_at_index, 0] = tree
      else
	childrenValues[@insert_at_index, 1] = tree
      end
      childrenValues = childrenValues.delete_at_indices(@delete_indices)
      return @chained_treebuilder.create_tree(childrenValues)
    else
      return tree
    end
  end

  def inactivate_child(child)
    if child.kind_of?(Integer) and 
	child >= @insert_at_index and @shifting_insert
      @chained_treebuilder.inactivate_child(child+1)
    else
      @chained_treebuilder.inactivate_child(child)
    end
  end
  
  def activate_child(child)
    if child.kind_of?(Integer) and 
	child >= @insert_at_index
      @chained_treebuilder.activate_child(child+1)
    else
      @chained_treebuilder.activate_child(child)
    end
  end

  def chain_treebuilder(chainedTreeBuilder, insertAtIndex, deleteIndices = [])
    @chained_treebuilder = chainedTreeBuilder
    @insert_at_index, @delete_indices = insertAtIndex, deleteIndices
  end

  def ==(other)
    other.type == self.type and
      other.indices == indices and other.array_index == array_index and
      (@insert_at_index ? 
        (other.instance_eval("@chained_treebuilder") == @chained_treebuilder and
	 other.instance_eval("@insert_at_index") == @insert_at_index and
	 other.instance_eval("@delete_indices") == @delete_indices) :
       true) and
      other.append_element == @append_element
  end

  def to_src(name = nil, nameHash = {})
    iai, di = @insert_at_index, @delete_indices
    ctb = @insert_at_index ? @chained_treebuilder : nil
    new_of_my_type(as_code(@indices.to_compact_src), @array_index,
		   ctb, iai, as_code(di.to_compact_src), @append_element)
  end

  def method_missing(methodId, *args)
    begin
      @chained_treebuilder.send(methodId, *args)
    rescue Exception
      super
    end
  end

  protected

  def create_tree_basic(childrenValues)
    if @indices.length > 0
      array_element = childrenValues.indexes(*@indices)
    else
      array_element = []
    end
    if @array_index and childrenValues[@array_index].kind_of?(ArrayNode)
      array_element = array_element[0] if indices.length == 1
      an = childrenValues[@array_index].deep_copy
      an.add_value(array_element, @append_element)
      return an
    else
      array_element = [array_element] if indices.length > 1
      return ArrayNode.new(array_element)
    end
  end
end

# A GroupingSyntaxTreeBuilder is a transform that alters the children values
# and then passes them on to the another STB that creates the tree. Needed for
# denormalization of grouped elements.
class GroupingSyntaxTreeBuilder < SyntaxTreeBuilder
  attr_reader :range, :chained_builder

  def GroupingSyntaxTreeBuilder.new(startIndex, endIndex, syntaxTreeBuilder)
    if startIndex == endIndex
      syntaxTreeBuilder
    else
      super(startIndex, endIndex, syntaxTreeBuilder)
    end
  end

  def copy
    GroupingSyntaxTreeBuilder.new(@range.first, @range.last, 
				  @chained_builder.copy)
  end

  def initialize(startIndex, endIndex, syntaxTreeBuilder)
    @range, @chained_builder = (startIndex..endIndex), syntaxTreeBuilder 
  end

  def create_tree(childrenValues)
    childrenValues[@range] = [childrenValues[@range]]
    @chained_builder.create_tree childrenValues
  end

  def ==(other)
    other.type == self.type and
      other.range == range and other.chained_builder == chained_builder
  end

  def to_src(name = nil, nameHash = {})
    new_of_my_type(@range.first, @range.last, @chained_builder)
  end
end

# Nodes in a syntax tree have a name and can have childrens. The childrens
# are also named. The childrens can be accessed as methods with their name
# or via [] with their name or order number. Syntax tree nodes can also have
# attributes in a hash.
class SyntaxTree
  attr_reader :attributes, :name, :childrens, :children_names

  def initialize(name, childrenNames = [], childrens = [])
    @name = name
    unless childrenNames.length == childrens.length
      raise ArgumentError, "All childrens must be given a name"
    end
    @children_names, @childrens = childrenNames, childrens
    @attributes = Hash.new
  end

  def ==(other)
    other.type == self.type and
      other.name == name and
      other.children_names == children_names and
      other.childrens == childrens
  end

  def to_graph
    syntaxtree_as_dot_digraph(self)
  end

  def [](anIntegerOrChildrenName)
    if anIntegerOrChildrenName.kind_of?(Integer)
      @childrens[anIntegerOrChildrenName]
    else
      child_with_name(anIntegerOrChildrenName)
    end
  end

  def method_missing(methodId, *args, &block)
    begin
      child_with_name(methodId.id2name)
    rescue ArgumentError
      if @childrens.respond_to?(methodId)
	@childrens.send(methodId, *args, &block)
      else
	super
      end
    end
  end

  # (recursively) delete childrens whose name is "_" or nil
  def compact!
    new_childrens, new_children_names = Array.new, Array.new
    @children_names.each_with_index do |childname, i|
      if childname != "_" and childname != nil
	new_childrens.push @childrens[i]
	new_children_names.push @children_names[i]
      end
    end
    @childrens, @children_names = new_childrens, new_children_names
    compact_childrens
    self
  end
  
  def compact_childrens
    @childrens.each {|c| c.compact! if c.kind_of?(SyntaxTree)}
  end

  # Depth first node visiting
  def each_node(&b)
    @childrens.each {|c| c.each_node(&b) if c.kind_of?(SyntaxTree)}
    b.call(self)
  end
  alias each_depth_first each_node

  # Breadt first node visiting
  def each_breadth_first(&b)
    b.call(self)
    @childrens.each {|c| c.each_node(&b) if c.kind_of?(SyntaxTree)}
  end

  # Compact inspect without newlines
  def inspect_compact
    if ["lexeme", "value"].sort == @children_names.sort
      return @childrens[0].inspect
    end
    str = "#{name}"
    if @childrens.length > 0
      str += ":["
      @childrens.each_with_index do |child, i|
	str += "," if i > 0
	str += child.inspect 
      end
      str += "]"
    end
    str
  end
  alias inspect inspect_compact

  # Print as multi-line string with children indented
  def inspect_multi(indentLevel = 0)
    str = "#{@name}"
    @childrens.each_with_index do |child, i|
      str += "\n" + (" " * (indentLevel+1)) + @children_names[i] + ": "
      str += child.kind_of?(SyntaxTree) ? child.inspect_multi(indentLevel+1) :
	child.inspect
      #str += "\n"
    end
    str
  end

  protected

  def child_with_name(anObject)
    child_index = @children_names.index(anObject)
    if child_index
      @childrens[child_index]
    else
      raise ArgumentError, "There is no child named #{anObject.inspect}"
    end
  end
end

class ArrayNode < SyntaxTree
  attr_reader :attributes, :name, :childrens

  def initialize(values)
    super("_ArrayNode", value_names(values), values)
  end

  def deep_copy
    ArrayNode.new(@childrens.clone)
  end

  def as_a
    @childrens
  end

  def add_value(value, append = false)
    if append
      @childrens.push value
      @children_names.push "c#{@childrens.length}"
    else
      @childrens.unshift value
      @children_names.unshift "c#{@childrens.length}"
    end
  end

  protected

  def value_names(values)
    (1..values.length).to_a.map {|v| "c#{v}"}
  end
end
