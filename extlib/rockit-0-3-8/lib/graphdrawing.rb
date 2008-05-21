
def stack_to_node(stack, includeShape = false)
  str = "#{stack.state} (#{stack.lexer.position.char_position})"
  if includeShape
    str = str.inspect + " [shape=box]"
  end
  str
end

def parsestack_as_dot_digraph(stack, nodes = Hash.new, links = Hash.new)
  nodes[stack] = stack_to_node(stack, true)
  stack.links.each do |link| 
    nodes, links = parsestack_as_dot_digraph(link.stack, nodes, links)
    links[link] = "#{stack_to_node(stack).inspect} -> #{stack_to_node(link.stack).inspect} [label=#{link.tree.inspect_compact.inspect}]"
  end
  return nodes, links
end

def parsestacks_as_dot_digraph(stacks)
  nodes, links = {}, {}
  stacks.each do |stack|
    nodes, links = parsestack_as_dot_digraph(stack, nodes, links)
  end
  "digraph G {\nsize=\"8,11\"\n" +
    nodes.values.uniq.join("\n") + "\n" +
    links.values.uniq.join("\n") + "\n" + 
    "}"
end

class DotGraphPrinter
  def initialize(size = "11,9", orientation = "landscape")
    @size, @orientation = size, orientation
  end

  def to_graph(ast)
    @nodes, @links = Hash.new, Hash.new
    eval_to_dot(ast, nil)
    to_graph_from_nodes_and_links(@nodes, @links)
  end

  protected

  def to_graph_from_nodes_and_links(nodes, links)
    "digraph G {\n" +
      "size = #{@size.inspect}\n" +
      "orientation = #{@orientation}\n" +
      @nodes.values.uniq.join("\n") + "\n" +
      @links.values.uniq.join("\n") + "\n" + 
      "}"
  end
end

class SyntaxTreeAsDotGraph < DotGraphPrinter
  def add_parent_link(parent, child, label = nil, weight = nil)
    if parent
      @links[[parent, child]] = 
	"#{parent.id} -> #{child.id}"
      if label or weight
	@links[[parent, child]] += " [" +
	  (label ? "label=#{label.inspect}" : "") +
	  ((label and weight) ? "," : "") +
	  (weight ? "weight=#{weight.inspect}" : "") +
	  "]"
      end
    end
  end

  def eval_to_dot(ast, parent = nil, linkname = nil, weight = nil)
    if ast
      if ast.kind_of?(SyntaxTree)
	case ast.name
	when "_ArrayNode"
	  add_parent_link(parent, ast, linkname, weight)
	  @nodes[ast] = "#{ast.id} [label=" + '"[]"]'
	  ast.each_with_index {|c,i| eval_to_dot(c, ast, i.inspect)}
	else
	  if parent
	  end
	  # Special handling of Token nodes since we only want to print 
	  # the lexeme
	  if ast.children_names.sort == ["lexeme", "value"].sort
	    @nodes[ast] = "#{ast.id} [shape=box,label=#{ast.lexeme.inspect}]"
	    add_parent_link(parent, ast, linkname, weight)
	  else
	    add_parent_link(parent, ast, linkname, weight)
	    @nodes[ast] = "#{ast.id} [label=#{ast.name.inspect}]" 
	    ast.childrens.each_with_index {|c,i| 
	      eval_to_dot(c, ast, ast.children_names[i])
	    }
	  end
	end
      elsif ast.type == Array
	# Or nodes return array but they should return ArrayNodes...
	add_parent_link(parent, ast, linkname, weight)
	@nodes[ast] = "#{ast.id} [label=\"[]\"]"
	ast.each_with_index {|c,i| eval_to_dot(c, ast, i)}
      end
    end
  end 
end

def syntaxtree_as_dot_digraph(syntaxtree)
  SyntaxTreeAsDotGraph.new.to_graph(syntaxtree)
end



