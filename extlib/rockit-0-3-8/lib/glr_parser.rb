require 'graphdrawing'
require 'version'
require 'sourcecode_dumpable'
require 'parsing_ambiguities'

class ParseException < Exception; end;

# Generalized LR Parsing class
#
# This is a modification of Jan Rekers and Eelco Vissers Generalized LR
# parsers which in turn are derived from the Tomita parsing algorithm. The
# main feature of these kinds of parsers is that aribtrary long lookahead is 
# used (when needed) since a parser is forked off every time there is an 
# ambiguity.
#
# This implementation assumes that the ambiguities (arising from lack of
# lookahead) are resolved later; it does not handle ambiguities arising 
# from the grammar. However, it can easily be extended to return a parse tree
# forest with all possible parse trees if there is a need for that. 
# Alternatively, the user can resolve ambiguities in the grammar by specifying
# production priorities.
# 
# The modification I've done is so that multiple token streams from the lexer
# can be handled. This allows simpler specification of lexers while still
# leading to valid parses as long as the grammar is unambigous.
#
# The algorithm used is copyright (c) 2001 Robert Feldt.
#
class GeneralizedLrParser
  include SourceCodeDumpable

  def initialize(aParseTable, aLexer = nil)
    @parse_table = aParseTable
    # puts @parse_table.inspect
    if aLexer
      @lexer = aLexer
    else
      tokens = @parse_table.tokens.clone
      tokens.delete(:EOF)
      @lexer = ForkingRegexpLexer.new(tokens)
    end
  end

  def parser_src_header
    "# Parser for #{@parse_table.language}\n" +
      "# created by Rockit version #{rockit_version} on #{Time.new.inspect}\n" +
      "# Rockit is copyright (c) 2001 Robert Feldt, feldt@ce.chalmers.se\n" +
      "# and licensed under GPL\n" +
      "# but this parser is under LGPL\n"
  end

  def to_src(assignToName = nil, nameHash = {})
    ptname = "@@parse_table" + self.id.inspect
    parser_src_header + @parse_table.to_src(ptname) + "\n" +
      assign_to(assignToName, 
		new_of_my_type(as_code(ptname)))
  end

  def parse(aString)
    @string_being_parsed = aString
    @accepted_stacks, @stacks_to_shift = [], []
    @lexer.init(aString)
    start_state = @parse_table.start_state
    @active_stacks = [ParseStack.new(start_state, @lexer)]
    @cnt, @reducer_cnt = -1, 0
    while @active_stacks.length > 0
      # File.open("as#{@cnt+=1}.graph", "w") {|f| f.write parsestacks_as_dot_digraph(@active_stacks)}
      @stacks_to_shift.clear
      @stacks_to_act_on = @active_stacks.clone
      actor(@stacks_to_act_on.shift) while @stacks_to_act_on.length > 0
      shifter
    end
    if @accepted_stacks.length > 0
      tree = @accepted_stacks.first.links_to_stack_in_state?(start_state).tree
      check_and_report_ambiguity tree
      return tree
    else
      handle_parse_error
    end
  end

  protected

  def check_and_report_ambiguity(tree)
    tree.each_node do |node|
      if node.kind_of?(AmbiguityNode)
	raise AmbigousParseException.new(@string_being_parsed, tree, 
					 *(node.ambigous_trees))
      end
    end
  end

  def handle_parse_error
    if @last_active_stacks
      str = "No valid token found on stacks:\n"
      @last_active_stacks.each_with_index do |stack, i|
	tokens = stack.lexer.peek
	str += "stack #{i}: #{context(stack.lexer.position.char_position)}" +
	  "in state #{stack.state}\n" +
	  "the lexer returns tokens = #{tokens.inspect} (#{tokens.map{|t| t.token_type.to_s}.inspect})\n" +
	  "and the valid tokens = #{@parse_table.valid_tokens(stack.state).inspect}\n"
      end
    else
      str = "String could not be parsed"
    end
    raise ParseException, str
  end

  def context(position)
    line, startpos = get_line_with_position(@string_being_parsed, position)
    indent = (" " * (position - startpos + 1))
    "on line:\n  '" + line + "'\n  " + 
      indent + "^\n  " +
      indent + "|\n  " +
      indent + "--- Parse error!!\n"
  end

  def get_line_with_position(string, position)
    startpos = position
    len = string.length
    while string[startpos,1] != "\n" and startpos > 1
      startpos -= 1
    end
    endpos = position
    while string[endpos,1] != "\n" and endpos < len
      endpos += 1
    end
    return string[startpos+1...endpos], startpos+1
  end

  def actor(stack)
    #puts "actor(#{stack.state}) @stacks_to_act_on = #{@stacks_to_act_on.map{|s| s.state}.inspect}, @active_stacks = #{@active_stacks.map{|s| s.state}.inspect}"
    tokens = stack.lexer.peek
    #print "tokens = #{tokens.inspect}, "
    tokens.each do |token|
      #print "state = #{stack.state.inspect}, "
      actions = @parse_table.actions(stack.state, token.token_type)
      next unless actions
      #puts "lexer = #{stack.lexer.inspect} (#{stack.lexer.id}), token = #{token.inspect}, actions = #{actions.inspect}"
      actions.each do |action|
	case action[0]
	when :SHIFT
	  @stacks_to_shift.push [stack, action[1], token]
	when :REDUCE
	  do_reductions(stack, action[1], stack.lexer)
	when :ACCEPT
	  @accepted_stacks.push stack
	end	
      end
    end
  end

  def do_reductions(stack, productionNumber, lexer, pathsInludingLink = nil)
    production = @parse_table.production(productionNumber)
    paths = 
      if @parse_table.priorities.in_some_conflict?(production)
	# Only return valid paths, ie. paths without conflicts. Prune the
	# invalid paths.
	stack.valid_paths_of_length_with_pruning(production.length, 
						 pathsInludingLink) do |l,p|
	  not @parse_table.priorities.conflict?(l.production, p, production)
        end
      else
	stack.paths_of_length(production.length, pathsInludingLink)
      end
    paths.each do |path|
      reducer(path.to, @parse_table.goto(path.to.state, productionNumber),
	      lexer, production, trees_on_path(path), productionNumber)
    end
  end

  def reducer(stack, newState, newLexer, production, childTrees, productionNum)
    # puts "#{@reducer_cnt+=1}: reducer(#{stack.state}, #{production.inspect})\n"
    tree = production.create_tree(childTrees)
    existing_stack = @active_stacks.detect do |as| 
      as.state==newState and as.lexer==newLexer
    end
    if existing_stack
      # There is already a stack with the same state and lexer, so instead
      # of creating a new stack we re-use the existing one.
      if (existing_link = existing_stack.links_to_stack?(stack))
	# There is already a link to the stack => Ambiguity unless existing
	# tree same as new tree. The latter can happen when multiple token
	# types that all match the current string are in the follow set of 
	# the same production.
	if tree != existing_link.tree
	  handle_ambiguity(existing_stack, existing_link, tree, production)
	end
      else
	new_link = existing_stack.add_link(stack, tree, production)
	recheck_stacks(@active_stacks - @stacks_to_act_on, new_link)
      end
    else
      new_stack = ParseStack.new(newState, newLexer)
      new_stack.add_link(stack, tree, production)
      @stacks_to_act_on.push new_stack
      @active_stacks.push new_stack
    end
    # File.open("as#{@cnt}_#{@reducer_cnt}.graph", "w") {|f| f.write parsestacks_as_dot_digraph(@active_stacks)}
  end

  def recheck_stacks(stacks, link)
    # Recheck stacks to see if new reductions are possible including the new
    # link
    stacks.each do |stack|
      actions(stack).each do |action|
	if action[0] == :REDUCE
	  do_reductions(stack, action[1], stack.lexer, link)
	end
      end
    end
  end

  def actions(stack)
    actions, state = [], stack.state
    stack.lexer.peek.each do |token| 
      actions.concat @parse_table.actions(state, token.token_type)
    end
    actions
  end

  def handle_ambiguity(existingStack, existingLink, newTree, production)
    # We can extend the parser here to return the full parse forest.
    # For now we simplify things and raise an exception.
    existing_tree = existingLink.tree
    if existing_tree.kind_of?(AmbiguityNode)
      existing_tree.add_ambigoustree(newTree)
    else
      existingLink.tree = AmbiguityNode.new(existing_tree, newTree)
    end
    #alternatives = [newTree.compact!]
    #existingStack.links.each {|link| alternatives.push link.tree.compact!}
    #raise AmbigousParseException.new(@string_being_parsed, *alternatives)
  end

  def write_graphs
    #File.open("atree1.graph", "w") {|f| f.write syntaxtree_as_dot_digraph(newTree)}
    #File.open("atree2.graph", "w") {|f| f.write syntaxtree_as_dot_digraph(existingStack.links[0].tree)}
    #File.open("as_at_ambiguity.graph", "w") {|f| f.write parsestacks_as_dot_digraph(@active_stacks)}
  end    

  def shifter
    # Save current active_stacks if there is a parse error
    @last_active_stacks = @active_stacks.clone
    @active_stacks.clear
    @stacks_to_shift.each do |stack, newstate, lexertoken|
      tree = lexertoken.create_tree
      existing_stack = @active_stacks.detect do |as| 
	as.state==newstate and as.lexer==lexertoken.lexer
      end
      if existing_stack
	existing_stack.add_link(stack, tree)
      else
	new_stack = ParseStack.new(newstate, lexertoken.lexer)
	new_stack.add_link(stack, tree)
	@active_stacks.push new_stack
      end
    end
  end

  def trees_on_path(path)
    path.links.reverse.map {|link| link.tree}
  end
end

class ParseStack
  Link = Struct.new("Link", :stack, :tree, :production)
  class Link
    def inspect
      "-#{tree.inspect}->#{stack.state.inspect}"
    end
  end

  attr_reader :state, :lexer, :links

  def initialize(aState, aLexer)
    @state, @lexer = aState, aLexer
    @links = Array.new
  end

  def add_link(aParseStack, aTree, aProduction = nil)
    @links.push(l = Link.new(aParseStack, aTree, aProduction))
    l
  end

  def links_to_stack_in_state?(state)
    @links.detect {|link| link.stack.state == state}
  end

  def links_to_stack?(stack)
    @links.detect {|link| link.stack == stack}
  end

  def paths_of_length(length, aLink = nil)
    return [] if length == 0
    paths = Array.new
    @links.each do |link|
      child_paths = link.stack.paths_of_length(length-1)
      if child_paths.length > 0
	child_paths.each {|cpath| paths.push(StackPath.new(self, [link]).add_path(cpath))}
      else
	paths.push StackPath.new(self, [link])
      end
    end
    delete_paths_without_link(paths, aLink)
  end

  def valid_paths_of_length_with_pruning(length, aLink = nil, 
					 &validity_checker)
    return [] if length == 0
    paths, new_links = Array.new, Array.new
    @links.each do |link|
      if validity_checker.call(link, length-1)
	if length == 1
	  new_links.push(link)
	  paths.push(StackPath.new(self, [link]))
	else
	  child_paths = 
	    link.stack.valid_paths_of_length_with_pruning(length-1, 
							&validity_checker)
	  if child_paths.length > 0
	    new_links.push(link)
	    child_paths.each do |cpath| 
	      paths.push(StackPath.new(self, [link]).add_path(cpath))
	    end
	  end
	end
      end
    end
    @links = new_links
    delete_paths_without_link(paths, aLink)
  end

  def inspect
    "PSt(#{@state}, #{@links.inspect}, #{@lexer.inspect})"
  end

  private

  def delete_paths_without_link(paths, aLink)
    if aLink
      return paths.find_all {|path| path.includes_link?(aLink)}
    else
      return paths
    end
  end
end

class StackPath
  attr_reader :to, :from
  attr_reader :links

  def initialize(from, links = [])
    @from, @links = from, Array.new
    links.each {|link| self.add_link(link)}
  end

  def add_link(link)
    @links.push link
    @to = link.stack
  end

  def add_path(aStackPath)
    @links.concat aStackPath.links
    @to = @links.last.stack
    self
  end

  def includes_link?(link)
    @links.detect {|l| l==link}
  end

  def inspect
    "#{from.state}(#{from.lexer.position.char_position}) " +
      @links.map {|l| "-#{l.tree.inspect}-> #{l.stack.state} (#{l.stack.lexer.position.char_position}) "}.join
  end
end
    
