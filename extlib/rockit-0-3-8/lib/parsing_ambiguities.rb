require 'syntax_tree'

class AmbiguityNode < SyntaxTree
  def initialize(alt1, alt2)
    @ambigous_trees = [alt1, alt2]
    super("_Ambiguity", ["ambigous_trees"], [@ambigous_trees])
  end

  def add_ambigoustree(tree)
    @ambigous_trees.push tree unless @ambigous_trees.include?(tree)
  end
end

class AmbigousParseException < Exception
  attr_reader :alternatives, :substring

  def initialize(stringBeingParsed, fullTree, *alternativeTrees)
    super("Ambigous parse")
    @alternatives, @full_tree = alternativeTrees, fullTree
    init_substring(stringBeingParsed)
  end

  def inspect(prettyPrinter = nil)
    str = "Ambiguity: The substring '#{substring}' can be parsed as:\n"
    #return str + report_on_tree_differences(alternatives[0], alternatives[1], 0, 1)
    strings_to_show, same_strings = Array.new, Array.new
    alternatives.each_with_index {|alt, i|
      s = prettyPrinter ? prettyPrinter.print(alt) : alt.inspect
      if (j = strings_to_show.index(s))
	same_strings.push [j,i]
      end
      strings_to_show.push s
    }
    alternatives.each_with_index do |alt,i| 
      str << " Alternative #{i+1}: #{strings_to_show[i]}"
      str << ", or" if i < alternatives.length-1
      str << "\n"
    end
    same_strings.each do |i,j| 
      str += report_on_tree_differences(alternatives[i], alternatives[j],
					i, j)
    end
    @full_tree.compact!
    str + "The full tree looks like:\n" + @full_tree.inspect
  end

  def difference_description(i, j, str0, o1, o2, childPath = "")
    child_str = 
      childPath.length > 0 ? "in the childrens '#{childPath[1..-1]}'" : ""
    " Alternatives #{i+1} and #{j+1} differ #{child_str} by not having" +
      " the same #{str0} (#{o1.inspect} and #{o2.inspect})"
  end
  
  def report_on_tree_differences(t1, t2, i, j, childPath = "")
    if t1.type != t2.type
      difference_description(i, j, "type", t1.type, t2.type, childPath)
    elsif not t1.kind_of?(SyntaxTree)
      if t1 != t2
	" Alternatives #{i} and #{j} are not SyntaxTree's and differ"
      else
	""
      end
    elsif t1.name != t2.name
      difference_description(i, j, "name", t1.name, t2.name, childPath)
    elsif t1.children_names != t2.children_names
      difference_description(i, j, "children_names",t1.children_names,
			     t2.children_names, childPath)
    else
      t1.childrens.each_with_index do |child, k|
	report = report_on_tree_differences(child, t2[k], i, j,
					    childPath + "." + 
					    t1.children_names[k])
	return report if report.length > 0
      end
      return ""
    end
  end

  protected

  def init_substring(string)
    @substring = string
  end
end
