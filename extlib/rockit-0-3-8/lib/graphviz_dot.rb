class DotGraphFormatter
  @@default_node_shaper = proc{|n| "box"}
  @@default_node_labeler = proc{|n| n.inspect}
  @@default_link_labeler = proc{|info| info ? info.inspect : nil}

  def initialize(nodeShaper = nil, nodeLabeler = nil, linkLabeler = nil,
		 size = "11,9", orientation = "landscape")
    @node_shaper = nodeShaper || @@default_node_shaper
    @node_labeler = nodeLabeler || @@default_node_labeler
    @link_labeler = linkLabeler || @@default_link_labeler
    @size, @orientation = size, orientation
  end

  # nodes is array of node objects
  # links is either array of 
  #                   arrays [fromNode, toNode [, infoOnLink]], or
  #                   objects with attributes :from, :to, :info
  def format(nodes, links)
    DotGraph.new("digraph G {\n" +
		   "size = #{@size.inspect}\n" +
		   "orientation = #{@orientation}\n" +
		   nodes.uniq.map {|n| format_node(n)}.join("\n") + "\n" +
		   links.uniq.map {|l| format_link(l)}.join("\n") + "\n" +
		 "}"
		 )
  end

  protected

  def format_node(node)
    node.id.inspect + " [" + 
      "shape=" + @node_shaper.call(node).inspect + ", " +
      "label=" + @node_labeler.call(node).inspect + "]" 
  end

  def get_link_data(link)
    begin
      return link.from, link.to, link.info
    rescue Exception
      return link[0], link[1], link[2]
    end
  end
  
  def format_link(link)
    from, to, info = get_link_data(link)
    label = @link_labeler.call(info)
    from.id.inspect + " -> " + to.id.inspect +
      (label ? " [label=" + label.inspect + "]" : "")
  end
end

DotGraph = Struct.new("DotGraph", :description)
class DotGraph
  def write_to_file(filename)
    tmpfile = filename + rand(100000).inspect
    while test(?f, tmpfile)
      tmpfile = filename + rand(100000).inspect
    end
    File.open(tmpfile, "w") {|f| f.write description}
    system "dot -Tps -o #{filename} #{tmpfile}"
    File.delete(tmpfile)
  end
end
