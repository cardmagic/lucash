module Rush

require 'optparse'

class Command

  attr_accessor :objects

  # What name should a command respond to
  def self.respond_to(names)
      meta_def :respond_to do; names; end
  end

  # When something inherits from Command we
  # add it to the commands list
  def self.inherited(classname)
    eval("Commands.add(#{classname}.new)")
    super
  end

  def writeObject(obj)
    @objects += obj
  end

  def print_collection
    # If there are objects left after all the commands, print them
    return if @objects.empty?

    ret = ""

    currentType = nil
    properties = nil
    @objects.each do |o|

      # Check if the header should be written again
      if currentType != o.class
        currentType = o.class

        if o.respond_to?("properties")
          properties = o.properties
          # Write the headers
          o.properties.each {|p| ret += p[0] + "\t"}
          ret += "\n"
          o.properties.each do |p|
            ret += "-"*p[0].length
            ret += "\t"
          end
          ret += "\n"
        end

      end

      # Write the object
      if o.respond_to?("properties")
        o.properties.each{|p| ret += p[1] + "\t"}
      else
        ret+=o.to_s
      end

      ret += "\n"
    end
    return ret
  end

  def begin_command(args=nil)
  end

  def end_command(args)
  end

  def initialize
    @objects = []
  end

end

end #module
