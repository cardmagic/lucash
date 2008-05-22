module SourceCodeDumpable
  def to_src_in_module(name, moduleName, *restargs)
    str = to_src(name, *restargs)
    "module #{moduleName}\n" +
      indent_lines(str) + (str[-1,1] == "\n" ? "" : "\n") +
      "end\n"
  end

  def to_compact_src(name = nil, nameHash = {})
    to_src(name, nameHash)
  end

  def type_to_src
    self.type.inspect.split("::").last
  end

  def create_new
    type_to_src + ".new(%s)"
  end

  def name_hash(array, &nameGenerator)
    conflicthash, namehash, counts = Hash.new, Hash.new, Hash.new([0,nil])
    array.each do |element|
      name = nameGenerator.call(element)
      if conflicthash[name]
	count, first = counts[name]
	if count == 0
	  namehash.delete name
	  namehash[name + "1"] = first
	  counts[name][0] = 1
	end
	name += "#{counts[name][0] += 1}"
      else
	counts[name] = [0,element]
	conflicthash[name] = true
      end
      namehash[name] = element
    end
    namehash.invert # Assuming all elements are unique
  end
  module_function :name_hash

  def as_code(aString)
    SpecialString.new(aString)
  end
  module_function :as_code

  def parameter_named(name)
    SpecialString.new(aString)
  end

  def new_of_my_type(*args)
    create_new % args.map do |arg| 
      if arg.type == SpecialString
	arg
      elsif arg.methods.include?("to_src")
	arg.to_src
      else
	arg.inspect
      end
    end.join(",")
  end

  protected

  class SpecialString < String; end

  # When producing source code of an object we can choose how the object is
  # to be created/accessed:
  #   * method: By using the 'as_method_named' modifier we get a method
  #   * module_method: 
  #   * default: We get the object in a variable with the given name
  MethodNamed = Struct.new("MethodNamed", :name)
  ModuleMethodNamed = Struct.new("ModuleMethodNamed", :module, :name)

  def as_method_named(name)
    MethodNamed.new(name)
  end
  module_function :as_method_named

  def as_module_method_named(moduleName, methodName)
    ModuleMethodNamed.new(moduleName, methodName)
  end
  module_function :as_module_method_named

  def assign_to(name, str)
    if name.kind_of?(MethodNamed)
      "def #{name.name}\n" + indent_lines(str) + "\nend\n"
    elsif name.kind_of?(ModuleMethodNamed)
      "def #{name.module}.#{name.name}\n" + indent_lines(str) + "\nend\n"
    elsif name != nil
      name + " = " + str
    else
      str
    end
  end

  def new_of_my_type_with_name(name, *args)
    assign_to(name, new_of_my_type(*args))
  end

  def indent_lines(string, indentString = "  ")
    indentString + string.split("\n").join("\n" + indentString) +
      (string[-1,1] == "\n" ? "\n" : "")
  end
  module_function :indent_lines
end

class Array
  include SourceCodeDumpable

  def to_src(name = nil, elementNameHash = {})
    strings = self.map do |e| 
      "  " + e.to_src(elementNameHash[e], elementNameHash)
    end
    ary_src = "[\n" + strings.join(",\n") + "\n]"
    assign_to(name, ary_src)
  end

  def to_compact_src(name = nil, elementNameHash = {})
    strings = self.map do |e| 
      e.to_compact_src(elementNameHash[e], elementNameHash)
    end
    ary_src = "[" + strings.join(", ") + "]"
    assign_to(name, ary_src)
  end
end

class Object
  include SourceCodeDumpable

  def to_src(name = nil, nameHash = {})
    assign_to(name, self.inspect)
  end
end

class Regexp
  def to_src(name = nil, nameHash = {})
    assign_to(name, "/" + source.gsub("/", "\\/") + "/" + (casefold? ? "i" : ""))
  end
end

class Hash
  include SourceCodeDumpable

  def to_src(name = nil, nameHash = {})
    if self.keys.length == 0
      hash_src = "{}" 
    else
      hash_src = "{\n%s\n}" % self.keys.map do |key|
	"  " + key.to_src(nameHash[key], nameHash) + " => " + 
	  self[key].to_src(nameHash[self[key]], nameHash)
      end.join(",\n")
    end
    assign_to(name, hash_src)
  end

  def to_compact_src(name = nil, nameHash = {})
    if self.keys.length == 0
      hash_src = "{}" 
    else
      hash_src = "{%s}" % self.keys.map do |key|
	if self[key]
	  key.to_compact_src(nameHash[key], nameHash) + " => " + 
	    self[key].to_compact_src(nameHash[self[key]], nameHash)
	else
	  nil
	end
      end.compact.join(", ")
    end
    assign_to(name, hash_src)
  end
end

class Integer
  include SourceCodeDumpable

  def to_src(name = nil, nameHash = {})
    assign_to(name, inspect)
  end
end
