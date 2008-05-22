module Indexable
  attr_accessor :index_number  # attr_reader_once_write instead?
  attr_accessor :factory
end

class IndexableFactory
  attr_reader :instances, :start_index, :next_index
  
  def initialize(klass, startIndex = 0)
    unless klass.ancestors.include?(Indexable)
      raise ArgumentError, "#{klass.inspect} is not Indexable" 
    end
    @klass, @start_index, @next_index = klass, startIndex, startIndex
    @instance_map, @instances = Hash.new, Array.new
  end

  def make(*args)
    obj = @instance_map[args]
    unless obj
      @instance_map[args] = obj = make_new_obj(args)
      @instances.push obj
    end
    obj
  end

  def make_unless_exists(*args)
    new_instance = @instance_map[args] == nil
    return make(*args), new_instance
  end

  def get_instance(*args)
    @instance_map[args]
  end

  def instance_with_args(*args)
    @instance_map[args] || (@instance_map[args] = make(*args))
  end

  protected

  def make_new_obj(arguments)
    obj = @klass.new(*arguments)
    obj.index_number = advance_index_number
    obj.factory = self
    obj
  end
  
  def advance_index_number
    i = @next_index
    @next_index += 1
    i
  end
end
