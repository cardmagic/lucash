class Lucash
  class Lambda
    attr_accessor :arg_names, :ast
    
    def initialize(arg_names, ast)
      @arg_names, @ast = arg_names, ast
    end
  end
end
