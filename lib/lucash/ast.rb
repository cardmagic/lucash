class Lucash
  class AST    
    attr_accessor :ast
    
    def initialize(ast=nil)
      @vars = {}
      @ast = ast
    end
    
    def eval
      raise InvalidAST unless @ast.is_a?(Array)
      lucash_eval(@ast)
    end
    
    def lucash_eval(ast)
      case ast[0]
      when :program
        ast[1].map {|stmt| lucash_eval(stmt)}.last
      when :and
        if lucash_eval(ast[1])
          lucash_eval(ast[2])
        end
      when :if
        if lucash_eval(ast[1])
          lucash_eval(ast[2])
        elsif ast[3]
          lucash_eval(ast[3])
        end
      when :for
        for i in (lucash_eval(ast[1])..lucash_eval(ast[2]))
          @vars[ast[3]] = i
          lucash_eval(ast[4])
        end
      when :assignment
        @vars[ast[1]] = lucash_eval(ast[2])
      when :functional_assignment
        @vars[ast[1]] = ast[2]
      when :add
        lucash_eval(ast[1]) + lucash_eval(ast[2])
      when :block
        lucash_eval(ast[1])
      when :subtract
        lucash_eval(ast[1]) - lucash_eval(ast[2])
      when :multiply
        lucash_eval(ast[1]) * lucash_eval(ast[2])
      when :divide
        lucash_eval(ast[1]) / lucash_eval(ast[2])
      when :mod
        lucash_eval(ast[1]) % lucash_eval(ast[2])
      when :array
        lucash_eval(ast[1])
      when :splat
        ast[1].map{|a| lucash_eval(a)}
      when :string
        ast[1]
      when :value
        case ast[1][0]
        when "false"
          false
        when "true"
          true
        else
          ast[1]
        end
      when :number
        ast[1]
      else
        raise InvalidAST, ast.inspect
      end
    end
  end
end