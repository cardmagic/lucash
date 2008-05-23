class Lucash
  class Ast
    class InvalidAst < StandardError; end
    
    attr_accessor :ast
    
    def initialize(ast=nil)
      @vals = {}
      @ast = ast
    end
    
    def eval
      raise InvalidAst unless @ast.is_a?(Array)
      lucash_eval(@ast)
    end
    
    def lucash_eval(ast)
      case ast[0]
      when :program
        ast[1].map {|stmt| lucash_eval(stmt)}.last
      when :line
        lucash_eval(ast[1])
      when :and
        if lucash_eval(ast[1])
          lucash_eval(ast[2])
        end
      when :if
        if lucash_eval(ast[1])
          lucash_eval(ast[2])
        end
      when :if_else
        if lucash_eval(ast[1])
          lucash_eval(ast[2])
        else
          lucash_eval(ast[3])
        end
      when :for
        for i in (lucash_eval(ast.from)..lucash_eval(ast.to))
          @vals[ast.ident.lexeme] = i
          lucash_eval(ast.statements)
        end
      when :assignment
        @vals[ast[1]] = lucash_eval(ast[2])
      when :functional_assignment
        @vals[ast[1]] = ast[2]
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
        raise InvalidAst
      end
    end
  end
end