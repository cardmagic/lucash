class Lucash
  class AST    
    attr_accessor :ast
    
    def initialize(ast=nil)
      @ast = ast
    end
    
    def eval
      raise InvalidAST unless @ast.is_a?(Array)
      lucash_eval(@ast)
    end
    
    def lucash_eval(ast)
      case ast.shift
      when :program
        Lucash::Variable.push_scope
        ast[0].map {|stmt| lucash_eval(stmt)}.last
        Lucash::Variable.pop_scope
      when :lambda
        Lucash::Lambda.new(lucash_eval(ast[0]), ast[1])
      when :method
        
      when :args
        lambda = lucash_eval(ast[0])
        args = lucash_eval(ast[1])
        if lambda.is_a?(Lucash::Lambda)
          Lucash::Variable.push_scope
          lambda.arg_names.each_with_index do |arg_name, i|
            Lucash::Variable.set(arg_name, args[i])
          end
          lucash_eval(lambda.ast)
          Lucash::Variable.pop_scope
        end
      when :and
        if lucash_eval(ast[0])
          lucash_eval(ast[1])
        end
      when :if
        if lucash_eval(ast[0])
          lucash_eval(ast[1])
        elsif ast[2]
          lucash_eval(ast[2])
        end
      when :for
        for i in (lucash_eval(ast[0])..lucash_eval(ast[1]))
          Lucash::Variable.set(ast[2], i)
          lucash_eval(ast[3])
        end
      when :assignment
        Lucash::Variable.set(ast[0], lucash_eval(ast[1]))
      when :==
        lucash_eval(ast[0]) == lucash_eval(ast[1])
      when :+
        lucash_eval(ast[0]) + lucash_eval(ast[1])
      when :-
        lucash_eval(ast[0]) - lucash_eval(ast[1])
      when :*
        lucash_eval(ast[0]) * lucash_eval(ast[1])
      when :slash
        lucash_eval(ast[0]) / lucash_eval(ast[1])
      when :%
        lucash_eval(ast[0]) % lucash_eval(ast[1])
      when :array
        lucash_eval(ast[0])
      when :splat
        ast[0].map{|a| lucash_eval(a)}
      when :string, :embedded_string
        ast[0]
      when :value
        Lucash::Variable.new(ast)
      when :number
        ast[0]
      else
        raise InvalidAST, ast.inspect
      end
    end
  end
end