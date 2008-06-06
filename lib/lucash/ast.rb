class Lucash
  class AST    
    def self.eval(ast=nil)
      raise InvalidAST unless ast.is_a?(Array)
      new.eval(ast)
    end
    
    def in_scope
      Lucash::Variable.push_scope
      r = yield
      Lucash::Variable.pop_scope
      r
    end
    
    def eval(ast=nil)
      case Array(ast).shift
      when :program
        in_scope { ast[0].map {|stmt| eval(stmt)}.last }
      when :lambda
        Lambda.new(eval(ast[0]), ast[1])
      when :method
        
      when :args
        lambda = eval(ast[0])
        args = eval(ast[1])
        if lambda.is_a?(Lucash::Lambda)
          in_scope do
            lambda.arg_names.each_with_index do |arg_name, i|
              Variable.set([:value, arg_name], args[i])
            end
            puts lambda.ast.inspect if ENV['DEBUG']
            eval(lambda.ast)
          end
        end
      when :and
        if eval(ast[0])
          eval(ast[1])
        end
      when :if
        if eval(ast[0])
          eval(ast[1])
        elsif ast[2]
          eval(ast[2])
        end
      when :for
        for i in (eval(ast[0])..eval(ast[1]))
          Variable.set(ast[2], i)
          eval(ast[3])
        end
      when :assignment
        Variable.set(ast[0], eval(ast[1]))
      when :==
        eval(ast[0]) == eval(ast[1])
      when :+
        eval(ast[0]) + eval(ast[1])
      when :-
        eval(ast[0]) - eval(ast[1])
      when :*
        eval(ast[0]) * eval(ast[1])
      when :slash
        eval(ast[0]) / eval(ast[1])
      when :%
        eval(ast[0]) % eval(ast[1])
      when :array
        eval(ast[0])
      when :splat
        ast[0].map{|a| eval(a)}
      when :string, :embedded_string
        ast[0]
      when :value
        variable = Variable.new(ast)
        variable.value
      when :number
        ast[0]
      else
        raise InvalidAST, ast.inspect
      end
    end
  end
end