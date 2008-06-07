class Lucash
  class AST    
    def self.eval(ast=nil)
      raise InvalidAST unless ast.is_a?(Array)
      new.eval(*Array(ast))
    end
    
    def in_scope
      Lucash::Variable.push_scope
      r = yield
      Lucash::Variable.pop_scope
      r
    end
    
    def eval(car, *cdr)
      case car
      when :program
        in_scope { cdr[0].map {|stmt| eval(*stmt)}.last }
      when :lambda
        Lambda.new(eval(*cdr[0]), cdr[1])
      when :method
        
      when :args
        lambda = eval(*cdr[0])
        args = eval(*cdr[1])
        if lambda.is_a?(Lucash::Lambda)
          in_scope do
            lambda.arg_names.each_with_index do |arg_name, i|
              Variable.set([:value, arg_name], args[i])
            end
            puts lambda.ast.inspect if ENV['DEBUG']
            eval(*lambda.ast)
          end
        end
      when :and
        if eval(*cdr[0])
          eval(*cdr[1])
        end
      when :if
        if eval(*cdr[0])
          eval(*cdr[1])
        elsif cdr[2]
          eval(*cdr[2])
        end
      when :for
        for i in (eval(*cdr[0])..eval(*cdr[1]))
          Variable.set(cdr[2], i)
          eval(*cdr[3])
        end
      when :assignment
        Variable.set(cdr[0], eval(*cdr[1]))
      when :==
        eval(*cdr[0]) == eval(*cdr[1])
      when :+
        eval(*cdr[0]) + eval(*cdr[1])
      when :-
        eval(*cdr[0]) - eval(*cdr[1])
      when :*
        eval(*cdr[0]) * eval(*cdr[1])
      when :slash
        eval(*cdr[0]) / eval(*cdr[1])
      when :%
        eval(*cdr[0]) % eval(*cdr[1])
      when :array
        eval(*cdr[0])
      when :splat
        cdr[0].map{|a| eval(*a)}
      when :string, :embedded_string
        cdr[0]
      when :value
        variable = Variable.new(*Array(cdr))
        variable.value
      when :number
        cdr[0]
      else
        raise InvalidAST, [car, *cdr].inspect
      end
    end
  end
end