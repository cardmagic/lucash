class Lucash
  class Variable
    @@scopes = [{}]
    
    def initialize(var)
      val = var.shift
      @value = self.class.find_through_scopes(val)
      @arguments = var
    end
    
    def value
      case @value
      when Executable
        @value.execute(@arguments.join(" "))
      else
        @value
      end
    end
    
    def self.scopes
      @@scopes
    end
    
    def self.current_scope
      @@scopes.last
    end
    
    def self.push_scope
      @@scopes.unshift({})
    end

    def self.pop_scope
      @@scopes.shift
    end
    
    def self.set(var, val)
      current_scope[var[1]] = val
    end
    
    def self.find_through_scopes(var)
      @@scopes.each do |scope|
        if r = scope[var]
          return r
        end
      end

      shell = Shell.new

      if shell.executables[var]
        return shell.executables[var]
      end
      
      return var
    end
  end
end