class Lucash
  class Variable
    @@scopes = []
    
    def initialize(var)
      self.class.find_through_scopes(var)
    end
    
    def self.set(var, val)
      self.class.current_scope[var] = val
    end
    
    def self.find_through_scopes(var)
      @@scopes.each do |scope|
        if r = scope[var]
          return r
        end
      end
      
      return var
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
  end
end