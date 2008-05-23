module Rush

class Alias

   @@aliases = {}
   def self.set(key,replace)
     if replace == ""
       @@aliases.delete(key)
       return
     end
     @@aliases[key] = replace
   end

   def self.get(key)
     ret = @@aliases[key]
     ret = key if ret.nil?
     return ret
   end

   def self.list
     ret = ""
     @@aliases.each_pair{|key,value| ret += "#{key}\t-\t#{value}\n"}
     return ret
   end

end

end
