
module Rush
class History

   @@lines = [""]
   @@current_line = 0

   def self.get
     return @@lines
   end

   def self.[] ( arg )
     return @@lines[ arg ]
   end
   def self.[]= ( arg, value )
     @@lines[ arg ] = value
   end


   def self.current_line
     return @@current_line
   end

   def self.current_line=(value)
     @@current_line = value
   end

end
end # module
