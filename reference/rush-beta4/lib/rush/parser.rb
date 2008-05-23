module Rush

class Parser

  def initialize
  end

  def self.execute(x)
    # Split the line seperated by pipes
    commands = x.split("|").map{|c| c.strip}
  
    objects = []
    command = nil
  
    r = "" # bad bad bad
    commands.each do |c|
   
      begin

      s = c.split(" ")
      commandString = s[0]
      rest = c[commandString.length+1..c.length]
      calias = Alias.get(commandString)
      command = Commands.find_respond_to(calias)

      if calias.strip[0] == 33 #"!"
        cid, stdin, stdout, stderr = Open4::popen4("#{calias.strip[1..-1]} #{rest}")
        stdin.close
        pid, status = Process::waitpid2 cid
        out = stdout.read
        err = stderr.read

        r = out.strip
        r += "Errors:\n#{err}" if err != ""
        objects = [r]
        next
      end
    
      if calias.strip[0] == 42 #"*"
        cid, stdin, stdout, stderr = Open4::popen4("#{calias.strip[1..-1]} #{rest}")
        stdin.close
        pid, status = Process::waitpid2 cid
        out = stdout.read
        err = stderr.read

        r = out.strip
        r += "Errors:\n#{err}" if err != ""
        objects = r.split("\n")
        next
      end
    
      if calias.strip[0] == 94 #"^"
        Rush.curses_stop
        system("#{calias.strip[1..-1]} #{rest}")
        r = ""
        objects = []
        Rush.curses_start
        next
      end
    
        
      # Could not find the command, try evaluating the ruby string
      # if it evaluates, set it as the current 'objects'
      if command == nil
        begin
          objects = r = eval(c)
          r = r.to_s if !r.nil?
          objects = [objects] if objects.class != Array
          next
        rescue LoadError => detail
          return "|#{s}| "+$!.to_s+"\n" 
        rescue SyntaxError => detail
          return "|#{s}| "+$!.to_s+"\n"
        rescue => detail
          return "|#{s}| "+detail.message
        end
      end

      #ouch
      r = ""

      command.objects = objects
      evalstr = "command.begin_command"
      evalstr += "\"#{rest.gsub("\"","\\\"")}\"" if !rest.nil?
      eval(evalstr)
      objects = command.objects.clone

    rescue DebugException => detail
      bp("|#{c}| "+detail.message)
      return "|#{c}| "+detail.message 

    rescue => detail
      return "|#{c}| "+detail.message
    end 

    end # end foreach part of the pipe
  
    return r if !r.nil? and r != "" and (command.nil? || command.empty?)
    return command.print_collection if command != nil
  end

end

end # module

