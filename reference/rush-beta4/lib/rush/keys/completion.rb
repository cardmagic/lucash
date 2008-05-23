module Rush

class Completion < Key

  respond_to [46,9] # 46 = . 9 = tab

  # Try checking if it is a ruby object
  def rubysyntax_completion(cmd,rest,allowmethods)

    # Try completing as a ruby object
    begin

      return false if cmd == ""
      cmd_is = eval("defined? #{cmd}")
      return true if cmd_is == "method"
      typestring = "Class::"+eval(cmd).class.name
      list = eval("[\"#{typestring}\"]+#{cmd}.methods.sort!").map{|m| Item.new(m)}
      return false if list.empty?
      list[0].align = :center
      b = ComboBox.new($bw,$bw.cury,$bw.curx-rest.length,10,list,rest)
      b.run
      if b.accepted
        0.upto(rest.length-1){|i| $bw.backspace}
        $bw.type_string(b.currentword)
      end
      b.destroy
      b = nil
      $bw.show_bottom
    rescue => detail
    rescue LoadError => detail
    rescue SyntaxError => detail
    ensure
      $bw.clear
    end

  end

  # Try completing as a file/directory
  def directory_completion(promptString)

    begin
      l = $bw.get_current_line[promptString.length..-1]
      line = l.split(" ")[-1]

      if line[-1].nil? # or !line[-1].include?("/")
        dir = "./"
        filename = ""
      elsif l[-1] == " "[0]
        dir = "./"
        filename = ""
      elsif !line.include?("/")
        dir = "./"
        filename = line
      elsif line[-1] == "/"[0]
        dir = line.split("/")[0..-1].join("/")
        filename = ""
      else
        dir = line.split("/")[0..-2].join("/")
        filename = line.split("/")[-1]
      end

      dir = "/" if dir == ""

      list = Dir.entries(dir).sort{|a,b| a.downcase <=> b.downcase}.map{|m| Item.new(m)}
      list = list[2..-1]

      return false if list == []

      b = ComboBox.new($bw,$bw.cury,$bw.curx-filename.length,10,list,filename)
      b.run
      if b.accepted
        0.upto(filename.length-1){|i| $bw.backspace}
        $bw.type_string(b.currentword)
      end
      b.destroy
      b = nil
      $bw.show_bottom

      return true

    rescue => detail
    rescue LoadError => detail
    rescue SyntaxError => detail
    ensure
      $bw.clear
    end

    return false
  end

  def execute(ch,promptString)

    case ch

      when 9  # tab

        # Try completing as a ruby object

        l = $bw.get_current_line[promptString.length..-1]
        throw :done if l == ""
        line = l.split(" ")[-1]
        if line[-1] == "."[0]
          cmd = line.split(".")[0..-1].join(".")
          rest = ""
        else
          cmd = line.split(".")[0..-2].join(".")
          rest = line.split(".")[-1]
        end

        syn = dir = false
        syn = rubysyntax_completion(cmd,rest,true)
        dir = directory_completion(promptString) if !syn

        Curses.beep if !dir and !syn

        $bw.display

      when 46 # . was pressed

        # Read the current line
        $bw.type_string(".")
        $bw.display

        cmd = $bw.get_current_line[promptString.length..-2].split(" ")[-1]
        Curses.beep if !rubysyntax_completion(cmd,"",false)

    end

    $bw.display

  end

end

end #module
