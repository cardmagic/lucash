module Rush
class Enter < Key

  respond_to [13] # 13 = enter

    def execute(ch,promptString)

      ret = ""
      begin
        line = $bw.get_current_line[promptString.length..-1]
        History[-1] = line
        if line != ""
          History.get.push("")
          History.current_line = History.get.length-1
        end
        ret = Parser.execute(line)
      rescue => detail
        ret = $!.to_s+"\n" #+detail.backtrace.join("\n")
      rescue SyntaxError => detail
        ret = $!.to_s+"\n" #+detail.backtrace.join("\n")
      end

      # Reset the prompt screen here, it might have been changed
      promptString = eval($env[:prompt])
      ret.split("\n").map{|l| $bw.add_line(l); $bw.cury += 1} if ret != nil
      $bw.cury += 1; $bw.lines.push(promptString)
      $bw.curx = promptString.length
      $bw.show_bottom
      $bw.display

    end

end
end # module
