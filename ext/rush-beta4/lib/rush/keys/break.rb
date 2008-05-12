module Rush

class Break < Key

  respond_to [3] # 3 = ctrl+c

    def execute(ch,promptString)
      curses_stop
      exit
    end

end

end # module
