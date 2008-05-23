module Rush

class Basic < Key

  respond_to [Curses::Key::BACKSPACE,
              Curses::Key::RIGHT,
              Curses::Key::LEFT,
              Curses::Key::UP,
              Curses::Key::DOWN,
              Curses::Key::HOME,
              Curses::Key::END,
              330,                        # 330 = delete key
              27]                         # 27 = esc

    def execute(ch,promptString)

      case ch

        when Curses::Key::RIGHT
          $bw.curx += 1
          $bw.curx = $bw[-1].length if $bw.curx > $bw[-1].length
          $bw.display

        when Curses::Key::LEFT
          $bw.curx -= 1
          $bw.curx = promptString.length if $bw.curx < promptString.length
          $bw.display

        when Curses::Key::HOME
          $bw.curx = promptString.length
          $bw.display

        when Curses::Key::END
          $bw.curx = $bw[-1].length
          $bw.display

        when Curses::Key::UP
          History.current_line -= 1
          History.current_line = 0 if History.current_line < 0
          $bw.lines[-1] = promptString+History[History.current_line]
          $bw.curx = $bw.lines[-1].length
          $bw.display

        when Curses::Key::DOWN
          History.current_line += 1
          History.current_line = History.get.length-1 if History.current_line > History.get.length-1
          $bw.lines[-1] = promptString+History[History.current_line]
          $bw.curx = $bw.lines[-1].length
          $bw.display

        when Curses::Key::BACKSPACE
          $bw.backspace
          $bw.display

        when 330 # Delete key
          $bw.deletekey
          $bw.display

        when 27 # Esc key
          History[-1] = ""
          $bw.curx = promptString.length
          $bw.cury += 1; $bw.lines.push(promptString)
          $bw.show_bottom
          $bw.display

      end

      $bw.display

    end

end

end # module
