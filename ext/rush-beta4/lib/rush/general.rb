module Rush

#lifted form ruvi :)
def self.load_script_file(script)
  # This would be good for omitting the module
  #Rush.send(:module_eval, File.read(script))
  load script
end

def self.curses_start

  Curses.init_screen
  Curses.start_color
  Curses.nonl
  Curses.raw
  Curses.cbreak
  Curses.noecho
  Curses.stdscr.scrollok true
  Curses.stdscr.idlok true
  Curses.setscrreg 0,Curses::lines
  Curses.stdscr.keypad true

  rush_init_curses
end

def self.curses_stop
    Curses.echo
    Curses.nocbreak
    Curses.nl
    Curses.close_screen
end

# Maybe we could later do this in curses aswell ?
def self.bp(message="")
  Binding.of_caller do |binding|

    curses_stop

  puts message
    breakpoint

    curses_start

  end
end

end # module
