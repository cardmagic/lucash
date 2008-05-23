module Rush

class BufferedWindow < Curses::Window

  attr_accessor :curx,:cury,:top,:lines,:width,:height,:done

  def get_current_line
    @lines[cury]
  end

  def initialize(height,width,y,x)

    # state
    @done = false

    # Position
    @x = x
    @y = y
    @top = @curx = @cury = 0

    # Dimensions
    @width = width
    @height = height

    # Contents
    @lines = [""]

    super(@height,@width, @y, @x)

    scrollok true
    idlok true
    setscrreg 0,@y

    # Window options
    #@window.intrflush(false) # turn off flush-on-interrupt
    keypad(true)     # turn on keypad mode
    setpos(0,0)

    # Set the main window colors
    bkgdset(" "[0] | Curses::color_pair(1))
    clear
  end

  # Main loop
  def run()
    # Main loop; get a character, process and show
    while not @done
      # Prepare
      ch = getch #@window.getch()

      # Execute
      yield ch

      # Refresh
      #show_bottom
      display
    end                               # while not @done
  end                                 # run()

  def backspace

    return if @curx == eval($env[:prompt]).length

    if @curx == @lines[@cury].length
      @lines[@cury] = @lines[@cury][0..-2]
      @curx -= 1
      return
    end

    @lines[@cury].slice!(@curx-1)
    @curx -= 1

  end

  def deletekey
    @lines[@cury].slice!(@curx)
  end


  def type_string(s)

    exit if @lines[@cury] == nil
    @lines[@cury].insert(@curx,s)
    @curx += s.length

  end

  def display

    setpos(0,0)
    theend = @top+@height
    theend = @lines.length if theend > @lines.length

    extralines = 0

    @top.upto(theend-1) do |i|
      i == theend-1 ? addstr("#{@lines[i]}") : addstr("#{@lines[i]}\n")
      extralines += (@lines[i].length/@width) if i != theend-1
    end

    clrtoeol

    # The last line is special
    lastliney = @lines[-1].length/@width

    # The last line is special
    linesonscreen = @cury-@top

    # Get where x should be
    (@curx/@width) == 0 ? xpos = @curx : xpos = @curx - @width*lastliney

    # Check if we are at the end of the screen
    if linesonscreen+extralines >= @height-1
      endofscreen = true
    else
      endofscreen = false
    end

    # Calculate how much the screen will scroll automatically
    willscroll = linesonscreen+extralines+lastliney-(@height-1)
    willscroll = 0 if willscroll < 0

    # Check what the ypos should be
    ypos = linesonscreen+extralines+(@curx/@width)

    if ypos > @height-1 and endofscreen
      ypos = linesonscreen-lastliney+(@curx/@width)
    end

    setpos(ypos,xpos)

    refresh()

    #file = File.open("log","a")
  #file << "-------------------\n"
    #file << "@top\t-\t#{@top}\n"
    #file << "@width\t-\t#{@width}\n"
    #file << "@height\t-\t#{@height}\n"
    #file << "xpos\t-\t#{xpos}\n"
    #file << "ypos\t-\t#{ypos}\n"
    #file << "@curx\t-\t#{@curx}\n"
    #file << "@cury\t-\t#{@cury}\n"
    #file << "willscroll\t-\t#{willscroll.to_s}\n"
    #file << "lastliney\t-\t#{lastliney.to_s}\n"
    #file << "endofscreen\t-\t#{endofscreen.to_s}\n"
    #file << "extralines\t-\t#{extralines.to_s}\n"
    #file << "linesonscreen\t-\t#{linesonscreen.to_s}\n"
    #file.close

  end

  def [] ( arg )
    return @lines[ arg ]
  end

  def cursor_to(y,x)
    #@window.move(y,x)
  end

  def show_bottom
    #@window.move(y,x)
    @top = @lines.length-@height
    @top = 0 if top < 0
  end

  def length
    return @lines.length
  end

  def add_line(line)
    @lines.push(line)
  end

end

end
