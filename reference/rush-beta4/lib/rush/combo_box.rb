module Rush

class Item
  attr_accessor :text, :tag, :align
  
  def initialize(text,tag=0,align=:left)
	@align = align
    @text = text
    @tag = tag
  end
  
end

class ComboBox
  
  attr_accessor :item,:items,:width,:x,:y,:done,:maxshown,:accepted,:currentword
  
  def initialize(parent,y,x,maxshown=10,items = [],currentword="")

	@parent = parent   
    @maxshown = maxshown
    @x = x
    @y = y
    @top = 0
    @width = 0
    @item = 0
    @items=[]
	@currentword = currentword

	# Status
    @done = false
	@accepted = false # Did the user press enter or escape
    
    add_items(items)
    
    # Make sure we dont show more than we can
    @maxshown = @items.length if @maxshown > @items.length

    # Make sure the subwindow is on the screen
    @y = Curses::lines-@maxshown if @y+@maxshown > Curses::lines-1
    @x = @parent.maxx-@width if @x+@width > @parent.maxx

	# Unfortunatly because of this we cant inherit from
	# Curses::Window ... or can we ?
	@subwindow = @parent.subwin(@maxshown,@width,@y,@x) 

	# Set the background color
	@subwindow.bkgdset(" "[0] | Curses::color_pair(3))
    @subwindow.keypad(true)

    @subwindow.clear

  end
  
  def display

    @subwindow.clear
    
    0.upto(@maxshown-1) do |i|
      
      @subwindow.setpos(i,0)
      item = @items[@top+i]
      text = item.text

      # Check what the alignment should be
      if item.align == :center
        frontspaces = (@width/2)-(text.length/2)
        text = " "*frontspaces+text
        #backspaces = (@width/2)-(text.length/2)
        #text += " "*backspaces
      end 

      if item.align == :right
        frontspaces = @width-text.length
        text = " "*frontspaces+text
      end 
      
      if @top+i == @item
        spaceLeft = (@width-text.length)
        str = text+" "*spaceLeft
        
        @subwindow.attron(Curses.color_pair(2))
        @subwindow.addstr(str) if !@items.empty?

      else
        #spaceLeft = (@width-@items[@top+i].text.length)
        
        @subwindow.attron(Curses.color_pair(3)){
          @subwindow.addstr(text) if !@items.empty?
        }
        #@window.attroff(Ncurses.COLOR_PAIR(2))
      end unless @items.empty?
      
    end
    
    @subwindow.refresh()
  end
  
  def run
    i,fits,matches = try_completion(@currentword) #downcase
    
    if matches == 1
       @currentword = @items[i].text
       @accepted = true
       @done = true
       return
    end

    @done = false
    choose_item(try_completion(@currentword)[0]) #downcase
    while !@done do
      display()
      focus()
    end
  end
  
  def focus()
    ch = @subwindow.getch()
    case ch
      
      when 338 # page down
        @item += @maxshown 
        @item = @items.length-1 if @item > @items.length-1
        @top += @maxshown
        @top = @items.length-@maxshown if @top > @items.length-@maxshown

      when 339 # page up
        @item -= @maxshown 
        @item = 0 if @item < 0
        @top -= @maxshown
        @top = 0 if @top < 0

      when Curses::Key::DOWN
        return if @item == @items.length-1 # We are at the bottom
        @item += 1
        @top += 1 if @item >= @top+@maxshown
        
      when Curses::Key::UP 
        return if @item == 0 # We are at the top
        @item -= 1 if @item > 0
        @top -= 1 if @item < @top
      
      when Curses::Key::ENTER 
        @done = true
        
      when 13 # enter
        @done = true
		@accepted = true
        @currentword = @items[@item].text
	    
      when 27 # esc
        @done = true
		@accepted = false # The user just wants to exit

      when Curses::Key::BACKSPACE
        if @currentword == ""
          @accept = false
          @done = true
        end
        @currentword = @currentword[0..-2]
        choose_item(try_completion(@currentword)[0]) #downcase
        display
      else
        @currentword += "%c"%ch        

        i,fits,matches = try_completion(@currentword) #downcase

        if i == 0 or !fits
          @accepted = true
          @done = true
        end

        if matches == 1
          @currentword = @items[i].text
          @accepted = true
          @done = true
        end

        choose_item(i)
		display

    end
      
  end

  # Set an item as selected and make sure it is shown
  def choose_item(num)
    @item = num
	@top = @item-(@maxshown/2)
	@top = @items.length-@maxshown if @top > @items.length-@maxshown
    @top = 0 if @top < 0
  end

  # sorted could be optimized alot
  def try_completion(word)
	# find the item that matches the best
	bestitem = 0
    bestamount = 0

    howmanymatchbest = 0

    itemcount = 0
    @items.each do |i|
      charnum = 0

      # Check how many match
      0.upto(i.text.length-1) do |c|
        break if i.text[c] != word[c]
        charnum += 1 
      end

      if charnum == bestamount
        howmanymatchbest += 1
      end

      if charnum > bestamount
        bestitem = itemcount
        bestamount = charnum
        howmanymatchbest = 1
      end

      itemcount += 1
    end

    all = (@items[bestitem].text =~ /#{word}/) #downcase
    all.nil? ? fits = false : fits = true

    return bestitem, fits , howmanymatchbest
  end
    
  def destroy
    @subwindow.close()	
  end
    
  def add_items(items)
    items.each do |i|
      @width = i.text.length if i.text.length > @width
      @items.push(i)
    end
  end

end

end # module
