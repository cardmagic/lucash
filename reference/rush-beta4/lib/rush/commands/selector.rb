module Rush

class Selector < Command

  respond_to "selector"

  def begin_command(args=nil)
    super

    raise NoInformationException.new(true,"No information passed") if @objects == nil || @objects.empty?

    list = @objects.map{|o| Item.new(o.to_s)}
    b = ComboBox.new(Curses::stdscr,$bw.cury,$bw.curx,10,list)
    b.run

    raise PipelineStopedException.new("Selector canceled") if !b.accepted

    @objects = [b.items[b.item].text]

    # need this
    $bw.clear
    $bw.display 
  end

  def initialize
    super
  end

end

end # module
