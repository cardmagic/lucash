module Rush


class Print < Command

  respond_to "print"

  def begin_command(args=nil)
    super
    @objects = [eval(args.to_s).to_s]
  end

  def end_command(*args)
  end

  def initialize
    super
  end

end

end #module
