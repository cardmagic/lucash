module Rush


class Bracket < Command

  respond_to "b"

  def begin_command(args=nil)
    super
    @objects.map!{|o| "("+o.to_s+")"}
  end

  def initialize
    super
  end

end

end # module
