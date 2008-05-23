module Rush


class Out < Command

  respond_to "out"

  def begin_command(args=nil)
    super

    args = "out" if args.nil?

    file = File.open(args,"w")
    @objects.each{|o| file << o.to_s << "\n"}
    file.close
  end

  def initialize
    super
  end

end

end #module
