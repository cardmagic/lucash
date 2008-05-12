module Rush


class Ls < Command

  respond_to "ls"

  def begin_command(args=".")
    super
    args = "." if args.nil?
    @objects = Dir.entries(args).sort
  end

  def end_command(*args)
  end

  def initialize
    super
  end

end

end # Module
