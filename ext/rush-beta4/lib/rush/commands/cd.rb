module Rush


class Cd < Command

  respond_to "cd"

  def begin_command(args=ENV["HOME"])
    super
    Dir.chdir(args)
  end

end

end # module
