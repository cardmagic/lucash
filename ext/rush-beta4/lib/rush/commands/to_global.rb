module Rush


class ToGlobal < Command

  respond_to "toglobal"

  def begin_command(args="glob")
    super
    eval("$#{args} = @objects.clone")
    @objects = []
   end

end

end
