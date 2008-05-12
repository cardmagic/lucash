module Rush
class Key
  # What name should a command respond to
  def self.respond_to(names)
      meta_def :respond_to do; names; end
  end

  # When something inherits from Command we
  # add it to the commands list
  def self.inherited(class_name)
    eval("Keys.add(#{class_name}.new)")
    super
  end

  def execute(ch, prompt_string)
    raise Exception.new("#execute not implemented for key binding!")
  end

end
end
