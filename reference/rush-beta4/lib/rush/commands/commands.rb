
# Required files
  # Project
  require 'rush/commands/command.rb'

module Rush

class Commands
  @@commands = []

  def self.get
    @@commands
  end

  def self.add(command)
    @@commands.push(command)
  end

  def self.find_respond_to(name)
    @@commands.each do |c|
      c.class.respond_to.each{|rt| return c if rt.downcase == name.downcase}
    end
    return nil
  end
end                                   # class Commands

  # Module initializations
  begin
    Dir.new(File.dirname(File.expand_path(__FILE__))).each do |file|
      if (file.match /.rb$/)
        require "rush/commands/#{file}"
      end                             # if (x.match...)
    end                               # Dir.new(dir)...
  end                                 # initializations

end                                   # module Rush