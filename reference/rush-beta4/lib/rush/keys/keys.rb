require 'rush/keys/key.rb'
module Rush
class Keys
  @@keys = []

  def self.get
    @@keys
  end

  def self.add(key)
    @@keys.push(key)
  end

  def self.find_respond_to(key)
    @@keys.each do |k|
      k.class.respond_to.each{|rt| return k if rt == key}
    end
    return nil
  end
end

  # Module initializations
  begin
    Dir.new(File.dirname(File.expand_path(__FILE__))).each do |file|
      if (file =~ /.rb$/)
        require "rush/keys/#{file}"
      end                             # if (x.match...)
    end                               # Dir.new(dir)...
  end                                 # initializations

end # module
