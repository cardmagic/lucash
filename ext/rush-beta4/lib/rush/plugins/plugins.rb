module Rush
  # Module initializations
  begin
    Dir.new(File.dirname(File.expand_path(__FILE__))).each do |file|
      if (file.match /.rb$/)
        require "rush/plugins/#{file}"
      end                             # if (x.match...)
    end                               # Dir.new(dir)...
  end                                 # initializations

end                                   # module Rush
