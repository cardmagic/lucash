class Lucash
  class Paths
    def executables
      @@executables
    end
    
    def initialize
      @@executables ||= {}
      refresh_executables if @@executables == {}
    end
    
    def refresh_executables
      ENV['PATH'].split(":").each do |path|
        load_executables(Dir["#{path}/*"])
      end
    end
    
    def load_executables(paths)
      paths.each do |path|
        if File.executable?(path)
          executable = Executable.new(path)
          @@executables[executable.name] = executable
        end
      end
    end
  end
end