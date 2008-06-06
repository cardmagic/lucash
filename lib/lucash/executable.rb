class Lucash
  class ExecutableError < StandardError; end
  class Executable
    attr_accessor :name, :path
    
    def initialize(path)
      @name = File.basename(path)
      @path = path
    end
    
    def execute(args)
    	r = []
    	er_t = nil
    	in_t = nil
    	o_t = nil
    	p = Open4::popen4("#{@path} #{args}") do |pid, stdin, stdout, stderr|
    		er_t = Thread.new do
    			loop do
    			  if stderr.stat.size > 0
      				$stderr.print stderr.read(stderr.stat.size)
      				$stderr.flush
    				end
    			end
    		end

    		in_t = Thread.new do
    			loop do
    				data = gets
    				stdin.write(data)
    			end
    		end
	
    		o_t = Thread.new do
    			loop do
    			  if stdout.stat.size > 0
      				r << stdout.read(stdout.stat.size)
    				end
    			end
    		end
    		
    		r << stdout.read unless stdout.eof?
    		unless stderr.eof?
      		$stderr.print stderr.read
      		$stderr.flush if $stderr.stat.size > 0
    		end
    	end
    	
    	$stdout.flush
    	er_t.kill
    	in_t.kill
    	o_t.kill
    	r.join("")
    rescue Errno::ENOENT, TypeError
    	raise ExecutableError, $!.message
    end
  end
end