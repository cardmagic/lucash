def do_command(command)
	case command
	when "true", "false"
		return command == "true"
	when /^cd (.*)$/
		return Dir.chdir($1)
	end

	r = []
	er_t = nil
	in_t = nil
	o_t = nil
	p = Open4::popen4(command) do |pid, stdin, stdout, stderr|
		er_t = Thread.new do
			loop do
				$stderr.print stderr.read(stderr.stat.size)
				$stderr.flush
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
				r << stdout.read(stdout.stat.size)
			end
		end
	end
	er_t.kill
	in_t.kill
	o_t.kill
	r.join("")
rescue Errno::ENOENT, TypeError
	command
end
