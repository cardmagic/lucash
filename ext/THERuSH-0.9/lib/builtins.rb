#!/usr/bin/env ruby
# The library of builtin functions for THERuSH

module Builtin

	## Handles the cd builtin
	def chDir( dir )

		## Only enabled if invoked with -d
		if $DEBUG
			if !dir.nil?
				puts "DEBUGGING VALUES: dir: " + dir + " at Line #{__LINE__} in #{__FILE__}"
			elsif dir.nil?
				puts "DEBUGGING VALUES: dir: nil at Line #{__LINE__} in #{__FILE__}"
			end
		end

		if dir.nil? || dir == "~"
			Dir.chdir( ENV["HOME"] )
		elsif dir.index("/") == 0 ## Means we were passed an absolute pathname
			if FileTest.directory?( dir )
				Dir.chdir( dir )
			else
				puts dir + " does not exist"
			end
		elsif dir == "." || dir == ".." ## Special Case that doesn't need checking
			Dir.chdir( dir )
		else ## Relative pathname
			if FileTest.directory?( dir )
				Dir.chdir( dir )
			else
				puts dir + " does not exist"
			end
		end
	end

end


