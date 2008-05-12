#!/usr/bin/env ruby
# Handles the History functions for THERuSH

module History

	## The Array with the history commands
	HISTORY = Array[]

	## Adds things to the history array ignoring duplicates
	def histAdd( cmd )
		if $DEBUG
			puts "DEBUGGING VALUES: cmd: " + cmd + " at Line #{__LINE__} in #{__FILE__}"
		end

		if !HISTORY.include?( cmd )
			HISTORY.push( cmd )
		end

		if $DEBUG
			puts "DEBUGGING VALUES: HISTORY: " + HISTORY.join( " , ") + " at Line #{__LINE__} in #{__FILE__}"
		end
	end

	## Handles History Addition (!h)
	def histCmd( hist )
		if $DEBUG
			if hist.nil?
				puts "DEBUGGING VALUES: hist: nil at Line #{__LINE__} in #{__FILE__}"
			else
				puts "DEBUGGING VALUES: hist: " + hist + " at Line #{__LINE__} in #{__FILE__}"
			end
		end

		if hist.nil? ## Just entering !h will print out the array
			HISTORY.each do | entry |
				puts HISTORY.index( entry ).to_s + ": " + entry
			end
		else
			hist = hist.to_i
			if HISTORY.at( hist ).nil?
				puts "Invalid history reference"
			else
				system( "#{HISTORY.at( hist )}" )
			end
		end
	end
end

