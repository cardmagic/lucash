#!/usr/bin/env ruby
# Misc shell functions for THERuSH

require 'history.rb'
include History

module Shellfuncs

	## Variable containing the Working Directory
	$cwd = Dir.pwd

	## Variable containing the Prompt
	$prompt = Dir.pwd + "> "

	## The PATH
	$path = %w{ /bin /sbin /usr/bin /usr/sbin /usr/local/bin}

	## Prints out the prompt
	def promptPrint
		print "#{$prompt}".chomp
	end

	## Updates Environment Variables and the prompt
	def envUpdate
		$cwd = Dir.pwd
		$prompt = $cwd + "> "

		if $DEBUG
			puts "DEBUGGING VALUES: CWD: " + $cwd + " PROMPT: " + $prompt + " at Line #{__LINE__} in #{__FILE__}"
		end
	end

	## Checks cmd to see if its in the PATH
	def inPath( cmd )
		if $DEBUG
			puts "DEBUGGING VALUES: cmd: " + cmd + " at Line #{__LINE__} in #{__FILE__}"
		end

		$path.each do | dir | ## For every entry in the PATH
			if File.exist?( dir + "/" + cmd )
				return true
			end
		end
		return false
	end

	## Runs the command from the command line and pushes it onto the stack
	def commandRun( cmd )
		if $DEBUG
			puts "DEBUGGING VALUES: cmd: " + cmd.join(" ") + " at Line #{__LINE__} in #{__FILE__}"
		end

		system( "#{cmd.join(' ')}" )
		History.histAdd( "#{cmd.join(' ')}" )
	end
end

