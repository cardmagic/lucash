#!/usr/bin/env ruby
# ==THERuSH
# The Highly Experimental Ruby Shell

require 'abbrev'
require 'readline'
require 'history.rb'
require 'builtins.rb'
require 'shellfuncs.rb'
require 'shellwords'

include Builtin
include History
include Shellfuncs
include Readline

## Setup some environment variables

## Array with the Builtins
$builtins = Array[ "cd", "!h" ]

## Math Operators
$mathoprs = Array[ "+", "-", "*", "/", "**" ]

$temp_array = %w{ cd !h }

$path.each do | entry |
if File.directory?( "#{entry}" )
	Dir.foreach( entry ) do | file |
		if File.exist?( "#{entry}/#{file}" ) && !File.directory?( "#{entry}/#{file}" ) && File.executable?( "#{entry}/#{file}" ) && !$temp_array.include?( "#{file}" )
			$temp_array.push( "#{file}" )
		end
	end
end

$comp_list = $temp_array.abbrev

Readline.completion_proc = proc do | string |
	$comp_list[string]
end
end


## The method for the built in calculator
def mathMethod( lhs, opr, rhs )

	## All this Debugging is getting tiring :D
	if $DEBUG
		puts "DEBUGGING VALUES: lhs: " + lhs + " opr: " + opr + " rhs: " + rhs
	end

	## Convert lhs and rhs to integers
	lhs = lhs.to_i
	rhs = rhs.to_i

	case opr
	when "+" ## Our Addition case
		puts lhs + rhs
	when "-" ## Our subtraction case
		puts lhs - rhs
	when "*" ## Our multiplication case
		puts lhs * rhs
	when "/" ## Our Division case
		puts lhs / rhs
	when "**" ## Our exponentiation case
		puts lhs ** rhs
	else
		puts "Illegal operator #{opr}"
	end
end


loop do

	if $DEBUG
		puts "DEBUGGING VALUES: CWD: " + $cwd + " PROMPT: " + $prompt + " at Line #{__LINE__} in #{__FILE__}"
	end

	linein = readline( "#{$prompt}", true )

	if linein.nil? ## Checks for CTRL-D and exits if it is caught
		puts
		exit 0
	elsif linein.chomp.empty? ## Checks to see if the user just hit RETURN and skips to the next loop
		Shellfuncs.envUpdate
		next
	else
		linein = Shellwords.shellwords( linein ) ## Break up the string into tokens so we can do fun things with the "words"
		cmd = linein.at(0) ## Since we reference it so often I gave it its own variable

		if Shellfuncs.inPath( cmd ) ## Runs the command if its in PATH
			Shellfuncs.commandRun( linein )
		elsif cmd.index('/') == 0 ## If the first character is a / then run the command as an absolute path to the command
			Shellfuncs.commandRun( linein )
		elsif $builtins.include?( cmd ) ## Checks against our BUILTINS
			## Temporary solution until I can figure out how to directly use the Hash
			case cmd
			when "cd"
				Builtin.chDir( linein.at(1) )
			when "!h"
				History.histCmd( linein.at(1) )
			end
		elsif $mathoprs.include?( linein.at(1) ) ## Checks for one of the defined Math Operators
			mathMethod( linein.at(0), linein.at(1), linein.at(2) )
		elsif cmd == "exit"
			exit 0
		else
			puts "-rubyshell: " + cmd + ": command not found" ## Error msg for not found command
		end
	end
	Shellfuncs.envUpdate
end
