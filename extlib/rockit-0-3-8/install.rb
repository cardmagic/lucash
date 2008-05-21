require 'rbconfig'
require 'find'
require 'ftools'

include Config

$version = CONFIG["MAJOR"]+"."+CONFIG["MINOR"]
$libdir = File.join(CONFIG["libdir"], "ruby", $version)

$bindir =  CONFIG["bindir"]
$sitedir = CONFIG["sitedir"]

# Generate version.rb file
version = nil
File.open("VERSION", "r") {|f| version = f.read.chomp.split(".")}
File.open("lib/version.rb", "w") do |f|
  f.write <<EOS
def rockit_version
  "#{version.join('.')}"
end
EOS
end

# Bootstrap parser if not already done
if !test(?f, "lib/rockit_grammars_parser.rb")
  $stderr.puts "Bootstrapping! This will take some time. Please be patient."
  Dir.chdir "lib"
  system "ruby bootstrap.rb"
  Dir.chdir ".."
end

installdir = "rockit"

# Install the lib files
dest = File.join($sitedir, installdir)
File::makedirs(dest)
File::chmod(0755, dest, true)

def copy_file_while_inserting_path_in_requires(srcfile, destfile, 
					       installdir, libfiles = [],
					       &outputblock)
  File.open(srcfile, "r") do |ip|
    File.open(destfile, "w") do |op|
      filecontent = ip.read.gsub(/require '(.+)'/) do |m|
	if libfiles.include?($1 + ".rb")
	  "require '#{installdir}/#{$1}'"
	else
	  m
	end
      end
      outputblock.call(op) if outputblock
      op.write filecontent
    end
  end
end

$stderr.puts "Installing library files"
libfiles = Dir["lib/*.rb"].collect {|s| s[4..-1]}
dir = Dir.open("lib")
dir.each do |file|
  next if file[0] == ?. or file.split(".").last != "rb"
  copy_file_while_inserting_path_in_requires(File.join("lib", file),
					     File.join(dest, file),
					     installdir, libfiles)
end

# and the executable
$stderr.puts "Installing executable"
copy_file_while_inserting_path_in_requires("lib/rockit.rb", "rockit",
					   installdir, libfiles) do |op| 
  op.puts "#!#{File.join($bindir, 'ruby')}"
end

opfile = "rockit"
File::install("rockit", File.join($bindir, opfile), 0755, true)
File.delete "rockit"

    
