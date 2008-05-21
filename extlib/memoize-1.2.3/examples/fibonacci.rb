###################################################################
# fibonacci.rb
#
# Demonstrates, via Benchmark, the difference between a memoized
# version of the fibonnaci method versus a non-memoized version.
###################################################################
base = File.basename(Dir.pwd)

if base == "examples" || base =~ /memoize/
   Dir.chdir("..") if base == "examples"
   $LOAD_PATH.unshift(Dir.pwd + "/lib")
   Dir.chdir("examples") rescue nil
end

require "benchmark"
require "memoize"
include Memoize

# Our fibonacci function
def fib(n)
   return n if n < 2
   fib(n-1) + fib(n-2)
end

file    = ENV["HOME"] || ENV["USERPROFILE"]+ "/fib.cache"
max     = ARGV[0].to_i || 100
max_fib = ARGV[1].to_i || 300

print "\nBenchmarking against version: " + MEMOIZE_VERSION + "\n\n"

Benchmark.bm(35) do |x|
   x.report("Not memoized:"){
      max.times{ fib(max_fib) }
   }

   x.report("Memoized:"){
      memoize(:fib)
      max.times{ fib(max_fib) }
   }
   
   x.report("Memoized to file:"){
      memoize(:fib, file)
      max.times{ fib(max_fib) }
   }
end

File.delete(file) if File.exists?(file)