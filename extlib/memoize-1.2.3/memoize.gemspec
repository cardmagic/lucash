require "rubygems"

spec = Gem::Specification.new do |gem|
   gem.name        = "memoize"
   gem.version     = "1.2.3"
   gem.author      = "Daniel J. Berger"
   gem.email       = "djberg96@gmail.com"
   gem.homepage    = "http://www.rubyforge.org/projects/shards"
   gem.platform    = Gem::Platform::RUBY
   gem.summary     = "Speeds up methods at the cost of memory (or disk space)"
   gem.description = "Speeds up methods at the cost of memory (or disk space)"
   gem.test_file   = "test/tc_memoize.rb"
   gem.has_rdoc    = true
   gem.files = Dir['lib/*.rb'] + Dir['[A-Z]*']  + Dir['test/*']
   gem.files.reject! { |fn| fn.include? "CVS" }
   gem.require_path = "lib"
   gem.extra_rdoc_files = ["MANIFEST","README","CHANGES"]
end

if $0 == __FILE__
   Gem.manage_gems
   Gem::Builder.new(spec).build
end
