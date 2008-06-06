# NOTE! When updating this file, also update INSTALL, if necessary.
# NOTE! Please keep your tasks grouped together.

LUCASH_BASE = File.expand_path(File.dirname(__FILE__))

task :default => :spec

# BUILD TASKS

desc "Build everything that needs to be built"
task :build => 'build:all'

namespace :build do

  task :all => %w[
    build:grammar
  ]

  desc "Compiles the grammar (with racc)"
  task :grammar do
    Dir.chdir(LUCASH_BASE + "/lib/lucash")
    `racc -o grammar.rb grammar.y`
  end
end

require 'spec/rake/spectask'

desc "Run all specs"
Spec::Rake::SpecTask.new do |t|
  t.spec_files = FileList[
    'spec/**/*_spec.rb'
  ]
  t.rcov = true
  t.rcov_dir = '../doc/output/coverage'
  t.rcov_opts = ['--exclude', 'spec\/spec,bin\/spec,examples']
end
