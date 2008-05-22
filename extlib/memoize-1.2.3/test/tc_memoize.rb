###############################################
# tc_memoize.rb
#
# Test suite for the memoize package.
###############################################
base = File.basename(Dir.pwd)

if base == "test" || base =~ /memoize/
   Dir.chdir("..") if base == "test"
   $LOAD_PATH.unshift(Dir.pwd + "/lib")
   Dir.chdir("test") rescue nil
end

require "test/unit"
require "memoize"

class TC_Memoize < Test::Unit::TestCase
   include Memoize

   def setup
      @cache1 = nil
      @cache2 = nil
      @file   = (ENV["HOME"] || ENV["USERPROFILE"]) + "/test.cache"
   end

   def fib(n)
      return n if n < 2
      fib(n-1) + fib(n-2)
   end

   def factorial(n)
     f = 1
     n.downto(2) { |x| f *= x }
     f
   end

   def test_version
      assert_equal("1.2.3", Memoize::MEMOIZE_VERSION)
   end

   def test_memoize
      assert_respond_to(self, :memoize)
      assert_nothing_raised{ fib(5) }
      assert_nothing_raised{ memoize(:fib) }
      assert_nothing_raised{ fib(50) }
      assert_equal(55, fib(10))
   end
   
   def test_memoize_with_file
      assert_nothing_raised{ fib(5) }
      assert_nothing_raised{ memoize(:fib, @file) }
      assert_nothing_raised{ fib(50) }
      assert_equal(55, fib(10))
   end

   # Ensure that a cache is returned, that it's a hash, and that each
   # memoized method retains its own cache properly.
   def test_memoize_cache
      assert_nothing_raised{ @cache1 = self.memoize(:fib) }
      assert_nothing_raised{ @cache2 = self.memoize(:factorial) }

      assert_nothing_raised{ self.fib(3) }
      assert_nothing_raised{ self.factorial(3) }

      assert_kind_of(Hash, @cache1)
      assert_kind_of(Hash, @cache2)

      assert_not_equal(@cache1, @cache2)
   end
   
   def teardown
      @cache1 = nil
      @cache2 = nil
      File.delete(@file) if File.exists?(@file)
   end
end
