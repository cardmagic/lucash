require 'runit/testcase'
require 'runit/testsuite'
require 'runit/cui/testrunner'
require 'runit/topublic'
include RUNIT::ToPublic

require 'rockit/sourcecode_dumpable' 

class T
  include SourceCodeDumpable
  def to_src(name = nil)
    new_of_my_type
  end

  def to_src_with_code(aString)
    new_of_my_type(as_code(aString))
  end
end

class Integer
  include SourceCodeDumpable
  def to_src(name = nil, nameHash = {})
    assign_to(name, self.inspect)
  end
end

class TestSourceCodeDumpable < RUNIT::TestCase
  def test_type_to_src
    assert_equals("T", T.new.type_to_src)
  end

  def test_new_of_my_type
    assert_equals("T.new(1)", T.new.new_of_my_type(1))
    assert_equals("T.new(T.new())", T.new.new_of_my_type(T.new))
    assert_equals("T.new(aspect)", T.new.to_src_with_code("aspect"))
  end

  def test_array_to_src
    s = SourceCodeDumpable.array_to_src([T.new, T.new])
    assert_equals("[T.new(), T.new()]", s)

    assert_equals("[1]", SourceCodeDumpable.array_to_src([1]))
  end

  def test_indent_lines
    assert_equals("  ab\n  cd", SourceCodeDumpable.indent_lines("ab\ncd"))
    assert_equals("  ab\n  cd\n", SourceCodeDumpable.indent_lines("ab\ncd\n"))
    assert_equals(" \t ab\n \t cd", 
		  SourceCodeDumpable.indent_lines("ab\ncd", " \t "))
  end

  def test_name_hash
    a = [1,2,3,4,5]
    names = SourceCodeDumpable.name_hash(a) {|e| "i"}
    assert_equals([[1,"i1"],[2,"i2"],[3,"i3"],[4,"i4"],[5,"i5"]],
		  names.to_a.sort)
  end

  def test_array_to_src
    ary = [1,2]
    names = SourceCodeDumpable.name_hash(ary) {|e| "i"}
    assert_equals("a = [\n  i1 = 1,\n  i2 = 2\n]" , ary.to_src("a", names))
    names = SourceCodeDumpable.name_hash(ary) {|e| "i#{e+1}"}
    assert_equals("a = [\n  i2 = 1,\n  i3 = 2\n]" , ary.to_src("a", names))
    names = SourceCodeDumpable.name_hash(ary) {|e| "i"}
    assert_equals("def a\n  [\n    i1 = 1,\n    i2 = 2\n  ]\nend\n" , 
		  ary.to_src(SourceCodeDumpable.as_method_named("a"), 
			     names))
    assert_equals("module T\n  def a\n    [\n      i1 = 1,\n      i2 = 2\n    ]\n  end\nend\n" , 
		  ary.to_src_in_module(SourceCodeDumpable.as_method_named("a"), 
				       "T", names))
    
  end

  def test_hash_to_src
    h = {1=>2, 2=>3}
    assert_equals("h = {\n  1 => 2,\n  2 => 3\n}" , h.to_src("h"))

    h[3] = nil
    assert_equals("h = {1 => 2, 2 => 3}" , h.to_compact_src("h"))
  end

  def test_string
    s = "abc"
    assert_equals(s, eval(s.to_src))
  end

  def test_regexp
    re = /a/
    assert_equals(re, eval(re.to_src))

    re = /a/i
    assert_equals(re, eval(re.to_src))
  end

  def test_regexp_slashbug
    re = /\\\//
    assert_equals(re, eval(re.to_src))
  end
end

# Run if we were directly called
if $0 == __FILE__
  testrunner = RUNIT::CUI::TestRunner.new
  if ARGV.size == 0
    suite = TestSourceCodeDumpable.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestSourceCodeDumpable.new(testmethod))
    end
  end
  testrunner.run(suite)
end

