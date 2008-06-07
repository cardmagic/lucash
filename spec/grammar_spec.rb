require File.dirname(__FILE__) + "/spec_helper"

class String
  def eval
    @lucash ||= Lucash.new
    @lucash.eval(self)
  end
end

describe "Basic Grammar" do
  it "should raise a parse error with bad input" do
    lambda { "(((".eval }.should raise_error(ParseError)
    lambda { "end".eval }.should raise_error(ParseError)
    lambda { "if".eval }.should raise_error(ParseError)
    lambda { "if foo else".eval }.should raise_error(ParseError)
    lambda { "}".eval }.should raise_error(ParseError)
  end

  it "should return a value for variables" do
    "foo".eval.should eql("foo")

    "()".eval.should eql(nil)

    "echo -la".eval.should eql("-la\n")

    "\"ls -l'a(}\"".eval.should eql("ls -l'a(}")

    "'ls -l\"\"a(}'".eval.should eql("ls -l\"\"a(}")

    "ls -la &".eval.should eql() # TBD
  end
    
  it "should return a value for math" do
    "1 + 3.0\n      \n \n".eval.to_s.should eql("4.0")
    
    "1 + 3.0\n      \n 1\n".eval.to_s.should eql("1")
    
    "foo + bar".eval.should eql("foobar")
    
    "5 + x * 10 - y".eval.should eql() # TBD

    "(5 + x) * (10 - y)".eval.should eql() # TBD
  end
  
  it "should return a value for conditionals" do
    "if true; echo foobar; end".eval.should eql("foobar\n")

    "if true\n echo foobar\n end".eval.should eql("foobar\n")

    "if false; echo foobar; end".eval.should eql(nil)

    "if false\n echo foobar\n end".eval.should eql(nil)

    "if (cd; true); echo foobar; end".eval.should eql("foobar\n")

    "if true; echo foobar\nelse; 123; end".eval.should eql("foobar\n")

    "if false; echo foobar\nelse; 123; end".eval.should eql(123)
    
    "if true; echo foobar; else 123; end".eval.should eql("foobar\n")

    "if false; echo foobar; else 123; end".eval.should eql(123)

  end
  
  it "should return a value for assignment" do
    "foo = 3".eval.should eql(3)

    "foo = 3 / 4".eval.to_s.should eql("0")

    "foo = 3.0 / 4".eval.should eql(0.75)

    "def foo; 3; end; foo()".eval.should eql(3)
    
    "def foo(); 3; end; foo()".eval.should eql(3)

    "def foo; 3; end; foo".eval.should eql("foo")

    "foo = -> (x) { 3 + x }; foo(2)".eval.should eql(5)

    "foo = -> (x) { 3 + x }; foo(2); foo(2)".eval.should eql(5)

    "def foo(x) 3 + x end; foo(3)".eval.should eql(6)

    "def foo(x) 3 + x end; foo(3); foo(3)".eval.should eql(6)

    "def factorial(n)
      -> (n, acc) {
        if n == 0
          acc
        else
          retry(n - 1, acc * n)
        end
      } (n, 1)
    end; factorial(5)".eval.should eql(5*4*3*2)
  end
  
  it "should return a value for or's and and's" do
    "true || true".eval.should eql(true)
    "true || false".eval.should eql(true)
    "false || true".eval.should eql(true)
    "false || false".eval.should eql(false)

    "true && true".eval.should eql(true)
    "true && false".eval.should eql(false)
    "false && true".eval.should eql(false)
    "false && false".eval.should eql(false)
  end
  
  
  it "should return a value for pipes" do
    "foo | bar".eval.should eql() # TBD
  end

  it "should return a value for method calls" do
    "foo.bar".eval.should eql() # TBD

    "foo.bar { baz }".eval.should eql() # TBD
    
    "foo.bar()".eval.should eql() # TBD

    "foo.bar(baz)".eval.should eql() # TBD

    "foo.bar(baz, aba)".eval.should eql() # TBD

    "foo.bar(baz, aba) { bab }".eval.should eql() # TBD

    "foo.bar(baz, aba) + 1".eval.should eql() # TBD

    "foo.bar + 1".eval.should eql() # TBD

    "foo.bar && 1".eval.should eql() # TBD

    "foo.(bar + 1)".eval.should eql() # TBD

    "foo(3)".eval.should eql() # TBD

    "foo.(bar + 1)(3)".eval.should eql() # TBD

    "foo.(bar + 1)(3) + 1".eval.should eql() # TBD
  end
end