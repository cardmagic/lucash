require File.dirname(__FILE__) + "/spec_helper"

describe Lucash::AST do
  before(:all) do
    @l = Lucash::AST.new
  end

  it "should not work without a valid ast" do
    lambda { @l.eval }.should raise_error(Lucash::InvalidAST)
    
    @l.ast = []
    lambda { @l.eval }.should raise_error(Lucash::InvalidAST)
    
    @l.ast = [:program, "foobar"]
    lambda { @l.eval }.should raise_error(Lucash::InvalidAST)
  end
  
  it "should work with valid ast" do
    @l.ast = [:program, [[:value, ["echo", 1]]]]
    @l.eval.should eql(["echo", 1])

    @l.ast = [:program, [[:value, ["ls", "-la"]]]]
    @l.eval.should eql(["ls", "-la"])
    
    @l.ast = [:program, [[:add, [:number, 1], [:number, 2.0]]]]
    @l.eval.should eql(3.0)

    @l.ast = [:program, []]
    @l.eval.should eql(nil)
  end
end
