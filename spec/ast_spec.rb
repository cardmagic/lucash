require File.dirname(__FILE__) + "/spec_helper"

describe Lucash::AST do
  before(:all) do
    @l = Lucash::AST.new
  end

  it "should not work without a valid ast" do
    lambda { @l.eval }.should raise_error(Lucash::InvalidAST)
    
    lambda { @l.eval([]) }.should raise_error(Lucash::InvalidAST)
    
    lambda { @l.eval([:program, "foobar"]) }.should raise_error(Lucash::InvalidAST)
  end
  
  it "should work with valid ast" do
    10.times do
      @l.eval([:program, [[:value, "echo", 1]]]).
      should eql("1\n")
    end

    10.times do
      @l.eval([:program, [[:value, "echo", "-la"]]]).
      should eql("-la\n")
    end
    
    @l.eval([:program, [[:+, [:number, 1], [:number, 2.0]]]]).
    should eql(3.0)

    @l.eval([:program, []]).
    should eql(nil)
  end
end
