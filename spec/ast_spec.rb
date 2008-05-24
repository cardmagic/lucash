require File.dirname(__FILE__) + "/../lib/lucash"

describe Lucash::AST do
  before(:each) do
    @l = Lucash::AST.new
  end

  it "should not work without a valid ast" do
    lambda { @l.eval }.should raise_error(Lucash::AST::InvalidAST)
  end
end
