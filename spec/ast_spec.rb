require File.expand_path(File.dirname(__FILE__) + "/../lib/lucash")

describe Lucash::Ast do
  before :each do
    @l = Lucash::Ast.new
  end
  
  it "should die with an invalid ast" do
    lambda { @l.eval }.should raise_error(Lucash::Ast::InvalidAst)
    
    @l.ast = []
    lambda { @l.eval }.should raise_error(Lucash::Ast::InvalidAst)
    
    @l.ast = ""
    lambda { @l.eval }.should raise_error(Lucash::Ast::InvalidAst)
    
    @l.ast = [:program, "foobar"]
    lambda { @l.eval }.should raise_error(Lucash::Ast::InvalidAst)
  end

  it "should work with a valid ast" do
    @l.ast = [:program, []]
    @l.eval.should eql(nil)

    @l.ast = [:program, [[:line, [:value, ["echo"]]]]]
    @l.eval.should eql(["echo"])
  end
  
  it "should return valid values" do
    @l.ast = [:program, [[:line, [:value, ["true"]]]]]
    @l.eval.should eql(true)

    @l.ast = [:program, [[:line, [:value, ["false"]]]]]
    @l.eval.should eql(false)

    @l.ast = [:program, [[:line, [:number, 1]]]]
    @l.eval.should eql(1)
    
    @l.ast = [:program, [[:line, [:add, [:number, 3.0], [:number, 4]]]]]
    @l.eval.should eql(7.0)
  end
end
