require File.dirname(__FILE__) + "/spec_helper"

describe Lucash::Paths do
  before(:all) do
    @sh = Lucash::Paths.new
  end

  it "should have executables" do
    @sh.executables.should_not be_empty

    @sh.executables["ls"].should_not be_nil
    @sh.executables["ls"].name.should eql("ls")
    @sh.executables["ls"].path.should eql("/bin/ls")

    @sh.executables["man"].should_not be_nil
    @sh.executables["man"].name.should eql("man")
    @sh.executables["man"].path.should eql("/usr/bin/man")
  end
  
  it "should leave crazy names nil" do
    @sh.executables["djfdjkfdfddf"].should be_nil
  end
  
  it "should refresh executables based on environmental variable" do
    ENV['PATH'] = "foo:bar"
    
    @sh.refresh_executables.should eql(["foo", "bar"])
  end
end
