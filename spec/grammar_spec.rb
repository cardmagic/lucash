require File.dirname(__FILE__) + "/spec_helper"

class String
  def parse
    @grammar ||= LucashGrammar.new
    @grammar.parse(self)
  end
end

describe LucashGrammar do
  it "should raise a parse error with bad input" do
    lambda { "(((".parse }.should raise_error(ParseError)
    lambda { "end".parse }.should raise_error(ParseError)
    lambda { "if".parse }.should raise_error(ParseError)
    lambda { "}".parse }.should raise_error(ParseError)
  end

  it "should return an AST with good input" do
    "foo".parse.should eql([:program, [
      [:line, 
        [:value, ["foo"]]
      ]
    ]])
    
    "1 + 3.0".parse.should eql([:program, [
      [:line, 
        [:add, 
          [:number, 1], [:number, 3.0]
        ]
      ]
    ]])
    
    "if true; cd foobar; end".parse.should eql([:program, [
      [:if, 
        [:line, 
          [:value, ["true"]]
        ],
        [:program, [
          [:line, 
            [:value, ["cd", "foobar"]]
          ]
        ]]
      ]
    ]])
    
    "foo = 3".parse.should eql([:program, [
      [:assignment,
        [:value, ["foo"]],
        [:line,
          [:number, 3]
        ]
      ]
    ]])
    
    "foo <- 3".parse.should eql([:program, [
      [:functional_assignment,
        [:value, ["foo"]],
        [:line,
          [:number, 3]
        ]
      ]
    ]])
    
    "foo || bar".parse.should eql([:program, [
      [:or, 
        [:value, ["foo"]], 
        [:line, 
          [:value, ["bar"]]
        ]
      ]
    ]])

    "foo && bar".parse.should eql([:program, [
      [:and, 
        [:value, ["foo"]], 
        [:line, 
          [:value, ["bar"]]
        ]
      ]
    ]])
    
    "foo + bar".parse.should eql([:program, [
      [:line, 
        [:add, 
          [:value, ["foo"]], 
          [:value, ["bar"]]
        ]
      ]
    ]])
    
    "foo | bar".parse.should eql([:program, [
      [:pipe, 
        [:line, 
          [:value, ["foo"]]
        ], 
        [:line, 
          [:value, ["bar"]]
        ]
      ]
    ]])

    "foo.bar".parse.should eql([:program, [
      [:method,
        [:line, 
          [:value, ["foo"]]
        ], 
        [:method_call,
          [:value, ["bar"]]
        ]
      ]
    ]])
    
    "foo.bar()".parse.should eql([:program, [
      [:method,
        [:line, 
          [:value, ["foo"]]
        ], 
        [:method_call,
          [:value, ["bar"]]
        ]
      ]
    ]])

    "foo.bar(baz)".parse.should eql([:program, [
      [:method, 
        [:line, 
          [:value, ["foo"]]
        ], 
        [:method_call,
          [:value, ["bar"]], 
          [:splat, [
            [:program, [
              [:line, 
                [:value, ["baz"]]
              ]
            ]]
          ]]
        ]
      ]
    ]])

    "foo.bar(baz, aba)".parse.should eql([:program, [
      [:method, 
        [:line, 
          [:value, ["foo"]]
        ], 
        [:method_call,
          [:value, ["bar"]], 
          [:splat, 
            [
              [:program, [
                [:line, 
                  [:value, ["baz"]]
                ]
              ]],
              [:program, [
                [:line, 
                  [:value, ["aba"]]
                ]
              ]]
            ]
          ]
        ]
      ]
    ]])
  end
end