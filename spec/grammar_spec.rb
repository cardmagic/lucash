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
    lambda { "if foo else".parse }.should raise_error(ParseError)
    lambda { "}".parse }.should raise_error(ParseError)
  end

  it "should return an AST for variables" do
    "foo".parse.should eql([:program, [
      [:line, 
        [:value, ["foo"]]
      ]
    ]])
  end
    
  it "should return an AST for addition" do
    "1 + 3.0\n      \n \n".parse.should eql([:program, [
      [:line, 
        [:add, 
          [:line, [:number, 1]], [:line, [:number, 3.0]]
        ]
      ]
    ]])
    
    "1 + 3.0\n      \n 1\n".parse.should eql([:program, [
      [:line, 
        [:add, 
          [:line, [:number, 1]], [:line, [:number, 3.0]]
        ]
      ],
      [:line, [:number, 1]]
    ]])
    
    "foo + bar".parse.should eql([:program, [
      [:line, 
        [:add, 
          [:line, [:value, ["foo"]]], 
          [:line, [:value, ["bar"]]]
        ]
      ]
    ]])
  end
  
  it "should return an AST for conditionals" do
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

    "if true; cd foobar; else; 123; end".parse.should eql([:program, [
      [:if, 
        [:line, 
          [:value, ["true"]]
        ],
        [:program, [
          [:line, 
            [:value, ["cd", "foobar"]]
          ]
        ]],
        [:program, [
          [:line, 
            [:number, 123]
          ]
        ]]
      ]
    ]])
    
    "if true; cd foobar; else 123; end".parse.should eql([:program, [
      [:if, 
        [:line, 
          [:value, ["true"]]
        ],
        [:program, [
          [:line, 
            [:value, ["cd", "foobar"]]
          ]
        ]],
        [:program, [
          [:line, 
            [:number, 123]
          ]
        ]]
      ]
    ]])
  end
  
  it "should return an AST for assignment" do
    "foo = 3".parse.should eql([:program, [
      [:assignment,
        [:value, ["foo"]],
        [:line,
          [:number, 3]
        ]
      ]
    ]])

    "foo = 3 / 4".parse.should eql([:program, [
      [:assignment,
        [:value, ["foo"]],
        [:line,
          [:divide,
            [:line, [:number, 3]],
            [:line, [:number, 4]]
          ]
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
  end
  
  it "should return an AST for or's and and's" do
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
  end
  
  
  it "should return an AST for pipes" do
    "foo | bar".parse.should eql([:program, [
      [:pipe, 
        [:line, [:value, ["foo"]]], 
        [:line, [:value, ["bar"]]]
      ]
    ]])
  end
  

  it "should return an AST for method calls" do
    "foo.bar".parse.should eql([:program, [
      [:method,
        [:line, [:value, ["foo"]]], 
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

    "foo.bar(baz, aba) + 1".parse.should eql([:program, [
      [:line,
        [:add,
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
          ],
          [:line, [:number, 1]]
        ]
      ]
    ]])

    "foo.bar + 1".parse.should eql([:program, [
      [:line,
        [:add,
          [:method,
            [:line, [:value, ["foo"]]], 
            [:method_call,
              [:value, ["bar"]]
            ]
          ],
          [:line, [:number, 1]]
        ]
      ]
    ]])

    "foo.(bar + 1)".parse.should eql([:program, [
      [:method,
        [:line, 
          [:value, ["foo"]]
        ], 
        [:method_call,
          [:add,
            [:line, [:value, ["bar"]]],
            [:line, [:number, 1]]
          ]
        ]
      ]
    ]])
  end
end