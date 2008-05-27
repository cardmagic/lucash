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
      [:value, "foo"]
    ]])

    "()".parse.should eql([:program, [
      [:empty_parens]
    ]])

    "ls -la".parse.should eql([:program, [
      [:value, "ls", "-la"]
    ]])

    "\"ls -l'a(}\"".parse.should eql([:program, [
      [:embedded_string, "ls -l'a(}"]
    ]])

    "'ls -l\"\"a(}'".parse.should eql([:program, [
      [:string, "ls -l\"\"a(}"]
    ]])

    "ls -la &".parse.should eql([:program, [
      [:background, [:value, "ls", "-la"]]
    ]])
  end
    
  it "should return an AST for math" do
    "1 + 3.0\n      \n \n".parse.should eql([:program, [
      [:add, 
        [:number, 1], [:number, 3.0]
      ]
    ]])
    
    "1 + 3.0\n      \n 1\n".parse.should eql([:program, [
      [:add, 
        [:number, 1], [:number, 3.0]
      ],
      [:number, 1]
    ]])
    
    "foo + bar".parse.should eql([:program, [
      [:add, 
        [:value, "foo"],
        [:value, "bar"]
      ]
    ]])
    
    "5 + x * 10 - y".parse.should eql([:program, [
      [:subtract,
        [:add, 
          [:number, 5],
          [:multiply,
            [:value, "x"],
            [:number, 10]
          ]
        ],
        [:value, "y"]
      ]
    ]])

    "(5 + x) * (10 - y)".parse.should eql([:program, [
      [:multiply, 
        [:program, [
          [:add, 
            [:number, 5], 
            [:value, "x"]
          ]
        ]], 
        [:program, [
          [:subtract, 
            [:number, 10], 
            [:value, "y"]
          ]
        ]]
      ]
    ]])
  end
  
  it "should return an AST for conditionals" do
    "if true; cd foobar; end".parse.should eql([:program, [
      [:if, 
        [:value, "true"],
        [:program, [
          [:value, "cd", "foobar"]
        ]]
      ]
    ]])

    "if true\n cd foobar\n end".parse.should eql([:program, [
      [:if, 
        [:value, "true"],
        [:program, [
          [:value, "cd", "foobar"]
        ]]
      ]
    ]])

    "if (cd; true); cd foobar; end".parse.should eql([:program, [
      [:if, 
        [:program, [
          [:value, "cd"],
          [:value, "true"]
        ]],
        [:program, [
          [:value, "cd", "foobar"]
        ]]
      ]
    ]])

    "if true; cd foobar\nelse; 123; end".parse.should eql([:program, [
      [:if, 
        [:value, "true"],
        [:program, [
          [:value, "cd", "foobar"]
        ]],
        [:program, [
          [:number, 123]
        ]]
      ]
    ]])
    
    "if true; cd foobar; else 123; end".parse.should eql([:program, [
      [:if, 
        [:value, "true"],
        [:program, [
          [:value, "cd", "foobar"]
        ]],
        [:program, [
          [:number, 123]
        ]]
      ]
    ]])
  end
  
  it "should return an AST for assignment" do
    "foo = 3".parse.should eql([:program, [
      [:assignment,
        [:value, "foo"],
        [:number, 3]
      ]
    ]])

    "foo = 3 / 4".parse.should eql([:program, [
      [:assignment,
        [:value, "foo"],
        [:divide,
          [:number, 3],
          [:number, 4]
        ]
      ]
    ]])
    
    "def foo; 3; end".parse.should eql([:program, [
      [:assignment, 
        [:value, "foo"], 
        [:lambda, 
          nil,
          [:program, [
            [:number, 3]
          ]]
        ]
      ]
    ]])

    "foo = -> (x) { 3 + x }".parse.should eql([:program, [
      [:assignment,
        [:value, "foo"],
        [:lambda, 
          [:splat, [
            [:program, [
              [:value, "x"]
            ]]
          ]],
          [:program, [
            [:add,
              [:number, 3],
              [:value, "x"]
            ]
          ]]
        ]
      ]
    ]])

    "def foo(x) 3 + x end".parse.should eql([:program, [
      [:assignment,
        [:value, "foo"],
        [:lambda, 
          [:splat, [
            [:program, [
              [:value, "x"]
            ]]
          ]],
          [:program, [
            [:add,
              [:number, 3],
              [:value, "x"]
            ]
          ]]
        ]
      ]
    ]])

    "def factorial(n)
      -> (n, acc) {
        if n == 0
          acc
        else
          retry(n - 1, acc * n)
        end
      } (n, 1)
    end".parse.should eql([:program, [
      [:assignment, 
        [:value, "factorial"], 
        [:lambda, 
          [:splat, [
            [:program, [
              [:value, "n"]
            ]]
          ]], 
          [:program, [
            [:args, 
              [:lambda, 
                [:splat, [
                  [:program, [
                    [:value, "n"]
                  ]], 
                  [:program, [
                    [:value, "acc"]
                  ]]
                ]], 
                [:program, [
                  [:if, 
                    [:==, 
                      [:value, "n"], 
                      [:number, 0]
                    ], 
                    [:program, [
                      [:value, "acc"]
                    ]], 
                    [:program, [
                      [:args, 
                        [:value, "retry"], 
                        [:splat, [
                          [:program, [
                            [:subtract, 
                              [:value, "n"], 
                              [:number, 1]
                            ]
                          ]], 
                          [:program, [
                            [:multiply, 
                              [:value, "acc"], 
                              [:value, "n"]
                            ]
                          ]]
                        ]]
                      ]
                    ]]
                  ]
                ]]
              ], 
              [:splat, [
                [:program, [
                  [:value, "n"]
                ]], 
                [:program, [
                  [:number, 1]
                ]]
              ]]
            ]
          ]]
        ]]
      ]])
  end
  
  it "should return an AST for or's and and's" do
    "foo || bar".parse.should eql([:program, [
      [:or, 
        [:value, "foo"], 
        [:value, "bar"]
      ]
    ]])

    "foo && bar".parse.should eql([:program, [
      [:and, 
        [:value, "foo"], 
        [:value, "bar"]
      ]
    ]])
  end
  
  
  it "should return an AST for pipes" do
    "foo | bar".parse.should eql([:program, [
      [:pipe, 
        [:value, "foo"], 
        [:value, "bar"]
      ]
    ]])
  end
  

  it "should return an AST for method calls" do
    "foo.bar".parse.should eql([:program, [
      [:method,
        [:value, "foo"], 
        [:value, "bar"]
      ]
    ]])

    "foo.bar { baz }".parse.should eql([:program, [
      [:method,
        [:value, "foo"], 
        [:yield,
          [:value, "bar"],
          [:program, [
            [:value, "baz"]
          ]]
        ]
      ]
    ]])
    
    "foo.bar()".parse.should eql([:program, [
      [:method,
        [:value, "foo"],
        [:value, "bar"]
      ]
    ]])

    "foo.bar(baz)".parse.should eql([:program, [
      [:method, 
        [:value, "foo"],
        [:args,
          [:value, "bar"], 
          [:splat, [
            [:program, [
              [:value, "baz"]
            ]]
          ]]
        ]
      ]
    ]])

    "foo.bar(baz, aba)".parse.should eql([:program, [
      [:method, 
        [:value, "foo"],
        [:args,
          [:value, "bar"], 
          [:splat, 
            [
              [:program, [
                [:value, "baz"]
              ]],
              [:program, [
                [:value, "aba"]
              ]]
            ]
          ]
        ]
      ]
    ]])

    "foo.bar(baz, aba) { bab }".parse.should eql([:program, [
      [:method, 
        [:value, "foo"], 
        [:yield, 
          [:args, 
            [:value, "bar"], 
            [:splat, [
              [:program, [
                [:value, "baz"]
              ]], 
              [:program, [
                [:value, "aba"]
              ]]
            ]]
          ], 
          [:program, [
            [:value, "bab"]
          ]]
        ]
      ]
    ]])
    
    

    "foo.bar(baz, aba) + 1".parse.should eql([:program, [
      [:add,
        [:method, 
          [:value, "foo"],
          [:args,
            [:value, "bar"], 
            [:splat, 
              [
                [:program, [
                  [:value, "baz"],
                ]],
                [:program, [
                  [:value, "aba"],
                ]]
              ]
            ]
          ]
        ],
        [:number, 1]
      ]
    ]])

    "foo.bar + 1".parse.should eql([:program, [
      [:add,
        [:method,
          [:value, "foo"], 
          [:value, "bar"]
        ],
        [:number, 1]
      ]
    ]])

    "foo.bar && 1".parse.should eql([:program, [
      [:and,
        [:method,
          [:value, "foo"], 
          [:value, "bar"]
        ],
        [:number, 1]
      ]
    ]])

    "foo.(bar + 1)".parse.should eql([:program, [
      [:method,
        [:value, "foo"],
        [:program, [
          [:add,
            [:value, "bar"],
            [:number, 1]
          ]
        ]]
      ]
    ]])

    "foo(3)".parse.should eql([:program, [
      [:args, 
        [:value, "foo"],
        [:splat, [
          [:program, [
            [:number, 3]
          ]
        ]]
      ]]
    ]])

    "foo.(bar + 1)(3)".parse.should eql([:program, [
      [:method,
        [:value, "foo"],
        [:args,
          [:program, [
            [:add,
              [:value, "bar"],
              [:number, 1]
            ]
          ]],
          [:splat, [
            [:program, [
              [:number, 3]
            ]]
          ]]
        ]
      ]
    ]])

    "foo.(bar + 1)(3) + 1".parse.should eql([:program, [
      [:add,
        [:method,
          [:value, "foo"],
          [:args,
            [:program, [
              [:add,
                [:value, "bar"],
                [:number, 1]
              ]
            ]],
            [:splat, [
              [:program, [
                [:number, 3]
              ]]
            ]]
          ]
        ],
        [:number, 1]
      ]
    ]])
  end
end