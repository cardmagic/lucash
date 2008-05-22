require 'rockit/rockit'

# This is after Stefan Mathias Aust in [ruby-talk:15611].
def extract_bnf_grammar_from_yacc_spec(yaccFile)
  bnfg = ""
  File.open(yaccFile, "r") do |f|
    lines = f.readlines
    line = lines.shift while line !~ /^%%/
    paren_level = 0        # > 0 when in C code
    outside_string = true  # false iff in a string
    while lines.first !~ /^%%/
      lines.shift.each_byte do |c|
	outside_string = !outside_string if paren_level == 0 && c == ?' #'
	paren_level += 1 if outside_string && c == ?{
	bnfg << c.chr if paren_level == 0
	paren_level -= 1 if outside_string && c == ?}
	raise "oops" if paren_level < 0
      end
    end
  end
  bnfg
end

# Parser for parsing whats left after applying
#  extract_bnf_grammar_from_yacc_spec
def ruby_bnf_grammar_parser
  parser = nil
  time_and_puts("Generating parser for Ruby BNF grammar") do
    begin
      parser = Parse.generate_parser <<-'EOG'
Grammar BnfGrammars
  Tokens
    Blank          = /\s+/                             [:Skip]
    EpsilonComment = /\/\* none \*\//
    Nonterm        = /[a-z][a-z_\d]*/        
    Keyword        = /k\w+/
    Token          = /t[A-Z_\d]+/
    String         = /'[^']*'/
  Productions
    BnfGrammar -> Prod+                                [: productions]
    Prod       -> Nonterm ':' list(Alt, '|')           [: nonterm,_,alts]
    Alt        -> Element*                             [: elements]
    Element    -> (Nonterm | Keyword | Token | 
                   String  | EpsilonComment)           [^]
EOG
    rescue Exception => e
      puts e.inspect
      exit -1
    end
  end
  parser
end

# Class for keeping info about the grammar we're analysing
class GrammarInfo
  attr_reader :productions

  def initialize
    @tokenspecs, @nonterm_info, @productions = Hash.new, Hash.new, Hash.new
    @nonterms_in_order, @opnames = Array.new, Hash.new
  end

  def tokens
    @tokenspecs.keys
  end

  def nonterms
    @nonterm_info.keys
  end

  def nonterm_info(nonterm)
    @nonterm_info[nonterm]
  end

  def add_string_name(string, name)
    # We can save info about what names have been given to certain strings
    # this can be used when we create AstSpecs later...
    @opnames[string] = name
  end

  def add_token(tokenName, tokenSpec = [])
    unless @tokenspecs[tokenName]
      @tokenspecs[tokenName] = tokenSpec unless @tokenspecs[tokenName]
    end
  end

  def add_nonterm(nonterm)
    @nonterms_in_order.push nonterm unless @nonterms_in_order.include?(nonterm)
    @nonterm_info[nonterm] = [] unless @nonterm_info[nonterm]
    @productions[nonterm] = [] unless @productions[nonterm]
  end

  def delete_nonterm(nonterm)
    @productions.delete nonterm
    @nonterm_info.delete nonterm
    @nonterms_in_order.delete nonterm
  end    

  def substitute_nonterm(nonterm, asType, *rest)
    delete_nonterm(nonterm)
    if asType == :String
      map_right_hand_side_elements {|e| e == nonterm ? rest[0] : e} 
    end      
  end

  def select_top_nonterms(numberToSelect)
    nts =nonterms_to_order_of_appearance_number.to_a.sort{|a,b| a[1] <=> b[1]}
    puts "number of nonterminals = #{nts.length}"
    total_num = nts.length
    nts[numberToSelect.to_i..-1].each do |nt,onum| 
      substitute_nonterm(nt, :String, "'DUMMY'")
    end
    puts "NoN is now #{numberToSelect}"
  end

  def map_right_hand_side_elements(&block)
    productions.values.each do |alts|
      alts.each do |prodinfo|
	prodinfo.elements = prodinfo.elements.map {|e| block.call(e)}
      end
    end
  end

  def nonterm_is_optional(nonterm)
    @nonterm_info[nonterm].push(:Optional) if @nonterm_info[nonterm]
  end

  def nonterm_is_optional?(nonterm)
    @nonterm_info[nonterm].include? :Optional
  end

  ProductionInfo = Struct.new("ProductionInfo", :nonterm, :elements, 
			      :node_name, :children_names)

  def add_production(nonterm, elements)
    add_nonterm(nonterm)
    @productions[nonterm].push ProductionInfo.new(nonterm, elements)
  end

  def delete_production(productionInfo)
    @productions[productionInfo.nonterm].delete productionInfo
    if @productions[productionInfo.nonterm].length == 0
      delete_nonterm(productionInfo.nonterm)
    end
  end

  def pretty_print(languageName = "UnknownLanguage")
    assign_astspecs
    "Grammar #{languageName}\n" +
      "  Tokens\n" +
      tokens.map {|t| pretty_print_token(t)}.join("\n") + "\n" +
      "  Productions\n" +
      pretty_print_productions(productions)
  end

  def assign_astspecs
    productions.values.each do |alts|
      alts.each do |prodinfo|
	prodinfo.node_name, prodinfo.children_names = astspec(prodinfo)
      end
    end
  end

  def astspec(prodinfo)
    nt, es = prodinfo.nonterm, prodinfo.elements
    # Xx -> Xx 'op' Xx  is given the astspec [XxOp: left, _, right]
    if nt == es[0] and nt == es[2] and (opname = @opnames[es[1]])
      return opname, ["left", "_", "right"]
    else
      return prodinfo.node_name, prodinfo.children_names
    end
  end

  def max_token_length
    @max_token_length || 
      (@max_token_length = tokens.map {|t| t.length}.max)
  end

  def pretty_print_token(token, indent = " " * 4)
    indent + token + " " * (max_token_length - token.length) + " = /" +
      (@tokenspecs[token][0] ? @tokenspecs[token][0].source : "") + "/" +
      (@tokenspecs[token][1] ? "\t" + @tokenspecs[token][1].inspect : "") 
  end

  # Assign order numbers so that nonterms are listed in the order they are 
  # mentioned in the grammar. For example:
  #   S -> A B
  #   A -> C
  #   B -> D E
  #   C -> "alias" GVAR GVAR
  #   D -> ...
  def nonterms_to_order_of_appearance_number
    order_numbers = Hash.new(1e10)
    next_free_number = 0
    order_numbers[@nonterms_in_order.first] = (next_free_number += 1)
    @nonterms_in_order.each do |nonterm|
      productions[nonterm].each do |production|
	production.elements.each do |element|
	  if nonterms.include?(element) and order_numbers[element] == 1e10
	    order_numbers[element] = (next_free_number += 1)
	  elsif element =~ /list\((\w+), (\w+)\)/ # Hack!!
	    order_numbers[$1] = (next_free_number += 1) if order_numbers[$1] == 1e10
	    order_numbers[$2] = (next_free_number += 1) if order_numbers[$2] == 1e10
	  elsif element =~ /^(\w*)\?$/
	    order_numbers[$1] = (next_free_number += 1) if order_numbers[$1] == 1e10
	  end
	end
      end
    end
    order_numbers
  end
  
  def pretty_print_productions(productions, indent = " " * 4)
    nts =nonterms_to_order_of_appearance_number.to_a.sort{|a,b| a[1] <=> b[1]}
    puts "Number of nonterminals = #{nts.length}"
    nts.map do |nonterm, num|
      rest = productions[nonterm][1..-1] || []
      str = indent + pretty_print_first_production(productions[nonterm].first) 
      if rest.length > 0
	str += "\n" + 
	  rest.map {|p| indent + pretty_print_production(p)}.join("\n")
      end
      str
    end.join("\n")
  end

  def pretty_print_production(production)
    " " * production.nonterm.length + " |  " + 
      production.elements.map {|e| pretty_print_element(e)}.join(" ") +
      pretty_print_astspec(production.node_name, production.children_names)
  end

  def pretty_print_astspec(nodeName, childrenNames)
    s = ""
    s << nodeName if nodeName
    if childrenNames and childrenNames.length > 0
      s << ": " if nodeName
      s << childrenNames.join(", ")
    end
    s.length > 0 ? "\t[" + s + "]" : s
  end

  def pretty_print_first_production(production)
    !production ? "" : 
      production.nonterm + " -> " + 
      production.elements.map {|e| pretty_print_element(e)}.join(" ") +
      pretty_print_astspec(production.node_name, production.children_names)
  end

  def pretty_print_element(element)
    if nonterms.include?(element) and nonterm_is_optional?(element)
      element + "?"
    else
      element
    end
  end
end

class GrammarInfoExtractorAndSymbolMapper
  def initialize(ast)
    @ast = ast
  end

  def grammar_info
    @ginfo = GrammarInfo.new
    extract(@ast)
    @ginfo
  end

  def extract(ast)
    case ast.name
    when "BnfGrammar"
      ast.productions.each {|prod| extract(prod)}
    when "Prod"
      @current_nonterm, indent = extract(ast.nonterm), nil
      ast.alts.each do |alt| 
	elements = extract(alt)
	if not elements.include?(nil)
	  @ginfo.add_production(@current_nonterm, elements)
	end
      end
    when "Alt"
      ast.elements.map {|e| extract(e)}
    when "Nonterm"
      map_and_add_nonterm(ast.lexeme)
    when "Keyword"
      map_keyword(ast)
    when "String"
      ast.lexeme
    when "EpsilonComment"
      # Currently active nonterminal is optional wherever it appears
      @ginfo.nonterm_is_optional(@current_nonterm)
      nil
    when "Token"
      map_token(ast)
    end
  end

  @@nonterm_name_map = {
    "Compstmt" => "CompoundStatement",
    "Stmts" => "Statements",
    "Stmt" => "Statement",
    "Expr" => "Expression",
    "Term" => "Terminator",
    "Terms" => "Terminators",
  }

  def map_and_add_nonterm(nontermName)
    nonterm = first_up_rest_down(nontermName)
    if nonterm == "None"
      # Currently active nonterminal is optional wherever it appears
      @ginfo.nonterm_is_optional(@current_nonterm)
      nil
    elsif nonterm == "Error"
      # We simply skip error for now...
      nil
    elsif nonterm =~ /F_(.*)arg(.*)/
      map_and_add_nonterm("Func" + first_up_rest_down($1) + "Arg" + 
			  first_up_rest_down($2))
    else
      nonterm = @@nonterm_name_map[nonterm] || nonterm
      @ginfo.add_nonterm(nonterm)
      nonterm
    end
  end

  @@token_map = {
    # TokenName => [Convert to type, Use this instead, Prioritiy (1 is high)]
    "Uplus" => [:String, "'+@'"],   # May not work since tUPLUS is also used in a special way when there are digits? See parse.y line 3080.
    "Uminus" => [:String, "'-@'"],  # Same as UPlus above?
    "Op_asgn" => [:Regexp, "OpAssignment", /(\+|-)=/],
    "Dot3" => [:String, "'...'"],
    "Dot2" => [:String, "'..'"],
    "Pow" => [:String, "'**'"],
    "Cmp" => [:String, "'<=>'"],
    "Eq" => [:String, "'=='"],
    "Eqq" => [:String, "'==='"],
    "Neq" => [:String, "'!='"],
    "Geq" => [:String, "'>='"],
    "Leq" => [:String, "'<='"],
    "Andop" => [:String, "'&&'"],
    "Orop" => [:String, "'||'"],
    "Match" => [:String, "'=~'"],
    "Nmatch" => [:String, "'!~'"],
    "Dot2" => [:String, "'..'"],
    "Dot3" => [:String, "'...'"],
    "Aref" => [:String, "'[]'"],
    "Aset" => [:String, "'[]='"],
    "Lshft" => [:String, "'<<'"],
    "Rshft" => [:String, "'>>'"],
    "Colon2" => [:String, "'::'"],
    "Colon3" => [:String, "'::'"],
    "Lparen" => [:String, "'('"],
    "Lbrack" => [:String, "'['"],
    "Lbrace" => [:String, "'{'"],
    "Star" => [:String, "'*'"],
    "Amper" => [:String, "'&'"],
    "Gvar" => [:Token, "GlobalVariable", /\$[a-zA-Z_]\w*/],
    "Ivar" => [:Token, "InstanceVariable", /@[a-zA-Z_]\w*/],
    "Cvar" => [:Token, "ClassVariable", /@@[a-zA-Z_]\w*/],
    "Assoc" => [:String, "'=>'"],
    "Symbeg" => [:String, "':'"],
    "Regexp" => [:Token, "Regexp", /\/((\\\/)|[^\/])*\//],
    "Float" => [:Token, "Float", /((\d(_*\d+)*)(((\.\d(_*\d+)*)?((E|e)-?(_?\d+)+))|((\.\d(_*\d+)*)((E|e)-?(_?\d+)+)?)))|((\.\d(_*\d+)*)((E|e)-?(_?\d+)+)?)/],
    "Integer" => [:Token, "Integer", /([1-9](_*\d+)*)|(0(x|X)(_*[\dA-Fa-f]+)+)|(0(b|B)(_*[01]+)+)|(0(_*[0-7]+)+)/],
    "Identifier" => [:Token, "Identifier", /[a-zA-Z_]\w*/],
    "Constant" => [:Token, "Constant", /[A-Z]\w*/],
    "Fid" => [:Token, "FunctionId", /\w*(!|\?)/],
    "Back_ref" => [:Token, "Back_ref", /\$(&|`|'|\+)/],
    "Nth_ref" => [:Token, "Nth_ref", /\$\d/],
    # We simplify strings for now. Need to dig into this later!
    "String" => [:Token, "SingleQuotedString", /'((\\')|[^'])*'/],
    "Dstring" => [:Token, "DoubleQuotedString", /"((\\")|[^"])*"/],
    "Xstring" => [:Token, "BackQuotedString", /`[^`]*`/],
    # These tokens I haven't understood at all yet so insert some bullshit
    # for now!
    "Dregexp" => [:String, "'tDREGEXP token not yet implemented!!'"],
    "Dxstring" => [:String, "'tDXSTRING token not yet implemented!!'"],
  }

  def map_token(ast)
    token_name = first_up_rest_down(ast.lexeme[1..-1])
    t = @@token_map[token_name]
    if t and t[0] == :String
      @ginfo.add_string_name(t[1], token_name)
      t[1]
    elsif t and t[0] == :Token
      @ginfo.add_token(token_name = t[1], t[2]? [t[2]] : [])
      token_name
    elsif t and t[0] == :Regexp
      @ginfo.add_token(token_name = t[1], [t[2]])
      token_name
    else
      @ginfo.add_token(token_name)
      token_name
    end
  end
  
  def map_keyword(keyword)
    case keyword.lexeme
    when /k([A-Z]+)_MOD/ # kIF_MOD et al needs to be stripped of _MOD
      "'" + $1.downcase + "'"
    when /kl([A-Z]+)/ # klBEGIN and klEND
      "'" + $1 + "'"
    when /k__([A-Z]+)__/ # k__LINE__ and k__FILE__
      "'__" + $1 + "__'"
    else
      "'" + keyword.lexeme[1..-1].downcase + "'"
    end   
  end
end

def first_up_rest_down(aString)
  return "" unless aString 
  if aString.length == 0
    ""
  elsif aString.length == 1
    aString.upcase
  else
    aString[0,1].upcase + aString[1..-1].downcase
  end
end

X = nil

$prod_transforms = {
  "CompoundStatement" => [
    [true, nil, ["list(Statement, StatementSeparator)", "';'*"]]
  ],
  "Statements" => [
    [[X,X,"Statement"], "StatementSeparator", proc{|old_elems| [old_elems[1]]}],
    [["Statement"], nil, :DELETE]
  ],
}

def transform_productions(grammarInfo)
  eliminate_unneeded_nonterminals(grammarInfo)
  $prod_transforms.to_a.each do |nt, transforms|
    prods = grammarInfo.productions[nt].clone
    next unless prods
    prods.each do |pi|
      transforms.each do |transform|
	pattern = transform[0]
	if pattern == true or pattern_match(pattern, pi)
	  if transform[2].type == Array
	    pi.elements = transform[2]
	  elsif transform[2].type == Proc
	    pi.elements = transform[2].call(pi.elements)
	  elsif transform[2] == :DELETE
	    grammarInfo.delete_production(pi)
	  end
	  if transform[1].type == String
	    grammarInfo.delete_production(pi)
	    puts "Adding #{transform[1]}"
	    grammarInfo.add_production(transform[1], pi.elements)
	  end
	end
      end
    end
  end
end

def eliminate_unneeded_nonterminals(grammarInfo)
  unneeded_nonterminals = {
    "Opt_terms" => [:String, "';'*"],
    "Terminators" => [:String, "(';' | '\\n' | '\\r' | '\\r\\n' | '\\n\\r')*"],
    "Opt_nl" => [:String, "'\\n'?"],
    "Undef_list" => [:String, "list(Fitem, ',')"],
  }

  unneeded_nonterminals.to_a.each do |nonterm,t|
    grammarInfo.substitute_nonterm(nonterm, t[0], t[1])
  end
end

def set_astspecs(grammarInfo)
  astspecs = {
    "Statement" => [
      [["'alias'"], "Alias", [:_, :new, :old]],
      [["'undef'"], "Undef", [:_, :items]],
      [["Statement", "'if'", "Expression"], "ConditionedStatement", 
	[:statement, :conditioner, :expr]],
      [["Statement", "'unless'", "Expression"], "ConditionedStatement", 
	[:statement, :conditioner, :expr]], 
      [["Statement", "'while'", "Expression"], "LoopedStatement", 
	[:statement, :loop, :expr]], 
      [["Statement", "'until'", "Expression"], "LoopedStatement", 
	[:statement, :loop, :expr]], 
      [["Statement", "'rescue'", "Statement"], "RescuedStatement", 
	[:statement, :_, :rescuestatement]], 
      [["'BEGIN'"], "BeginBlock", [:_, :_, :statements, :_]],
      [["'END'"], "EndBlock", [:_, :_, :statements, :_]],
      [["Expression"], "^", []],
      [["Lhs", "'='"], "Assignment", [:var, :_, :rhs]],
      [["Mlhs", "'='"], "Assignment", [:var, :_, :rhs]]
    ],
    "CompoundStatement" => [
      [true, "^", [:statements, :_]]
    ],
    "Primary" => [
      [["'class'", "Cname"], "ClassDefinition",[:_, :name, :supername, :body]],
      [["'module'", "Cname"], "ModuleDefinition",[:_, :name, :body]],
      [["'break'"], "Break", []],
      [["'next'"], "Next", []],
      [["'redo'"], "Redo", []],
      [["'retry'"], "Retry", []],
    ],
    :ANY => [
      [["'return'"], "Return", :args],
      [["'yield'"], "Yield", :args],
      [["'super'"], "Super", :args],
    ]
  }

  astspecs.to_a.each do |nt,patterns|
    if nt == :ANY
      prods = grammarInfo.productions.values.flatten
    else
      prods = grammarInfo.productions[nt]
    end
    next unless prods
    prods.each do |pi|
      patterns.each do |pattern, name, cnames|
	if pattern == true or pattern_match(pattern, pi)
	  pi.node_name = name
	  if cnames.kind_of?(Array)
	    pi.children_names = cnames ? cnames.map {|n| n.to_s} : nil
	  else
	    pi.children_names = 
	      assign_dont_care_until_nonterminal(pi.elements, cnames)
	  end
	end
      end
    end
  end
end

def assign_dont_care_until_nonterminal(elements, name)
  names = Array.new
  elements.each do |element|
    if element =~ /^'.*'$/
      names.push :_
    else
      names.push name
      return names
    end
  end
  names.push name
  names
end

def pattern_match(pattern, pi)
  pattern.each_with_index {|p,i| return false if p and p != pi.elements[i]}
  true
end

$new_tokens = [
  ["Blank", /\s+/, [:Skip]],
  # We dont skip comments since people might want to extract info from them
  ["OneLineComment", /#[^(\n|\r|(\n\r)|(\r\n))]*/],
  ["CommentBlock", /(\n|\r|(\n\r)|(\r\n))=begin.*(\n|\r|(\n\r)|(\r\n))=end/]
] 

def add_tokens(grammarInfo)
  $new_tokens.each do |name, regexp, options|
    grammarInfo.add_token(name, [regexp, options])
  end 
end

$new_productions = [
  ["Comment", ["OneLineComment"]],
  ["Comment", ["CommentBlock"]],  
  ["Statement", ["Statement?", "Comment"]],
]

def add_productions(grammarInfo)
  $new_productions.each do |name, elements|
    grammarInfo.add_production(name, elements)
  end
end

def yacc_to_rockit(ast, numberToSelect)
  extractor = GrammarInfoExtractorAndSymbolMapper.new(ast)
  grammar_info = extractor.grammar_info
  transform_productions(grammar_info)
  set_astspecs(grammar_info)
  add_tokens(grammar_info)
  add_productions(grammar_info)
  grammar_info.select_top_nonterms(numberToSelect) if numberToSelect
  grammar_info
end

#############################################################################
# Run as command line utility
#############################################################################
if $0 == __FILE__
  if ARGV.length < 2 or ARGV.length > 3 
    puts <<-EOS
usage: #{$0} yaccgrammar rockitgrammar
EOS
    exit -1
  end

  $TIME_AND_PUTS_VERBOSE = true

  grammarfile, ast, bnf_grammar = ARGV[0], nil, ""
  
  time_and_puts("Extracting BNF grammar from #{grammarfile}") do
    bnf_grammar = extract_bnf_grammar_from_yacc_spec(grammarfile)
  end

  parser = ruby_bnf_grammar_parser

  time_and_puts("Parsing the BNF grammar in file #{grammarfile}") do
    begin
      ast = parser.parse bnf_grammar
    rescue Exception => e
      puts e.inspect
      exit -1
    end
  end
  ast.compact!

  outputfile = ARGV[1]
  File.open(outputfile, "w") do |f|
    f.write yacc_to_rockit(ast, ARGV[2]).pretty_print
    puts "Wrote grammar to #{outputfile}"
    puts "!!You probably have to edit it manually!!"
  end
end
