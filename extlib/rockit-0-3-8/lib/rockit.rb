require 'glr_parser'
require 'lalr_parsetable_generator'
require 'base_extensions'
require 'rockit_grammar_ast_eval'

begin
  require 'rockit_grammars_parser'
rescue LoadError
  # We get here when bootstrapping so its ok.
end

# Facade for rockit
module Parse
  def Parse.parser_from_grammar(aGrammar,
				parserType = GeneralizedLrParser,
				tableGenerator = LaLr1ParseTableGenerator)
    pt = tableGenerator.new(aGrammar).generate_parse_table
    parserType.new(pt)
  end

  def Parse.generate_parser(aString,
			    parserType = GeneralizedLrParser,
			    tableGenerator = LaLr1ParseTableGenerator)
    ast = rockit_grammars_parser.parse(aString)
    ast.compact!
    grammar = rockit_grammar_eval(ast)
    Parse.parser_from_grammar(grammar)
  end

  def Parse.generate_parser_from_file_to_file(grammarFile,
					      outputFile,
					      parserName = nil,
					      moduleName = "Parse",
					      grammarParser = rockit_grammars_parser,
					      parserType = GeneralizedLrParser,
					      tableGen = 
					        LaLr1ParseTableGenerator)
    if parserName == nil or parserName.kind_of?(String)
      parserName = as_module_method_named(moduleName, parserName || "parser")
    end
    grammar_text = nil
    File.open(grammarFile, "r") {|f| grammar_text = f.read}
    ast, grammar, parser = nil, nil, nil
    time_and_puts("Parsing #{grammarFile}") {
      ast = grammarParser.parse(grammar_text)
      ast.compact!
    }

    # File.open(outputFile + ".graph", "w") {|f| f.write ast.to_graph}

    time_and_puts("Building grammar from abstract syntax tree") {
      grammar = rockit_grammar_eval(ast)
    }
    time_and_puts("Generating parser from grammar") {
      parser = Parse.parser_from_grammar(grammar)
    }
    File.open(outputFile, "w") do |f|
      time_and_puts("Writing parser to file #{outputFile}") {
	f.write "require 'rockit'\n" + 
	  parser.to_src_in_module(parserName, moduleName)
      }
    end
  end
end

if __FILE__ == $0
  if ARGV.length < 2
    puts <<EOS
Rockit parser generator version #{rockit_version}
usage: #{$0} <grammar_file> <output_file> [module [method]]
EOS
    exit(-1)
  end

  $TIME_AND_PUTS_VERBOSE = true

  Parse.generate_parser_from_file_to_file(ARGV[0], ARGV[1],
					  ARGV[3] || "parser",
					  ARGV[2] || "Parse")
					  
end
