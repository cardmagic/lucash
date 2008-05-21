require 'minibasic_parser'
$vars = Hash.new(0)  # For variables and their values. Default value is 0.

def mb_eval(ast)
  case ast.name
  when "Statements"
    ast.statements.each {|stmt| mb_eval(stmt)}
  when "If"
    if mb_eval(ast.condition) # What is true and false in basic?
      mb_eval(ast.statements)
    elsif ast.optelse
      mb_eval(ast.optelse[2])
    end
  when "For"
    for i in (mb_eval(ast.from)..mb_eval(ast.to))
      $vars[ast.ident.lexeme] = i
      mb_eval(ast.statements)
    end
  when "Read"
    print "? "; STDOUT.flush
    $vars[ast.ident.lexeme] = STDIN.gets.to_i   # Error catching?!
  when "Print"
    print mb_eval(ast.message); STDOUT.flush
  when "PrintLn"
    print "\n"; STDOUT.flush
  when "Assignment"
    $vars[ast.ident.lexeme] = mb_eval(ast.expression)
  when "Condition"
    map = {">" => :>, "<" => :<, "=" => :==}
    mb_eval(ast.left).send(map[ast.op.lexeme], mb_eval(ast.right))
  when "BinExpr"
    map = {"+"=>:+, "-"=>:-, "*"=>:*, "/"=>"/".intern, "MOD"=>"%".intern }
    mb_eval(ast.left).send(map[ast.op.lexeme], mb_eval(ast.right))
  when "String"
    ast.lexeme[1..-2]
  when "Identifier"
    $vars[ast.lexeme]
  when "Number"
    ast.lexeme.to_i
  end
end

File.open(ARGV[0], "r") do |bf|
  ast = MiniBasic.parser.parse(bf.read).compact!
  mb_eval(ast)
end
