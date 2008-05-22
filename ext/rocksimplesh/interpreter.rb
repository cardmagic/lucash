require 'rocksimplesh_parser'

def do_command(command)
  case command
  when "true", "false"
    return command == "true"
  when /^cd (.*)$/
    return Dir.chdir($1)
  end

  r = []
  er_t = nil
  in_t = nil
  o_t = nil
  p = Open4::popen4(command) do |pid, stdin, stdout, stderr|
    er_t = Thread.new do
      loop do
        $stderr.print stderr.read(stderr.stat.size)
        $stderr.flush
      end
    end

    in_t = Thread.new do
      loop do
        data = gets
        stdin.write(data)
      end
    end
  
    o_t = Thread.new do
      loop do
        r << stdout.read(stdout.stat.size)
      end
    end
  end
  er_t.kill
  in_t.kill
  o_t.kill
  r.join("")
rescue Errno::ENOENT, TypeError
  command
end

def rocksimplesh_eval(ast)
  case ast.name
  when "Statements"
    ast.statements.each {|stmt| rocksimplesh_eval(stmt)}
  when "If"
    if rocksimplesh_eval(ast.condition) # What is true and false in basic?
      rocksimplesh_eval(ast.statements)
    elsif ast.optelse
      rocksimplesh_eval(ast.optelse[2])
    end
  when "For"
    for i in (rocksimplesh_eval(ast.from)..rocksimplesh_eval(ast.to))
      $vars[ast.ident.lexeme] = i
      rocksimplesh_eval(ast.statements)
    end
  when "Print"
    print rocksimplesh_eval(ast.message)
    STDOUT.flush
  when "PrintLn"
    puts rocksimplesh_eval(ast.message)
  when "Assignment"
    $vars[ast.ident.lexeme] = rocksimplesh_eval(ast.expression)
  when "FunctionalAssignment"
    $vars[ast.ident.lexeme] = ast.expression
  when "Condition"
    map = {">" => :>, "<" => :<, "==" => :==}
    rocksimplesh_eval(ast.left).send(map[ast.op.lexeme], rocksimplesh_eval(ast.right))
  when "BinExpr"
    map = {"+"=>:+, "-"=>:-, "*"=>:*, "/"=>:/, "%"=>:% }
    rocksimplesh_eval(ast.left).send(map[ast.op.lexeme], rocksimplesh_eval(ast.right))
  when "ArrayStruct"
    [*rocksimplesh_eval(ast.data)]
  when "BasicArrayData"
    [rocksimplesh_eval(ast.data)]
  when "ArrayData"
    [rocksimplesh_eval(ast.data), *rocksimplesh_eval(ast.array_data)]
  when "String"
    ast.lexeme[1..-2]
  when "Var"
    $vars[ast.lexeme]
  when "Number"
    if ast.lexeme =~ /\./
      ast.lexeme.to_f
    else
      ast.lexeme.to_i
    end
  when "CommandData"
    "#{$vars[ast.var]} #{rocksimplesh_eval(ast.command)}"
  end
end

parser = RockSimpleSh.new
puts
puts 'type "Q" to quit.'
puts
while true
  puts
  print '? '
  if str = gets
    break if /q/i =~ str
    begin
      ast = parser.parse(str).compact!
      rocksimplesh_eval(ast)
    rescue ParseError
      puts $!
    end
  else
    exit
  end
end