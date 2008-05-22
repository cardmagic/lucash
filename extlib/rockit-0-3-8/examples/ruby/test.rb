require 'rockit/base_extensions'

time_and_puts("Loading ruby_parser") {
require 'ruby_parser'
}

$rp, $cnt = Ruby.parser, 0

def safe_parse(srcCode)
  puts "#{($cnt += 1).inspect}: Parsing \"#{srcCode}\""
  begin
    ast = $rp.parse srcCode
    s = ast.inspect
  rescue Exception => e
    s = e.inspect
  end
  puts s; puts ""
end

safe_parse "'a small string'"
safe_parse "'a small string'   'and an even smaller'"
safe_parse "1 # Lets try with a comment"
safe_parse "1 + 2.5"
safe_parse "1e34 * 3 unless 1"
safe_parse "alias old new"

# And here are some ambigous ones. We need to introduce priorities to work
# around them... Or is it a problem with the parsing process?
safe_parse "1e34 * 3 unless 1; a = 1 + 2"
safe_parse "1 + a unless 1"

# This one shows one rockit-internal problem. There is an ambiguity between
# "nil" as an identifier and as the "nil" token. Or should the grammar be
# changed? Note that the main ambiguity shown is due to the ruby.grammar 
# though...
safe_parse "1e34 * 3 unless nil"
