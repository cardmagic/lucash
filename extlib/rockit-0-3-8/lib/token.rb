require 'syntax_tree'
require 'sourcecode_dumpable'
require 'bounded_lru_cache'

class TokenRegexp < Regexp
  def initialize(aStringOrRegexp)
    if aStringOrRegexp.type == String
      @string = aStringOrRegexp
      @string = "^(" + @string + ")" unless @string[0,1] == "^"
      super @string
    elsif aStringOrRegexp.kind_of?(Regexp)
      @string = aStringOrRegexp.source
      @string = "^(" + @string + ")" unless @string[0,1] == "^"
      super(@string, aStringOrRegexp.casefold?)
    else
      raise ArgumentError
    end
  end

  def string
    if @string =~ /\^\(.*\)/
      @string[2...-1]
    else
      @string
    end
  end
end

# Short hands for composing token regexp's
def tr(aStringOrRegexp)
  aStringOrRegexp = aStringOrRegexp.source if aStringOrRegexp.type == Regexp
  TokenRegexp.new(aStringOrRegexp)
end

def tre_compose(tokens, map, separator)
  str = (map % tokens[0].string)
  tokens[1..-1].each {|token| str += separator + (map % token.string)}
  tr(str)
end

def ror(*tokens)
  tre_compose(tokens, "(%s)", "|")
end

def rseq(*tokens)
  tre_compose(tokens, "(%s)", "")
end

def r?(tokenregexp)
  tr("(%s)?" % tokenregexp.string)
end

def rm(tokenregexp)
  tr("(%s)*" % tokenregexp.string)
end

def rp(tokenregexp)
  tr("(%s)+" % tokenregexp.string)
end

class Token
  include SourceCodeDumpable
  attr_reader :skip, :regexp
  attr_accessor :name

  def initialize(aString, aStringOrRegexpOrTokenRegexp = "", *options)
    @name, @regexp = aString, TokenRegexp.new(aStringOrRegexpOrTokenRegexp)
    parse_options(options)
  end
  
  def hash
    @hashvalue || (@hashvalue = [type, @name, @regexp, @skip].hash)
  end

  def parse_options(options)
    option_names = options.map do |option| 
      if option.kind_of? Symbol
	option.id2name.downcase
      else
	option.downcase
      end
    end
    @skip = true if option_names.include? "skip"
  end

  def ==(other)
    other.type == self.type and 
      other.name == name and
      other.regexp.inspect == regexp.inspect and
      other.skip == skip
  end

  def match(aString)
    @regexp.match aString
  end

  def value(lexeme)
    # TODO: Add blocks that map lexeme's to values.
    lexeme
  end

  def create_tree(lexeme, position)
    t = SyntaxTree.new(@name, ["lexeme", "value"], [value(lexeme), lexeme])
    t.attributes[:position] = position
    t
  end

  def to_src(assignToName = nil, nameHash = {})
    if skip
      assign_to(assignToName, new_of_my_type(name, as_code(regexp.to_src), :Skip))
    else
      assign_to(assignToName, new_of_my_type(name, as_code(regexp.to_src)))
    end
  end

  def inspect
    name || regexp.inspect
    #osrc = options_to_src
    #"#{name} = #{regexp.inspect} #{osrc.length>0 ? '['+osrc+']' : ''}"
  end

  protected

  def options_to_src
    if skip
      ":Skip"
    else
      ""
    end
  end
end

class EofToken < Token
  def initialize(*args)
    # Shouldn't match anything but since I'm not sure how to do a regexp
    # with that chareacteristic we use a highly unlikely string in the mean 
    # time.
    super("EOF", "才~中~^^~" + rand(1e10).inspect)
  end

  def ==(other)
    other.type == self.type
  end
end

class EpsilonToken < Token
  def initialize
    # Shouldn't match anything but since I'm not sure how to do a regexp
    # with that chareacteristic we use a highly unlikely string in the mean 
    # time.
    super("epsilon", "才~中~^^~" + rand(1e10).inspect)
  end

  def ==(other)
    other.type == self.type
  end
end

class StringToken < Token
  def initialize(name, string = name)
    @string = string
    super(name, Regexp.escape(string))
  end

  def to_src(assignToName = nil, nameHash = {})
    assign_to(assignToName, new_of_my_type(name, @string))
  end

  def hash
    @hashvalue || (@hashvalue = [type, @name, @string].hash)
  end

  def to_s
    "#{id} #{@string} #{name.inspect} #{hash}"
  end

  def inspect
    @string.inspect
  end
end

def string_token(string)
  StringToken.new("StrToken" + string.hash.inspect, string)
end

class RegexpToken < Token
  def initialize(aString, regexp, *options)
    @name, @regexp = aString, regexp
    parse_options(options)
  end
end

def regexp_token(regexp, *options)
  RegexpToken.new("RegexpToken" + regexp.hash.inspect, regexp, *options)
end

def t(name, re, *options)
  if re.type == String
    StringToken.new("StrToken" + re.hash.inspect, re)
  else
    Token.new(name, re, *options)
  end
end

require 'stringscanner' # DO *NOT* alter since install.rb exploits formatting

# Forking lexers return LexerToken's with the info about a matching token
# and the lexer to access for next tokens.
class LexerToken
  attr_reader :lexeme, :token_type, :lexer, :position

  def initialize(lexeme, tokenType, lexer, position = nil)
    @lexeme, @token_type, @lexer = lexeme, tokenType, lexer
    @position = position
  end

  def create_tree
    @token_type.create_tree(@lexeme, @position)
  end

  def inspect
    "LT(#{lexeme.inspect}, #{token_type.name})"
  end
end

class LexerPosition
  attr_reader :row, :column, :char_position

  def initialize(row = 0, column = 0, char_position = 0)
    @row, @column, @char_position = row, column, char_position
  end
  
  def +(aString)
    char_position = @char_position + aString.length
    num_newlines = aString.count "\r\n"
    row = @row
    if num_newlines == 0
      column = @column + aString.length
    else
      row += num_newlines
      begin
	column = aString.split("\n").last.split("\r").last.length
      rescue NameError
	column = 0
      end
    end
    LexerPosition.new(row, column, char_position)
  end

  def inspect
    "(row=#{row},column=#{@column})"
  end
end

# NOTE: If more performance is needed it might be good to use one char of
# lookahead to group tokens and reduce the number of tokens that needs to
# be tested.
class ForkingRegexpLexer
  attr_accessor :position
  attr_reader :scanner, :tokens, :lexer_cache, :eof_token
  protected :lexer_cache

  def initialize(tokens, eofToken = nil)
    @tokens = tokens
    @eof_token = tokens.detect {|t| t.kind_of?(EofToken)}
    @tokens.delete_if {|t| t.kind_of?(EofToken)}
  end

  @@eof_token = EofToken.new

  def init(aString)
    @position, @current_tokens = LexerPosition.new, nil
    @scanner = StringScanner.new(aString)

    # We speed things up by only having one lexer at each position. Since there
    # are typically only a small number of positions we use a BoundedLruCache
    # of size 20 to keep them in. The cache throws out oldest (least recently
    # used, NOTE! accessed in the cache not used in the parser) lexer when
    # new one inserted. This is to keep the memory consumption down.
    #
    @lexer_cache = BoundedLruCache.new(20)
  end

  # Refactor! Complex interactions when tokens are skipped since the next_lexer
  # update "our" scanner. Find cleaner way of expressing this!
  def peek
    return @current_tokens if @current_tokens
    scanner.pointer = @position.char_position
    @current_tokens = Array.new
    tokens.each do |token|
      if (match = scanner.check(token.regexp))
	if token.skip
	  # Token to be skipped => return tokens matching after the skipped one
	  @current_tokens.concat next_lexer(match).peek
	  scanner.pointer = @position.char_position
	else
	  @current_tokens.push LexerToken.new(match, token, 
					      next_lexer(match), @position)
	end
      end
    end
    if @current_tokens.length == 0
      @string_length = scanner.string.length unless @string_length
      if @position.char_position >= @string_length
	@current_tokens.push LexerToken.new(nil, eof_token || @@eof_token, 
					    nil, @position) 
      end
    end
    return @current_tokens
  end

  def inspect
    "Lexer(#{@position.inspect})"
  end

  protected

  def next_lexer(matchingString)
    pos = @position + matchingString
    #create_next_lexer(pos)
    char_pos = pos.char_position
    lexer = self.lexer_cache[char_pos]
    self.lexer_cache[char_pos] = lexer = create_next_lexer(pos) unless lexer
    lexer
  end


  def create_next_lexer(pos)
    ReferencingRegexpLexer.new(self, pos)
  end
end

class ReferencingRegexpLexer < ForkingRegexpLexer
  def initialize(aForkingRegexpLexer, position)
    @parent_lexer, @position = aForkingRegexpLexer, position
  end

  def inspect
    "RefLexer(#{@position.inspect})"
  end

  protected

  def create_next_lexer(pos)
    ReferencingRegexpLexer.new(@parent_lexer, pos)
  end

  def lexer_cache
    @parent_lexer.lexer_cache
  end

  def scanner
    @parent_lexer.scanner
  end

  def eof_token
    @parent_lexer.eof_token
  end

  def tokens
    @parent_lexer.tokens
  end
end

