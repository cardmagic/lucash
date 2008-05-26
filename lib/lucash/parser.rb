class Lucash
  module Parser
    def parse(str)
      @q = []
      until str.empty?
        case str
        when /\A["']([^\"]*)["']/
          @q.push ['"', '"']
          @q.push [:IDENT, $1]
          @q.push ['"', '"']
        when /\A\-\s+/
          @q.push ['-', '-']
        when /\A\s(\.\.*)/
            @q.push [:IDENT, $1]
        when /\A[ \t\r]+/
    	  when /\A(if|else|end)/i
    	    @q.push [$&, $&]
    	  when /\A\n/
    	    @q.push ['\n', '\n']
        when /\A(&&|\|\||\|)/
    	    @q.push [$&, $&]
        when /\A<-/
    	    @q.push [$&, $&]
        when /\A\-?\d+\.\d+/
          @q.push [:NUMBER, $&.to_f]
        when /\A\-?[\d]+/
          @q.push [:NUMBER, $&.to_i]
        when /\A\:([\w\-]+)/
          @q.push [:IDENT, $1.intern]
        when /\A[\w\-][\w\-\=]*/
          @q.push [:IDENT, $&]
        when /\A\/([^\/]+)\//
          @q.push [:IDENT, Regexp.new($1)]
        when /\A.|\n/o
          s = $&
          @q.push [s, s]
        end
        str = $'
      end
      @q.push [false, '$end']
      puts @q.inspect if ENV['DEBUG']
      do_parse
    end
  end
end