if $USING_STRSCAN
  # Try loading strscan. If its not there we use the StringScanner class below.
  begin
    require 'strscan'
    $USING_STRSCAN = true
  rescue LoadError
    $USING_STRSCAN = false
  end
end

unless $USING_STRSCAN
  # Simple substitution for strscan in Ruby for users that lack strscan.
  #
  class StringScanner
    attr_accessor :pointer

    def initialize(string)
      @orig, @pointer = string, 0
    end

    def string
      @orig
    end

    def rest
      @orig[pointer..-1]
    end

    def rest?
      rest.length > 0
    end

    def check(regexp)
      apply_regexp(regexp, false)
    end

    def scan(regexp)
      apply_regexp(regexp, true)
    end

    private
    
    def advance_pointer(matchData)
      @pointer += matchData[0].length
    end
    
    def apply_regexp(regexp, advancePointer)
      md = regexp.match(rest)
      if md && md[0] && (md.begin(0) == 0)
	advance_pointer(md) if advancePointer
	return md[0]
      else
	return nil
      end
    end    
  end
end
