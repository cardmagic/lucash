#
# $Id: compat.rb 2168 2006-10-29 04:27:35Z aamine $
#
# Copyright (c) 1999-2006 Minero Aoki
#
# This program is free software.
# You can distribute/modify this program under the terms of
# the GNU LGPL, Lesser General Public License version 2.1.
# For details of the GNU LGPL, see the file "COPYING".
#

unless Object.method_defined?(:__send)
  class Object
    alias __send __send__
  end
end

unless Object.method_defined?(:__send!)
  class Object
    alias __send! __send__
  end
end

unless String.method_defined?(:lines)
  class String
    alias lines to_a
  end
end

unless Array.method_defined?(:map!)
  class Array
    if Array.method_defined?(:collect!)
      alias map! collect!
    else
      alias map! filter
    end
  end
end

unless Enumerable.method_defined?(:map)
  module Enumerable
    alias map collect
  end
end

unless File.respond_to?(:read)
  def File.read(path)
    File.open(path) {|f| return f.read }
  end
end

unless Enumerable.method_defined?(:each_slice)
  module Enumerable
    def each_slice(n)
      buf = []
      each do |x|
        buf.push x
        if buf.size == n
          yield(*buf)
          buf.clear
        end
      end
      yield(*buf) unless buf.empty?
    end
  end
end
