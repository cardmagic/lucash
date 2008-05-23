#
# $Id: exception.rb 2146 2006-07-03 17:24:42Z aamine $
#
# Copyright (c) 1999-2006 Minero Aoki
#
# This program is free software.
# You can distribute/modify this program under the terms of
# the GNU LGPL, Lesser General Public License version 2.1.
# For details of the GNU LGPL, see the file "COPYING".
#

module Racc
  class Error < StandardError; end
  class CompileError < Error; end
end
