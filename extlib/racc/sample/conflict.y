# $Id: conflict.y 2112 2005-11-20 13:29:32Z aamine $
#
# Example of conflicted grammer.
# This grammer contains 1 Shift/Reduce conflict and 1 Reduce/Reduce conflict.

class A
rule
  target : outer

  outer  :
         | outer inner

  inner  :
         | inner ITEM
end
