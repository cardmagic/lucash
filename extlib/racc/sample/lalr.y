# $Id: lalr.y 2112 2005-11-20 13:29:32Z aamine $
#
# This is LALR grammer, and not LL/SLR.

class A
rule
  A : L '=' E

  L : i
    | R '^' i

  E : E '+' R
    | R
    | '@' L

  R : i
end
