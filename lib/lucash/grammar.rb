#
# DO NOT MODIFY!!!!
# This file is automatically generated by Racc 1.4.5
# from Racc grammer file "".
#

require 'racc/parser.rb'
class LucashGrammar < Racc::Parser

module_eval(<<'...end grammar.y/module_eval...', 'grammar.y', 55)

require 'lucash/parser'

include Lucash::Parser

def next_token
  @q.shift
end

...end grammar.y/module_eval...
##### State transition tables begin ###

racc_action_table = [
    20,     6,     8,     5,    22,    23,    24,    40,    25,    26,
    12,    47,     3,     4,    27,     7,    22,    23,    24,    27,
    27,     5,    27,     2,     6,     8,     6,     8,    12,    58,
     3,     4,    27,     7,    56,    57,    22,    23,    24,     5,
    36,     2,     7,    66,     6,     8,    12,    54,     3,     4,
     2,     7,    27,     6,     8,    67,    27,     5,    70,     2,
     7,   nil,     6,     8,    12,   nil,     3,     4,     2,     7,
   nil,     6,     8,   nil,   nil,     5,   nil,     2,   nil,   nil,
     6,     8,    12,   nil,     3,     4,    69,     7,   nil,   nil,
   nil,   nil,   nil,     5,   nil,     2,     7,   nil,     6,     8,
    12,   nil,     3,     4,     2,     7,   nil,     6,     8,   nil,
   nil,     5,   nil,     2,     7,   nil,     6,     8,    12,   nil,
     3,     4,     2,     7,   nil,     6,     8,   nil,   nil,     5,
   nil,     2,    14,   nil,     6,     8,    12,   nil,     3,     4,
    63,     7,   nil,   nil,   nil,   nil,   nil,     5,   nil,     2,
     7,   nil,     6,     8,    12,   nil,     3,     4,     2,     7,
   nil,     6,     8,   nil,   nil,     5,   nil,     2,     7,   nil,
     6,     8,    12,   nil,     3,     4,     2,     7,    65,     6,
     8,   nil,   nil,     5,   nil,     2,     7,   nil,     6,     8,
    12,   nil,     3,     4,     2,     7,   nil,     6,     8,   nil,
   nil,     5,   nil,     2,     7,    37,     6,     8,    12,   nil,
     3,     4,     2,     7,   nil,     6,     8,   nil,    30,     5,
   nil,     2,    31,   nil,     6,     8,    12,    29,     3,     4,
   nil,     7,   nil,    32,    33,   nil,    34,    28,   nil,     2,
   nil,   nil,     6,     8,     5,    22,    23,    24,   nil,    25,
    26,    12,   nil,     3,     4,   nil,     7,   nil,   nil,   nil,
   nil,    27,    30,   nil,     2,   nil,    31,     6,     8,   nil,
   nil,    29,   nil,    64,   nil,   nil,   nil,    32,    33,   nil,
    34,    28,    22,    23,    24,   nil,    25,    26,   nil,    22,
    23,    24,    38,    25,    26,    22,    23,    24,    27,    25,
    26,   nil,    22,    23,    24,    27,    25,    26,    22,    23,
    24,    27,    25,    26,   nil,    22,    23,    24,    27,    25,
    26,    16,     7,   nil,    27,   nil,   nil,     7,   nil,     7,
     2,    27,   nil,     6,     8,     2,     7,     2,     6,     8,
     6,     8,     7,   nil,     2,   nil,   nil,     6,     8,   nil,
     2,   nil,   nil,     6,     8 ]

racc_action_check = [
     9,     8,     8,     9,    51,    51,    51,    20,    51,    51,
     9,    27,     9,     9,    43,     9,    45,    45,    45,    42,
    51,    39,    41,     9,    27,    27,     9,     9,    39,    46,
    39,    39,    45,    39,    39,    39,    44,    44,    44,    35,
    13,    39,    24,    62,    39,    39,    35,    35,    35,    35,
    24,    35,    44,    24,    24,    64,    50,    37,    68,    35,
    28,   nil,    35,    35,    37,   nil,    37,    37,    28,    37,
   nil,    28,    28,   nil,   nil,    67,   nil,    37,   nil,   nil,
    37,    37,    67,   nil,    67,    67,    67,    67,   nil,   nil,
   nil,   nil,   nil,     0,   nil,    67,    33,   nil,    67,    67,
     0,   nil,     0,     0,    33,     0,   nil,    33,    33,   nil,
   nil,     2,   nil,     0,    25,   nil,     0,     0,     2,   nil,
     2,     2,    25,     2,   nil,    25,    25,   nil,   nil,    58,
   nil,     2,     2,   nil,     2,     2,    58,   nil,    58,    58,
    58,    58,   nil,   nil,   nil,   nil,   nil,    12,   nil,    58,
    31,   nil,    58,    58,    12,   nil,    12,    12,    31,    12,
   nil,    31,    31,   nil,   nil,    61,   nil,    12,    23,   nil,
    12,    12,    61,   nil,    61,    61,    23,    61,    61,    23,
    23,   nil,   nil,    15,   nil,    61,    22,   nil,    61,    61,
    15,   nil,    15,    15,    22,    15,   nil,    22,    22,   nil,
   nil,    57,   nil,    15,     7,    15,    15,    15,    57,   nil,
    57,    57,     7,    57,   nil,     7,     7,   nil,    11,    21,
   nil,    57,    11,   nil,    57,    57,    21,    11,    21,    21,
   nil,    21,   nil,    11,    11,   nil,    11,    11,   nil,    21,
   nil,   nil,    21,    21,    18,    18,    18,    18,   nil,    18,
    18,    18,   nil,    18,    18,   nil,    18,   nil,   nil,   nil,
   nil,    18,    60,   nil,    18,   nil,    60,    18,    18,   nil,
   nil,    60,   nil,    60,   nil,   nil,   nil,    60,    60,   nil,
    60,    60,    17,    17,    17,   nil,    17,    17,   nil,    59,
    59,    59,    17,    59,    59,    10,    10,    10,    17,    10,
    10,   nil,    53,    53,    53,    59,    53,    53,    49,    49,
    49,    10,    49,    49,   nil,    52,    52,    52,    53,    52,
    52,     4,     4,   nil,    49,   nil,   nil,    47,   nil,    32,
     4,    52,   nil,     4,     4,    47,    26,    32,    47,    47,
    32,    32,    34,   nil,    26,   nil,   nil,    26,    26,   nil,
    34,   nil,   nil,    34,    34 ]

racc_action_pointer = [
    90,   nil,   108,   nil,   307,   nil,   nil,   189,   -25,     0,
   291,   215,   144,    16,   nil,   180,   nil,   278,   241,   nil,
     7,   216,   171,   153,    27,    99,   321,    -2,    45,   nil,
   nil,   135,   314,    81,   327,    36,   nil,    54,   nil,    18,
   nil,     2,    -1,    -6,    32,    12,    16,   312,   nil,   304,
    36,     0,   311,   298,   nil,   nil,   nil,   198,   126,   285,
   259,   162,    29,   nil,    42,   nil,   nil,    72,    44,   nil,
   nil ]

racc_action_default = [
   -38,   -32,   -38,    -4,   -38,    -5,   -35,   -38,   -36,   -38,
    -3,    -8,   -38,   -38,   -31,   -33,    -7,   -38,   -38,   -37,
   -38,    -1,   -38,   -38,   -38,   -38,   -38,   -38,   -38,   -10,
    -9,   -38,   -38,   -38,   -38,   -38,   -30,   -38,    -6,   -38,
    71,   -27,   -28,   -29,   -25,   -26,   -19,   -38,   -16,   -18,
   -14,   -13,   -15,   -17,    -2,   -34,   -11,   -38,   -38,   -38,
    -8,   -38,   -38,   -20,   -22,   -12,   -21,   -38,   -38,   -23,
   -24 ]

racc_goto_table = [
     9,    17,    15,    13,    18,    19,    60,    48,   nil,   nil,
   nil,   nil,    35,   nil,   nil,   nil,   nil,   nil,    39,    41,
    42,    43,    44,    45,    46,    49,   nil,   nil,    50,    51,
    52,    53,   nil,   nil,   nil,   nil,   nil,    15,    55,   nil,
   nil,   nil,   nil,   nil,    59,   nil,   nil,   nil,   nil,   nil,
   nil,   nil,   nil,   nil,   nil,   nil,   nil,    61,    15,    62,
   nil,   nil,   nil,   nil,   nil,   nil,   nil,    15,    68 ]

racc_goto_check = [
     1,     2,     1,     6,     2,     5,     3,     4,   nil,   nil,
   nil,   nil,     1,   nil,   nil,   nil,   nil,   nil,     1,     2,
     2,     2,     2,     2,     5,     2,   nil,   nil,     2,     2,
     2,     2,   nil,   nil,   nil,   nil,   nil,     1,     6,   nil,
   nil,   nil,   nil,   nil,     2,   nil,   nil,   nil,   nil,   nil,
   nil,   nil,   nil,   nil,   nil,   nil,   nil,     1,     1,     6,
   nil,   nil,   nil,   nil,   nil,   nil,   nil,     1,     6 ]

racc_goto_pointer = [
   nil,     0,    -3,   -41,   -20,    -3,     1 ]

racc_goto_default = [
   nil,    21,    10,    11,   nil,     1,   nil ]

racc_reduce_table = [
  0, 0, :racc_error,
  2, 29, :_reduce_1,
  3, 29, :_reduce_2,
  1, 29, :_reduce_3,
  1, 29, :_reduce_4,
  1, 29, :_reduce_5,
  3, 29, :_reduce_6,
  2, 29, :_reduce_7,
  1, 30, :_reduce_8,
  2, 30, :_reduce_9,
  2, 30, :_reduce_10,
  4, 30, :_reduce_11,
  6, 30, :_reduce_12,
  3, 30, :_reduce_13,
  3, 30, :_reduce_14,
  3, 30, :_reduce_15,
  3, 30, :_reduce_16,
  3, 30, :_reduce_17,
  3, 30, :_reduce_18,
  1, 32, :_reduce_19,
  3, 32, :_reduce_20,
  4, 32, :_reduce_21,
  3, 32, :_reduce_22,
  5, 32, :_reduce_23,
  6, 32, :_reduce_24,
  3, 31, :_reduce_25,
  3, 31, :_reduce_26,
  3, 31, :_reduce_27,
  3, 31, :_reduce_28,
  3, 31, :_reduce_29,
  3, 31, :_reduce_30,
  2, 31, :_reduce_31,
  1, 31, :_reduce_32,
  1, 34, :_reduce_33,
  3, 34, :_reduce_34,
  1, 33, :_reduce_35,
  1, 33, :_reduce_36,
  2, 33, :_reduce_37 ]

racc_reduce_n = 38

racc_shift_n = 71

racc_token_table = {
  false => 0,
  :error => 1,
  :UMINUS => 2,
  ";" => 3,
  "*" => 4,
  "/" => 5,
  "%" => 6,
  "|" => 7,
  "+" => 8,
  "-" => 9,
  "{" => 10,
  "}" => 11,
  "\\n" => 12,
  "(" => 13,
  ")" => 14,
  "if" => 15,
  "end" => 16,
  "else" => 17,
  "&&" => 18,
  "||" => 19,
  "." => 20,
  "=" => 21,
  "<-" => 22,
  "[" => 23,
  "]" => 24,
  "," => 25,
  :NUMBER => 26,
  :IDENT => 27 }

racc_nt_base = 28

racc_use_result_var = false

Racc_arg = [
  racc_action_table,
  racc_action_check,
  racc_action_default,
  racc_action_pointer,
  racc_goto_table,
  racc_goto_check,
  racc_goto_default,
  racc_goto_pointer,
  racc_nt_base,
  racc_reduce_table,
  racc_token_table,
  racc_shift_n,
  racc_reduce_n,
  racc_use_result_var ]

Racc_token_to_s_table = [
  "$end",
  "error",
  "UMINUS",
  "\";\"",
  "\"*\"",
  "\"/\"",
  "\"%\"",
  "\"|\"",
  "\"+\"",
  "\"-\"",
  "\"{\"",
  "\"}\"",
  "\"\\\\n\"",
  "\"(\"",
  "\")\"",
  "\"if\"",
  "\"end\"",
  "\"else\"",
  "\"&&\"",
  "\"||\"",
  "\".\"",
  "\"=\"",
  "\"<-\"",
  "\"[\"",
  "\"]\"",
  "\",\"",
  "NUMBER",
  "IDENT",
  "$start",
  "program",
  "line",
  "expr",
  "method_call",
  "atom",
  "basic_result" ]

Racc_debug_parser = false

##### State transition tables end #####

# reduce 0 omitted

module_eval(<<'.,.,', 'grammar.y', 9)
  def _reduce_1(val, _values)
     [:program, val[0][1] + val[1][1]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 10)
  def _reduce_2(val, _values)
     [:block, val[1]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 11)
  def _reduce_3(val, _values)
     [:program, [val[0]]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 12)
  def _reduce_4(val, _values)
     [:program, []] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 13)
  def _reduce_5(val, _values)
     [:program, []] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 14)
  def _reduce_6(val, _values)
     val[1] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 15)
  def _reduce_7(val, _values)
     [:empty_parens] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 17)
  def _reduce_8(val, _values)
     val[0] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 18)
  def _reduce_9(val, _values)
     val[0] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 19)
  def _reduce_10(val, _values)
     val[0] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 20)
  def _reduce_11(val, _values)
     [:if, val[1], val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 21)
  def _reduce_12(val, _values)
     [:if, val[1], val[2], val[4]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 22)
  def _reduce_13(val, _values)
     [:and, val[0], val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 23)
  def _reduce_14(val, _values)
     [:pipe, val[0], val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 24)
  def _reduce_15(val, _values)
     [:or, val[0], val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 25)
  def _reduce_16(val, _values)
     [:method, val[0], val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 26)
  def _reduce_17(val, _values)
     [:assignment, val[0], val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 27)
  def _reduce_18(val, _values)
     [:functional_assignment, val[0], val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 29)
  def _reduce_19(val, _values)
     [:method_call, val[0]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 30)
  def _reduce_20(val, _values)
     [:method_call, val[0]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 31)
  def _reduce_21(val, _values)
     [:method_call, val[0], val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 32)
  def _reduce_22(val, _values)
     [:method_call, val[1]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 33)
  def _reduce_23(val, _values)
     [:method_call, val[1]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 34)
  def _reduce_24(val, _values)
     [:method_call, val[1], val[4]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 36)
  def _reduce_25(val, _values)
     [:add, val[0], val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 37)
  def _reduce_26(val, _values)
     [:subtract, val[0], val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 38)
  def _reduce_27(val, _values)
     [:multiply, val[0], val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 39)
  def _reduce_28(val, _values)
     [:divide, val[0], val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 40)
  def _reduce_29(val, _values)
     [:mod, val[0], val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 41)
  def _reduce_30(val, _values)
     [:array, val[1]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 42)
  def _reduce_31(val, _values)
     [:empty_array] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 43)
  def _reduce_32(val, _values)
     val[0] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 45)
  def _reduce_33(val, _values)
     [:splat, [val[0]]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 46)
  def _reduce_34(val, _values)
     [:splat, [val[0], *val[2][1]]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 48)
  def _reduce_35(val, _values)
     [:number, val[0]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 49)
  def _reduce_36(val, _values)
     [:value, [val[0]]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 50)
  def _reduce_37(val, _values)
     [:value, [val[0], *val[1][1]]] 
  end
.,.,

def _reduce_none(val, _values)
  val[0]
end

end   # class LucashGrammar


