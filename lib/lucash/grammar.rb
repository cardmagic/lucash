#
# DO NOT MODIFY!!!!
# This file is automatically generated by Racc 1.4.5
# from Racc grammer file "".
#

require 'racc/parser.rb'
class LucashGrammar < Racc::Parser

module_eval(<<'...end grammar.y/module_eval...', 'grammar.y', 53)

require 'lucash/parser'

include Lucash::Parser

def next_token
  @q.shift
end

...end grammar.y/module_eval...
##### State transition tables begin ###

racc_action_table = [
     5,    22,    36,    22,     5,    57,    58,    13,     7,     2,
     4,    13,     6,     2,     4,    22,     6,    22,    12,    22,
     5,     3,    12,   -35,     7,     3,    15,    13,     7,     2,
     4,    55,     6,     5,   -35,   -35,    20,    22,    12,    39,
    13,     3,     2,     4,     7,     6,     5,    38,    42,    64,
    22,    12,   nil,    13,     3,     2,     4,     7,     6,     5,
   nil,   nil,   nil,   nil,    12,   nil,    13,     3,     2,     4,
     7,     6,     5,   nil,   nil,   nil,    22,    12,   nil,    13,
     3,     2,     4,     7,     6,    36,    36,   nil,    36,    40,
    12,     7,     7,     3,     7,    22,     7,   -34,   -34,   -34,
   -34,   -34,   -34,    25,    26,    27,    59,    30,    32,   nil,
   nil,   -34,   -34,    65,    36,    33,    34,     3,   nil,   nil,
     7,    24,    25,    26,    27,    28,    30,    32,   nil,   nil,
    23,     4,     4,     6,     6,    36,    29,    31,     3,    12,
    12,     7,     3,     3,    36,     7,     7,     4,    36,     6,
     7,     4,    17,     6,     7,    12,   nil,   nil,     3,    12,
   nil,     7,     3,   nil,     4,     7,     6,   nil,     4,     4,
     6,     6,    12,   nil,   nil,     3,    44,    12,     7,     3,
     3,   nil,     7,     7,     4,   nil,     6,    36,    33,    34,
   nil,   nil,    12,     7,   nil,     3,   nil,   nil,     7,    25,
    26,    27,   nil,    30,    32 ]

racc_action_check = [
     0,    54,    32,    48,     3,    41,    41,     0,    32,     0,
     0,     3,     0,     3,     3,    49,     3,    51,     0,    53,
    19,     0,     3,    61,     0,     3,     3,    19,     3,    19,
    19,    37,    19,    39,    61,    61,     8,    19,    19,    16,
    39,    19,    39,    39,    19,    39,    58,    14,    20,    62,
    43,    39,   nil,    58,    39,    58,    58,    39,    58,     9,
   nil,   nil,   nil,   nil,    58,   nil,     9,    58,     9,     9,
    58,     9,    13,   nil,   nil,   nil,     9,     9,   nil,    13,
     9,    13,    13,     9,    13,    30,    26,   nil,    25,    18,
    13,    30,    26,    13,    25,    18,    13,    44,    44,    44,
    44,    44,    44,    63,    63,    63,    44,    63,    63,   nil,
   nil,    44,    44,    63,    44,    44,    44,    44,   nil,   nil,
    44,    10,    10,    10,    10,    10,    10,    10,   nil,   nil,
    10,    33,    34,    33,    34,    59,    10,    10,    59,    33,
    34,    59,    33,    34,    36,    33,    34,     6,    27,     6,
    36,     4,     4,     4,    27,     6,   nil,   nil,     6,     4,
   nil,     6,     4,   nil,    28,     4,    28,   nil,    22,    29,
    22,    29,    28,   nil,   nil,    28,    22,    29,    28,    22,
    29,   nil,    22,    29,    31,   nil,    31,    12,    12,    12,
   nil,   nil,    31,    12,   nil,    31,   nil,   nil,    31,    60,
    60,    60,   nil,    60,    60 ]

racc_action_pointer = [
    -3,   nil,   nil,     1,   138,   nil,   134,   nil,    36,    56,
   118,   nil,   166,    69,    22,   nil,    13,   nil,    75,    17,
    48,   nil,   155,   nil,   nil,    67,    65,   127,   151,   156,
    64,   171,   -19,   118,   119,   nil,   123,    20,   nil,    30,
   nil,   -11,   nil,    30,    93,   nil,   nil,   nil,   -17,    -5,
   nil,    -3,   nil,    -1,   -19,   nil,   nil,   nil,    43,   114,
   195,    16,    33,    99,   nil,   nil ]

racc_action_default = [
   -36,   -27,    -4,   -36,   -36,    -5,   -36,   -33,   -36,    -3,
    -6,   -30,   -34,   -36,   -36,   -29,   -31,   -10,   -36,   -36,
   -36,    -1,   -36,    -8,    -7,   -36,   -36,   -36,   -36,   -36,
   -36,   -36,   -36,   -36,   -36,   -35,   -34,   -36,   -28,   -36,
    -9,   -36,    66,   -16,   -17,   -24,   -25,   -26,   -14,   -13,
   -22,   -15,   -23,   -20,   -21,    -2,   -32,   -11,   -36,   -36,
   -18,   -30,   -36,   -36,   -12,   -19 ]

racc_goto_table = [
     8,    14,    35,    18,   nil,    19,   nil,   nil,    60,    21,
   nil,   nil,   nil,    37,   nil,    45,    46,    47,   nil,    41,
    50,    43,    52,    63,   nil,   nil,    35,    48,    49,   nil,
    51,   nil,    53,    54,    61,   nil,   nil,    56,   nil,   nil,
   nil,   nil,   nil,   nil,   nil,   nil,   nil,   nil,   nil,   nil,
   nil,   nil,   nil,   nil,   nil,   nil,   nil,   nil,    62 ]

racc_goto_check = [
     1,     6,     4,     2,   nil,     2,   nil,   nil,     3,     1,
   nil,   nil,   nil,     1,   nil,     4,     4,     4,   nil,     1,
     4,     2,     4,     3,   nil,   nil,     4,     2,     2,   nil,
     2,   nil,     2,     2,     4,   nil,   nil,     6,   nil,   nil,
   nil,   nil,   nil,   nil,   nil,   nil,   nil,   nil,   nil,   nil,
   nil,   nil,   nil,   nil,   nil,   nil,   nil,   nil,     1 ]

racc_goto_pointer = [
   nil,     0,    -1,   -36,   -10,   nil,    -2 ]

racc_goto_default = [
   nil,    16,     9,    10,    11,     1,   nil ]

racc_reduce_table = [
  0, 0, :racc_error,
  2, 29, :_reduce_1,
  3, 29, :_reduce_2,
  1, 29, :_reduce_3,
  1, 29, :_reduce_4,
  1, 29, :_reduce_5,
  1, 30, :_reduce_6,
  2, 30, :_reduce_7,
  2, 30, :_reduce_8,
  3, 30, :_reduce_9,
  2, 30, :_reduce_10,
  4, 30, :_reduce_11,
  6, 30, :_reduce_12,
  3, 30, :_reduce_13,
  3, 30, :_reduce_14,
  3, 30, :_reduce_15,
  3, 30, :_reduce_16,
  3, 30, :_reduce_17,
  4, 30, :_reduce_18,
  6, 30, :_reduce_19,
  3, 30, :_reduce_20,
  3, 30, :_reduce_21,
  3, 31, :_reduce_22,
  3, 31, :_reduce_23,
  3, 31, :_reduce_24,
  3, 31, :_reduce_25,
  3, 31, :_reduce_26,
  1, 31, :_reduce_27,
  3, 33, :_reduce_28,
  2, 33, :_reduce_29,
  1, 33, :_reduce_30,
  1, 34, :_reduce_31,
  3, 34, :_reduce_32,
  1, 32, :_reduce_33,
  1, 32, :_reduce_34,
  2, 32, :_reduce_35 ]

racc_reduce_n = 36

racc_shift_n = 66

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
  :IDENT => 21,
  "=" => 22,
  "<-" => 23,
  "[" => 24,
  "]" => 25,
  "," => 26,
  :NUMBER => 27 }

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
  "IDENT",
  "\"=\"",
  "\"<-\"",
  "\"[\"",
  "\"]\"",
  "\",\"",
  "NUMBER",
  "$start",
  "program",
  "line",
  "expr",
  "atom",
  "array",
  "basic_result" ]

Racc_debug_parser = false

##### State transition tables end #####

# reduce 0 omitted

module_eval(<<'.,.,', 'grammar.y', 9)
  def _reduce_1(val, _values)
     [:program, [val[0], *val[1][1]]] 
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

module_eval(<<'.,.,', 'grammar.y', 15)
  def _reduce_6(val, _values)
     [:line, val[0]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 16)
  def _reduce_7(val, _values)
     [:line, val[0]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 17)
  def _reduce_8(val, _values)
     [:line, val[0]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 18)
  def _reduce_9(val, _values)
     [:line, val[1]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 19)
  def _reduce_10(val, _values)
     [:empty_parens] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 20)
  def _reduce_11(val, _values)
     [:if, val[1], val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 21)
  def _reduce_12(val, _values)
     [:if_else, val[1], val[2], val[4]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 22)
  def _reduce_13(val, _values)
     [:and, val[0], val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 23)
  def _reduce_14(val, _values)
     [:pipe, [:line, val[0]], val[2]] 
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
     [:method, val[0], val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 27)
  def _reduce_18(val, _values)
     [:method_with_args, val[0], val[2], val[3]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 28)
  def _reduce_19(val, _values)
     [:method_with_args, val[0], val[2], val[4]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 29)
  def _reduce_20(val, _values)
     [:assignment, val[0], val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 30)
  def _reduce_21(val, _values)
     [:functional_assignment, val[0], val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 32)
  def _reduce_22(val, _values)
     [:add, val[0], val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 33)
  def _reduce_23(val, _values)
     [:subtract, val[0], val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 34)
  def _reduce_24(val, _values)
     [:multiply, val[0], val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 35)
  def _reduce_25(val, _values)
     [:divide, val[0], val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 36)
  def _reduce_26(val, _values)
     [:mod, val[0], val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 37)
  def _reduce_27(val, _values)
     val[0] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 39)
  def _reduce_28(val, _values)
     [:array, val[1]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 40)
  def _reduce_29(val, _values)
     [:empty_array] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 41)
  def _reduce_30(val, _values)
     val[0] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 43)
  def _reduce_31(val, _values)
     [:splat, [val[0]]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 44)
  def _reduce_32(val, _values)
     [:splat, [val[0], *val[2][1]]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 46)
  def _reduce_33(val, _values)
     [:number, val[0]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 47)
  def _reduce_34(val, _values)
     [:value, [val[0]]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 48)
  def _reduce_35(val, _values)
     [:value, [val[0], *val[1][1]]] 
  end
.,.,

def _reduce_none(val, _values)
  val[0]
end

end   # class LucashGrammar


