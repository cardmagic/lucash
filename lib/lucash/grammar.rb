#
# DO NOT MODIFY!!!!
# This file is automatically generated by Racc 1.4.5
# from Racc grammer file "".
#

require 'racc/parser.rb'
class LucashGrammar < Racc::Parser

module_eval(<<'...end grammar.y/module_eval...', 'grammar.y', 66)

require 'lucash/parser'

include Lucash::Parser

def next_token
  @q.shift
end

...end grammar.y/module_eval...
##### State transition tables begin ###

racc_action_table = [
    24,     7,     7,     3,     7,     7,    37,    38,    29,    30,
    21,    88,     2,     8,     5,     7,    10,    13,    54,    47,
     1,     1,    46,     1,     1,     4,     8,    45,    12,    16,
    18,    19,     3,    46,     1,    87,    42,    44,    52,    21,
    89,     2,    55,     5,     7,    10,    13,    56,    43,    37,
    38,    29,    30,    39,     4,     8,    76,    12,    16,    18,
    19,     3,    75,     1,    37,    38,    29,    30,    21,    23,
     2,    96,     5,     7,    10,    13,    95,    31,    32,    33,
    34,    35,    36,     4,     8,    68,    12,    16,    18,    19,
     3,    98,     1,    37,    38,    29,    30,    21,   nil,     2,
   nil,     5,     7,    10,    13,   nil,    31,    32,    33,    34,
    35,    36,     4,     8,   nil,    12,    16,    18,    19,     3,
   nil,     1,    37,    38,    29,    30,    21,    94,     2,   nil,
     5,     7,    10,    13,   nil,    31,    32,    33,    34,    35,
    36,     4,     8,   nil,    12,    16,    18,    19,     3,   nil,
     1,    37,    38,    29,    30,    21,   nil,     2,   nil,     5,
     7,    10,    13,   nil,    31,    32,    33,    34,    35,    36,
     4,     8,    28,    12,    16,    18,    19,     3,   nil,     1,
    37,    38,    29,    30,    21,   nil,     2,   nil,     5,     7,
    10,    13,    93,    31,    32,    33,    34,    35,    36,     4,
     8,   nil,    12,    16,    18,    19,     3,   nil,     1,    37,
    38,    29,    30,    21,   nil,     2,   nil,     5,     7,    10,
    13,   nil,    31,    32,    33,    34,   nil,   nil,     4,     8,
   nil,    12,    16,    18,    19,     3,   nil,     1,    37,    38,
    29,    30,    21,   nil,     2,   nil,     5,     7,    10,    13,
   nil,    31,    32,    33,    34,   nil,   nil,     4,     8,   nil,
    12,    16,    18,    19,     3,   nil,     1,    37,    38,    29,
    30,    21,   nil,     2,   nil,     5,     7,    10,    13,    37,
    38,    29,    30,   nil,   nil,   nil,     4,     8,   nil,    12,
    16,    18,    19,     3,   nil,     1,   nil,   nil,   nil,   nil,
    21,   nil,     2,   nil,     5,     7,    10,    13,   nil,   nil,
   nil,   nil,   nil,   nil,   nil,     4,     8,   nil,    12,    16,
    18,    19,     3,   nil,     1,   nil,   nil,   nil,   nil,    21,
    85,     2,   nil,     5,     7,    10,    13,   nil,   nil,   nil,
   nil,   nil,   nil,   nil,     4,     8,   nil,    12,    16,    18,
    19,     3,   nil,     1,   nil,   nil,   nil,   nil,    21,    84,
     2,   nil,     5,     7,    10,    13,   nil,   nil,   nil,   nil,
   nil,   nil,   nil,     4,     8,   nil,    12,    16,    18,    19,
     3,   nil,     1,   nil,   nil,   nil,   nil,    21,   nil,     2,
   nil,     5,     7,    10,    13,   nil,   nil,   nil,   nil,   nil,
   nil,   nil,     4,     8,   nil,    12,    16,    18,    19,     3,
   nil,     1,   nil,   nil,   nil,   nil,    21,   nil,     2,   nil,
     5,     7,    10,    13,   nil,   nil,   nil,   nil,   nil,   nil,
   nil,     4,     8,   nil,    12,    16,    18,    19,    51,     3,
     1,   nil,   nil,   nil,   nil,   nil,    21,   nil,     2,   nil,
     5,     7,    10,    13,    82,    83,   nil,   nil,   nil,   nil,
   nil,     4,     8,   nil,    12,    16,    18,    19,     3,   nil,
     1,   nil,   nil,   nil,   nil,    21,   nil,     2,   nil,     5,
     7,    10,    13,   nil,   nil,   nil,   nil,   nil,   nil,   nil,
     4,     8,   nil,    12,    16,    18,    19,     3,   nil,     1,
   nil,   nil,   nil,   nil,    21,   nil,     2,   nil,     5,     7,
    10,    13,   nil,   nil,   nil,   nil,   nil,   nil,   nil,     4,
     8,   nil,    12,    16,    18,    19,     3,   nil,     1,   nil,
   nil,   nil,   nil,    21,   nil,     2,   nil,     5,     7,    10,
    13,   nil,   nil,   nil,   nil,   nil,   nil,   nil,     4,     8,
   nil,    12,    16,    18,    19,     3,   nil,     1,   nil,   nil,
   nil,   nil,    21,    79,     2,   nil,     5,     7,    10,    13,
   nil,   nil,   nil,   nil,   nil,   nil,   nil,     4,     8,   nil,
    12,    16,    18,    19,     3,   nil,     1,   nil,   nil,   nil,
   nil,    21,   nil,     2,   nil,     5,     7,    10,    13,   nil,
   nil,   nil,   nil,   nil,   nil,   nil,     4,     8,   nil,    12,
    16,    18,    19,     3,   nil,     1,   nil,   nil,   nil,   nil,
    21,   nil,     2,   nil,     5,     7,    10,    13,   nil,   nil,
   nil,   nil,   nil,   nil,   nil,     4,     8,    57,    12,    16,
    18,    19,     3,   nil,     1,   nil,   nil,   nil,   nil,    21,
   nil,     2,   nil,     5,     7,    10,    13,   nil,   nil,   nil,
   nil,   nil,   nil,   nil,     4,     8,    77,    12,    16,    18,
    19,     3,   nil,     1,   nil,   nil,   nil,   nil,    21,   nil,
     2,   nil,     5,     7,    10,    13,   nil,   nil,   nil,   nil,
   nil,   nil,   nil,     4,     8,   nil,    12,    16,    18,    19,
     3,    74,     1,   nil,   nil,   nil,   nil,    21,   nil,     2,
   nil,     5,     7,    10,    13,   nil,   nil,   nil,   nil,   nil,
   nil,   nil,     4,     8,   nil,    12,    16,    18,    19,     3,
   nil,     1,   nil,   nil,   nil,   nil,    21,   nil,     2,   nil,
     5,     7,    10,    13,   nil,   nil,   nil,   nil,   nil,   nil,
   nil,     4,     8,   nil,    12,    16,    18,    19,   nil,   nil,
     1,     3,    31,    32,    33,    34,    35,    36,    21,   nil,
     2,   nil,     5,     7,    10,    13,   nil,   nil,    37,    38,
    29,    30,   nil,     4,     8,   nil,    12,    16,    18,    19,
     3,   nil,     1,   nil,   nil,   nil,   nil,    21,   nil,     2,
   nil,     5,     7,    10,    13,    81,   nil,   nil,     5,     7,
    10,    13,     4,     8,   nil,    12,    16,    18,    19,     4,
     8,     1,    12,    16,    18,    19,   nil,   nil,     1,     5,
     7,    10,    13,   nil,   nil,   nil,     5,     7,    10,    13,
     4,     8,   nil,    12,    16,    18,    19,     4,     8,     1,
    12,    16,    18,    19,   nil,   nil,     1,     5,     7,    10,
    13,   nil,   nil,   nil,     5,     7,    10,    13,     4,     8,
   nil,    12,    16,    18,    19,     4,     8,     1,    12,    16,
    18,    19,   nil,   nil,     1,     5,     7,    10,    13,   nil,
   nil,   nil,     5,     7,    10,    13,     4,     8,   nil,    12,
    16,    18,    19,     4,     8,     1,    12,    16,    18,    19,
   nil,   nil,     1,     5,     7,    10,    13,   nil,   nil,   nil,
     5,     7,    10,    13,     4,     8,   nil,    12,    16,    18,
    19,     4,     8,     1,    12,    16,    18,    19,   nil,   nil,
     1,     5,     7,    10,    13,   nil,   nil,   nil,     5,     7,
    10,    13,     4,     8,   nil,    12,    16,    18,    19,     4,
     8,     1,    12,    16,    18,    19,   nil,   nil,     1 ]

racc_action_check = [
     6,     7,    29,     6,    12,     4,    63,    63,    63,    63,
     6,    78,     6,    29,     6,     6,     6,     6,    22,    16,
     7,    29,    15,    12,     4,     6,     6,    14,     6,     6,
     6,     6,    18,    58,     6,    75,    14,    14,    20,    18,
    80,    18,    23,    18,    18,    18,    18,    24,    14,    62,
    62,    62,    62,    10,    18,    18,    50,    18,    18,    18,
    18,    92,    49,    18,    61,    61,    61,    61,    92,     5,
    92,    94,    92,    92,    92,    92,    92,     9,     9,     9,
     9,     9,     9,    92,    92,    39,    92,    92,    92,    92,
     0,    97,    92,     9,     9,     9,     9,     0,   nil,     0,
   nil,     0,     0,     0,     0,   nil,    59,    59,    59,    59,
    59,    59,     0,     0,   nil,     0,     0,     0,     0,    91,
   nil,     0,    59,    59,    59,    59,    91,    91,    91,   nil,
    91,    91,    91,    91,   nil,    71,    71,    71,    71,    71,
    71,    91,    91,   nil,    91,    91,    91,    91,     8,   nil,
    91,    71,    71,    71,    71,     8,   nil,     8,   nil,     8,
     8,     8,     8,   nil,    67,    67,    67,    67,    67,    67,
     8,     8,     8,     8,     8,     8,     8,    90,   nil,     8,
    67,    67,    67,    67,    90,   nil,    90,   nil,    90,    90,
    90,    90,    90,    66,    66,    66,    66,    66,    66,    90,
    90,   nil,    90,    90,    90,    90,    89,   nil,    90,    66,
    66,    66,    66,    89,   nil,    89,   nil,    89,    89,    89,
    89,   nil,    65,    65,    65,    65,   nil,   nil,    89,    89,
   nil,    89,    89,    89,    89,    87,   nil,    89,    65,    65,
    65,    65,    87,   nil,    87,   nil,    87,    87,    87,    87,
   nil,    64,    64,    64,    64,   nil,   nil,    87,    87,   nil,
    87,    87,    87,    87,    83,   nil,    87,    64,    64,    64,
    64,    83,   nil,    83,   nil,    83,    83,    83,    83,    60,
    60,    60,    60,   nil,   nil,   nil,    83,    83,   nil,    83,
    83,    83,    83,    74,   nil,    83,   nil,   nil,   nil,   nil,
    74,   nil,    74,   nil,    74,    74,    74,    74,   nil,   nil,
   nil,   nil,   nil,   nil,   nil,    74,    74,   nil,    74,    74,
    74,    74,    73,   nil,    74,   nil,   nil,   nil,   nil,    73,
    73,    73,   nil,    73,    73,    73,    73,   nil,   nil,   nil,
   nil,   nil,   nil,   nil,    73,    73,   nil,    73,    73,    73,
    73,    72,   nil,    73,   nil,   nil,   nil,   nil,    72,    72,
    72,   nil,    72,    72,    72,    72,   nil,   nil,   nil,   nil,
   nil,   nil,   nil,    72,    72,   nil,    72,    72,    72,    72,
    96,   nil,    72,   nil,   nil,   nil,   nil,    96,   nil,    96,
   nil,    96,    96,    96,    96,   nil,   nil,   nil,   nil,   nil,
   nil,   nil,    96,    96,   nil,    96,    96,    96,    96,    19,
   nil,    96,   nil,   nil,   nil,   nil,    19,   nil,    19,   nil,
    19,    19,    19,    19,   nil,   nil,   nil,   nil,   nil,   nil,
   nil,    19,    19,   nil,    19,    19,    19,    19,    19,    70,
    19,   nil,   nil,   nil,   nil,   nil,    70,   nil,    70,   nil,
    70,    70,    70,    70,    70,    70,   nil,   nil,   nil,   nil,
   nil,    70,    70,   nil,    70,    70,    70,    70,    21,   nil,
    70,   nil,   nil,   nil,   nil,    21,   nil,    21,   nil,    21,
    21,    21,    21,   nil,   nil,   nil,   nil,   nil,   nil,   nil,
    21,    21,   nil,    21,    21,    21,    21,    40,   nil,    21,
   nil,   nil,   nil,   nil,    40,   nil,    40,   nil,    40,    40,
    40,    40,   nil,   nil,   nil,   nil,   nil,   nil,   nil,    40,
    40,   nil,    40,    40,    40,    40,    54,   nil,    40,   nil,
   nil,   nil,   nil,    54,   nil,    54,   nil,    54,    54,    54,
    54,   nil,   nil,   nil,   nil,   nil,   nil,   nil,    54,    54,
   nil,    54,    54,    54,    54,    53,   nil,    54,   nil,   nil,
   nil,   nil,    53,    53,    53,   nil,    53,    53,    53,    53,
   nil,   nil,   nil,   nil,   nil,   nil,   nil,    53,    53,   nil,
    53,    53,    53,    53,    25,   nil,    53,   nil,   nil,   nil,
   nil,    25,   nil,    25,   nil,    25,    25,    25,    25,   nil,
   nil,   nil,   nil,   nil,   nil,   nil,    25,    25,   nil,    25,
    25,    25,    25,    27,   nil,    25,   nil,   nil,   nil,   nil,
    27,   nil,    27,   nil,    27,    27,    27,    27,   nil,   nil,
   nil,   nil,   nil,   nil,   nil,    27,    27,    27,    27,    27,
    27,    27,    52,   nil,    27,   nil,   nil,   nil,   nil,    52,
   nil,    52,   nil,    52,    52,    52,    52,   nil,   nil,   nil,
   nil,   nil,   nil,   nil,    52,    52,    52,    52,    52,    52,
    52,    48,   nil,    52,   nil,   nil,   nil,   nil,    48,   nil,
    48,   nil,    48,    48,    48,    48,   nil,   nil,   nil,   nil,
   nil,   nil,   nil,    48,    48,   nil,    48,    48,    48,    48,
    47,    48,    48,   nil,   nil,   nil,   nil,    47,   nil,    47,
   nil,    47,    47,    47,    47,   nil,   nil,   nil,   nil,   nil,
   nil,   nil,    47,    47,   nil,    47,    47,    47,    47,    46,
   nil,    47,   nil,   nil,   nil,   nil,    46,   nil,    46,   nil,
    46,    46,    46,    46,   nil,   nil,   nil,   nil,   nil,   nil,
   nil,    46,    46,   nil,    46,    46,    46,    46,   nil,   nil,
    46,    41,    41,    41,    41,    41,    41,    41,    41,   nil,
    41,   nil,    41,    41,    41,    41,   nil,   nil,    41,    41,
    41,    41,   nil,    41,    41,   nil,    41,    41,    41,    41,
    69,   nil,    41,   nil,   nil,   nil,   nil,    69,   nil,    69,
   nil,    69,    69,    69,    69,    69,   nil,   nil,    33,    33,
    33,    33,    69,    69,   nil,    69,    69,    69,    69,    33,
    33,    69,    33,    33,    33,    33,   nil,   nil,    33,    37,
    37,    37,    37,   nil,   nil,   nil,    38,    38,    38,    38,
    37,    37,   nil,    37,    37,    37,    37,    38,    38,    37,
    38,    38,    38,    38,   nil,   nil,    38,    35,    35,    35,
    35,   nil,   nil,   nil,    34,    34,    34,    34,    35,    35,
   nil,    35,    35,    35,    35,    34,    34,    35,    34,    34,
    34,    34,   nil,   nil,    34,    36,    36,    36,    36,   nil,
   nil,   nil,    43,    43,    43,    43,    36,    36,   nil,    36,
    36,    36,    36,    43,    43,    36,    43,    43,    43,    43,
   nil,   nil,    43,    32,    32,    32,    32,   nil,   nil,   nil,
    31,    31,    31,    31,    32,    32,   nil,    32,    32,    32,
    32,    31,    31,    32,    31,    31,    31,    31,   nil,   nil,
    31,    30,    30,    30,    30,   nil,   nil,   nil,    13,    13,
    13,    13,    30,    30,   nil,    30,    30,    30,    30,    13,
    13,    30,    13,    13,    13,    13,   nil,   nil,    13 ]

racc_action_pointer = [
    87,   nil,   nil,   nil,   -10,    54,     0,   -14,   145,    73,
    38,   nil,   -11,   934,    24,    12,     9,   nil,    29,   406,
    12,   465,    -8,    28,    47,   581,   nil,   610,   nil,   -13,
   927,   906,   899,   794,   850,   843,   871,   815,   822,    69,
   494,   758,   nil,   878,   nil,   nil,   726,   697,   668,    35,
    24,   nil,   639,   552,   523,   nil,   nil,   nil,    23,   102,
   259,    44,    29,   -14,   247,   218,   189,   160,   nil,   787,
   436,   131,   348,   319,   290,    25,   nil,   nil,   -16,   nil,
    13,   nil,   nil,   261,   nil,   nil,   nil,   232,   nil,   203,
   174,   116,    58,   nil,    45,   nil,   377,    64,   nil ]

racc_action_default = [
   -46,   -43,   -24,   -25,   -46,   -46,   -46,   -44,   -46,    -3,
   -46,    -4,   -46,   -46,    -5,   -33,   -46,   -42,   -46,   -46,
   -34,   -46,   -46,   -46,   -46,    -1,   -45,   -46,   -41,   -46,
   -46,   -46,   -46,   -46,   -46,   -46,   -46,   -46,   -46,   -46,
   -46,   -46,    -7,   -46,    -8,    -6,   -46,   -46,   -38,   -46,
   -46,   -32,   -46,   -46,   -46,    -9,    99,   -40,   -16,   -17,
   -28,   -29,   -30,   -14,   -26,   -27,   -13,   -15,   -10,   -46,
   -46,   -18,   -46,   -46,   -46,   -46,   -31,   -35,   -46,    -2,
   -46,   -20,   -11,   -46,   -37,   -21,   -39,   -46,   -36,   -46,
   -46,   -46,   -46,   -12,   -22,   -19,   -46,   -46,   -23 ]

racc_goto_table = [
     6,    49,    50,    58,    22,   nil,    41,    26,    27,   nil,
   nil,   nil,    40,   nil,   nil,   nil,   nil,   nil,    48,    48,
   nil,    53,   nil,    59,    60,    61,    62,    63,    64,    65,
    66,    67,   nil,   nil,   nil,    78,    71,    80,   nil,   nil,
    69,    70,   nil,   nil,   nil,   nil,    72,    73,   nil,   nil,
   nil,   nil,    48,   nil,    48,   nil,   nil,    86,   nil,   nil,
   nil,   nil,   nil,   nil,   nil,   nil,   nil,   nil,   nil,   nil,
   nil,   nil,   nil,   nil,    48,   nil,   nil,   nil,   nil,    97,
   nil,   nil,   nil,    90,   nil,   nil,   nil,    91,   nil,    92,
   nil,   nil,   nil,   nil,   nil,   nil,    48 ]

racc_goto_check = [
     1,     7,     7,     5,     6,   nil,     2,     6,     1,   nil,
   nil,   nil,     6,   nil,   nil,   nil,   nil,   nil,     1,     1,
   nil,     1,   nil,     2,     2,     2,     2,     2,     2,     2,
     2,     2,   nil,   nil,   nil,     7,     2,     7,   nil,   nil,
     1,     1,   nil,   nil,   nil,   nil,     1,     1,   nil,   nil,
   nil,   nil,     1,   nil,     1,   nil,   nil,     7,   nil,   nil,
   nil,   nil,   nil,   nil,   nil,   nil,   nil,   nil,   nil,   nil,
   nil,   nil,   nil,   nil,     1,   nil,   nil,   nil,   nil,     7,
   nil,   nil,   nil,     1,   nil,   nil,   nil,     1,   nil,     1,
   nil,   nil,   nil,   nil,   nil,   nil,     1 ]

racc_goto_pointer = [
   nil,     0,    -7,   nil,   nil,   -26,     0,   -17,   nil ]

racc_goto_default = [
   nil,    25,     9,    11,    14,    15,    17,   nil,    20 ]

racc_reduce_table = [
  0, 0, :racc_error,
  2, 36, :_reduce_1,
  3, 36, :_reduce_2,
  1, 36, :_reduce_3,
  1, 36, :_reduce_4,
  1, 37, :_reduce_5,
  2, 37, :_reduce_6,
  2, 37, :_reduce_7,
  2, 37, :_reduce_8,
  3, 37, :_reduce_9,
  3, 37, :_reduce_10,
  4, 37, :_reduce_11,
  6, 37, :_reduce_12,
  3, 37, :_reduce_13,
  3, 37, :_reduce_14,
  3, 37, :_reduce_15,
  3, 37, :_reduce_16,
  3, 37, :_reduce_17,
  3, 37, :_reduce_18,
  7, 37, :_reduce_19,
  4, 37, :_reduce_20,
  4, 37, :_reduce_21,
  6, 37, :_reduce_22,
  9, 37, :_reduce_23,
  1, 38, :_reduce_24,
  1, 38, :_reduce_25,
  3, 39, :_reduce_26,
  3, 39, :_reduce_27,
  3, 39, :_reduce_28,
  3, 39, :_reduce_29,
  3, 39, :_reduce_30,
  3, 39, :_reduce_31,
  2, 39, :_reduce_32,
  1, 39, :_reduce_33,
  1, 40, :_reduce_34,
  3, 40, :_reduce_35,
  4, 40, :_reduce_36,
  4, 40, :_reduce_37,
  1, 42, :_reduce_38,
  3, 42, :_reduce_39,
  3, 43, :_reduce_40,
  2, 43, :_reduce_41,
  1, 43, :_reduce_42,
  1, 41, :_reduce_43,
  1, 41, :_reduce_44,
  2, 41, :_reduce_45 ]

racc_reduce_n = 46

racc_shift_n = 99

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
  "&" => 13,
  "\"" => 14,
  :IDENT => 15,
  "'" => 16,
  "if" => 17,
  "end" => 18,
  "else" => 19,
  "&&" => 20,
  "||" => 21,
  "." => 22,
  "==" => 23,
  "=" => 24,
  "defn" => 25,
  "(" => 26,
  ")" => 27,
  "def" => 28,
  "->" => 29,
  "->(" => 30,
  "[" => 31,
  "]" => 32,
  "," => 33,
  :NUMBER => 34 }

racc_nt_base = 35

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
  "\"&\"",
  "\"\\\"\"",
  "IDENT",
  "\"'\"",
  "\"if\"",
  "\"end\"",
  "\"else\"",
  "\"&&\"",
  "\"||\"",
  "\".\"",
  "\"==\"",
  "\"=\"",
  "\"defn\"",
  "\"(\"",
  "\")\"",
  "\"def\"",
  "\"->\"",
  "\"->(\"",
  "\"[\"",
  "\"]\"",
  "\",\"",
  "NUMBER",
  "$start",
  "program",
  "line",
  "endline",
  "expr",
  "method_call",
  "atom",
  "splat",
  "parens" ]

Racc_debug_parser = false

##### State transition tables end #####

# reduce 0 omitted

module_eval(<<'.,.,', 'grammar.y', 10)
  def _reduce_1(val, _values)
     [:program, val[0][1] + val[1][1]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 11)
  def _reduce_2(val, _values)
     val[1] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 12)
  def _reduce_3(val, _values)
     [:program, [val[0]]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 13)
  def _reduce_4(val, _values)
     val[0] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 15)
  def _reduce_5(val, _values)
     val[0] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 16)
  def _reduce_6(val, _values)
     val[0] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 17)
  def _reduce_7(val, _values)
     val[0] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 18)
  def _reduce_8(val, _values)
     [:background, val[0]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 19)
  def _reduce_9(val, _values)
     [:embedded_string, val[1]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 20)
  def _reduce_10(val, _values)
     [:string, val[1]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 21)
  def _reduce_11(val, _values)
     [:if, val[1], val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 22)
  def _reduce_12(val, _values)
     [:if, val[1], val[2], val[4]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 23)
  def _reduce_13(val, _values)
     [:and, val[0], val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 24)
  def _reduce_14(val, _values)
     [:pipe, val[0], val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 25)
  def _reduce_15(val, _values)
     [:or, val[0], val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 26)
  def _reduce_16(val, _values)
     [:method, val[0], val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 27)
  def _reduce_17(val, _values)
     [:==, val[0], val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 28)
  def _reduce_18(val, _values)
     [:assignment, val[0], val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 29)
  def _reduce_19(val, _values)
     [:assignment, val[1], [:lambda, val[3], val[5]]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 30)
  def _reduce_20(val, _values)
     [:assignment, val[1], [:lambda, nil, val[2]]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 31)
  def _reduce_21(val, _values)
     [:lambda, nil, val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 32)
  def _reduce_22(val, _values)
     [:lambda, val[1], val[4]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 33)
  def _reduce_23(val, _values)
     [:args, [:lambda, val[1], val[4]], val[7]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 35)
  def _reduce_24(val, _values)
     [:newline, []] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 36)
  def _reduce_25(val, _values)
     [:newline, []] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 38)
  def _reduce_26(val, _values)
     [:add, val[0], val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 39)
  def _reduce_27(val, _values)
     [:subtract, val[0], val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 40)
  def _reduce_28(val, _values)
     [:multiply, val[0], val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 41)
  def _reduce_29(val, _values)
     [:divide, val[0], val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 42)
  def _reduce_30(val, _values)
     [:mod, val[0], val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 43)
  def _reduce_31(val, _values)
     [:array, val[1]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 44)
  def _reduce_32(val, _values)
     [:empty_array] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 45)
  def _reduce_33(val, _values)
     val[0] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 47)
  def _reduce_34(val, _values)
     val[0] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 48)
  def _reduce_35(val, _values)
     val[0] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 49)
  def _reduce_36(val, _values)
     [:args, val[0], val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 50)
  def _reduce_37(val, _values)
     [:yield, val[0], val[2]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 52)
  def _reduce_38(val, _values)
     [:splat, [val[0]]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 53)
  def _reduce_39(val, _values)
     [:splat, [val[0], *val[2][1]]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 55)
  def _reduce_40(val, _values)
     val[1] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 56)
  def _reduce_41(val, _values)
     [:empty_parens] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 57)
  def _reduce_42(val, _values)
     val[0] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 59)
  def _reduce_43(val, _values)
     [:number, val[0]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 60)
  def _reduce_44(val, _values)
     [:value, val[0]] 
  end
.,.,

module_eval(<<'.,.,', 'grammar.y', 61)
  def _reduce_45(val, _values)
     [:value, val[0], *val[1][1]] 
  end
.,.,

def _reduce_none(val, _values)
  val[0]
end

end   # class LucashGrammar


