/* C++ code produced by gperf version 3.0.3 */
/* Command-line: gperf -L C++ -t xmltags.gperf  */
/* Computed positions: -k'1,$' */

#if !((' ' == 32) && ('!' == 33) && ('"' == 34) && ('#' == 35) \
      && ('%' == 37) && ('&' == 38) && ('\'' == 39) && ('(' == 40) \
      && (')' == 41) && ('*' == 42) && ('+' == 43) && (',' == 44) \
      && ('-' == 45) && ('.' == 46) && ('/' == 47) && ('0' == 48) \
      && ('1' == 49) && ('2' == 50) && ('3' == 51) && ('4' == 52) \
      && ('5' == 53) && ('6' == 54) && ('7' == 55) && ('8' == 56) \
      && ('9' == 57) && (':' == 58) && (';' == 59) && ('<' == 60) \
      && ('=' == 61) && ('>' == 62) && ('?' == 63) && ('A' == 65) \
      && ('B' == 66) && ('C' == 67) && ('D' == 68) && ('E' == 69) \
      && ('F' == 70) && ('G' == 71) && ('H' == 72) && ('I' == 73) \
      && ('J' == 74) && ('K' == 75) && ('L' == 76) && ('M' == 77) \
      && ('N' == 78) && ('O' == 79) && ('P' == 80) && ('Q' == 81) \
      && ('R' == 82) && ('S' == 83) && ('T' == 84) && ('U' == 85) \
      && ('V' == 86) && ('W' == 87) && ('X' == 88) && ('Y' == 89) \
      && ('Z' == 90) && ('[' == 91) && ('\\' == 92) && (']' == 93) \
      && ('^' == 94) && ('_' == 95) && ('a' == 97) && ('b' == 98) \
      && ('c' == 99) && ('d' == 100) && ('e' == 101) && ('f' == 102) \
      && ('g' == 103) && ('h' == 104) && ('i' == 105) && ('j' == 106) \
      && ('k' == 107) && ('l' == 108) && ('m' == 109) && ('n' == 110) \
      && ('o' == 111) && ('p' == 112) && ('q' == 113) && ('r' == 114) \
      && ('s' == 115) && ('t' == 116) && ('u' == 117) && ('v' == 118) \
      && ('w' == 119) && ('x' == 120) && ('y' == 121) && ('z' == 122) \
      && ('{' == 123) && ('|' == 124) && ('}' == 125) && ('~' == 126))
/* The character set is not based on ISO-646.  */
#error "gperf generated tables don't work with this execution character set. Please report a bug to <bug-gnu-gperf@gnu.org>."
#endif

#line 23 "xmltags.gperf"

#include <string.h>
#include "xmlparse.h"
#line 28 "xmltags.gperf"
struct XMLTagHashPair;

#define TOTAL_KEYWORDS 65
#define MIN_WORD_LENGTH 1
#define MAX_WORD_LENGTH 17
#define MIN_HASH_VALUE 1
#define MAX_HASH_VALUE 113
/* maximum key range = 113, duplicates = 0 */

class Perfect_Hash
{
private:
  static inline unsigned int hash (const char *str, unsigned int len);
public:
  static struct XMLTagHashPair *in_word_set (const char *str, unsigned int len);
};

inline unsigned int
Perfect_Hash::hash (register const char *str, register unsigned int len)
{
  static unsigned char asso_values[] =
    {
      114, 114, 114, 114, 114, 114, 114, 114, 114, 114,
      114, 114, 114, 114, 114, 114, 114, 114, 114, 114,
      114, 114, 114, 114, 114, 114, 114, 114, 114, 114,
      114, 114, 114, 114, 114, 114, 114, 114, 114, 114,
      114, 114, 114, 114, 114, 114, 114, 114, 114, 114,
      114, 114, 114, 114, 114, 114, 114, 114, 114, 114,
      114, 114, 114, 114, 114, 114, 114, 114, 114, 114,
      114, 114, 114, 114, 114, 114, 114, 114, 114, 114,
      114, 114, 114, 114, 114, 114, 114, 114, 114, 114,
      114, 114, 114, 114, 114, 114, 114,  60,   0,  20,
       35,  10,  25,  10,  10,   0, 114,   0,  35,  25,
       40,   5,   0, 114,  45,  15,   0, 114, 114,  60,
        0,  30, 114, 114, 114, 114, 114, 114, 114, 114,
      114, 114, 114, 114, 114, 114, 114, 114, 114, 114,
      114, 114, 114, 114, 114, 114, 114, 114, 114, 114,
      114, 114, 114, 114, 114, 114, 114, 114, 114, 114,
      114, 114, 114, 114, 114, 114, 114, 114, 114, 114,
      114, 114, 114, 114, 114, 114, 114, 114, 114, 114,
      114, 114, 114, 114, 114, 114, 114, 114, 114, 114,
      114, 114, 114, 114, 114, 114, 114, 114, 114, 114,
      114, 114, 114, 114, 114, 114, 114, 114, 114, 114,
      114, 114, 114, 114, 114, 114, 114, 114, 114, 114,
      114, 114, 114, 114, 114, 114, 114, 114, 114, 114,
      114, 114, 114, 114, 114, 114, 114, 114, 114, 114,
      114, 114, 114, 114, 114, 114, 114, 114, 114, 114,
      114, 114, 114, 114, 114, 114
    };
  return len + asso_values[(unsigned char)str[len - 1]] + asso_values[(unsigned char)str[0]];
}

struct XMLTagHashPair *
Perfect_Hash::in_word_set (register const char *str, register unsigned int len)
{
  static struct XMLTagHashPair wordlist[] =
    {
      {""},
#line 36 "xmltags.gperf"
      {"t", TAG_t},
      {""}, {""},
#line 48 "xmltags.gperf"
      {"text", TAG_text},
#line 74 "xmltags.gperf"
      {"break", TAG_break},
      {""},
#line 93 "xmltags.gperf"
      {"postpop", TAG_postpop},
#line 66 "xmltags.gperf"
      {"init_act", TAG_init_act},
      {""},
#line 35 "xmltags.gperf"
      {"trans_list", TAG_trans_list},
      {""},
#line 81 "xmltags.gperf"
      {"ex", TAG_ex},
#line 70 "xmltags.gperf"
      {"init_tokstart", TAG_init_tokstart},
#line 32 "xmltags.gperf"
      {"host", TAG_host},
#line 94 "xmltags.gperf"
      {"eof_t", TAG_eof_t},
      {""},
#line 92 "xmltags.gperf"
      {"prepush", TAG_prepush},
      {""},
#line 49 "xmltags.gperf"
      {"goto", TAG_goto},
#line 61 "xmltags.gperf"
      {"targs", TAG_targs},
      {""},
#line 67 "xmltags.gperf"
      {"set_act", TAG_set_act},
      {""}, {""},
#line 33 "xmltags.gperf"
      {"state_list", TAG_state_list},
      {""},
#line 71 "xmltags.gperf"
      {"set_tokstart", TAG_set_tokstart},
      {""},
#line 78 "xmltags.gperf"
      {"cond_list", TAG_cond_list},
#line 34 "xmltags.gperf"
      {"state", TAG_state},
#line 39 "xmltags.gperf"
      {"error_state", TAG_error_state},
#line 80 "xmltags.gperf"
      {"exports", TAG_exports},
      {""},
#line 59 "xmltags.gperf"
      {"exec", TAG_exec},
#line 76 "xmltags.gperf"
      {"cond_space_list", TAG_cond_space_list},
#line 38 "xmltags.gperf"
      {"start_state", TAG_start_state},
#line 47 "xmltags.gperf"
      {"entry_points", TAG_entry_points},
      {""},
#line 60 "xmltags.gperf"
      {"curs", TAG_curs},
#line 77 "xmltags.gperf"
      {"cond_space", TAG_cond_space},
#line 79 "xmltags.gperf"
      {"c", TAG_c},
#line 37 "xmltags.gperf"
      {"machine", TAG_machine},
#line 46 "xmltags.gperf"
      {"state_actions", TAG_state_actions},
#line 51 "xmltags.gperf"
      {"next", TAG_next},
#line 62 "xmltags.gperf"
      {"entry", TAG_entry},
#line 45 "xmltags.gperf"
      {"getkey", TAG_getkey},
      {""},
#line 55 "xmltags.gperf"
      {"ret", TAG_ret},
#line 58 "xmltags.gperf"
      {"hold", TAG_hold},
#line 56 "xmltags.gperf"
      {"pchar", TAG_pchar},
#line 82 "xmltags.gperf"
      {"p_expr", TAG_p_expr},
#line 83 "xmltags.gperf"
      {"pe_expr", TAG_pe_expr},
#line 86 "xmltags.gperf"
      {"top_expr", TAG_top_expr},
#line 64 "xmltags.gperf"
      {"lm_switch", TAG_lm_switch},
#line 68 "xmltags.gperf"
      {"get_tokend", TAG_get_tokend},
#line 90 "xmltags.gperf"
      {"tokend_expr", TAG_tokend_expr},
      {""},
#line 89 "xmltags.gperf"
      {"tokstart_expr", TAG_tokstart_expr},
#line 50 "xmltags.gperf"
      {"call", TAG_call},
#line 69 "xmltags.gperf"
      {"set_tokend", TAG_set_tokend},
      {""}, {""},
#line 84 "xmltags.gperf"
      {"eof_expr", TAG_eof_expr},
#line 52 "xmltags.gperf"
      {"goto_expr", TAG_goto_expr},
#line 65 "xmltags.gperf"
      {"sub_action", TAG_sub_action},
      {""}, {""}, {""},
#line 57 "xmltags.gperf"
      {"char", TAG_char},
#line 87 "xmltags.gperf"
      {"stack_expr", TAG_stack_expr},
#line 40 "xmltags.gperf"
      {"action_list", TAG_action_list},
#line 85 "xmltags.gperf"
      {"cs_expr", TAG_cs_expr},
#line 75 "xmltags.gperf"
      {"arg", TAG_arg},
#line 53 "xmltags.gperf"
      {"call_expr", TAG_call_expr},
#line 72 "xmltags.gperf"
      {"write", TAG_write},
      {""},
#line 42 "xmltags.gperf"
      {"action_table_list", TAG_action_table_list},
#line 44 "xmltags.gperf"
      {"alphtype", TAG_alphtype},
#line 31 "xmltags.gperf"
      {"ragel_def", TAG_ragel_def},
      {""},
#line 73 "xmltags.gperf"
      {"access", TAG_access},
#line 43 "xmltags.gperf"
      {"action_table", TAG_action_table},
      {""}, {""},
#line 30 "xmltags.gperf"
      {"ragel", TAG_ragel},
      {""}, {""}, {""},
#line 91 "xmltags.gperf"
      {"data_expr", TAG_data_expr},
      {""}, {""}, {""}, {""},
#line 54 "xmltags.gperf"
      {"next_expr", TAG_next_expr},
      {""}, {""}, {""}, {""},
#line 63 "xmltags.gperf"
      {"data", TAG_data},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 41 "xmltags.gperf"
      {"action", TAG_action},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 88 "xmltags.gperf"
      {"act_expr", TAG_act_expr}
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register int key = hash (str, len);

      if (key <= MAX_HASH_VALUE && key >= 0)
        {
          register const char *s = wordlist[key].name;

          if (*str == *s && !strncmp (str + 1, s + 1, len - 1) && s[len] == '\0')
            return &wordlist[key];
        }
    }
  return 0;
}
