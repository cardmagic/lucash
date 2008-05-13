#include "scheme48.h"

/* StobData is used by fdports.c. It should be changed over to STOB_REF
** by removing the extra indirection. */
#define StobData(x) (S48_ADDRESS_AFTER_HEADER(x, s48_value))

#define IsChar(x) ((((long) x) & 0xff) == S48_CHAR) 
/* JMG: untested !! */

#define StrByte(x, i)  ((i) + S48_ADDRESS_AFTER_HEADER((x), char))
#define cig_string_body(x) (S48_ADDRESS_AFTER_HEADER((x), char))

#define AlienVal(x) (S48_STOB_REF((x),0))
/* JMG: no () around this, because it's a do..while(0) */
#define SetAlienVal(x, v) S48_STOB_SET((x), 0, (v))

/*  JMG: some hacks to leave to old sources untouched */
#define ENTER_BOOLEAN(x) (x ? S48_TRUE : S48_FALSE)
#define EXTRACT_BOOLEAN(x) ((x==S48_TRUE) ? 1 : 0)
/* #define ENTER_FIXNUM(x) (s48_enter_fixnum(x)) */
/* #define SCHFALSE S48_FALSE */

extern char *scheme2c_strcpy(s48_value sstr);

extern s48_value strlen_or_false(const char *s);

extern char *copystring_or_die(const char *);
extern char *copystring(char *, const char *);

extern s48_value strlen_or_false(const char *);

extern void cig_check_nargs(int arity, int nargs, const char *fn);
