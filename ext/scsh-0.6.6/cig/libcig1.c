/* Generic routines for Scheme48/C interfacing -- mostly for converting
** strings and null-terminated vectors back and forth.
** Copyright (c) 1993 by Olin Shivers.
*/

#include "libcig.h"
#include <string.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#define Malloc(type,n)	((type *) malloc(sizeof(type)*(n)))
#define Free(p)		(free((char *)(p)))

/* (c2scheme_strcpy dest_scheme_string source_C_string)
** Copies C string's chars into Scheme string. Return #t.
** If C string is NULL, do nothing and return #f.
*/

int c2scheme_strcpy(s48_value sstr, const char *cstr)
{
    if( cstr ) {
	strncpy( (char*) StobData(sstr), cstr, S48_STRING_LENGTH(sstr) );
	return 1;
	}
    else return 0;
    }


/* Same as above, but free the C string when we are done. */
int c2scheme_strcpy_free(s48_value sstr, const char *cstr)
{
    if( cstr ) {
	strncpy( (char*) StobData(sstr), cstr, S48_STRING_LENGTH(sstr) );
	Free(cstr);
	return 1;
	}
    else return 0;
    }

char *scheme2c_strcpy(s48_value sstr)
{
    char *result;
    int slen;

    slen = S48_STRING_LENGTH(sstr);
    result = Malloc(char, slen+1);

    if( result == NULL ) {
        fprintf(stderr,
		"Fatal error: C stub tried to copy Scheme string,\n"
		"but malloc failed on arg 0x%x, errno %d.\n",
		sstr, errno);
	exit(-1);
	}

    memcpy(result, cig_string_body(sstr), slen);
    result[slen] = '\000';
    return result;
    }


/*  One arg, a zero-terminated C word vec. Returns length.
**  The terminating null is not counted. Returns #f on NULL.
*/

s48_value c_veclen(const long *vec)
{
    const long *vptr = vec;
    if( !vptr ) return S48_FALSE;
    while( *vptr ) vptr++;
    return s48_enter_fixnum(vptr - vec);
    }
    

/* Copy string from into string to. If to is NULL, malloc a fresh string
** (if the malloc loses, return NULL).
** If from is NULL, then
** - if to is NULL, do nothing and return NULL.
** - Otherwise, deposit a single nul byte.
** Under normal conditions, this routine returns the destination string.
**
** The little boundary cases of this procedure are a study in obfuscation
** because C doesn't have a reasonable string data type. Give me a break.
*/
char *copystring(char *to, const char *from)
{
    if( from ) {
	int slen = strlen(from)+1;
	if( !to && !(to = Malloc(char, slen)) ) return NULL;
	else return memcpy(to, from, slen);
	}

    else
	return to ? *to = '\000', to : NULL;
    }

/* As in copystring, but if malloc loses, print out an error msg and croak. */
char *copystring_or_die(const char *str ) /* Note: NULL -> NULL. */
{
    if( str ) {
	int len = strlen(str)+1;
	char *new_str = Malloc(char, len);
	if( ! new_str ) {
	    fprintf(stderr, "copystring: Malloc failed.\n");
	    exit(-1);
	    }
	return memcpy(new_str, str, len);
	}
    else return NULL;
    }

int cstring_nullp( const char *s ) { return ! s; }

s48_value strlen_or_false(const char *s)
{ return s ?  s48_enter_fixnum(strlen(s)) : S48_FALSE; }



/* svec is a Scheme vector of C string carriers. Scan over the C strings
** in cvec, and initialise the corresponding string carriers in svec.
*/
void set_strvec_carriers(s48_value svec, char const * const * cvec)
{
    int svec_len = S48_VECTOR_LENGTH(svec);
    char const * const * cv = cvec;
    int i = 0;
       
    /* JMG: now using normal array access, instead of pointer++ on a s48_value */
    for(; svec_len > 0; i++, cv++, svec_len-- ) {
      s48_value carrier, alien;
      int strl;
   
	/* *sv is a (cons (make-alien <c-string>) <string-length>). */
	carrier = S48_VECTOR_REF(svec,i);
	alien = S48_CAR(carrier);
	strl = strlen(*cv);
	S48_SET_CDR(carrier, s48_enter_fixnum(strl));
	SetAlienVal(alien, (long) *cv);
    }
}

/* Helper function for arg checking. Why bother, actually? */
void cig_check_nargs(int arity, int nargs, const char *fn)
{
   if( arity != nargs ) {
       fprintf(stderr,
	       "Cig fatal error (%s) -- C stub expected %d arg%s, "
	       "but got %d.\n",
	       fn, arity, (arity == 1) ? "" : "s", nargs);
       exit(-1);
       }
   }
/* void ciginit(){ */
/*   S48_EXPORT_FUNCTION (df_strlen_or_false); */
/*   S48_EXPORT_FUNCTION (df_c_veclen); */
/*   S48_EXPORT_FUNCTION (df_set_strvec_carriers); */
/*   S48_EXPORT_FUNCTION (df_c2scheme_strcpy_free); */
/*   S48_EXPORT_FUNCTION (df_cstring_nullp); */
/*   S48_EXPORT_FUNCTION (df_free); */
/*   S48_EXPORT_FUNCTION (df_c2scheme_strcpy); */
/* } */
