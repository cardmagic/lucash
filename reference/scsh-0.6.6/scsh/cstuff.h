#include "libcig.h"
#define Alloc(type) 	((type *) malloc(sizeof(type)))
#define Malloc(type,n)	((type *) malloc(sizeof(type)*(n)))
#define Free(p)		(free((char *)(p)))
#define Realloc(type,p,n) ((type *) realloc(p, (n)*sizeof(type)))

/* String equality predicate. */
#define streq(a,b) (!strcmp((a),(b)))

s48_value char_pp_2_string_list(char **);
