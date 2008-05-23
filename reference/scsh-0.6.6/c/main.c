/* Copyright (c) 1993-1999 by Richard Kelsey and Jonathan Rees.
   See file COPYING. */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

/* I bumped this up from 1.5 Mcell because the debugging info put us over
** the top. -Olin
*/
#if !defined(DEFAULT_HEAP_SIZE)
/* 5 megacell = 20 megabytes (10 meg per semispace) */
#define DEFAULT_HEAP_SIZE 5000000L
#endif

#if !defined(DEFAULT_STACK_SIZE)
/* 2500 cells = 10000 bytes */
#define DEFAULT_STACK_SIZE 2500L
#endif

#if defined(STATIC_AREAS)
#define DEFAULT_IMAGE_NAME NULL
#else

/* DEFAULT_IMAGE_NAME should be defined using the -D switch to cc. */
#if !defined(DEFAULT_IMAGE_NAME)
#define DEFAULT_IMAGE_NAME "scheme48.image"
#endif

#endif /* STATIC_AREAS */



char ** process_args(char **argv,
                     char* prog_name,
		     long *heap_size,
		     long *stack_size,
		     char **object_file,
		     char **image_name);

extern int 
internal_s48_main(long heap_size, long stack_size, 
		  char* prog_name, char* object_file, char* image_name, 
		  int argc, char** argv);

int
main(argc, argv)
     int argc; char **argv;
{
  char **argp; /* JMG */
  char *image_name = DEFAULT_IMAGE_NAME;
  long heap_size = DEFAULT_HEAP_SIZE;    /* in numbers of cells */
  long stack_size = DEFAULT_STACK_SIZE;  /* in numbers of cells */
  char *object_file = NULL;   /* specified via a command line argument */
  char *prog_name;

#if defined(STATIC_AREAS)
  extern long static_entry;
  extern long static_symbol_table;
  extern long static_imported_binding_table, static_exported_binding_table;
  extern long p_count, *p_areas[], p_sizes[];
  extern long i_count, *i_areas[], i_sizes[];
#endif

  long vm_argc = 0;
  char *me = *argv;		/* Save program name. */
  prog_name = *argv++;

  argv=process_args(argv, prog_name, 		
		    &heap_size, &stack_size,
		    &object_file, &image_name);
  for(argc=0, argp=argv; *argp; argc++, argp++); /* Recompute argc. */
  return internal_s48_main(heap_size, stack_size, prog_name, object_file, image_name, argc, argv);
}
