/* Copyright (c) 1993-1999 by Richard Kelsey and Jonathan Rees.
   See file COPYING. */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include "scheme48vm.h"
#include "scheme48heap.h"

extern void	s48_sysdep_init(void);
extern void	s48_initialize_external_modules(void);

/* JMG: s48_object_file is obsolete according to s48 manual */
char *s48_object_file;   /* specified via a command line argument */

char *s48_reloc_file;    /* dynamic loading will set this */

char *prog_name;

void *heap, *stack;

int s48_main (long heap_size, long stack_size,
	      char *image_name, int argc, char** argv)
{
  int ret = internal_s48_main(heap_size, stack_size, "libscsh", "libscsh", 
			      image_name, argc, argv);
  free(heap);
  free(stack);
  return ret;
}
    
int
internal_s48_main(long heap_size, long stack_size, char * _prog_name, 
		  char* object_file, char *image_name, int argc, char** argv)
{
  long return_value;
  long required_heap_size;
  int warn_undefined_imported_bindings_p = 0;

#if defined(STATIC_AREAS)
  extern long static_entry;
  extern long static_symbol_table;
  extern long static_imported_binding_table, static_exported_binding_table;
  extern long p_count, *p_areas[], p_sizes[];
  extern long i_count, *i_areas[], i_sizes[];
#endif

  prog_name = _prog_name;

  s48_object_file = object_file;
  s48_reloc_file = NULL;


  s48_sysdep_init();
  s48_heap_init();
  s48_init();

  if (image_name == NULL)
    required_heap_size = 0;
  else {
    /* check_image_header returns number of bytes; required_heap_size
       is number of cells. */
    required_heap_size =
      s48_check_image_header((unsigned char *)image_name) >> 2;
    if (-1 == required_heap_size) {
      fprintf(stderr, "Image file \"%s\" is unusable.\n", image_name);
      return 1; }
  }

  /* two semi-spaces, plus we want some room to maneuver */
  if (heap_size < 4 * required_heap_size) {
    fprintf(stderr, "heap size %ld is too small, using %ld\n",
	    heap_size, 4 * required_heap_size);
    heap_size = 4 * required_heap_size; }

  heap = (void *) malloc(heap_size * sizeof(long));
  stack = (void *) malloc(stack_size * sizeof(long));
    
  if (!heap || !stack) {
    fprintf(stderr, "system is out of memory\n");
    return 1; }

  s48_initialize_heap((long)heap, heap_size);

#if defined(STATIC_AREAS)
  if (image_name == NULL) {
    s48_register_static_areas(p_count, p_areas, p_sizes,
			      i_count, i_areas, i_sizes);
    s48_set_image_valuesB(static_entry,
			  static_symbol_table,
			  static_imported_binding_table,
			  static_exported_binding_table);
  } else if (s48_read_image() == -1) {
    fprintf(stderr, "Image file \"%s\" is unusable.\n", image_name);
    return 1; }
#else
  if (s48_read_image() == -1) {
    fprintf(stderr, "Image file \"%s\" is unusable.\n", image_name);
    return 1; }
#endif

  s48_initialize_vm(stack, stack_size);

  s48_initialize_external_modules();
  
  if (warn_undefined_imported_bindings_p)
    s48_warn_about_undefined_imported_bindings();
  
  return_value = s48_call_startup_procedure(argv, argc);
  
  if (s48_reloc_file != NULL)
    if (0 != unlink(s48_reloc_file))
      fprintf(stderr, "unable to delete file %s\n", s48_reloc_file);

  return(return_value);
}

