/* Copyright (c) 1993-1999 by Richard Kelsey and Jonathan Rees.
   See file COPYING. */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <setjmp.h>
#include <stdarg.h>

#include "c-mods.h"
#include "scheme48.h"
#include "scheme48vm.h"

/*
 * The Joy of C
 * I don't understand why we need this, but we do.
 */

struct s_jmp_buf {
  jmp_buf buf;
};

/*
 * Longjump target set up by the most recent call into C.
 */
static struct s_jmp_buf	current_return_point;

/*
 * The name of the procedure we are currently executing; used for error messages.
 */
static s48_value current_procedure;

/*
 * Stack of Scheme stack-block records which represent portions of the process
 * stack.
 */
static s48_value current_stack_block = S48_FALSE;

/*
 * These need to agree with the record definition in callback.scm.
 */
#define STACK_BLOCK_FREE(stack_block)	S48_UNSAFE_RECORD_REF(stack_block, 0)
#define STACK_BLOCK_UNWIND(stack_block)	S48_UNSAFE_RECORD_REF(stack_block, 1)
#define STACK_BLOCK_PROC(stack_block)	S48_UNSAFE_RECORD_REF(stack_block, 2)
#define STACK_BLOCK_THREAD(stack_block)	S48_UNSAFE_RECORD_REF(stack_block, 3)
#define STACK_BLOCK_NEXT(stack_block)	S48_UNSAFE_RECORD_REF(stack_block, 4)

/*
 * For debugging.
 */
/*
static int callback_depth()
{
  int depth = 0;
  s48_value stack = current_stack_block;

  for(; stack != S48_FALSE; depth++, stack = STACK_BLOCK_NEXT(stack));

  return depth;
}
*/
/*
 * The value being returned from an external call.  The returns may be preceded
 * by a longjmp(), so we stash the value here.
 */
static s48_value external_return_value;

/* Exports to Scheme */
static s48_value	s48_clear_stack_top(void);
static s48_value	s48_trampoline(s48_value proc, s48_value nargs);

/* Imports from Scheme */
static s48_value 	the_record_type_binding = S48_FALSE;
static s48_value 	stack_block_type_binding = S48_FALSE;
static s48_value 	callback_binding = S48_FALSE;
static s48_value 	delay_callback_return_binding = S48_FALSE;
static s48_value 	bignum_to_long_binding = S48_FALSE;
static s48_value 	long_to_bignum_binding = S48_FALSE;

void
s48_initialize_external()
{
  S48_GC_PROTECT_GLOBAL(the_record_type_binding);
  the_record_type_binding = s48_get_imported_binding("s48-the-record-type");

  S48_GC_PROTECT_GLOBAL(stack_block_type_binding);
  stack_block_type_binding = s48_get_imported_binding("s48-stack-block-type");

  S48_GC_PROTECT_GLOBAL(callback_binding);
  callback_binding = s48_get_imported_binding("s48-callback");

  S48_GC_PROTECT_GLOBAL(delay_callback_return_binding);
  delay_callback_return_binding =
    s48_get_imported_binding("s48-delay-callback-return");

  S48_GC_PROTECT_GLOBAL(bignum_to_long_binding);
  bignum_to_long_binding = s48_get_imported_binding("s48-bignum-to-long");

  S48_GC_PROTECT_GLOBAL(long_to_bignum_binding);
  long_to_bignum_binding = s48_get_imported_binding("s48-long-to-bignum");

  S48_GC_PROTECT_GLOBAL(current_stack_block);
  S48_GC_PROTECT_GLOBAL(current_procedure);

  S48_EXPORT_FUNCTION(s48_clear_stack_top);
  S48_EXPORT_FUNCTION(s48_trampoline);

}

/* The three reasons for an extern-call longjump. */

#define NO_THROW        0
#define EXCEPTION_THROW 1
#define CLEANUP_THROW   2

/*
 * Used to call `proc' from Scheme code. `nargs' the number of arguments in
 * vector `argv'.  If `spread_p' is true the procedure is applied to the
 * arguments, otherwise `proc' is just called on `nargs' and `argv'.
 *
 * We do a setjmp() to get a return point for clearing off this portion of
 * the process stack.  This is used when `proc' calls back to Scheme and
 * then a throw transfers control up past the call to `proc'.
 */

s48_value
s48_external_call(s48_value sch_proc, s48_value proc_name,
		  long nargs, char *char_argv)
{
  volatile char *gc_marker;		/* volatile to survive longjumps */
  char *gc_marker_temp;			/* C wants it so */
  volatile char *gc_roots_marker;	/* volatile to survive longjumps */
  volatile s48_value name = proc_name;	/* volatile to survive longjumps */
  
  /* int depth = callback_depth(); */	/* debugging */

  long *argv = (long *) char_argv;

  s48_value (*proc)() = (s48_value (*)())
                         *S48_EXTRACT_VALUE_POINTER(sch_proc, long);

  int throw_reason;

  current_procedure = name;

  S48_CHECK_VALUE(sch_proc);
  S48_CHECK_STRING(name);

  gc_roots_marker = s48_set_gc_roots_baseB(&gc_marker_temp);
  gc_marker = gc_marker_temp;

  /* fprintf(stderr, "[external_call at depth %d]\n", depth); */

  throw_reason = setjmp(current_return_point.buf);

  if (throw_reason == NO_THROW) {	/* initial entry */
    switch (nargs) {
    case 0:
      external_return_value = proc();
      break;
    case 1:
      external_return_value = proc(argv[0]);
      break;
    case 2:
      external_return_value = proc(argv[1], argv[0]);
      break;
    case 3:
      external_return_value = proc(argv[2], argv[1], argv[0]);
      break;
    case 4:
      external_return_value = proc(argv[3], argv[2], argv[1], argv[0]);
      break;
    case 5:
      external_return_value = proc(argv[4],
				   argv[3], argv[2], argv[1], argv[0]);
      break;
    case 6:
      external_return_value = proc(argv[5], argv[4],
				   argv[3], argv[2], argv[1], argv[0]);
      break;
    case 7:
      external_return_value = proc(argv[6], argv[5], argv[4],
				   argv[3], argv[2], argv[1], argv[0]);
      break;
    case 8:
      external_return_value = proc(argv[7], argv[6], argv[5], argv[4],
				   argv[3], argv[2], argv[1], argv[0]);
      break;
    case 9:
      external_return_value = proc(argv[8],
				   argv[7], argv[6], argv[5], argv[4],
				   argv[3], argv[2], argv[1], argv[0]);
      break;
    case 10:
      external_return_value = proc(argv[9], argv[8],
				   argv[7], argv[6], argv[5], argv[4],
				   argv[3], argv[2], argv[1], argv[0]);
      break;
    case 11:
      external_return_value = proc(argv[10], argv[9], argv[8],
				   argv[7], argv[6], argv[5], argv[4],
				   argv[3], argv[2], argv[1], argv[0]);
      break;
    case 12:
      external_return_value = proc(argv[11], argv[10], argv[9], argv[8],
				   argv[7], argv[6], argv[5], argv[4],
				   argv[3], argv[2], argv[1], argv[0]);
      break;
    default:
      fprintf(stderr, "external-apply called with too many arguments");
      exit(-1); }

    /* Raise an exception if the user neglected to pop off some gc roots. */
    
    if (! s48_release_gc_roots_baseB((char *)gc_roots_marker, (char *)gc_marker)) {
      s48_raise_scheme_exception(S48_EXCEPTION_GC_PROTECTION_MISMATCH, 0);
    }
    
    /* Clear any free stack-blocks off of the top of the stack-block stack and
       then longjmp past the corresponding portions of the process stack. */
    
    if (current_stack_block != S48_FALSE &&
	STACK_BLOCK_FREE(current_stack_block) == S48_TRUE) {

      s48_value bottom_free_block;
      
      do {
	bottom_free_block = current_stack_block;
	current_stack_block = STACK_BLOCK_NEXT(current_stack_block);
      }
      while (current_stack_block != S48_FALSE &&
	     STACK_BLOCK_FREE(current_stack_block) == S48_TRUE);
      
      /* fprintf(stderr, "[Freeing stack blocks from %d to %d]\n",
	                 depth,
	                 callback_depth()); */

      longjmp(S48_EXTRACT_VALUE_POINTER(STACK_BLOCK_UNWIND(bottom_free_block),
					struct s_jmp_buf)->buf,
	      CLEANUP_THROW);
    }
  }
  else {	/* throwing an exception or uwinding the stack */
    /* fprintf(stderr, "[external_call throw; was %d and now %d]\n",
                       depth,
                      callback_depth());
     fprintf(stderr, "[throw unrolling to %ld]\n", gc_roots_marker); */
    s48_release_gc_roots_baseB((char *)gc_roots_marker, (char *)gc_marker);
  }

  /* Check to see if a thread is waiting to return to the next block down. */
     
  if (current_stack_block != S48_FALSE &&
      STACK_BLOCK_THREAD(current_stack_block) != S48_FALSE) {
    /* fprintf(stderr, "[releasing return at %d]\n", callback_depth()); */

    if (throw_reason == EXCEPTION_THROW) {
      /* We are in the midst of raising an exception, so we need to piggyback
	 our exception on that one. */
      s48_value old_exception
	= s48_resetup_external_exception(S48_EXCEPTION_CALLBACK_RETURN_UNCOVERED,
					 2);
      s48_push(old_exception);
      s48_push(current_stack_block);
      external_return_value = S48_UNSPECIFIC;
    }
    else {
      s48_setup_external_exception(S48_EXCEPTION_CALLBACK_RETURN_UNCOVERED, 2);
      s48_push(current_stack_block);
      s48_push(external_return_value);
      external_return_value = S48_UNSPECIFIC;
    }
  }
  
  return external_return_value;
}

/*
 * Call Scheme function `proc' from C.  We push the call-back depth, `proc',
 * and the arguments on the Scheme stack and then restart the VM.  The restarted
 * VM calls the Scheme procedure `callback' which wraps the call to `proc' with
 * a dynamic-wind.  This prevents downward throws back into the call to `proc',
 * which C can't handle, and allows the C stack to be cleaned up if an upward
 * throw occurs.
 *
 * The maximum number of arguments is determined by the amount of space reserved
 * on the Scheme stack for exceptions. See the definition of stack-slack in
 * scheme/vm/stack.scm.
 */
s48_value
s48_call_scheme(s48_value proc, long nargs, ...)
{
  int i;
  va_list arguments;
  s48_value value;
  s48_value unwind, stack_block;
  S48_DECLARE_GC_PROTECT(2);

  S48_GC_PROTECT_2(unwind, proc);
  
  va_start(arguments, nargs);

  S48_SHARED_BINDING_CHECK(callback_binding);

  /* It would be nice to push a list of the arguments, but we have no way
     of preserving them across a cons. */
  if (nargs < 0 || 12 < nargs) {  /* DO NOT INCREASE THIS NUMBER */
    s48_value sch_nargs = s48_enter_integer(nargs);  /* `proc' is protected */
    s48_raise_scheme_exception(S48_EXCEPTION_TOO_MANY_ARGUMENTS_IN_CALLBACK,
			       2, proc, sch_nargs);
  }

  /* fprintf(stderr, "[s48_call, %ld args, depth %d]\n",
	  nargs, callback_depth()); */

  s48_push(S48_UNSPECIFIC);	/* placeholder */
  s48_push(proc);
  for (i = 0; i < nargs; i++)
    s48_push(va_arg(arguments, s48_value));

  va_end(arguments);

  /* With everything safely on the stack we can do the necessary allocation. */

  unwind = S48_MAKE_VALUE(struct s_jmp_buf);
  S48_EXTRACT_VALUE(unwind, struct s_jmp_buf) = current_return_point;

  stack_block = s48_make_record(stack_block_type_binding);
  STACK_BLOCK_UNWIND(stack_block) = unwind;
  STACK_BLOCK_PROC(stack_block) = current_procedure;
  STACK_BLOCK_NEXT(stack_block) = current_stack_block;
  STACK_BLOCK_FREE(stack_block) = S48_FALSE;
  STACK_BLOCK_THREAD(stack_block) = S48_FALSE;

  S48_GC_UNPROTECT();		/* no more references to `unwind' or `proc'. */

  current_stack_block = stack_block;

  /* if(s48_stack_ref(nargs + 1) != S48_UNSPECIFIC)
     fprintf(stderr, "[stack_block set missed]\n"); */

  s48_stack_setB(nargs + 1, stack_block);

  /* fprintf(stderr, "[s48_call, %ld args, depth %d, off we go]\n",
	  nargs, callback_depth()); */

  value = s48_restart(S48_UNSAFE_SHARED_BINDING_REF(callback_binding),
		      nargs + 2);

  for (;s48_Scallback_return_stack_blockS != current_stack_block;) {
    if (s48_Scallback_return_stack_blockS == S48_FALSE) {

      /* fprintf(stderr, "[s48_call returning from VM %ld]\n", callback_depth()); */

      exit(value);
    }
    else {

      /* Someone has returned (because of threads) to the wrong section of the
	 C stack.  We call back to a Scheme procedure that will suspend until
	 out block is at the top of the stack. */

      s48_push(s48_Scallback_return_stack_blockS);
      s48_push(S48_UNSAFE_SHARED_BINDING_REF(delay_callback_return_binding));
      s48_push(s48_Scallback_return_stack_blockS);
      s48_push(value);

      /* fprintf(stderr, "[Premature return, %ld args, depth %d, back we go]\n",
	      nargs, callback_depth()); */

      s48_disable_interruptsB();
      value = s48_restart(S48_UNSAFE_SHARED_BINDING_REF(callback_binding), 4);
    }
  }

  /* Restore the state of the current stack block. */

  unwind = STACK_BLOCK_UNWIND(current_stack_block);
  current_return_point = S48_EXTRACT_VALUE(unwind, struct s_jmp_buf);
  current_procedure = STACK_BLOCK_PROC(current_stack_block);
  current_stack_block = STACK_BLOCK_NEXT(current_stack_block);

  /* fprintf(stderr, "[s48_call returns from depth %d]\n", callback_depth()); */

  return value;
}

/*
 * Because the top of the stack is cleared on the return from every external
 * call, this doesn't have to do anything but exist.
 */
static s48_value
s48_clear_stack_top()
{
  /* fprintf(stderr, "[Clearing stack top]\n"); */
  return S48_UNSPECIFIC;
}

/*
 * For testing callbacks.  This just calls its argument on the specified number
 * of values.
 */
static s48_value
s48_trampoline(s48_value proc, s48_value nargs)
{

  fprintf(stderr, "[C trampoline, %ld args]\n", S48_UNSAFE_EXTRACT_FIXNUM(nargs));

  switch (s48_extract_fixnum(nargs)) {
  case -2: {
    S48_DECLARE_GC_PROTECT(1);
    
    S48_GC_PROTECT_1(proc);

    return S48_FALSE;
  }
  case -1: {
    long n = - s48_extract_integer(proc);
    fprintf(stderr, "[extract magnitude is %ld (%lx)]\n", n, n);
    return s48_enter_integer(n);
  }
  case 0: {
    s48_value value = s48_call_scheme(proc, 0);
    if (value == S48_FALSE)
      s48_raise_string_os_error("trampoline bouncing");
    return value;
  }
  case 1:
    return s48_call_scheme(proc, 1, s48_enter_fixnum(100));
  case 2:
    return s48_call_scheme(proc, 2, s48_enter_fixnum(100), s48_enter_fixnum(200));
  case 3:
    return s48_call_scheme(proc, 3, s48_enter_fixnum(100), s48_enter_fixnum(200),
		    s48_enter_fixnum(300));
  default:
    s48_raise_range_error(nargs, s48_enter_fixnum(0), s48_enter_fixnum(3));
    return S48_UNDEFINED; /* not that we ever get here */
  }
}

/********************************/
/*
 * Raising exceptions.  We push the arguments on the stack end then throw out
 * of the most recent call from Scheme.
 *
 * The maximum number of arguments is determined by the amount of space reserved
 * on the Scheme stack for exceptions. See the definition of stack-slack in
 * scheme/vm/stack.scm.
 */
void
s48_raise_scheme_exception(long why, long nargs, ...)
{
  int i;
  va_list irritants;

  va_start(irritants, nargs);

  s48_setup_external_exception(why, nargs + 1);
  
  if (10 < nargs) {   /* DO NOT INCREASE THIS NUMBER */
    fprintf(stderr, "s48_raise_scheme_exception() called with more than 10 arguments, discarding surplus\n");
    nargs = 10;
  }
  
  s48_push(current_procedure);

  for (i = 0; i < nargs; i++)
    s48_push(va_arg(irritants, s48_value));

  va_end(irritants);

   external_return_value = S48_UNSPECIFIC;
   longjmp(current_return_point.buf, EXCEPTION_THROW);
}

/* Specific exceptions */

void
s48_raise_argument_type_error(s48_value value) {
  s48_raise_scheme_exception(S48_EXCEPTION_WRONG_TYPE_ARGUMENT, 1, value);
}

void
s48_raise_argument_number_error(s48_value value, s48_value min, s48_value max) {
  s48_raise_scheme_exception(S48_EXCEPTION_WRONG_NUMBER_OF_ARGUMENTS,
			     3, value, min, max);
}

void
s48_raise_range_error(s48_value value, s48_value min, s48_value max) {
  s48_raise_scheme_exception(S48_EXCEPTION_INDEX_OUT_OF_RANGE,
			     3, value, min, max);
}

void
s48_raise_closed_channel_error() {
  s48_raise_scheme_exception(S48_EXCEPTION_CLOSED_CHANNEL, 0);
}

void
s48_raise_os_error(int the_errno) {
  s48_raise_scheme_exception(S48_EXCEPTION_OS_ERROR, 2,
			     s48_enter_integer(the_errno),
			     s48_enter_string(strerror(the_errno)));
}

void
s48_raise_os_error_1(int the_errno, s48_value arg1) {

  s48_value sch_errno = S48_UNSPECIFIC;
  
  S48_DECLARE_GC_PROTECT(2);
  S48_GC_PROTECT_2 (arg1, sch_errno);
  
  sch_errno = s48_enter_integer(the_errno);

  s48_raise_scheme_exception(S48_EXCEPTION_OS_ERROR, 3,
			     sch_errno,
			     s48_enter_string(strerror(the_errno)),
			     arg1);

  S48_GC_UNPROTECT();
}

void
s48_raise_os_error_2(int the_errno, s48_value arg1, s48_value arg2) {

  s48_value sch_errno = S48_UNSPECIFIC;
  
  S48_DECLARE_GC_PROTECT(3);
  S48_GC_PROTECT_3 (arg1, arg2, sch_errno);
  
  sch_errno = s48_enter_integer(the_errno);

  s48_raise_scheme_exception(S48_EXCEPTION_OS_ERROR, 4,
                             sch_errno,
			     s48_enter_string(strerror(the_errno)),
			     arg1, arg2);
  S48_GC_UNPROTECT();
}

void
s48_raise_os_error_3(int the_errno, s48_value arg1, s48_value arg2, 
		     s48_value arg3) {

  s48_value sch_errno = S48_UNSPECIFIC;
  
  S48_DECLARE_GC_PROTECT(4);
  S48_GC_PROTECT_4 (arg1, arg2, arg3, sch_errno);
  
  sch_errno = s48_enter_integer(the_errno);
  s48_raise_scheme_exception(S48_EXCEPTION_OS_ERROR, 5,
                             sch_errno,
			     s48_enter_string(strerror(the_errno)),
			     arg1, arg2, arg3);
  S48_GC_UNPROTECT();
}

void
s48_raise_os_error_4(int the_errno, s48_value arg1, s48_value arg2, 
		     s48_value arg3, s48_value arg4) {

  s48_value sch_errno = S48_UNSPECIFIC;
  
  S48_DECLARE_GC_PROTECT(5);
  S48_GC_PROTECT_5 (arg1, arg2, arg3, arg4, sch_errno);
  
  sch_errno = s48_enter_integer(the_errno);
  s48_raise_scheme_exception(S48_EXCEPTION_OS_ERROR, 6,
                             sch_errno,
			     s48_enter_string(strerror(the_errno)),
			     arg1, arg2, arg3, arg4);
  S48_GC_UNPROTECT();
}

void
s48_raise_os_error_5(int the_errno, s48_value arg1, s48_value arg2, 
		     s48_value arg3, s48_value arg4, s48_value arg5) {

  s48_value sch_errno = S48_UNSPECIFIC;
  
  S48_DECLARE_GC_PROTECT(6);
  S48_GC_PROTECT_6 (arg1, arg2, arg3, arg4, arg5, sch_errno);
  
  sch_errno = s48_enter_integer(the_errno);
  s48_raise_scheme_exception(S48_EXCEPTION_OS_ERROR, 7,
                             sch_errno,
			     s48_enter_string(strerror(the_errno)),
			     arg1, arg2, arg3, arg4, arg5);
  S48_GC_UNPROTECT();
}

void
s48_raise_os_error_6(int the_errno, s48_value arg1, s48_value arg2, 
		     s48_value arg3, s48_value arg4, s48_value arg5,
		     s48_value arg6) {

  s48_value sch_errno = S48_UNSPECIFIC;
  
  S48_DECLARE_GC_PROTECT(7);
  S48_GC_PROTECT_7 (arg1, arg2, arg3, arg4, arg5, arg6, sch_errno);
  
  sch_errno = s48_enter_integer(the_errno);
  s48_raise_scheme_exception(S48_EXCEPTION_OS_ERROR, 8,
                             sch_errno,
			     s48_enter_string(strerror(the_errno)),
			     arg1, arg2, arg3, arg4, arg5, arg6);
  S48_GC_UNPROTECT();
}

void
s48_raise_os_error_7(int the_errno, s48_value arg1, s48_value arg2, 
		     s48_value arg3, s48_value arg4, s48_value arg5,
		     s48_value arg6, s48_value arg7) {

  s48_value sch_errno = S48_UNSPECIFIC;
  
  S48_DECLARE_GC_PROTECT(8);
  S48_GC_PROTECT_8 (arg1, arg2, arg3, arg4, arg5, arg6, arg7, sch_errno);
  
  sch_errno = s48_enter_integer(the_errno);
  s48_raise_scheme_exception(S48_EXCEPTION_OS_ERROR, 9,
                             sch_errno,
			     s48_enter_string(strerror(the_errno)),
			     arg1, arg2, arg3, arg4, arg5, arg6, arg7);
  S48_GC_UNPROTECT();
}

void
s48_raise_string_os_error(char *reason) {
  s48_raise_scheme_exception(S48_EXCEPTION_OS_ERROR, 1,
			     s48_enter_string(reason));
}

void
s48_raise_out_of_memory_error() {
  s48_raise_scheme_exception(S48_EXCEPTION_OUT_OF_MEMORY, 0);
}

/********************************/
/* Support routines for external code */

/*
 * Type-safe procedures for checking types and dereferencing and setting slots.
 */

int
s48_stob_has_type(s48_value thing, int type)
{
  return S48_STOB_P(thing) && (S48_STOB_TYPE(thing) == type);
}

long
s48_stob_length(s48_value thing, int type)
{
  if (!(S48_STOB_P(thing) && (S48_STOB_TYPE(thing) == type)))
    s48_raise_argument_type_error(thing);
  
  return S48_STOB_DESCRIPTOR_LENGTH(thing);
}

long
s48_stob_byte_length(s48_value thing, int type)
{
  if (!(S48_STOB_P(thing) && (S48_STOB_TYPE(thing) == type)))
    s48_raise_argument_type_error(thing);

  if (type == S48_STOBTYPE_STRING)
    return S48_STOB_BYTE_LENGTH(thing) - 1;
  else
    return S48_STOB_BYTE_LENGTH(thing);
}

s48_value
s48_stob_ref(s48_value thing, int type, long offset)
{
  long length;

  if (!(S48_STOB_P(thing) && (S48_STOB_TYPE(thing) == type)))
    s48_raise_argument_type_error(thing);

  length = S48_STOB_DESCRIPTOR_LENGTH(thing);

  if (offset < 0 || length <= offset)
    s48_raise_range_error(s48_enter_integer(offset),
			  S48_UNSAFE_ENTER_FIXNUM(0),
		          S48_UNSAFE_ENTER_FIXNUM(length - 1));
			  
  return S48_STOB_REF(thing, offset);
}

void
s48_stob_set(s48_value thing, int type, long offset, s48_value value)
{
  long length;
  
  if (!(S48_STOB_P(thing) &&
	(S48_STOB_TYPE(thing) == type) &&
	!S48_STOB_IMMUTABLEP(thing)))
    s48_raise_argument_type_error(thing);
  
  length = S48_STOB_DESCRIPTOR_LENGTH(thing);

  if (offset < 0 || length <= offset)
    s48_raise_range_error(s48_enter_integer(offset),
			  S48_UNSAFE_ENTER_FIXNUM(0),
			  S48_UNSAFE_ENTER_FIXNUM(length - 1));
			  
  S48_STOB_SET(thing, offset, value);
}

char
s48_stob_byte_ref(s48_value thing, int type, long offset)
{
  long length;

  if (!(S48_STOB_P(thing) && (S48_STOB_TYPE(thing) == type)))
    s48_raise_argument_type_error(thing);
  
  length = (type == S48_STOBTYPE_STRING) ?
           S48_STOB_BYTE_LENGTH(thing) - 1 :
           S48_STOB_BYTE_LENGTH(thing);
  
  if (offset < 0 || length <= offset)
    s48_raise_range_error(s48_enter_integer(offset),
			  S48_UNSAFE_ENTER_FIXNUM(0),
			  S48_UNSAFE_ENTER_FIXNUM(length - 1));
			  
  return S48_STOB_BYTE_REF(thing, offset);
}

void
s48_stob_byte_set(s48_value thing, int type, long offset, char value)
{
  long length;

  if (!(S48_STOB_P(thing) && (S48_STOB_TYPE(thing) == type)))
    s48_raise_argument_type_error(thing);
  
  length = (type == S48_STOBTYPE_STRING) ?
           S48_STOB_BYTE_LENGTH(thing) - 1 :
           S48_STOB_BYTE_LENGTH(thing);
  
  if (offset < 0 || length <= offset)
    s48_raise_range_error(s48_enter_integer(offset),
			  S48_UNSAFE_ENTER_FIXNUM(0),
			  S48_UNSAFE_ENTER_FIXNUM(length - 1));
			  
  S48_STOB_BYTE_SET(thing, offset, value);
}

void *
s48_value_pointer(s48_value value)
{
  S48_CHECK_VALUE(value);

  return S48_ADDRESS_AFTER_HEADER(value, void *);
}

/********************************/
/* Numbers, characters, and pointers. */

/*
 * These two functions have the same range as the unsafe macros, but they signal
 * an error if things go wrong, instead of silently producing garbage.  Unlike
 * the integer versions they cannot cause a GC.
 */

s48_value
s48_enter_fixnum(long value)
{
  if (value < S48_MIN_FIXNUM_VALUE || S48_MAX_FIXNUM_VALUE < value)
    s48_raise_argument_type_error(s48_enter_integer(value));

  return S48_UNSAFE_ENTER_FIXNUM(value);
}

long
s48_extract_fixnum(s48_value value)
{
  if (! S48_FIXNUM_P(value))
    s48_raise_argument_type_error(value);
  
  return S48_UNSAFE_EXTRACT_FIXNUM(value);
}

/*
 * If `value' fits in a fixnum we put it there.  Larger values are passed to the
 * Scheme procedure LONG-TO-BIGNUM as the sign and the two sixteen-bit halves of
 * the negative magnitude.  Using the negative magnitude avoids problems with
 * two's complement's asymmetry.
 */

s48_value
s48_enter_integer(long value)
{
  if (S48_MIN_FIXNUM_VALUE <= value && value <= S48_MAX_FIXNUM_VALUE)
    return S48_UNSAFE_ENTER_FIXNUM(value);
  else {
    S48_SHARED_BINDING_CHECK(long_to_bignum_binding);

    if (value < 0)
      return s48_call_scheme(S48_SHARED_BINDING_REF(long_to_bignum_binding),
			     3,
			     S48_FALSE,
			     S48_UNSAFE_ENTER_FIXNUM(value >> 16),
			     S48_UNSAFE_ENTER_FIXNUM(value & 0xFFFF));
    else
      return s48_call_scheme(S48_SHARED_BINDING_REF(long_to_bignum_binding),
			     3,
			     S48_TRUE,
			     S48_UNSAFE_ENTER_FIXNUM((- value) >> 16),
			     S48_UNSAFE_ENTER_FIXNUM((- value) & 0xFFFF));
  }
}

s48_value
s48_enter_unsigned_integer(unsigned long value)
{
  if (value <= S48_MAX_FIXNUM_VALUE)
    return S48_UNSAFE_ENTER_FIXNUM(value);
  else {
    S48_SHARED_BINDING_CHECK(long_to_bignum_binding);

    return s48_call_scheme(S48_SHARED_BINDING_REF(long_to_bignum_binding),
			   3,
			   S48_FALSE,   /* this is ok */
			   S48_UNSAFE_ENTER_FIXNUM(value >> 16),
			   S48_UNSAFE_ENTER_FIXNUM(value & 0xFFFF));
  }
}

/*
 * If we have a fixnum we just extract it.  Bignums require a call back into
 * Scheme 48.  (BIGNUM-TO-LONG n) returns a vector containing the sign and the
 * high and low sixteen-bit halves of N.  If N is not an integer we get #f back.
 * Again, we use negative numbers to stay out of trouble.
 */

long
s48_extract_integer(s48_value value)
{
  if (S48_FIXNUM_P(value))
    return S48_UNSAFE_EXTRACT_FIXNUM(value);

  else {
    s48_value stuff;
    S48_DECLARE_GC_PROTECT(1);

    S48_GC_PROTECT_1(value);

    S48_SHARED_BINDING_CHECK(bignum_to_long_binding);

    stuff = s48_call_scheme(S48_SHARED_BINDING_REF(bignum_to_long_binding),
			    1,
			    value);

    S48_GC_UNPROTECT();

    if (stuff == S48_FALSE)
      s48_raise_argument_type_error(value);

    /* The first VECTOR_REF does the type checking for the rest. */
    {
      long low  = S48_UNSAFE_EXTRACT_FIXNUM(S48_VECTOR_REF(stuff, 2));
      s48_value boxed_high = S48_UNSAFE_VECTOR_REF(stuff, 1);
      long high = S48_UNSAFE_EXTRACT_FIXNUM(boxed_high);
      int pos_p = S48_EXTRACT_BOOLEAN(S48_UNSAFE_VECTOR_REF(stuff, 0));
      
      if ((! S48_FIXNUM_P(boxed_high)) ||
	  high > (pos_p ? 0x7FFF : 0x8000))
	s48_raise_argument_type_error(value);
      
      {
      long magnitude = ((- high) << 16) - low;
      return pos_p ? - magnitude : magnitude;
      }
    }
  }
}

unsigned long
s48_extract_unsigned_integer(s48_value value)
{
  long temp;
  if (S48_FIXNUM_P(value)){
    temp = S48_UNSAFE_EXTRACT_FIXNUM(value);
    if (temp < 0)
      s48_raise_argument_type_error(value);
    else return (unsigned long) temp;
  }
  else {
    s48_value stuff;
    S48_DECLARE_GC_PROTECT(1);

    S48_GC_PROTECT_1(value);

    S48_SHARED_BINDING_CHECK(bignum_to_long_binding);

    stuff = s48_call_scheme(S48_SHARED_BINDING_REF(bignum_to_long_binding),
			    1,
			    value);

    S48_GC_UNPROTECT();

    if (stuff == S48_FALSE)
      s48_raise_argument_type_error(value);

    /* The first VECTOR_REF does the type checking for the rest. */
    {
      long low  = S48_UNSAFE_EXTRACT_FIXNUM(S48_VECTOR_REF(stuff, 2));
      s48_value boxed_high = S48_UNSAFE_VECTOR_REF(stuff, 1);
      long high = S48_UNSAFE_EXTRACT_FIXNUM(boxed_high);
      int pos_p = S48_EXTRACT_BOOLEAN(S48_UNSAFE_VECTOR_REF(stuff, 0));
      
      if ((!pos_p) || 
	  (! S48_FIXNUM_P(boxed_high)) ||
	  (high >  0xFFFF)) 
	s48_raise_argument_type_error(value);
      else return ((((unsigned long) high) << 16) + low);
    }
  }
}

/*
 * Doubles and characters are straightforward.
 */

s48_value
s48_enter_double(double value)
{
  s48_value obj;

  obj = s48_allocate_stob(S48_STOBTYPE_DOUBLE, sizeof(double));
  S48_UNSAFE_EXTRACT_DOUBLE(obj) = value;

  return obj;
}

double
s48_extract_double(s48_value s48_double)
{
  if (! S48_DOUBLE_P(s48_double))
    s48_raise_argument_type_error(s48_double);
  
  return S48_UNSAFE_EXTRACT_DOUBLE(s48_double);
}

s48_value
s48_enter_char(unsigned char a_char)
{
  if (a_char > 255)
    s48_raise_range_error(s48_enter_fixnum(a_char),
			  s48_enter_fixnum(0),
			  s48_enter_fixnum(255));

  return S48_UNSAFE_ENTER_CHAR(a_char);
}

unsigned char
s48_extract_char(s48_value a_char)
{
  if (! S48_CHAR_P(a_char))
    s48_raise_argument_type_error(a_char);
  
  return S48_UNSAFE_EXTRACT_CHAR(a_char);
}
  
/********************************/
/* Allocation */

s48_value
s48_enter_pointer(void *pointer)
{
  s48_value obj;

  obj = s48_allocate_stob(S48_STOBTYPE_BYTE_VECTOR, sizeof(void *));
  *(S48_ADDRESS_AFTER_HEADER(obj, void **)) = pointer;

  return obj;
}

s48_value
s48_cons(s48_value v1, s48_value v2)
{
  s48_value obj;
  S48_DECLARE_GC_PROTECT(2);

  S48_GC_PROTECT_2(v1, v2);

  obj = s48_allocate_stob(S48_STOBTYPE_PAIR, 2 * sizeof(s48_value));
  S48_UNSAFE_SET_CAR(obj, v1);
  S48_UNSAFE_SET_CDR(obj, v2);

  S48_GC_UNPROTECT();
  return obj;
}

s48_value
s48_list_1(s48_value v1)
{
  return (s48_cons (v1, S48_NULL));
}

s48_value
s48_list_2(s48_value v1, s48_value v2)
{
  s48_value list = S48_UNSPECIFIC;
  S48_DECLARE_GC_PROTECT(1);
  
  S48_GC_PROTECT_1(v1);
  list = s48_list_1 (v2);
  list = s48_cons (v1, list);
  S48_GC_UNPROTECT();
  return list;
}

s48_value
s48_list_3(s48_value v1, s48_value v2, s48_value v3)
{
  s48_value list = S48_UNSPECIFIC;
  S48_DECLARE_GC_PROTECT(1);
  
  S48_GC_PROTECT_1(v1);
  list = s48_list_2 (v2, v3);
  list = s48_cons (v1, list);
  S48_GC_UNPROTECT();
  return list;
}

s48_value
s48_list_4(s48_value v1, s48_value v2, s48_value v3, s48_value v4)
{
  s48_value list = S48_UNSPECIFIC;
  S48_DECLARE_GC_PROTECT(1);
  
  S48_GC_PROTECT_1(v1);
  list = s48_list_3 (v2, v3, v4);
  list = s48_cons (v1, list);
  S48_GC_UNPROTECT();
  return list;
}

s48_value
s48_list_5(s48_value v1, s48_value v2, s48_value v3, s48_value v4, s48_value v5)
{
  s48_value list = S48_UNSPECIFIC;
  S48_DECLARE_GC_PROTECT(1);
  
  S48_GC_PROTECT_1(v1);
  list = s48_list_4 (v2, v3, v4, v5);
  list = s48_cons (v1, list);
  S48_GC_UNPROTECT();
  return list;
}

s48_value
s48_list_6(s48_value v1, s48_value v2, s48_value v3, s48_value v4, s48_value v5,
	   s48_value v6)
{
  s48_value list = S48_UNSPECIFIC;
  S48_DECLARE_GC_PROTECT(1);
  
  S48_GC_PROTECT_1(v1);
  list = s48_list_5 (v2, v3, v4, v5, v6);
  list = s48_cons (v1, list);
  S48_GC_UNPROTECT();
  return list;
}

s48_value
s48_list_7(s48_value v1, s48_value v2, s48_value v3, s48_value v4, s48_value v5,
	   s48_value v6, s48_value v7)
{
  s48_value list = S48_UNSPECIFIC;
  S48_DECLARE_GC_PROTECT(1);
  
  S48_GC_PROTECT_1(v1);
  list = s48_list_6 (v2, v3, v4, v5, v6, v7);
  list = s48_cons (v1, list);
  S48_GC_UNPROTECT();
  return list;
}

s48_value
s48_list_8(s48_value v1, s48_value v2, s48_value v3, s48_value v4, s48_value v5,
	   s48_value v6, s48_value v7, s48_value v8)
{
  s48_value list = S48_UNSPECIFIC;
  S48_DECLARE_GC_PROTECT(1);
  
  S48_GC_PROTECT_1(v1);
  list = s48_list_7 (v2, v3, v4, v5, v6, v7, v8);
  list = s48_cons (v1, list);
  S48_GC_UNPROTECT();
  return list;
}

s48_value
s48_list_9(s48_value v1, s48_value v2, s48_value v3, s48_value v4, s48_value v5,
	   s48_value v6, s48_value v7, s48_value v8, s48_value v9)
{
  s48_value list = S48_UNSPECIFIC;
  S48_DECLARE_GC_PROTECT(1);
  
  S48_GC_PROTECT_1(v1);
  list = s48_list_8 (v2, v3, v4, v5, v6, v7, v8, v9);
  list = s48_cons (v1, list);
  S48_GC_UNPROTECT();
  return list;
}

s48_value
s48_list_10(s48_value v1, s48_value v2, s48_value v3, s48_value v4, s48_value v5,
	    s48_value v6, s48_value v7, s48_value v8, s48_value v9, s48_value v10)
{
  s48_value list = S48_UNSPECIFIC;
  S48_DECLARE_GC_PROTECT(1);
  
  S48_GC_PROTECT_1(v1);
  list = s48_list_9 (v2, v3, v4, v5, v6, v7, v8, v9, v10);
  list = s48_cons (v1, list);
  S48_GC_UNPROTECT();
  return list;
}

s48_value
s48_list_11(s48_value v1, s48_value v2, s48_value v3, s48_value v4, s48_value v5,
	    s48_value v6, s48_value v7, s48_value v8, s48_value v9, s48_value v10,
	    s48_value v11)
{
  s48_value list = S48_UNSPECIFIC;
  S48_DECLARE_GC_PROTECT(1);
  
  S48_GC_PROTECT_1(v1);
  list = s48_list_10 (v2, v3, v4, v5, v6, v7, v8, v9, v10, v11);
  list = s48_cons (v1, list);
  S48_GC_UNPROTECT();
  return list;
}

s48_value
s48_list_12(s48_value v1, s48_value v2, s48_value v3, s48_value v4, s48_value v5,
	    s48_value v6, s48_value v7, s48_value v8, s48_value v9, s48_value v10,
	    s48_value v11, s48_value v12)
{
  s48_value list = S48_UNSPECIFIC;
  S48_DECLARE_GC_PROTECT(1);
  
  S48_GC_PROTECT_1(v1);
  list = s48_list_11 (v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12);
  list = s48_cons (v1, list);
  S48_GC_UNPROTECT();
  return list;
}

s48_value
s48_make_weak_pointer(s48_value value)
{
  s48_value obj;
  S48_DECLARE_GC_PROTECT(1);

  S48_GC_PROTECT_1(value);

  obj = s48_allocate_stob(S48_STOBTYPE_WEAK_POINTER, sizeof(s48_value));
  S48_STOB_SET(obj, 0, value);

  S48_GC_UNPROTECT();
  return obj;
}

s48_value
s48_enter_substring(char *str, int length)
{
  s48_value obj = s48_allocate_stob(S48_STOBTYPE_STRING, length + 1);
  memcpy(S48_UNSAFE_EXTRACT_STRING(obj), str, length);
  *(S48_UNSAFE_EXTRACT_STRING(obj) + length) = '\0';
  return obj;
}

s48_value
s48_enter_string(char *str)
{
  return s48_enter_substring(str, strlen(str));
}

char *
s48_extract_string(s48_value string)
{
  S48_CHECK_STRING(string);

  return S48_UNSAFE_EXTRACT_STRING(string);
}

s48_value
s48_make_string(int length, char init)
{
  s48_value obj = s48_allocate_stob(S48_STOBTYPE_STRING, length+1);
  memset(S48_UNSAFE_EXTRACT_STRING(obj), init, length);
  S48_UNSAFE_EXTRACT_STRING(obj)[length] = '\0';
  return obj;
}

s48_value
s48_make_vector(int length, s48_value init)
{
  int i;
  s48_value obj;
  S48_DECLARE_GC_PROTECT(1);

  S48_GC_PROTECT_1(init);

  obj = s48_allocate_stob(S48_STOBTYPE_VECTOR, length * sizeof(s48_value));
  for (i = 0; i < length; ++i)
    S48_UNSAFE_VECTOR_SET(obj, i, init);

  S48_GC_UNPROTECT();

  return obj;
}

s48_value
s48_enter_byte_vector(char *bvec, int length)
{
  s48_value obj = s48_allocate_stob(S48_STOBTYPE_BYTE_VECTOR, length);
  memcpy(S48_UNSAFE_EXTRACT_BYTE_VECTOR(obj), bvec, length);
  return obj;
}

char *
s48_extract_byte_vector(s48_value bvec)
{
  S48_CHECK_BYTE_VECTOR(bvec);

  return S48_UNSAFE_EXTRACT_BYTE_VECTOR(bvec);
}

s48_value
s48_make_byte_vector(int length, int init)
{
  int i;
  s48_value obj = s48_allocate_stob(S48_STOBTYPE_BYTE_VECTOR, length);
  
  for (i = 0; i < length; i++)
    S48_BYTE_VECTOR_SET(obj, i, init);
  
  return obj;
}

s48_value
s48_make_record(s48_value type_shared_binding)
{
    int i, number_of_fields;
    s48_value record = S48_FALSE;
    s48_value record_type = S48_FALSE;
    S48_DECLARE_GC_PROTECT(1);

    S48_GC_PROTECT_1(record_type);

    S48_SHARED_BINDING_CHECK(type_shared_binding);
    S48_SHARED_BINDING_CHECK(the_record_type_binding);

    record_type = S48_SHARED_BINDING_REF(type_shared_binding);

    s48_check_record_type(record_type, the_record_type_binding);

    number_of_fields =
      S48_UNSAFE_EXTRACT_FIXNUM(S48_RECORD_TYPE_NUMBER_OF_FIELDS(record_type));

    record = s48_allocate_stob(S48_STOBTYPE_RECORD,
			   (number_of_fields + 1) * sizeof(s48_value));

    S48_UNSAFE_RECORD_SET(record, -1, record_type);
    for (i = 0; i < number_of_fields; ++i)
      S48_UNSAFE_RECORD_SET(record, i, S48_UNSPECIFIC);

    S48_GC_UNPROTECT();

    return record;
}

/*
 * Raise an exception if `record' is not a record whose type is the one
 * found in `type_binding'.
 */
void
s48_check_record_type(s48_value record, s48_value type_binding)
{
  if (! S48_RECORD_P(S48_SHARED_BINDING_REF(type_binding)))
    s48_raise_scheme_exception(S48_EXCEPTION_UNBOUND_EXTERNAL_NAME, 1,
			       S48_SHARED_BINDING_NAME(type_binding));

  if ((! S48_RECORD_P(record)) ||
      (S48_UNSAFE_SHARED_BINDING_REF(type_binding) !=
       S48_UNSAFE_RECORD_REF(record, -1)))
    s48_raise_argument_type_error(record);
}    

long
s48_length(s48_value list)
{
  long i = 0;

  while (!(S48_EQ(list, S48_NULL)))
    {
      list = S48_CDR(list);
      ++i;
    }
  return S48_UNSAFE_ENTER_FIXNUM(i);
}

/*
** 
**  Support for libscsh.a: add external initializers without the Makefile
**
*/

struct simple_list{
  void (*init)();          /* pointer to init-function */
  struct  simple_list* next;
};

struct simple_list* additional_inits = 0;

/*
 * This function is part of EXTERNAL_INITIALIZERS in the scsh Makefile.
 * It calls the init-functions in additional_inits.
 */

void s48_init_additional_inits(){

  struct simple_list* ptr = additional_inits;
  struct simple_list* free_me;
  
  while (ptr != 0){
    ptr->init();
    free_me = ptr;
    ptr = ptr->next;
    free (free_me);
  }
}

/* 
 * Actual API function: argument is an init-function.  You have to
 * ensure, that all s48_add_external_inits are called before you call
 * s48_main. 
 */

int s48_add_external_init(void (*init)()){

  struct simple_list *new_list;

  new_list = (struct simple_list *) malloc(sizeof (struct simple_list));

  if (new_list == 0) return 0;

  new_list->init = init;
  new_list->next = additional_inits;
  additional_inits = new_list;

  return 1;
}
