/* trap.h -- data structures used in the trap mechanism. */

/* Copyright (C) 1993 Free Software Foundation, Inc.

   This file is part of GNU Bash, the Bourne Again SHell.

   Bash is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 2, or (at your option) any later
   version.

   Bash is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License along
   with Bash; see the file COPYING.  If not, write to the Free Software
   Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. */

#if !defined (__TRAP_H__)
#define __TRAP_H__

#include "stdc.h"

#if !defined (SIG_DFL)
#include <sys/types.h>
#include <signal.h>
#endif /* SIG_DFL */

#if !defined (NSIG)
#define NSIG 64
#endif /* !NSIG */

#define NO_SIG -1
#define DEFAULT_SIG SIG_DFL
#define IGNORE_SIG  SIG_IGN

#define signal_object_p(x) (decode_signal (x) != NO_SIG)

extern char *trap_list[NSIG];

/* Externally-visible functions declared in trap.c. */
extern void initialize_traps __P((void));
extern void run_pending_traps __P((void));
extern void maybe_set_sigchld_trap __P((char *));
extern void set_sigchld_trap __P((char *));
extern void set_signal __P((int, char *));
extern void restore_default_signal __P((int));
extern void ignore_signal __P((int));
extern int run_exit_trap __P((void));
extern void free_trap_strings __P((void));
extern void reset_signal_handlers __P((void));
extern void restore_original_signals __P((void));

extern char *signal_name __P((int));

extern int decode_signal __P((char *));
extern void run_interrupt_trap __P((void));
extern int maybe_call_trap_handler __P((int));
extern int signal_is_trapped __P((int));
extern int signal_is_special __P((int));
extern int signal_is_ignored __P((int));

#endif /* __TRAP_H__ */
