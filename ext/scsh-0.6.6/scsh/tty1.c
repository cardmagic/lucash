/* To do:
 * - Replace explicit 8/24 splits with macros.
 * - We need to pass the control-chars vecs in as Scheme
 *   strings, and test the length before doing the memcpy.
 */

/* 
 * Scheme48/scsh terminal control interface.
 * Routines that require custom C support.
 * Copyright (c) 1995 by Brian D. Carlstrom
 * Re-written by Olin.
 */

#include <unistd.h>
#include <stdio.h>	/* ctermid decl */
#include <termios.h>
#include <string.h>
#include <sys/types.h>
#include <errno.h>
#include "scheme48.h"

/* This #include is for the #ifdef'd code in open_ctty() below, and
** is therefor ifdef'd identically.
*/
#if defined(TIOCSCTTY) && !defined(CIBAUD)
#include <sys/ioctl.h>
#endif

#include "tty1.h"	/* Make sure the .h interface agrees with the code. */

/*****************************************************************************/

s48_value scheme_tcgetattr(s48_value sch_fd, s48_value sch_control_chars)
     /*		     int *iflag,
		     int *oflag,
		     int *cflag,
		     int *lflag,
		     int *ispeed,    int *ospeed)*/
{
  struct termios t;
  int result = tcgetattr(s48_extract_fixnum (sch_fd), &t);
  int i;
  s48_value sch_iflag = S48_UNSPECIFIC;
  s48_value sch_oflag = S48_UNSPECIFIC;
  s48_value sch_cflag = S48_UNSPECIFIC;
  s48_value sch_lflag = S48_UNSPECIFIC;
  s48_value sch_ispeed = S48_UNSPECIFIC;
  s48_value sch_ospeed = S48_UNSPECIFIC;
  s48_value sch_retval = S48_UNSPECIFIC;
  S48_DECLARE_GC_PROTECT(6);
  
  S48_GC_PROTECT_6(sch_iflag, sch_oflag, sch_cflag, sch_lflag, sch_ispeed,
		   sch_ospeed);

  if (result == -1) {
    S48_GC_UNPROTECT();
    s48_raise_os_error_2 (errno, sch_fd, sch_control_chars);
  }

  for (i = 0; i < NCCS; i++)
    S48_STRING_SET(sch_control_chars, i, t.c_cc[i]);
  {
    sch_iflag = s48_enter_integer(t.c_iflag);
    sch_oflag = s48_enter_integer(t.c_oflag);
    sch_cflag = s48_enter_integer(t.c_cflag);
    sch_lflag = s48_enter_integer(t.c_lflag);
    sch_ispeed = s48_enter_integer(cfgetispeed(&t));
    sch_ospeed = s48_enter_integer(cfgetospeed(&t));

    sch_retval = s48_list_6 (sch_iflag, sch_oflag, sch_cflag, sch_lflag,
			     sch_ispeed, sch_ospeed);
    S48_GC_UNPROTECT();
    
    return sch_retval;
  }
}

/* The Scheme caller of this is commented out...*/
int scheme_tcgetattrB(int fd, char *control_chars, s48_value scmvec)
{
  struct termios t;
  int result = tcgetattr(fd, &t);
  S48_DECLARE_GC_PROTECT(1);
  S48_GC_PROTECT_1(scmvec);
  /* JMG  int *ivec = ADDRESS_AFTER_HEADER(scmvec, int);*/
  
  if (result != -1) {
      memcpy(control_chars, t.c_cc, NCCS);
      S48_VECTOR_SET(scmvec, 0, s48_enter_integer(t.c_iflag));
      S48_VECTOR_SET(scmvec, 1, s48_enter_integer(t.c_oflag));
      S48_VECTOR_SET(scmvec, 2, s48_enter_integer(t.c_cflag));
      S48_VECTOR_SET(scmvec, 3, s48_enter_integer(t.c_lflag));
      S48_VECTOR_SET(scmvec, 4, s48_enter_fixnum(cfgetispeed(&t)));
      S48_VECTOR_SET(scmvec, 5, s48_enter_fixnum(cfgetospeed(&t)));
      }
  S48_GC_UNPROTECT();
  return result;
  }


/*****************************************************************************/

s48_value scheme_tcsetattr(s48_value sch_fd, s48_value sch_option,
			   s48_value sch_control_chars,
			   s48_value sch_iflag,
			   s48_value sch_oflag,
			   s48_value sch_cflag,
			   s48_value sch_lflag,
			   s48_value sch_ispeed, s48_value sch_ospeed,
			   s48_value sch_min, s48_value sch_time)
{
  struct termios t;

  memcpy(t.c_cc, s48_extract_string (sch_control_chars), NCCS);

  /* This first clause of this conditional test will hopefully
  ** resolve the branch at compile time. However, since VMIN/VEOF
  ** and VTIME/VEOL are allowed by POSIX to colllide, we have to check.
  ** If they do collide, we set EOF & EOL in canonical mode, and MIN & TIME
  ** in raw mode. Ah, Unix.
  */

  t.c_iflag = s48_extract_integer (sch_iflag);
  t.c_oflag = s48_extract_integer (sch_oflag);
  t.c_cflag = s48_extract_integer (sch_cflag);
  t.c_lflag = s48_extract_integer (sch_lflag);

  if( (VMIN != VEOF && VTIME != VEOL) || !(t.c_lflag & ICANON) ) {
      t.c_cc[VMIN] = s48_extract_fixnum (sch_min);
      t.c_cc[VTIME] = s48_extract_integer (sch_time);
      }

  cfsetispeed(&t, s48_extract_integer (sch_ispeed));
  cfsetospeed(&t, s48_extract_integer (sch_ospeed));

  if (tcsetattr(s48_extract_fixnum (sch_fd), s48_extract_integer (sch_option), 
		&t) 
      == -1)
    s48_raise_os_error_1 (errno, sch_fd);
  return S48_UNSPECIFIC;
}


s48_value sch_tcsendbreak (s48_value sch_fd, s48_value sch_duration)
{
  if (tcsendbreak (s48_extract_fixnum (sch_fd), 
		   s48_extract_integer (sch_duration)) == -1)
    s48_raise_os_error_2 (errno, sch_fd, sch_duration);
  return S48_UNSPECIFIC;
}

s48_value sch_tcdrain (s48_value sch_fd)
{
  if (tcdrain (s48_extract_fixnum (sch_fd)) == -1)
    s48_raise_os_error_1 (errno, sch_fd);
  return S48_UNSPECIFIC;
}

s48_value sch_tcflush (s48_value sch_fd, s48_value sch_action)
{
  if (tcflush (s48_extract_fixnum (sch_fd),
	       s48_extract_fixnum (sch_action)) == -1)
    s48_raise_os_error_2 (errno, sch_fd, sch_action);
  return S48_UNSPECIFIC;
}

s48_value sch_tcflow (s48_value sch_fd, s48_value sch_action)
{
  if (tcflow (s48_extract_fixnum (sch_fd),
	      s48_extract_fixnum (sch_action)) == -1)
    s48_raise_os_error_2 (errno, sch_fd, sch_action);
  return S48_UNSPECIFIC;
}

s48_value sch_tcsetpgrp (s48_value sch_fd, s48_value sch_pid)
{
   if (tcsetpgrp (s48_extract_fixnum (sch_fd),
		  s48_extract_fixnum (sch_pid)) == -1)
    s48_raise_os_error_2 (errno, sch_fd, sch_pid);
  return S48_UNSPECIFIC;
}
	       
s48_value sch_tcgetpgrp (s48_value sch_fd)
{
  int ret = tcgetpgrp (s48_extract_fixnum (sch_fd));
  if (ret == -1)
    s48_raise_os_error_1 (errno, sch_fd);
  return s48_enter_integer (ret);
}

/*****************************************************************************/

s48_value open_ctty(s48_value sch_ttyname, s48_value sch_flags)
{
    int fd = open(s48_extract_string (sch_ttyname), 
		  s48_extract_integer (sch_flags));
    
#if defined(TIOCSCTTY) && !defined(CIBAUD) && !defined(__hpux)
    /* 4.3+BSD way to acquire control tty. !CIBAUD rules out SunOS.
    ** This code stolen from Steven's *Advanced Prog. in the Unix Env.*
    */
    if( (fd >= 0) && (ioctl(fd, TIOCSCTTY, (char *) 0) < 0) ) {
	int e = errno;
	close(fd);
	s48_raise_os_error_2 (e, sch_ttyname, sch_flags);
	}
#endif
    if (fd == -1)
      s48_raise_os_error_2 (errno, sch_ttyname, sch_flags);
    return s48_enter_fixnum (fd);
}

s48_value sch_isatty (s48_value sch_fd)
{
  return ((isatty (s48_extract_fixnum (sch_fd))) ? S48_TRUE : S48_FALSE);
}

s48_value sch_ttyname (s48_value sch_fd)
{
  char* ret = ttyname (s48_extract_fixnum (sch_fd));
  if (ret == NULL)
    s48_raise_os_error_1 (errno, sch_fd);
  return s48_enter_string (ret);
}

s48_value scm_ctermid() 
{ 
  char* ret = ctermid(0);
  if (ret == NULL)
    s48_raise_os_error (errno);
  return s48_enter_string (ret);
}

void s48_init_tty(void)
{
    S48_EXPORT_FUNCTION(scheme_tcgetattr);
    S48_EXPORT_FUNCTION(scheme_tcsetattr);
    S48_EXPORT_FUNCTION(sch_tcsendbreak);
    S48_EXPORT_FUNCTION(sch_tcdrain);
    S48_EXPORT_FUNCTION(sch_tcflush);
    S48_EXPORT_FUNCTION(sch_tcflow);
    S48_EXPORT_FUNCTION(sch_tcsetpgrp);
    S48_EXPORT_FUNCTION(sch_tcgetpgrp);
    S48_EXPORT_FUNCTION(open_ctty);
    S48_EXPORT_FUNCTION(sch_isatty);
    S48_EXPORT_FUNCTION(sch_ttyname);
    S48_EXPORT_FUNCTION(scm_ctermid);
}
