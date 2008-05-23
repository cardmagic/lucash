/* Copyright (c) 1993-1999 by Richard Kelsey and Jonathan Rees.
   See file COPYING. */

#include <unistd.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/time.h>
#include <errno.h>              /* for errno, (POSIX?/ANSI) */
#include "sysdep.h"
#include "c-mods.h"
#include "scheme48vm.h"
#include "event.h"


/* Non-blocking I/O on file descriptors.

 There appear to be two ways to get non-blocking input and output.  One
 is to open files with the O_NONBLOCK flag (and to use fcntl() to do the
 same to stdin and stdout), the other is to call select() on each file
 descriptor before doing the I/O operation.  O_NONBLOCK has the problem
 of being a property of the file descriptor, and its use with stdin and
 stdout can lead to horrible results.

 We use a mixture of both.  For input files we call select() before doing
 a read(), because read() will return immediately if there are any bytes
 available at all, and using O_NONBLOCK on stdin is a very bad idea.
 Output files are opened using O_NONBLOCK and stdout is left alone.

*/

int
ps_open_fd(char *filename, bool is_input, long *status)
{
#define FILE_NAME_SIZE 1024
#define PERMISSION 0666   /* read and write for everyone */

  char filename_temp[FILE_NAME_SIZE];
  char *expanded;
  extern char *s48_expand_file_name(char *, char *, int);

  int flags;
  mode_t mode;

  expanded = s48_expand_file_name(filename, filename_temp, FILE_NAME_SIZE);
  if (expanded == NULL)
    return -1;

  if (is_input) {
    flags = O_RDONLY;
    mode = 0; }
  else {
    flags = O_WRONLY | O_CREAT | O_TRUNC | O_NONBLOCK;
    mode = PERMISSION; }

  /* keep trying if interrupted */
  while(TRUE) {
    int fd = open(expanded, flags, mode);
    if (fd != -1) {
      *status = NO_ERRORS;
      return fd; }
    else if (errno != EINTR) {
      *status = errno;
      return -1; }
  }
}

int
ps_close_fd(long fd_as_long)
{
  int fd = (int)fd_as_long;

  /* keep retrying if interrupted */
  while(TRUE) {
    int status = close(fd);
    if (status != -1) {
      s48_remove_fd(fd);
      return NO_ERRORS; }
    else if (errno != EINTR)
      return errno;
  }
}

bool ps_check_fd(long fd_as_long, bool is_read, long *status)
{
  int fd = (int)fd_as_long;
  int ready;

  struct timeval timeout;
  fd_set fds;

  FD_ZERO(&fds);
  FD_SET(fd, &fds);
  timerclear(&timeout);

  *status = NO_ERRORS;

  while(TRUE) {
    ready = select(fd + 1,
		   is_read ? &fds : NULL,
		   is_read ? NULL : &fds,
		   &fds,
		   &timeout);
    if (ready == 0)
	return FALSE;
    else if (ready == 1)
	return TRUE;
    else if (errno != EINTR) {
	*status = errno;
	return FALSE; } } 
}

/*
 * Return TRUE if successful, and FALSE otherwise.
 */

bool
ps_add_pending_fd(long fd_as_long, bool is_input)
{
  return s48_add_pending_fd((int) fd_as_long, is_input);
}

long
ps_read_fd(long fd_as_long, char *buffer, long max, bool waitp,
	   bool *eofp, bool *pending, long *status)
{
  int got, ready;
  void *buf = (void *)buffer;
  int fd = (int)fd_as_long;

  struct timeval timeout;
  fd_set readfds;

  FD_ZERO(&readfds);
  FD_SET(fd, &readfds);
  timerclear(&timeout);

  /* for the normal return */
  *eofp = FALSE;
  *pending = FALSE;
  *status = NO_ERRORS;

  while(TRUE) {
    ready = select(fd + 1, &readfds, NULL, &readfds, &timeout);
    if (ready == 0) {
      if (!waitp)
	return 0;
      else if (s48_add_pending_fd(fd, TRUE)) {
	*pending = TRUE;
	return 0; }
      else {
	*status = ENOMEM;    /* as close as POSIX gets */
	return 0; }}
    else if (ready == -1) {
      if (errno != EINTR) {
	*status = errno;
	return 0; } }
    else {          /* characters waiting */

      got = read(fd, buf, max);

      if (got > 0) {       /* all is well */
	return got; }
      else if (got == 0) { /* end of file */
	*eofp = TRUE;
	return 0; }
      else if (errno == EINTR) {			/* HCC */
	return 0; }
      else if (errno == EAGAIN) {			/* HCC */
	if (!waitp)
	  return 0;
	else if (s48_add_pending_fd(fd, TRUE)) {
	  *pending = TRUE;
	  return 0; }
	else {
	  *status = ENOMEM;    /* as close as POSIX gets */
	  return 0; } }
      else {
	*status = errno;
	return 0; } } }
}

long
ps_write_fd(long fd_as_long, char *buffer, long max, bool *pending, long *status)
{
  int sent;
  int fd = (int)fd_as_long;
  void *buf = (void *)buffer;

  *pending = FALSE;
  *status = NO_ERRORS;

  sent = write(fd, buf, max);
  if (sent > 0)
    {}
  else if (errno == EINTR || errno == EAGAIN) {		/* HCC */
    if (s48_add_pending_fd(fd, FALSE))
      *pending = TRUE;
    else
      *status = ENOMEM;    /* as close as POSIX gets */
    sent = 0; }
  else {
    *status = errno;
    sent = 0; }

  return sent;
}

long
ps_abort_fd_op(long fd_as_long)
{
  int fd = (int)fd_as_long;
  fprintf(stderr, "aborting %d\n", fd);
  if (!s48_remove_fd(fd))
    fprintf(stderr, "Error: ps_abort_fd_op, no pending operation on fd %d\n",
	            fd);
  return 0;      /* because we do not actually do any I/O in parallel the
		    status is always zero: no characters transfered. */
}
