/* input.c -- functions to perform buffered input with synchronization. */

/* Copyright (C) 1992 Free Software Foundation, Inc.

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

/* similar to stdio, but input-only. */

#include "bashtypes.h"
#include <sys/file.h>
#include "filecntl.h"
#include "posixstat.h"
#include <stdio.h>
#include <errno.h>

#include "bashansi.h"
#include "config.h"
#include "command.h"
#include "general.h"
#include "input.h"

#if !defined (errno)
extern int errno;
#endif /* !errno */

#define MAX_INPUT_BUFFER_SIZE	8192

#if !defined (SEEK_CUR)
#  define SEEK_CUR 1
#endif /* !SEEK_CUR */

void free_buffered_stream ();

extern int interactive_shell;

int bash_input_fd_changed;
/* This provides a way to map from a file descriptor to the buffer
   associated with that file descriptor, rather than just the other
   way around.  This is needed so that buffers are managed properly
   in constructs like 3<&4.  buffers[x]->b_fd == x -- that is how the
   correspondence is maintained. */
BUFFERED_STREAM **buffers = (BUFFERED_STREAM **)NULL;
static int nbuffers = 0;

#define max(a, b)  (((a) > (b)) ? (a) : (b))

#define ALLOCATE_BUFFERS(n) \
	do { if ((n) >= nbuffers) allocate_buffers (n); } while (0)

/* Make sure `buffers' has at least N elements. */
static void
allocate_buffers (n)
     int n;
{
  register int i, orig_nbuffers;

  orig_nbuffers = nbuffers;
  nbuffers = n + 20;
  buffers = (BUFFERED_STREAM **)xrealloc
    (buffers, nbuffers * sizeof (BUFFERED_STREAM *));

  /* Zero out the new buffers. */
  for (i = orig_nbuffers; i < nbuffers; i++)
    buffers[i] = (BUFFERED_STREAM *)NULL;
}

/* Construct and return a BUFFERED_STREAM corresponding to file descriptor
   FD, using BUFFER. */
static BUFFERED_STREAM *
make_buffered_stream (fd, buffer, bufsize)
     int fd;
     char *buffer;
     int bufsize;
{
  BUFFERED_STREAM *bp;

  bp = (BUFFERED_STREAM *)xmalloc (sizeof (BUFFERED_STREAM));
  ALLOCATE_BUFFERS (fd);
  buffers[fd] = bp;
  bp->b_fd = fd;
  bp->b_buffer = buffer;
  bp->b_size = bufsize;
  bp->b_used = 0;
  bp->b_inputp = 0;
  bp->b_flag = 0;
  if (bufsize == 1)
    bp->b_flag |= B_UNBUFF;
  return (bp);
}

/* Allocate a new BUFFERED_STREAM, copy BP to it, and return the new copy. */
static BUFFERED_STREAM *
copy_buffered_stream (bp)
     BUFFERED_STREAM *bp;
{
  BUFFERED_STREAM *nbp;

  if (!bp)
    return ((BUFFERED_STREAM *)NULL);

  nbp = (BUFFERED_STREAM *)xmalloc (sizeof (BUFFERED_STREAM));
  xbcopy ((char *)bp, (char *)nbp, sizeof (BUFFERED_STREAM));
  return (nbp);
}

/* Check that file descriptor FD is not the one that bash is currently
   using to read input from a script.  FD is about to be duplicated onto,
   which means that the kernel will close it for us.  If FD is the bash
   input file descriptor, we need to seek backwards in the script (if
   possible and necessary -- scripts read from stdin are still unbuffered),
   allocate a new file descriptor to use for bash input, and re-initialize
   the buffered stream. */
int
check_bash_input (fd)
     int fd;
{
  int nfd;

  if (fd > 0 && ((bash_input.type == st_bstream && bash_input.location.buffered_fd == fd) ||
  		 (interactive_shell == 0 && default_buffered_input == fd)))
    {
      /* Sync the stream so we can re-read from the new file descriptor.  We
	 might be able to avoid this by copying the buffered stream verbatim
	 to the new file descriptor. */
      if (buffers[fd])
	sync_buffered_stream (fd);

      /* Now take care of duplicating the file descriptor that bash is
	 using for input, so we can reinitialize it later. */
      nfd = fcntl (fd, F_DUPFD, 10);
      if (nfd == -1)
	{
	  if (fcntl (fd, F_GETFD, 0) == 0)
	    report_error
	      ("cannot allocate new file descriptor for bash input from fd %d: %s",
		fd, strerror (errno));
	  return -1;
	}

      if (buffers[nfd])
	{
	  /* What's this?  A stray buffer without an associated open file
	     descriptor?  Free up the buffer and report the error. */
	  report_error ("check_bash_input: buffer already exists for new fd %d", nfd);
	  free_buffered_stream (buffers[nfd]);
	}

      /* Reinitialize bash_input.location. */
      if (bash_input.type == st_bstream)
	{
	  bash_input.location.buffered_fd = nfd;
	  fd_to_buffered_stream (nfd);
	  close_buffered_fd (fd);	/* XXX */
	}
      else
	/* If the current input type is not a buffered stream, but the shell
	   is not interactive and therefore using a buffered stream to read
	   input (e.g. with an `eval exec 3>output' inside a script), note
	   that the input fd has been changed.  pop_stream() looks at this
	   value and adjusts the input fd to the new value of
	   default_buffered_input accordingly. */
	bash_input_fd_changed++;

      if (default_buffered_input == fd)
	default_buffered_input = nfd;
    }
  return 0;
}
      
/* This is the buffered stream analogue of dup2(fd1, fd2).  The
   BUFFERED_STREAM corresponding to fd2 is deallocated, if one exists.
   BUFFERS[fd1] is copied to BUFFERS[fd2].  This is called by the
   redirect code for constructs like 4<&0 and 3</etc/rc.local. */
duplicate_buffered_stream (fd1, fd2)
     int fd1, fd2;
{
  int is_bash_input, m;

  if (fd1 == fd2)
    return 0;

  m = max (fd1, fd2);
  ALLOCATE_BUFFERS (m);

  /* If FD2 is the file descriptor bash is currently using for shell input,
     we need to do some extra work to make sure that the buffered stream
     actually exists (it might not if fd1 was not active, and the copy
     didn't actually do anything). */
  is_bash_input = (bash_input.type == st_bstream) &&
		  (bash_input.location.buffered_fd == fd2);

  if (buffers[fd2])
    free_buffered_stream (buffers[fd2]);
  buffers[fd2] = copy_buffered_stream (buffers[fd1]);
  if (buffers[fd2])
    buffers[fd2]->b_fd = fd2;

  if (is_bash_input)
    {
      if (!buffers[fd2])
	fd_to_buffered_stream (fd2);
    }
  return (fd2);
}

/* Return 1 if a seek on FD will succeed. */
#define fd_is_seekable(fd) (lseek ((fd), 0L, SEEK_CUR) >= 0)

/* Take FD, a file descriptor, and create and return a buffered stream
   corresponding to it.  If something is wrong and the file descriptor
   is invalid, return a NULL stream. */
BUFFERED_STREAM *
fd_to_buffered_stream (fd)
     int fd;
{
  char *buffer;
  int size;
  struct stat sb;

  if (fstat (fd, &sb) < 0)
    {
      close (fd);
      return ((BUFFERED_STREAM *)NULL);
    }

  if (fd_is_seekable (fd) == 0)
    size = 1;
  else
    size = (sb.st_size > MAX_INPUT_BUFFER_SIZE) ? MAX_INPUT_BUFFER_SIZE
						: sb.st_size;
      
  buffer = (char *)xmalloc (size);

  return (make_buffered_stream (fd, buffer, size));
}

/* Return a buffered stream corresponding to FILE, a file name. */
BUFFERED_STREAM *
open_buffered_stream (file)
     char *file;
{
  int fd;

  fd = open (file, O_RDONLY);
  if (fd == -1)
    return ((BUFFERED_STREAM *)NULL);
  return (fd_to_buffered_stream (fd));
}

/* Deallocate a buffered stream and free up its resources.  Make sure we
   zero out the slot in BUFFERS that points to BP. */
void
free_buffered_stream (bp)
     BUFFERED_STREAM *bp;
{
  int n;

  if (!bp)
    return;

  n = bp->b_fd;
  if (bp->b_buffer)
    free (bp->b_buffer);
  free (bp);
  buffers[n] = (BUFFERED_STREAM *)NULL;
}

/* Close the file descriptor associated with BP, a buffered stream, and free
   up the stream.  Return the status of closing BP's file descriptor. */
int
close_buffered_stream (bp)
     BUFFERED_STREAM *bp;
{
  int fd;

  if (!bp)
    return (0);
  fd = bp->b_fd;
  free_buffered_stream (bp);
  return (close (fd));
}

/* Deallocate the buffered stream associated with file descriptor FD, and
   close FD.  Return the status of the close on FD. */
int
close_buffered_fd (fd)
     int fd;
{
  if (fd >= nbuffers || !buffers || !buffers[fd])
    return (close (fd));
  return (close_buffered_stream (buffers[fd]));
}

/* Read a buffer full of characters from BP, a buffered stream. */
static int
b_fill_buffer (bp)
     BUFFERED_STREAM *bp;
{
  do
    {
      bp->b_used = read (bp->b_fd, bp->b_buffer, bp->b_size);
    }
  while (bp->b_used < 0 && errno == EINTR);
  if (bp->b_used <= 0)
    {
      bp->b_buffer[0] = 0;
      if (bp->b_used == 0)
	bp->b_flag |= B_EOF;
      else
	bp->b_flag |= B_ERROR;
      return (EOF);
    }
  bp->b_inputp = 0;
  return (bp->b_buffer[bp->b_inputp++] & 0xFF);
}

/* Get a character from buffered stream BP. */
#define bufstream_getc(bp) \
  (bp->b_inputp == bp->b_used || !bp->b_used) \
  		? b_fill_buffer (bp) \
		: bp->b_buffer[bp->b_inputp++] & 0xFF

/* Push C back onto buffered stream BP. */
static int
bufstream_ungetc(c, bp)
     int c;
     BUFFERED_STREAM *bp;
{
  if (c == EOF || bp->b_inputp == 0)
    return (EOF);

  bp->b_buffer[--bp->b_inputp] = c;
  return (c);
}

/* Seek backwards on file BFD to synchronize what we've read so far
   with the underlying file pointer. */
int
sync_buffered_stream (bfd)
     int bfd;
{
  BUFFERED_STREAM *bp;
  int chars_left;

  bp = buffers[bfd];
  if (!bp)
    return (-1);
  chars_left = bp->b_used - bp->b_inputp;
  if (chars_left)
    lseek (bp->b_fd, -chars_left, SEEK_CUR);
  bp->b_used = bp->b_inputp = 0;
  return (0);
}

int
buffered_getchar ()
{
  return (bufstream_getc (buffers[bash_input.location.buffered_fd]));
}

int
buffered_ungetchar (c)
     int c;
{
  return (bufstream_ungetc (c, buffers[bash_input.location.buffered_fd]));
}

/* Make input come from file descriptor BFD through a buffered stream. */
void
with_input_from_buffered_stream (bfd, name)
     int bfd;
     char *name;
{
  INPUT_STREAM location;

  location.buffered_fd = bfd;
  /* Make sure the buffered stream exists. */
  fd_to_buffered_stream (bfd);
  init_yy_io (buffered_getchar, buffered_ungetchar, st_bstream, name, location);
}

#if defined (TEST)

char *
xmalloc(s)
int s;
{
	return ((char *)malloc (s));
}

char *
xrealloc(s, size)
char	*s;
int	size;
{
	if (!s)
		return((char *)malloc (size));
	else
		return((char *)realloc (s, size));
}

void
init_yy_io ()
{
}

process(bp)
BUFFERED_STREAM *bp;
{
	int c;

	while ((c = bufstream_getc(bp)) != EOF)
		putchar(c);
}

BASH_INPUT bash_input;

struct stat dsb;		/* can be used from gdb */

/* imitate /bin/cat */
main(argc, argv)
int	argc;
char	**argv;
{
	register int i;
	BUFFERED_STREAM *bp;

	if (argc == 1) {
		bp = fd_to_buffered_stream (0);
		process(bp);
		exit(0);
	}
	for (i = 1; i < argc; i++) {
		if (argv[i][0] == '-' && argv[i][1] == '\0') {
			bp = fd_to_buffered_stream (0);
			if (!bp)
				continue;
			process(bp);
			free_buffered_stream (bp);
		} else {
			bp = open_buffered_stream (argv[i]);
			if (!bp)
				continue;
			process(bp);
			close_buffered_stream (bp);
		}
	}
	exit(0);
}
#endif
