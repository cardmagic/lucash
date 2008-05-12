/* filecntl.h - Definitions to set file descriptors to close-on-exec. */

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

#if !defined (_FILECNTL_H_)
#define _FILECNTL_H_

#include <fcntl.h>

/* Definitions to set file descriptors to close-on-exec, the Posix way. */
#if !defined (FD_CLOEXEC)
#define FD_CLOEXEC     1
#endif

#define FD_NCLOEXEC    0

#define SET_CLOSE_ON_EXEC(fd)  (fcntl ((fd), F_SETFD, FD_CLOEXEC))
#define SET_OPEN_ON_EXEC(fd)   (fcntl ((fd), F_SETFD, FD_NCLOEXEC))

#endif /* ! _FILECNTL_H_ */
