/* psize.c - Find pipe size. */

/* Copyright (C) 1987, 1991 Free Software Foundation, Inc.

   This file is part of GNU Bash, the Bourne Again SHell.

   Bash is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 1, or (at your option)
   any later version.

   Bash is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with Bash; see the file COPYING.  If not, write to the Free
   Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. */

/*  Write output in 128-byte chunks until we get a sigpipe or write gets an
    EPIPE.  Then report how many bytes we wrote.  We assume that this is the
    pipe size. */

#include <stdio.h>
#include <sys/types.h>
#include <signal.h>
#include <errno.h>

#include "../general.h"
extern int errno;

int nw;

sighandler
sigpipe (sig)
     int sig;
{
  fprintf (stderr, "%d\n", nw);
  exit (0);
}

#if !defined (NeXT)
char *
memset (s, c, n)
     register char *s;
     register int c, n;
{
  register char *p = s;

  while (--n >= 0)
    *s++ = c;

  return (p);
}
#endif /* !NeXT */

main (argc, argv)
     int argc;
     char **argv;
{
  char buf[128];

  memset (buf, ' ', 128);
  signal (SIGPIPE, sigpipe);

  nw = 0;
  for (;;)
    {
      int n;
      n = write (1, buf, 128);
      nw += n;
    }
}
