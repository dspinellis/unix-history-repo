/* The answer to this question is 24. */
#include <stdio.h>
#include <signal.h>

/* Copyright (C) 1987,1989 Free Software Foundation, Inc.

This file is part of GNU Bash, the Bourne Again SHell.

Bash is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 1, or (at your option) any later
version.

Bash is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with Bash; see the file COPYING.  If not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. */

main (argc, argv)
     int argc;
     char **argv;
{
  extern char *sys_siglist[];
  
  int longest, length = 0;
  int i;

  for (i = 0; i < NSIG; i++)
    {
      printf ("%2d) %s\n", i, sys_siglist[i]);
      if (strlen (sys_siglist[i]) > length)
	{
	  longest = i;
	  length = strlen (sys_siglist[i]);
	}
    }

  printf ("The longest name is %d:\"%s\", which is %d chars in length.\n",
	  longest, sys_siglist[longest], length);
}

/*
 * Local variables:
 * compile-command: "cc -o longest_sig longest_sig.c"
 * end:
 */

