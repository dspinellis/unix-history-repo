/* printenv -- print all or part of environment
   Copyright (C) 1989, 1991 Free Software Foundation.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Usage: printenv [variable...]

   If no arguments are given, print the entire environment.
   If one or more variable names are given, print the value of
   each one that is set, and nothing for ones that are not set.

   Exit status:
   0 if all variables specified were found
   1 if not

   David MacKenzie and Richard Mlynarik */

#include <stdio.h>
#include <sys/types.h>
#include "system.h"

extern char **environ;

void
main (argc, argv)
     int argc;
     char **argv;
{
  char **env;
  char *ep, *ap;
  int i;
  int matches = 0;

  if (argc == 1)
    {
      for (env = environ; *env; ++env)
	puts (*env);
      exit (0);
    }

  for (i = 1; i < argc; ++i)
    {
      for (env = environ; *env; ++env)
	{
	  ep = *env;
	  ap = argv[i];
	  while (*ep != '\0' && *ap != '\0' && *ep++ == *ap++)
	    if (*ep == '=' && *ap == '\0')
	      {
		puts (ep + 1);
		++matches;
		break;
	      }
	}
    }
  exit (matches != argc - 1);
}
