/* rmdir -- remove directories
   Copyright (C) 1990, 1991 Free Software Foundation, Inc.

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

/* Options:
   -p, --path		Remove any parent dirs that are explicitly mentioned
			in an argument, if they become empty after the
			argument file is removed.

   David MacKenzie <djm@ai.mit.edu>  */

#include <stdio.h>
#include <getopt.h>
#include <sys/types.h>
#include "system.h"

void remove_parents ();
void error ();
void strip_trailing_slashes ();
void usage ();

/* If nonzero, remove empty parent directories. */
int empty_paths;

/* The name this program was run with. */
char *program_name;

struct option longopts[] =
{
  {"path", 0, &empty_paths, 1},
  {NULL, 0, NULL, 0}
};

void
main (argc, argv)
     int argc;
     char **argv;
{
  int errors = 0;
  int optc;

  program_name = argv[0];
  empty_paths = 0;

  while ((optc = getopt_long (argc, argv, "p", longopts, (int *) 0)) != EOF)
    {
      switch (optc)
	{
	case 0:			/* Long option. */
	  break;
	case 'p':
	  empty_paths = 1;
	  break;
	default:
	  usage ();
	}
    }

  if (optind == argc)
    usage ();
  
  for (; optind < argc; ++optind)
    {
      strip_trailing_slashes (argv[optind]);
      if (rmdir (argv[optind]) != 0)
	{
	  error (0, errno, "%s", argv[optind]);
	  errors = 1;
	}
      else if (empty_paths)
	remove_parents (argv[optind]);
    }

  exit (errors);
}

/* Remove any empty parent directories of `path'.
   Replaces '/' characters in `path' with NULs. */

void
remove_parents (path)
     char *path;
{
  char *slash;

  do
    {
      slash = rindex (path, '/');
      if (slash == NULL)
	break;
      /* Remove any characters after the slash, skipping any extra
	 slashes in a row. */
      while (slash > path && *slash == '/')
	--slash;
      slash[1] = 0;
    }
  while (rmdir (path) == 0);
}

void
usage ()
{
  fprintf (stderr, "Usage: %s [-p] [--path] dir...\n",
	   program_name);
  exit (1);
}
