/* dirname -- strip filename suffix from pathname
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

/* Written by David MacKenzie and Jim Meyering. */

#include <stdio.h>
#include <sys/types.h>
#include "system.h"

void strip_trailing_slashes ();

void
main (argc, argv)
     int argc;
     char **argv;
{
  register char *path;
  register char *slash;

  if (argc != 2)
    {
      fprintf (stderr, "Usage: %s path\n", argv[0]);
      exit (1);
    }

  path = argv[1];
  strip_trailing_slashes (path);

  slash = rindex (path, '/');
  if (slash == NULL)
    path = ".";
  else
    {
      /* Remove any trailing slashes and final element. */
      while (slash > path && *slash == '/')
	--slash;
      slash[1] = 0;
    }
  puts (path);

  exit (0);
}

