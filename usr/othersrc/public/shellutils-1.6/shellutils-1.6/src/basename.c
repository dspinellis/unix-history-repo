/* basename -- strip directory and suffix from filenames
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

/* Usage: basename name [suffix]
   NAME is a pathname; SUFFIX is a suffix to strip from it.

   basename /usr/foo/lossage/functions.l
   => functions.l
   basename /usr/foo/lossage/functions.l .l
   => functions
   basename functions.lisp p
   => functions.lis */

#include <stdio.h>
#include <sys/types.h>
#include "system.h"

char *basename ();
void remove_suffix ();
void strip_trailing_slashes ();

void
main (argc, argv)
     int argc;
     char **argv;
{
  char *name;

  if (argc == 1 || argc > 3)
    {
      fprintf (stderr, "Usage: %s name [suffix]\n", argv[0]);
      exit (1);
    }

  strip_trailing_slashes (argv[1]);

  name = basename (argv[1]);

  if (argc == 3)
    remove_suffix (name, argv[2]);

  puts (name);

  exit (0);
}

/* Remove SUFFIX from the end of NAME if it is there, unless NAME
   consists entirely of SUFFIX. */

void
remove_suffix (name, suffix)
     register char *name, *suffix;
{
  register char *np, *sp;

  np = name + strlen (name);
  sp = suffix + strlen (suffix);

  while (np > name && sp > suffix)
    if (*--np != *--sp)
      return;
  if (np > name)
    *np = '\0';
}
