/* whoami -- print effective userid
   Copyright (C) 1989, 1991 Free Software Foundation, Inc.

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

/* Equivalent to `id -un'. */
/* Written by Richard Mlynarik. */

#include <stdio.h>
#include <sys/types.h>
#include <pwd.h>
#include "system.h"

void
main (argc, argv)
     int argc;
     char *argv[];
{
  register struct passwd *pw;
  register uid_t uid;

  if (argc != 1)
    {
      fprintf (stderr, "Usage: %s\n", argv[0]);
      exit (1);
    }

  uid = geteuid ();
  pw = getpwuid (uid);
  if (pw)
    {
      puts (pw->pw_name);
      exit (0);
    }
  fprintf (stderr,"%s: cannot find username for UID %u\n", argv[0], uid);
  exit (1);
}
