/* logname -- print user's login name
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

#include <stdio.h>
#include <sys/types.h>
#include "system.h"

void
main (argc, argv)
     int argc;
     char **argv;
{
  register char *cp;

  if (argc != 1)
    {
      fprintf (stderr, "Usage: %s\n", argv[0]);
      exit (1);
    }

  cp = getlogin ();
  if (cp)
    {
      puts (cp);
      exit (0);
    }
  fprintf (stderr,"%s: no login name\n", argv[0]);
  exit (1);
}
