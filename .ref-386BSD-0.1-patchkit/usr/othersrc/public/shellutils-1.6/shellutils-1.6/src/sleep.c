/* sleep - delay for a specified amount of time.
   Copyright (C) 1984, 1991 Free Software Foundation, Inc.

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

long argdecode ();
void error ();

/* The name by which this program was run. */
char *program_name;

void
main (argc, argv)
     int argc;
     char **argv;
{
  int i;
  long seconds = 0;

  program_name = argv[0];

  if (argc == 1)
    {
      fprintf (stderr, "Usage: %s number[smhd]...\n", argv[0]);
      exit (1);
    }

  for (i = 1; i < argc; i++)
    seconds += argdecode (argv[i]);

  sleep (seconds);

  exit (0);
}

long
argdecode (s)
     char *s;
{
  long value;
  register char *p = s;
  register char c;

  value = 0;
  while ((c = *p++) >= '0' && c <= '9')
    value = value * 10 + c - '0';

  switch (c)
    {
    case 's':
      break;
    case 'm':
      value *= 60;
      break;
    case 'h':
      value *= 60 * 60;
      break;
    case 'd':
      value *= 60 * 60 * 24;
      break;
    default:
      p--;
    }

  if (*p)
    error (1, 0, "invalid time interval `%s'", s);
  return value;
}
