/* tty -- print the path of the terminal connected to standard input
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

/* Displays "not a tty" if stdin is not a terminal.
   Displays nothing if -s option is given.
   Exit status 0 if stdin is a tty, 1 if not, 2 if usage error.

 Written by David MacKenzie (djm@ai.mit.edu).  */

#include <stdio.h>
#include <getopt.h>
#include <sys/types.h>
#include "system.h"

void usage ();

/* The name under which this program was run. */
char *program_name;

/* If nonzero, return an exit status but produce no output. */
int silent;

struct option longopts[] =
{
  {"silent", 0, NULL, 's'},
  {"quiet", 0, NULL, 's'},
  {NULL, 0, NULL, 0}
};

void
main (argc, argv)
     int argc;
     char **argv;
{
  char *tty;
  int optc;

  program_name = argv[0];
  silent = 0;

  while ((optc = getopt_long (argc, argv, "s", longopts, (int *) 0)) != EOF)
    {
      switch (optc)
	{
	case 's':
	  silent = 1;
	  break;
	default:
	  usage ();
	}
    }

  if (optind != argc)
    usage ();

  tty = ttyname (0);
  if (!silent)
    {
      if (tty)
	puts (tty);
      else
	puts ("not a tty");
    }

  exit (tty == NULL);
}

void
usage ()
{
  fprintf (stderr, "\
Usage: %s [-s] [--silent] [--quiet]\n", program_name);
  exit (2);
}
