/* mkfifo -- make fifo's (named pipes)
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
   -m, --mode=mode	Set the mode of created fifo's to MODE, which is
			symbolic as in chmod and uses the umask as a point of
			departure.

   David MacKenzie <djm@ai.mit.edu>  */

#include <stdio.h>
#include <getopt.h>
#include <sys/types.h>
#include "system.h"
#include "modechange.h"

void error ();
void strip_trailing_slashes ();
void usage ();

/* The name this program was run with. */
char *program_name;

struct option longopts[] =
{
  {"mode", 1, NULL, 'm'},
  {NULL, 0, NULL, 0}
};

void
main (argc, argv)
     int argc;
     char **argv;
{
  unsigned short newmode;
  struct mode_change *change;
  char *symbolic_mode;
  int errors = 0;
  int optc;

  program_name = argv[0];
  symbolic_mode = NULL;

#ifndef S_ISFIFO
  error (4, 0, "fifo files not supported");
#else
  while ((optc = getopt_long (argc, argv, "m:", longopts, (int *) 0)) != EOF)
    {
      switch (optc)
	{
	case 'm':
	  symbolic_mode = optarg;
	  break;
	default:
	  usage ();
	}
    }

  if (optind == argc)
    usage ();
  
  newmode = 0666 & ~umask (0);
  if (symbolic_mode)
    {
      change = mode_compile (symbolic_mode, 0);
      if (change == MODE_INVALID)
	error (1, 0, "invalid mode");
      else if (change == MODE_MEMORY_EXHAUSTED)
	error (1, 0, "virtual memory exhausted");
      newmode = mode_adjust (newmode, change);
    }

  for (; optind < argc; ++optind)
    {
      strip_trailing_slashes (argv[optind]);
      if (mkfifo (argv[optind], newmode))
	{
	  error (0, errno, "cannot make fifo `%s'", argv[optind]);
	  errors = 1;
	}
    }

  exit (errors);
#endif
}

#ifdef S_ISFIFO
void
usage ()
{
  fprintf (stderr, "\
Usage: %s [-m mode] [--mode=mode] path...\n",
	   program_name);
  exit (1);
}
#endif
