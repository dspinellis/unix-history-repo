/* nice -- run a program with modified scheduling priority
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

/* David MacKenzie <djm@ai.mit.edu> */

#include <stdio.h>
#include <getopt.h>
#include <sys/types.h>
#ifndef NICE_PRIORITY
#include <sys/time.h>
#include <sys/resource.h>
#endif
#include "system.h"

int isinteger ();
void error ();
void usage ();

/* The name this program was run with. */
char *program_name;

struct option longopts[] =
{
  {"adjustment", 1, NULL, 'n'},
  {NULL, 0, NULL, 0}
};

void
main (argc, argv)
     int argc;
     char **argv;
{
  int current_priority;
  int adjustment = 0;
  int minusflag = 0;
  int adjustment_given = 0;
  int optc;

  program_name = argv[0];

  while ((optc = getopt_long (argc, argv, "+0123456789-n:", longopts,
			      (int *) 0)) != EOF)
    {
      switch (optc)
	{
	case '?':
	  usage ();
	  
	case 'n':
	  if (!isinteger (optarg))
	    error (1, 0, "invalid priority `%s'", optarg);
	  adjustment = atoi (optarg);
	  adjustment_given = 1;
	  break;

	case '-':
	  minusflag = 1;
	  break;

	default:
	  adjustment = adjustment * 10 + optc - '0';
	  adjustment_given = 1;
	}
    }

  if (minusflag)
    adjustment = -adjustment;
  if (!adjustment_given)
    adjustment = 10;

  if (optind == argc)
    {
      if (adjustment_given)
	usage ();
      /* No command given; print the priority. */
      errno = 0;
#ifndef NICE_PRIORITY
      current_priority = getpriority (PRIO_PROCESS, 0);
#else
      current_priority = nice (0);
#endif
      if (current_priority == -1 && errno != 0)
	error (1, errno, "cannot get priority");
      printf ("%d\n", current_priority);
      exit (0);
    }

  errno = 0;
#ifndef NICE_PRIORITY
  current_priority = getpriority (PRIO_PROCESS, 0);
#else
  current_priority = nice (0);
#endif
  if (current_priority == -1 && errno != 0)
    error (1, errno, "cannot get priority");

#ifndef NICE_PRIORITY
  if (setpriority (PRIO_PROCESS, 0, current_priority + adjustment))
#else
  if (nice (adjustment) == -1)
#endif
    error (1, errno, "cannot set priority");

  execvp (argv[optind], &argv[optind]);
  error (errno == ENOENT ? 127 : 126, errno, "%s", argv[optind]);
}

/* Return nonzero if S represents a (possibly signed) decimal integer,
   zero if not. */

int
isinteger (s)
     char *s;
{
  if (*s == '-')
    ++s;
  if (*s == 0)
    return 0;
  while (*s)
    {
      if (*s < '0' || *s > '9')
	return 0;
      ++s;
    }
  return 1;
}

void
usage ()
{
  fprintf (stderr, "\
Usage: %s [-n adjustment] [-adjustment] [--adjustment=adjustment]\n\
       [command [arg...]]\n",
	   program_name);
  exit (1);
}
