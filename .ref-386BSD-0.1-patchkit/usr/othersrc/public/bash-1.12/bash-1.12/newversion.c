/* Simple program to make new version numbers for the shell.
   Big deal, but it was getting out of hand to do everything
   in the makefile. */

/* Copyright (C) 1989 Free Software Foundation, Inc.

This file is part of GNU Bash, the Bourne Again SHell.

Bash is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 1, or (at your option) any later
version.

Bash is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with Bash; see the file COPYING.  If not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. */

#include <stdio.h>
char *progname;

main (argc, argv)
     int argc;
     char **argv;
{
  FILE *file, *must_open ();
  float distver = 0.0;
  int dist = 0, build = 0, buildver = 0;
  int dist_inc = 0, build_inc = 0;
  int dot_dist_needs_making = 0;
  int arg_index = 1;

  progname = argv[0];

  while (arg_index < argc && argv[arg_index][0] == '-')
    {
      if (strcmp (&(argv[arg_index][1]), "dist") == 0)
	dist++;
      else if (strcmp (&(argv[arg_index][1]), "build") == 0)
	build++;
      else
	{
	  fprintf (stderr, "Bad flag to %s: expected -dist or -build.\n",
		   argv[0]);
	  exit (1);
	}
      arg_index++;
    }

  if (!get_float_from_file (".distribution", &distver))
    dot_dist_needs_making++;

  if (!dist)
    {
      get_int_from_file (".build", &buildver);
      build_inc++;
    }
  else
    dist_inc++;

  if (dist && arg_index < argc)
    if (1 != sscanf (argv[arg_index], "%f", &distver))
      {
	fprintf (stderr, "Bad input `%s'.  Expected float value for -dist.\n",
		 argv[arg_index]);
	exit (1);
      }
    else
      {
	arg_index++;
	dist_inc = 0;
      }

  if (build && arg_index < argc)
    if (1 != sscanf (argv[arg_index], "%d", &buildver))
      {
	fprintf (stderr, "Bad input `%s'.  Expected int value for -build.\n",
		 argv[arg_index]);
	exit (1);
      }
    else
      {
	arg_index++;
	build_inc = 0;
      }

  if (dot_dist_needs_making && !distver)
    {
      fprintf (stderr, "There is no `.distribution' file for me to infer from.\n");
      exit (1);
    }

  if (build)
    buildver = buildver + 1;
  
  if (dist_inc)
    distver = distver + 0.01;

  file = must_open ("newversion.h", "w");
  fprintf (file, "%s%.2f%s%d\n","\
/* Version control for the shell.  This file gets changed when you say\n\
   `make newversion' to the Makefile.  It is created by newversion.aux. */\n\
\n\
/* The distribution version number of this shell. */\n\
#define DISTVERSION \"", distver, "\"\n\n/* The last built version of this shell. */\n\
#define BUILDVERSION ", buildver);
  fclose (file);

  file = must_open (".build", "w");
  fprintf (file, "%d\n", buildver);
  fclose (file);

  if (dist)
    {
      file = must_open (".distribution", "w");
      fprintf (file, "%.2f\n", distver);
      fclose (file);
    }
  exit (0);
}

get_float_from_file (filename, var)
     char *filename;
     float *var;
{
  FILE *stream;
  int result;

  if ((stream  = fopen (filename, "r")) == (FILE *)NULL)
    return (1);
  result = fscanf (stream, "%f\n", var);
  fclose (stream);
  return (result == 1);
}

get_int_from_file (filename, var)
     char *filename;
     int *var;
{
  FILE *stream;
  int result;

  if ((stream  = fopen (filename, "r")) == (FILE *)NULL)
    return (1);
  result = fscanf (stream, "%d\n", var);
  fclose (stream);
  return (result == 1);
}

FILE *
must_open (name, mode)
     char *name, *mode;
{
  FILE *temp = fopen (name, mode);

  if (!temp)
    {
      fprintf (stderr, "%s: Cannot open `%s' for mode `%s'.\n",
	       progname, name, mode);
      fprintf
	(stderr,
	 "Perhaps you don't have %s permission to the file or directory.\n",
	 (strcmp (mode, "w") == 0) ? "write" : "read");
      exit (3);
    }
  return (temp);
}
