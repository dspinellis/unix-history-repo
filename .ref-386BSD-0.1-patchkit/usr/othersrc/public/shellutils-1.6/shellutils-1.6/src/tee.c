/* tee - read from standard input and write to standard output and files.
   Copyright (C) 1985, 1990, 1991 Free Software Foundation, Inc.

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

/* Mike Parker, Richard M. Stallman, and David MacKenzie */

#include <stdio.h>
#include <sys/types.h>
#include <signal.h>
#include <getopt.h>
#include "system.h"

char *xmalloc ();
int tee ();
void error ();
void xwrite ();

/* If nonzero, append to output files rather than truncating them. */
int append;

/* If nonzero, ignore interrupts. */
int ignore_interrupts;

/* The name that this program was run with. */
char *program_name;

struct option long_options[] =
{
  {"append", 0, NULL, 'a'},
  {"ignore-interrupts", 0, NULL, 'i'},
  {NULL, 0, NULL, 0}
};

void
main (argc, argv)
     int argc;
     char **argv;
{
  int errs;
  int optc;
	
  program_name = argv[0];
  append = 0;
  ignore_interrupts = 0;

  while ((optc = getopt_long (argc, argv, "ai", long_options, (int *) 0))
	 != EOF)
    {
      switch (optc)
	{
	case 'a':
	  append = 1;
	  break;
	case 'i':
	  ignore_interrupts = 1;
	  break;
	default:
	  fprintf (stderr, "\
Usage: %s [-ai] [--append] [--ignore-interrupts] [file...]\n",
		   program_name);
	  exit (1);
	}
    }

  if (ignore_interrupts)
#ifdef _POSIX_VERSION
    {
      struct sigaction sigact;

      sigact.sa_handler = SIG_IGN;
      sigemptyset (&sigact.sa_mask);
      sigact.sa_flags = 0;
      sigaction (SIGINT, &sigact, NULL);
    }
#else				/* !_POSIX_VERSION */
    signal (SIGINT, SIG_IGN);
#endif				/* _POSIX_VERSION */

  errs = tee (argc - optind, &argv[optind]);
  if (close (0) == -1)
    error (1, errno, "standard input");
  if (close (1) == -1)
    error (1, errno, "standard output");
  exit (errs);
}

/* Copy the standard input into each of the NFILES files in FILES
   and into the standard output.
   Return 0 if successful, 1 if any errors occur. */

int
tee (nfiles, files)
     int nfiles;
     char **files;
{
  int *descriptors;
  char buffer[BUFSIZ];
  register int bytes_read, i, ret = 0, mode;

  descriptors = (int *) xmalloc (nfiles * sizeof (int));
  mode = O_WRONLY | O_CREAT;
  if (append)
    mode |= O_APPEND;
  else
    mode |= O_TRUNC;

  for (i = 0; i < nfiles; i++)
    {
      descriptors[i] = open (files[i], mode, 0666);
      if (descriptors[i] == -1)
	{
	  error (0, errno, "%s", files[i]);
	  ret = 1;
	}
    }

  while ((bytes_read = read (0, buffer, sizeof buffer)) > 0)
    {
      xwrite (1, buffer, bytes_read);
      for (i = 0; i < nfiles; i++)
	if (descriptors[i] != -1)
	  xwrite (descriptors[i], buffer, bytes_read);
    }
  if (bytes_read == -1)
    {
      error (0, errno, "read error");
      ret = 1;
    }

  for (i = 0; i < nfiles; i++)
    if (descriptors[i] != -1 && close (descriptors[i]) == -1)
      {
	error (0, errno, "%s", files[i]);
	ret = 1;
      }

  return ret;
}
