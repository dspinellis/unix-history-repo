/* sum -- checksum and count the blocks in a file
   Copyright (C) 1986, 1989, 1991 Free Software Foundation, Inc.

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

/* Like BSD sum or SysV sum -r, except like SysV sum if -s option is given. */

/* Written by Kayvan Aghaiepour and David MacKenzie. */

#include <stdio.h>
#include <sys/types.h>
#include <getopt.h>
#include "system.h"

int bsd_sum_file ();
int sysv_sum_file ();
void error ();

/* The name this program was run with. */
char *program_name;

/* Nonzero if any of the files read were the standard input. */
int have_read_stdin;

/* Right-rotate 32-bit integer variable C. */
#define ROTATE_RIGHT(c) if ((c) & 01) (c) = ((c) >>1) + 0x8000; else (c) >>= 1;

struct option longopts[] =
{
  {"sysv", 0, NULL, 's'},
  {NULL, 0, NULL, 0}
};

void
main (argc, argv)
     int argc;
     char **argv;
{
  int errors = 0;
  int optc;
  int (*sum_func) () = bsd_sum_file;

  program_name = argv[0];
  have_read_stdin = 0;

  while ((optc = getopt_long (argc, argv, "rs", longopts, (int *) 0)) != -1)
    {
      switch (optc)
	{
	case 'r':		/* For SysV compatibility. */
	  sum_func = bsd_sum_file;
	  break;

	case 's':
	  sum_func = sysv_sum_file;
	  break;

	case '?':
	  fprintf (stderr, "\
Usage: %s [-rs] [--sysv] [file...]\n", argv[0]);
	  exit (1);
	}
    }

  if (optind == argc)
    {
      if ((*sum_func) ("-", 0) < 0)
	errors = 1;
    }
  else
    for (; optind < argc; optind++)
      if ((*sum_func) (argv[optind], 1) < 0)
	errors = 1;

  if (have_read_stdin && fclose (stdin) == EOF)
    error (1, errno, "-");
  exit (errors);
}

/* Calculate and print the rotated checksum and the size in 1K blocks
   of file FILE, or of the standard input if FILE is "-".
   If PRINT_NAME is nonzero, print FILE next to the checksum and size.
   The checksum varies depending on sizeof(int).
   Return 0 if successful, -1 if an error occurs. */

int
bsd_sum_file (file, print_name)
     char *file;
     int print_name;
{
  register FILE *fp;
  register unsigned long checksum = 0; /* The checksum mod 2^16. */
  register long total_bytes = 0; /* The number of bytes. */
  register int ch;		/* Each character read. */

  if (!strcmp (file, "-"))
    {
      fp = stdin;
      have_read_stdin = 1;
    }
  else
    {
      fp = fopen (file, "r");
      if (fp == NULL)
	{
	  error (0, errno, "%s", file);
	  return -1;
	}
    }
 
  /* This algorithm seems to depend on sign extension in `ch' in order to
     give the right results.  Ick.  */
  while ((ch = getc (fp)) != EOF)
    {
      total_bytes++;
      ROTATE_RIGHT (checksum);
      checksum += ch;
      checksum &= 0xffff;	/* Keep it within bounds. */
    }

  if (ferror (fp))
    {
      error (0, errno, "%s", file);
      if (strcmp (file, "-"))
	fclose (fp);
      return -1;
    }

  if (strcmp (file, "-") && fclose (fp) == EOF)
    {
      error (0, errno, "%s", file);
      return -1;
    }

  printf ("%05lu %5ld", checksum, (total_bytes + 1024 - 1) / 1024);
  if (print_name)
    printf (" %s", file);
  putchar ('\n');

  return 0;
}

/* Calculate and print the checksum and the size in 512-byte blocks
   of file FILE, or of the standard input if FILE is "-".
   If PRINT_NAME is nonzero, print FILE next to the checksum and size.
   Return 0 if successful, -1 if an error occurs. */

int
sysv_sum_file (file, print_name)
     char *file;
     int print_name;
{
  int fd;
  unsigned char buf[8192];
  register int bytes_read;
  register unsigned long checksum = 0;
  long total_bytes = 0;

  if (!strcmp (file, "-"))
    {
      fd = 0;
      have_read_stdin = 1;
    }
  else
    {
      fd = open (file, O_RDONLY);
      if (fd == -1)
	{
	  error (0, errno, "%s", file);
	  return -1;
	}
    }

  while ((bytes_read = read (fd, buf, sizeof buf)) > 0)
    {
      register int i;

      for (i = 0; i < bytes_read; i++)
	checksum += buf[i];
      total_bytes += bytes_read;
    }

  if (bytes_read < 0)
    {
      error (0, errno, "%s", file);
      if (strcmp (file, "-"))
	close (fd);
      return -1;
    }

  if (strcmp (file, "-") && close (fd) == -1)
    {
      error (0, errno, "%s", file);
      return -1;
    }

  printf ("%lu %ld", checksum % 0xffff, (total_bytes + 512 - 1) / 512);
  if (print_name)
    printf (" %s", file);
  putchar ('\n');

  return 0;
}
