/* head -- output first part of file(s)
   Copyright (C) 1989, 1990, 1991 Free Software Foundation, Inc.

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
   -b			Print first N 512-byte blocks.
   -c, --bytes=N[bkm]	Print first N bytes
			[or 512-byte blocks, kilobytes, or megabytes].
   -k			Print first N kilobytes.
   -N, -l, -n, --lines=N	Print first N lines.
   -m			Print first N megabytes.
   -q, --quiet, --silent	Never print filename headers.
   -v, --verbose		Always print filename headers.

   Reads from standard input if no files are given or when a filename of
   ``-'' is encountered.
   By default, filename headers are printed only if more than one file
   is given.
   By default, prints the first 10 lines (head -n 10).

   David MacKenzie <djm@ai.mit.edu> */

#include <stdio.h>
#include <getopt.h>
#include <ctype.h>
#include <sys/types.h>
#include "system.h"

#ifdef isascii
#define ISDIGIT(c) (isascii ((c)) && isdigit ((c)))
#else
#define ISDIGIT(c) (isdigit ((c)))
#endif

/* Number of lines/chars/blocks to head. */
#define DEFAULT_NUMBER 10

/* Size of atomic reads. */
#define BUFSIZE (512 * 8)

/* Number of bytes per item we are printing.
   If 0, head in lines. */
int unit_size;

/* If nonzero, print filename headers. */
int print_headers;

/* When to print the filename banners. */
enum header_mode
{
  multiple_files, always, never
};

int head ();
int head_bytes ();
int head_file ();
int head_lines ();
long atou ();
void error ();
void parse_unit ();
void usage ();
void write_header ();
void xwrite ();

/* The name this program was run with. */
char *program_name;

/* Have we ever read standard input?  */
int have_read_stdin;

struct option long_options[] =
{
  {"bytes", 1, NULL, 'c'},
  {"lines", 1, NULL, 'n'},
  {"quiet", 0, NULL, 'q'},
  {"silent", 0, NULL, 'q'},
  {"verbose", 0, NULL, 'v'},
  {NULL, 0, NULL, 0}
};

void
main (argc, argv)
     int argc;
     char **argv;
{
  enum header_mode header_mode = multiple_files;
  int exit_status = 0;
  long number = -1;		/* Number of items to print (-1 if undef.). */
  int c;			/* Option character. */

  program_name = argv[0];
  have_read_stdin = 0;
  unit_size = 0;
  print_headers = 0;

  if (argc > 1 && argv[1][0] == '-' && ISDIGIT (argv[1][1]))
    {
      /* Old option syntax; a dash, one or more digits, and one or
	 more option letters.  Move past the number. */
      for (number = 0, ++argv[1]; ISDIGIT (*argv[1]); ++argv[1])
	number = number * 10 + *argv[1] - '0';
      /* Parse any appended option letters. */
      while (*argv[1])
	{
	  switch (*argv[1])
	    {
	    case 'b':
	      unit_size = 512;
	      break;

	    case 'c':
	      unit_size = 1;
	      break;

	    case 'k':
	      unit_size = 1024;
	      break;

	    case 'l':
	      unit_size = 0;
	      break;

	    case 'm':
	      unit_size = 1048576;
	      break;

	    case 'q':
	      header_mode = never;
	      break;

	    case 'v':
	      header_mode = always;
	      break;

	    default:
	      error (0, 0, "unrecognized option `-%c'", *argv[1]);
	      usage ();
	    }
	  ++argv[1];
	}
      /* Make the options we just parsed invisible to getopt. */
      argv[1] = argv[0];
      argv++;
      argc--;
    }

  while ((c = getopt_long (argc, argv, "c:n:qv", long_options, (int *) 0))
	 != EOF)
    {
      switch (c)
	{
	case 'c':
	  unit_size = 1;
	  parse_unit (optarg);
	  goto getnum;
	case 'n':
	  unit_size = 0;
	getnum:
	  number = atou (optarg);
	  if (number == -1)
	    error (1, 0, "invalid number `%s'", optarg);
	  break;

	case 'q':
	  header_mode = never;
	  break;

	case 'v':
	  header_mode = always;
	  break;

	default:
	  usage ();
	}
    }

  if (number == -1)
    number = DEFAULT_NUMBER;

  if (unit_size > 1)
    number *= unit_size;

  if (header_mode == always
      || (header_mode == multiple_files && optind < argc - 1))
    print_headers = 1;

  if (optind == argc)
    exit_status |= head_file ("-", number);

  for (; optind < argc; ++optind)
    exit_status |= head_file (argv[optind], number);

  if (have_read_stdin && close (0) < 0)
    error (1, errno, "-");
  if (close (1) < 0)
    error (1, errno, "write error");

  exit (exit_status);
}

int
head_file (filename, number)
     char *filename;
     long number;
{
  int fd;

  if (!strcmp (filename, "-"))
    {
      have_read_stdin = 1;
      filename = "standard input";
      if (print_headers)
	write_header (filename);
      return head (filename, 0, number);
    }
  else
    {
      fd = open (filename, O_RDONLY);
      if (fd >= 0)
	{
	  int errors;

	  if (print_headers)
	    write_header (filename);
	  errors = head (filename, fd, number);
	  if (close (fd) == 0)
	    return errors;
	}
      error (0, errno, "%s", filename);
      return 1;
    }
}

void
write_header (filename)
     char *filename;
{
  static int first_file = 1;

  if (first_file)
    {
      xwrite (1, "==> ", 4);
      first_file = 0;
    }
  else
    xwrite (1, "\n==> ", 5);
  xwrite (1, filename, strlen (filename));
  xwrite (1, " <==\n", 5);
}

int
head (filename, fd, number)
     char *filename;
     int fd;
     long number;
{
  if (unit_size)
    return head_bytes (filename, fd, number);
  else
    return head_lines (filename, fd, number);
}

int
head_bytes (filename, fd, bytes_to_write)
     char *filename;
     int fd;
     long bytes_to_write;
{
  char buffer[BUFSIZE];
  int bytes_read;

  while (bytes_to_write)
    {
      bytes_read = read (fd, buffer, BUFSIZE);
      if (bytes_read == -1)
	{
	  error (0, errno, "%s", filename);
	  return 1;
	}
      if (bytes_read == 0)
	break;
      if (bytes_read > bytes_to_write)
	bytes_read = bytes_to_write;
      xwrite (1, buffer, bytes_read);
      bytes_to_write -= bytes_read;
    }
  return 0;
}

int
head_lines (filename, fd, lines_to_write)
     char *filename;
     int fd;
     long lines_to_write;
{
  char buffer[BUFSIZE];
  int bytes_read;
  int bytes_to_write;

  while (lines_to_write)
    {
      bytes_read = read (fd, buffer, BUFSIZE);
      if (bytes_read == -1)
	{
	  error (0, errno, "%s", filename);
	  return 1;
	}
      if (bytes_read == 0)
	break;
      bytes_to_write = 0;
      while (bytes_to_write < bytes_read)
	if (buffer[bytes_to_write++] == '\n' && --lines_to_write == 0)
	  break;
      xwrite (1, buffer, bytes_to_write);
    }
  return 0;
}

void
parse_unit (str)
     char *str;
{
  int arglen = strlen (str);

  if (arglen == 0)
    return;

  switch (str[arglen - 1])
    {
    case 'b':
      unit_size = 512;
      str[arglen - 1] = '\0';
      break;
    case 'k':
      unit_size = 1024;
      str[arglen - 1] = '\0';
      break;
    case 'm':
      unit_size = 1048576;
      str[arglen - 1] = '\0';
      break;
    }
}

/* Convert STR, a string of ASCII digits, into an unsigned integer.
   Return -1 if STR does not represent a valid unsigned integer. */

long
atou (str)
     char *str;
{
  int value;

  for (value = 0; ISDIGIT (*str); ++str)
    value = value * 10 + *str - '0';
  return *str ? -1 : value;
}

void
usage ()
{
  fprintf (stderr, "\
Usage: %s [-c N[bkm]] [-n N] [-qv] [--bytes=N[bkm]] [--lines=N]\n\
       [--quiet] [--silent] [--verbose] [file...]\n\
       %s [-Nbcklmqv] [file...]\n", program_name, program_name);
  exit (1);
}
