/* wc - print the number of bytes, words, and lines in files
   Copyright (C) 1985, 1991 Free Software Foundation, Inc.

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

/* Written by Paul Rubin, phr@ocf.berkeley.edu
   and David MacKenzie, djm@gnu.ai.mit.edu. */

#include <stdio.h>
#include <getopt.h>
#include <sys/types.h>
#include "system.h"

/* Size of atomic reads. */
#define BUFFER_SIZE (16 * 1024)

void error ();
void wc ();
void wc_file ();
void write_counts ();

/* Cumulative number of lines, words, and chars in all files so far. */
unsigned long total_lines, total_words, total_chars;

/* Which counts to print. */
int print_lines, print_words, print_chars;

/* Nonzero if we have ever read the standard input. */
int have_read_stdin;

/* The name this program was run with. */
char *program_name;

/* The error code to return to the system. */
int exit_status;

struct option longopts[] =
{
  {"bytes", 0, NULL, 'c'},
  {"chars", 0, NULL, 'c'},
  {"lines", 0, NULL, 'l'},
  {"words", 0, NULL, 'w'},
  {NULL, 0, NULL, 0}
};

void
main (argc, argv)
     int argc;
     char **argv;
{
  int optc;
  int nfiles;

  program_name = argv[0];
  exit_status = 0;
  print_lines = print_words = print_chars = 0;
  total_lines = total_words = total_chars = 0;

  while ((optc = getopt_long (argc, argv, "clw", longopts, (int *) 0)) != EOF)
    switch (optc)
      {
      case 'c':
	print_chars = 1;
	break;

      case 'l':
	print_lines = 1;
	break;

      case 'w':
	print_words = 1;
	break;

      default:
	fprintf (stderr, "\
Usage: %s [-clw] [--bytes] [--chars] [--lines] [--words] [file...]\n", argv[0]);
	exit (1);
      }

  if (print_lines + print_words + print_chars == 0)
    print_lines = print_words = print_chars = 1;

  nfiles = argc - optind;

  if (nfiles == 0)
    {
      have_read_stdin = 1;
      wc (0, "");
    }
  else
    {
      for (; optind < argc; ++optind)
	wc_file (argv[optind]);

      if (nfiles > 1)
	write_counts (total_lines, total_words, total_chars, "total");
    }
  
  if (have_read_stdin && close (0))
    error (1, errno, "-");

  exit (exit_status);
}

void
wc_file (file)
     char *file;
{
  if (!strcmp (file, "-"))
    {
      have_read_stdin = 1;
      wc (0, file);
    }
  else
    {
      int fd = open (file, O_RDONLY);
      if (fd == -1)
	{
	  error (0, errno, "%s", file);
	  exit_status = 1;
	  return;
	}
      wc (fd, file);
      if (close (fd))
	{
	  error (0, errno, "%s", file);
	  exit_status = 1;
	}
    }
}

void
wc (fd, file)
     int fd;
     char *file;
{
  char buf[BUFFER_SIZE];
  register int bytes_read;
  register int in_word = 0;
  register unsigned long lines, words, chars;
  
  lines = words = chars = 0;
  
  while ((bytes_read = read (fd, buf, BUFFER_SIZE)) > 0)
    {
      register char *p = buf;
      do
	{
	  chars++;
	  switch (*p++)
	    {
	    case '\n':
	      lines++;
	      /* Fall through. */
	    case '\r':
	    case '\f':
	    case '\t':
	    case '\v':
	    case ' ':
	      if (in_word)
		{
		  in_word = 0;
		  words++;
		}
	      break;
	    default:
	      in_word = 1;
	      break;
	    }
	} while (--bytes_read);
    }
  if (bytes_read < 0)
    {
      error (0, errno, "%s", file);
      exit_status = 1;
    }
  if (in_word)
    words++;
  write_counts (lines, words, chars, file);
  total_lines += lines;
  total_words += words;
  total_chars += chars;
}

void
write_counts (lc, wc, cc, file)
     unsigned long lc, wc, cc;
     char *file;
{
  if (print_lines)
    printf ("%7lu", lc);
  if (print_words)
    {
      if (print_lines)
	putchar (' ');
      printf ("%7lu", wc);
    }
  if (print_chars)
    {
      if (print_lines || print_words)
	putchar (' ');
      printf ("%7lu", cc);
    }
  if (*file)
    printf (" %s", file);
  putchar ('\n');
}
