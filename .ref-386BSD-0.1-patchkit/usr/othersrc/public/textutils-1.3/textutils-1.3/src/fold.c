/* fold -- wrap each input line to fit in specified width.
   Copyright (C) 1991 Free Software Foundation, Inc.

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

/* Written by David MacKenzie. */

#define _GNU_SOURCE
#include <ctype.h>
#ifndef isblank
#define isblank(c) ((c) == ' ' || (c) == '\t')
#endif
#include <stdio.h>
#include <getopt.h>
#include <sys/types.h>
#include "system.h"

char *xrealloc ();
int adjust_column ();
int fold_file ();
void error ();

/* If nonzero, try to break on whitespace. */
int break_spaces;

/* If nonzero, count bytes, not column positions. */
int count_bytes;

/* If nonzero, at least one of the files we read was standard input. */
int have_read_stdin;

/* The name this program was run with. */
char *program_name;

struct option longopts[] =
{
  {"bytes", 0, NULL, 'b'},
  {"spaces", 0, NULL, 's'},
  {"width", 1, NULL, 'w'},
  {NULL, 0, NULL, 0}
};

void
main (argc, argv)
     int argc;
     char **argv;
{
  int width = 80;
  int i;
  int optc;
  int errs = 0;

  program_name = argv[0];
  break_spaces = count_bytes = have_read_stdin = 0;

  while ((optc = getopt_long (argc, argv, "bsw:", longopts, (int *) 0))
	 != EOF)
    {
      switch (optc)
	{
	case 'b':		/* Count bytes rather than columns. */
	  count_bytes = 1;
	  break;

	case 's':		/* Break at word boundaries. */
	  break_spaces = 1;
	  break;

	case 'w':		/* Line width. */
	  width = atoi (optarg);
	  if (width < 1)
	    error (1, 0, "%s: invalid line width", optarg);
	  break;

	default:
	  fprintf (stderr, "\
Usage: %s [-bs] [-w width] [--bytes] [--spaces] [--width=width] [file...]\n",
		   argv[0]);
	  exit (1);
	}
    }

  if (argc == optind)
    errs |= fold_file ("-", width);
  else
    for (i = optind; i < argc; i++)
      errs |= fold_file (argv[i], width);

  if (have_read_stdin && fclose (stdin) == EOF)
    error (1, errno, "-");
  if (fclose (stdout) == EOF)
    error (1, errno, "write error");

  exit (errs);
}

/* Fold file FILENAME, or standard input if FILENAME is "-",
   to stdout, with maximum line length WIDTH.
   Return 0 if successful, 1 if an error occurs. */

int
fold_file (filename, width)
     char *filename;
     int width;
{
  FILE *istream;
  register int c;
  int column = 0;		/* Screen column where next char will go. */
  int offset_out = 0;		/* Index in `line_out' for next char. */
  static char *line_out = NULL;
  static size_t allocated_out = 0;

  if (!strcmp (filename, "-"))
    {
      istream = stdin;
      have_read_stdin = 1;
    }
  else
    istream = fopen (filename, "r");

  if (istream == NULL)
    {
      error (0, errno, "%s", filename);
      return 1;
    }

  while ((c = getc (istream)) != EOF)
    {
      if (offset_out + 1 >= allocated_out)
	{
	  allocated_out += 1024;
	  line_out = xrealloc (line_out, allocated_out);
	}
      
      if (c == '\n')
	{
	  line_out[offset_out++] = c;
	  fwrite (line_out, sizeof (char), offset_out, stdout);
	  column = offset_out = 0;
	  continue;
	}

    rescan:
      column = adjust_column (column, c);

      if (column >= width)
	{
	  /* This character would make the line too long.
	     Print the line plus a newline, and make this character
	     start the next line. */
	  if (break_spaces)
	    {
	      /* Look for the last blank. */
	      int logical_end;

	      for (logical_end = offset_out - 1; logical_end >= 0;
		   logical_end--)
		if (isblank (line_out[logical_end]))
		  break;
	      if (logical_end >= 0)
		{
		  int i;

		  /* Found a blank.  Don't output the part after it. */
		  logical_end++;
		  fwrite (line_out, sizeof (char), logical_end, stdout);
		  putchar ('\n');
		  /* Move the remainder to the beginning of the next line.
		     The areas being copied here might overlap. */
		  bcopy (line_out + logical_end, line_out,
			 offset_out - logical_end);
		  offset_out -= logical_end;
		  for (column = i = 0; i < offset_out; i++)
		    column = adjust_column (column, line_out[i]);
		  goto rescan;
		}
	    }
	  line_out[offset_out++] = '\n';
	  fwrite (line_out, sizeof (char), offset_out, stdout);
	  column = offset_out = 0;
	  goto rescan;
	}

      line_out[offset_out++] = c;
    }

  if (offset_out)
    fwrite (line_out, sizeof (char), offset_out, stdout);

  if (ferror (istream))
    {
      error (0, errno, "%s", filename);
      if (strcmp (filename, "-"))
	fclose (istream);
      return 1;
    }
  if (strcmp (filename, "-") && fclose (istream) == EOF)
    {
      error (0, errno, "%s", filename);
      return 1;
    }

  if (ferror (stdout))
    {
      error (0, errno, "write error");
      return 1;
    }

  return 0;
}

/* Assuming the current column is COLUMN, return the column that
   printing C will move the cursor to.
   The first column is 0. */

int
adjust_column (column, c)
     int column;
     char c;
{
  if (!count_bytes)
    {
      if (c == '\b')
	{
	  if (column > 0)
	    column--;
	}
      else if (c == '\r')
	column = 0;
      else if (c == '\t')
	column = column + 8 - column % 8;
      else /* if (isprint (c)) */
	column++;
    }
  else
    column++;
  return column;
}
