/* uniq -- remove duplicate lines from a sorted file
   Copyright (C) 1986, 1991 Free Software Foundation, Inc.

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

/* Written by Richard Stallman and David MacKenzie. */

#define _GNU_SOURCE
#include <ctype.h>
#ifndef isblank
#define isblank(c) ((c) == ' ' || (c) == '\t')
#endif
#include <stdio.h>
#include <getopt.h>
#include <sys/types.h>
#include "system.h"
#include "linebuffer.h"

#define min(x, y) ((x) < (y) ? (x) : (y))

char *find_field ();
int different ();
void check_file ();
void error ();
void usage ();
void writeline ();

/* Number of fields to skip on each line when doing comparisons. */
int skip_fields;

/* Number of chars to skip after skipping any fields. */
int skip_chars;

/* Number of chars to compare; if 0, compare the whole lines. */
int check_chars;

enum countmode
{
  count_occurrences,		/* -c Print count before output lines. */
  count_none			/* Default.  Do not print counts. */
};

/* Whether and how to precede the output lines with a count of the number of
   times they occurred in the input. */
enum countmode countmode;

enum output_mode
{
  output_repeated,		/* -d Only lines that are repeated. */
  output_unique,		/* -u Only lines that are not repeated. */
  output_all			/* Default.  Print first copy of each line. */
};

/* Which lines to output. */
enum output_mode mode;

/* The name this program was run with. */
char *program_name;

struct option longopts[] =
{
  {"count", 0, NULL, 'c'},
  {"repeated", 0, NULL, 'd'},
  {"unique", 0, NULL, 'u'},
  {"skip-fields", 1, NULL, 'f'},
  {"skip-chars", 1, NULL, 's'},
  {"check-chars", 1, NULL, 'w'},
  {NULL, 0, NULL, 0}
};

void
main (argc, argv)
     int argc;
     char *argv[];
{
  int optc;
  char *infile = "-", *outfile = "-";

  program_name = argv[0];
  skip_chars = 0;
  skip_fields = 0;
  check_chars = 0;
  mode = output_all;
  countmode = count_none;

  while ((optc = getopt_long (argc, argv, "0123456789cdf:s:uw:", longopts,
			      (int *) 0)) != EOF)
    {
      switch (optc)
	{
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	  skip_fields = skip_fields * 10 + optc - '0';
	  break;

	case 'c':
	  countmode = count_occurrences;
	  break;

	case 'd':
	  mode = output_repeated;
	  break;

	case 'f':		/* Like '-#'. */
	  skip_fields = atoi (optarg);
	  break;

	case 's':		/* Like '+#'. */
	  skip_chars = atoi (optarg);
	  break;

	case 'u':
	  mode = output_unique;
	  break;
	  
	case 'w':
	  check_chars = atoi (optarg);
	  break;
	  
	default:
	  usage ();
	}
    }

  while (optind < argc && argv[optind][0] == '+')
    skip_chars = atoi (argv[optind++]);

  if (optind < argc)
    infile = argv[optind++];

  if (optind < argc)
    outfile = argv[optind++];

  if (optind < argc)
    usage ();			/* Extra arguments. */

  check_file (infile, outfile);

  exit (0);
}

/* Process input file INFILE with output to OUTFILE.
   If either is "-", use the standard I/O stream for it instead. */

void
check_file (infile, outfile)
     char *infile, *outfile;
{
  FILE *istream;
  FILE *ostream;
  struct linebuffer lb1, lb2;
  struct linebuffer *thisline, *prevline, *exch;
  char *prevfield, *thisfield;
  int prevlen, thislen;
  int match_count = 0;

  if (!strcmp (infile, "-"))
    istream = stdin;
  else
    istream = fopen (infile, "r");
  if (istream == NULL)
    error (1, errno, "%s", infile);

  if (!strcmp (outfile, "-"))
    ostream = stdout;
  else
    ostream = fopen (outfile, "w");
  if (ostream == NULL)
    error (1, errno, "%s", outfile);

  thisline = &lb1;
  prevline = &lb2;

  initbuffer (thisline);
  initbuffer (prevline);

  if (readline (prevline, istream) == 0)
    goto closefiles;
  prevfield = find_field (prevline);
  prevlen = prevline->length - (prevfield - prevline->buffer);

  while (!feof (istream))
    {
      if (readline (thisline, istream) == 0)
	break;
      thisfield = find_field (thisline);
      thislen = thisline->length - (thisfield - thisline->buffer);
      if (!different (thisfield, prevfield, thislen, prevlen))
	match_count++;
      else
	{
	  writeline (prevline, ostream, match_count);
	  match_count = 0;

	  exch = prevline;
	  prevline = thisline;
	  thisline = exch;
	  prevfield = thisfield;
	  prevlen = thislen;
	}
    }

  writeline (prevline, ostream, match_count);

 closefiles:
  if (ferror (istream) || fclose (istream) == EOF)
    error (1, errno, "error reading %s", infile);

  if (ferror (ostream) || fclose (ostream) == EOF)
    error (1, errno, "error writing %s", outfile);

  free (lb1.buffer);
  free (lb2.buffer);
}

/* Given a linebuffer LINE,
   return a pointer to the beginning of the line's field to be compared. */

char *
find_field (line)
     struct linebuffer *line;
{
  register int count;
  register char *lp = line->buffer;
  register int size = line->length;
  register int i = 0;

  for (count = 0; count < skip_fields && i < size; count++)
    {
      while (i < size && isblank (lp[i]))
	i++;
      while (i < size && !isblank (lp[i]))
	i++;
    }

  for (count = 0; count < skip_chars && i < size; count++)
    i++;

  return lp + i;
}

/* Return zero if two strings OLD and NEW match, nonzero if not.
   OLD and NEW point not to the beginnings of the lines
   but rather to the beginnings of the fields to compare.
   OLDLEN and NEWLEN are their lengths. */

int
different (old, new, oldlen, newlen)
     char *old;
     char *new;
     int oldlen;
     int newlen;
{
  register int order;

  if (check_chars)
    {
      if (oldlen > check_chars)
	oldlen = check_chars;
      if (newlen > check_chars)
	newlen = check_chars;
    }
  order = memcmp (old, new, min (oldlen, newlen));
  if (order == 0)
    return oldlen - newlen;
  return order;
}

/* Output the line in linebuffer LINE to stream STREAM
   provided that the switches say it should be output.
   If requested, print the number of times it occurred, as well;
   LINECOUNT + 1 is the number of times that the line occurred. */

void
writeline (line, stream, linecount)
     struct linebuffer *line;
     FILE *stream;
     int linecount;
{
  if ((mode == output_unique && linecount != 0)
      || (mode == output_repeated && linecount == 0))
    return;

  if (countmode == count_occurrences)
    fprintf (stream, "%7d\t", linecount + 1);

  fwrite (line->buffer, sizeof (char), line->length, stream);
  putc ('\n', stream);
}

void
usage ()
{
  fprintf (stderr, "\
Usage: %s [-cdu] [-f skip-fields] [-s skip-chars] [-w check-chars]\n\
       [-#skip-fields] [+#skip-chars] [--count] [--repeated] [--unique]\n\
       [--skip-fields=skip-fields] [--skip-chars=skip-chars]\n\
       [--check-chars=check-chars] [infile] [outfile]\n",
	   program_name);
  exit (1);
}
