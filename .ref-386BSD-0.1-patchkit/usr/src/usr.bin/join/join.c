/* join - join lines of two files on a common field
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
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   Written by Mike Haertel, mike@gnu.ai.mit.edu. */

#define _GNU_SOURCE
#include <ctype.h>
#ifndef isblank
#define isblank(c) ((c) == ' ' || (c) == '\t')
#endif
#include <stdio.h>
#include <sys/types.h>
#include "getopt.h"
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
/*#include <stdlib.h>*/
#include <fcntl.h>

#ifdef isascii
#define ISSPACE(c) (isascii(c) && isspace(c))
#define ISDIGIT(c) (isascii(c) && isdigit(c))
#else
#define ISSPACE(c) isspace(c)
#define ISDIGIT(c) isdigit(c)
#endif

void error (int, int, char *, ...);
static void usage ();

#define min(A, B) ((A) < (B) ? (A) : (B))

/* An element of the list describing the format of each
   output line. */
struct outlist
{
  int file;			/* File to take field from (1 or 2). */
  int field;			/* Field number to print. */
  struct outlist *next;
};

/* A field of a line. */
struct field
{
  char *beg;			/* First character in field. */
  char *lim;			/* Character after last character in field. */
};

/* A line read from an input file.  Newlines are not stored. */
struct line
{
  char *beg;			/* First character in line. */
  char *lim;			/* Character after last character in line. */
  int nfields;			/* Number of elements in `fields'. */
  struct field *fields;
};

/* One or more consecutive lines read from a file that all have the
   same join field value. */
struct seq
{
  int count;			/* Elements used in `lines'. */
  int alloc;			/* Elements allocated in `lines'. */
  struct line *lines;
};

/* If nonzero, print unpairable lines in file 1 or 2. */
static int print_unpairables_1, print_unpairables_2;

/* If nonzero, print pairable lines. */
static int print_pairables;

/* Empty output field filler. */
static char *empty_filler;

/* Field to join on. */
static int join_field_1, join_field_2;

/* List of fields to print. */
struct outlist *outlist;

/* Last element in `outlist', where a new element can be added. */
struct outlist *outlist_end;

/* Tab character separating fields; if this is NUL fields are separated
   by any nonempty string of white space, otherwise by exactly one
   tab character. */
static char tab;

/* The name this program was run with. */
char *program_name;

/* Fill in the `fields' structure in LINE. */

static void
xfields (line)
     struct line *line;
{
  static int nfields = 2;
  int i;
  register char *ptr, *lim;

  line->fields = (struct field *) malloc (nfields * sizeof (struct field));

  ptr = line->beg;
  lim = line->lim;

  for (i = 0; ptr < lim; ++i)
    {
      if (i == nfields)
	{
	  nfields *= 2;
	  line->fields = (struct field *)
	    realloc ((char *) line->fields, nfields * sizeof (struct field));
	}
      if (tab)
	{
	  line->fields[i].beg = ptr;
	  while (ptr < lim && *ptr != tab)
	    ++ptr;
	  line->fields[i].lim = ptr;
	  if (ptr < lim)
	    ++ptr;
	}
      else
	{
	  line->fields[i].beg = ptr;
	  while (ptr < lim && !ISSPACE (*ptr))
	    ++ptr;
	  line->fields[i].lim = ptr;
	  while (ptr < lim && ISSPACE (*ptr))
	    ++ptr;
	}
    }

  line->nfields = i;
}

/* Read a line from FP into LINE and split it into fields.
   Return 0 if EOF, 1 otherwise. */

static int
get_line (fp, line)
     FILE *fp;
     struct line *line;
{
  static int linesize = 80;
  int c, i;
  char *ptr;

  if (feof (fp))
    return 0;

  ptr = malloc (linesize);

  for (i = 0; (c = getc (fp)) != EOF && c != '\n'; ++i)
    {
      if (i == linesize)
	{
	  linesize *= 2;
	  ptr = realloc (ptr, linesize);
	}
      ptr[i] = c;
    }

  if (c == EOF && i == 0)
    {
      free (ptr);
      return 0;
    }

  line->beg = ptr;
  line->lim = line->beg + i;
  xfields (line);
  return 1;
}

static void
freeline (line)
     struct line *line;
{
  free ((char *) line->fields);
  free (line->beg);
}

static void
initseq (seq)
     struct seq *seq;
{
  seq->count = 0;
  seq->alloc = 1;
  seq->lines = (struct line *) malloc (seq->alloc * sizeof (struct line));
}

/* Read a line from FP and add it to SEQ.  Return 0 if EOF, 1 otherwise. */

static int
getseq (fp, seq)
     FILE *fp;
     struct seq *seq;
{
  if (seq->count == seq->alloc)
    {
      seq->alloc *= 2;
      seq->lines = (struct line *)
	realloc ((char *) seq->lines, seq->alloc * sizeof (struct line));
    }

  if (get_line (fp, &seq->lines[seq->count]))
    {
      ++seq->count;
      return 1;
    }
  return 0;
}

static void
delseq (seq)
     struct seq *seq;
{
  free ((char *) seq->lines);
}

/* Return <0 if the join field in LINE1 compares less than the one in LINE2;
   >0 if it compares greater; 0 if it compares equal. */

static int
keycmp (line1, line2)
     struct line *line1;
     struct line *line2;
{
  char *beg1, *beg2;		/* Start of field to compare in each file. */
  int len1, len2;		/* Length of fields to compare. */
  int diff;

  if (join_field_1 < line1->nfields)
    {
      beg1 = line1->fields[join_field_1].beg;
      len1 = line1->fields[join_field_1].lim
	- line1->fields[join_field_1].beg;
    }
  else
    {
      beg1 = NULL;
      len1 = 0;
    }

  if (join_field_2 < line2->nfields)
    {
      beg2 = line2->fields[join_field_2].beg;
      len2 = line2->fields[join_field_2].lim
	- line2->fields[join_field_2].beg;
    }
  else
    {
      beg2 = NULL;
      len2 = 0;
    }

  if (len1 == 0)
    return len2 == 0 ? 0 : -1;
  if (len2 == 0)
    return 1;
  diff = memcmp (beg1, beg2, min (len1, len2));
  if (diff)
    return diff;
  return len1 - len2;
}

/* Print field N of LINE if it exists and is nonempty, otherwise
   `empty_filler' if it is nonempty. */

static void
prfield (n, line)
     int n;
     struct line *line;
{
  int len;

  if (n < line->nfields)
    {
      len = line->fields[n].lim - line->fields[n].beg;
      if (len)
	fwrite (line->fields[n].beg, 1, len, stdout);
      else if (empty_filler)
	fputs (empty_filler, stdout);
    }
  else if (empty_filler)
    fputs (empty_filler, stdout);
}

/* Print LINE, with its fields separated by `tab'. */

static void
prline (line)
     struct line *line;
{
  int i;

  for (i = 0; i < line->nfields; ++i)
    {
      prfield (i, line);
      if (i == line->nfields - 1)
	putchar ('\n');
      else
	putchar (tab ? tab : ' ');
    }
}

/* Print the join of LINE1 and LINE2. */

static void
prjoin (line1, line2)
     struct line *line1;
     struct line *line2;
{
  if (outlist)
    {
      struct outlist *o;

      prfield (outlist->field - 1, outlist->file == 1 ? line1 : line2);
      for (o = outlist->next; o; o = o->next)
	{
	  putchar (tab ? tab : ' ');
	  prfield (o->field - 1, o->file == 1 ? line1 : line2);
	}
      putchar ('\n');
    }
  else
    {
      int i;

      prfield (join_field_1, line1);
      for (i = 0; i < join_field_1 && i < line1->nfields; ++i)
	{
	  putchar (tab ? tab : ' ');
	  prfield (i, line1);
	}
      for (i = join_field_1 + 1; i < line1->nfields; ++i)
	{
	  putchar (tab ? tab : ' ');
	  prfield (i, line1);
	}

      for (i = 0; i < join_field_2 && i < line2->nfields; ++i)
	{
	  putchar (tab ? tab : ' ');
	  prfield (i, line2);
	}
      for (i = join_field_2 + 1; i < line2->nfields; ++i)
	{
	  putchar (tab ? tab : ' ');
	  prfield (i, line2);
	}
      putchar ('\n');
    }
}

/* Print the join of the files in FP1 and FP2. */

static void
join (fp1, fp2)
     FILE *fp1;
     FILE *fp2;
{
  struct seq seq1, seq2;
  struct line line;
  int diff, i, j, eof1, eof2;

  /* Read the first line of each file. */
  initseq (&seq1);
  getseq (fp1, &seq1);
  initseq (&seq2);
  getseq (fp2, &seq2);

  while (seq1.count && seq2.count)
    {
      diff = keycmp (&seq1.lines[0], &seq2.lines[0]);
      if (diff < 0)
	{
	  if (print_unpairables_1)
	    prline (&seq1.lines[0]);
	  freeline (&seq1.lines[0]);
	  seq1.count = 0;
	  getseq (fp1, &seq1);
	  continue;
	}
      if (diff > 0)
	{
	  if (print_unpairables_2)
	    prline (&seq2.lines[0]);
	  freeline (&seq2.lines[0]);
	  seq2.count = 0;
	  getseq (fp2, &seq2);
	  continue;
	}

      /* Keep reading lines from file1 as long as they continue to
	 match the current line from file2. */
      eof1 = 0;
      do
	if (!getseq (fp1, &seq1))
	  {
	    eof1 = 1;
	    ++seq1.count;
	    break;
	  }
      while (!keycmp (&seq1.lines[seq1.count - 1], &seq2.lines[0]));

      /* Keep reading lines from file2 as long as they continue to
	 match the current line from file1. */
      eof2 = 0;
      do
	if (!getseq (fp2, &seq2))
	  {
	    eof2 = 1;
	    ++seq2.count;
	    break;
	  }
      while (!keycmp (&seq1.lines[0], &seq2.lines[seq2.count - 1]));

      if (print_pairables)
	{
	  for (i = 0; i < seq1.count - 1; ++i)
	    for (j = 0; j < seq2.count - 1; ++j)
	      prjoin (&seq1.lines[i], &seq2.lines[j]);
	}

      for (i = 0; i < seq1.count - 1; ++i)
	freeline (&seq1.lines[i]);
      if (!eof1)
	{
	  seq1.lines[0] = seq1.lines[seq1.count - 1];
	  seq1.count = 1;
	}
      else
	seq1.count = 0;

      for (i = 0; i < seq2.count - 1; ++i)
	freeline (&seq2.lines[i]);
      if (!eof2)
	{
	  seq2.lines[0] = seq2.lines[seq2.count - 1];
	  seq2.count = 1;
	}
      else
	seq2.count = 0;
    }

  if (print_unpairables_1 && seq1.count)
    {
      prline (&seq1.lines[0]);
      freeline (&seq1.lines[0]);
      while (get_line (fp1, &line))
	{
	  prline (&line);
	  freeline (&line);
	}
    }

  if (print_unpairables_2 && seq2.count)
    {
      prline (&seq2.lines[0]);
      freeline (&seq2.lines[0]);
      while (get_line (fp2, &line))
	{
	  prline (&line);
	  freeline (&line);
	}
    }

  delseq (&seq1);
  delseq (&seq2);
}

/* Add a field spec for field FIELD of file FILE to `outlist' and return 1,
   unless either argument is invalid; then just return 0. */

static int
add_field (file, field)
     int file;
     int field;
{
  struct outlist *o;

  if (file < 1 || file > 2 || field < 1)
    return 0;
  o = (struct outlist *) malloc (sizeof (struct outlist));
  o->file = file;
  o->field = field;
  o->next = NULL;

  /* Add to the end of the list so the fields are in the right order. */
  if (outlist == NULL)
    outlist = o;
  else
    outlist_end->next = o;
  outlist_end = o;

  return 1;
}

/* Add the comma or blank separated field spec(s) in STR to `outlist'.
   Return the number of fields added. */

static int
add_field_list (str)
     char *str;
{
  int added = 0;
  int file = -1, field = -1;
  int dot_found = 0;

  for (; *str; str++)
    {
      if (*str == ',' || isblank (*str))
	{
	  added += add_field (file, field);
	  file = field = -1;
	  dot_found = 0;
	}
      else if (*str == '.')
	dot_found = 1;
      else if (ISDIGIT (*str))
	{
	  if (!dot_found)
	    {
	      if (file == -1)
		file = 0;
	      file = file * 10 + *str - '0';
	    }
	  else
	    {
	      if (field == -1)
		field = 0;
	      field = field * 10 + *str - '0';
	    }
	}
      else
	return 0;
    }

  added += add_field (file, field);
  return added;
}

/* When using getopt_long_only, no long option can start with
   a character that is a short option. */
static struct option longopts[] =
{
  {"j", 1, NULL, 'j'},
  {"j1", 1, NULL, '1'},
  {"j2", 1, NULL, '2'},
  {NULL, 0, NULL, 0}
};

void
main (argc, argv)
     int argc;
     char *argv[];
{
  char *names[2];
  FILE *fp1, *fp2;
  int optc, prev_optc = 0, nfiles, val;

  program_name = argv[0];
  nfiles = 0;
  print_pairables = 1;

  while ((optc = getopt_long_only (argc, argv, "-a:e:1:2:o:t:v:", longopts,
				   (int *) 0)) != EOF)
    {
      switch (optc)
	{
	case 'a':
	  val = atoi (optarg);
	  if (val == 1)
	    print_unpairables_1 = 1;
	  else if (val == 2)
	    print_unpairables_2 = 1;
	  else
	    error (2, 0, "invalid file number for `-a'");
	  break;

	case 'e':
	  empty_filler = optarg;
	  break;

	case '1':
	  val = atoi (optarg);
	  if (val <= 0)
	    error (2, 0, "invalid field number for `-1'");
	  join_field_1 = val - 1;
	  break;

	case '2':
	  val = atoi (optarg);
	  if (val <= 0)
	    error (2, 0, "invalid field number for `-2'");
	  join_field_2 = val - 1;
	  break;

	case 'j':
	  val = atoi (optarg);
	  if (val <= 0)
	    error (2, 0, "invalid field number for `-j'");
	  join_field_1 = join_field_2 = val - 1;
	  break;

	case 'o':
	  if (add_field_list (optarg) == 0)
	    error (2, 0, "invalid field list for `-o'");
	  break;

	case 't':
	  tab = *optarg;
	  break;

	case 'v':
	  val = atoi (optarg);
	  if (val == 1)
	    print_unpairables_1 = 1;
	  else if (val == 2)
	    print_unpairables_2 = 1;
	  else
	    error (2, 0, "invalid file number for `-v'");
	  print_pairables = 0;
	  break;

	case 1:			/* Non-option argument. */
	  if (prev_optc == 'o')
	    {
	      /* Might be continuation of args to -o. */
	      if (add_field_list (optarg) > 0)
		continue;	/* Don't change `prev_optc'. */
	    }

	  if (nfiles > 1)
	    usage ();
	  names[nfiles++] = optarg;
	  break;

	case '?':
	  usage ();
	}
      prev_optc = optc;
    }
  
  if (nfiles != 2)
    usage ();

  fp1 = strcmp (names[0], "-") ? fopen (names[0], "r") : stdin;
  if (!fp1)
    error (1, errno, "%s", names[0]);
  fp2 = strcmp (names[1], "-") ? fopen (names[1], "r") : stdin;
  if (!fp2)
    error (1, errno, "%s", names[1]);
  if (fp1 == fp2)
    error (1, errno, "both files cannot be standard input");
  join (fp1, fp2);

  if ((fp1 == stdin || fp2 == stdin) && fclose (stdin) == EOF)
    error (1, errno, "-");
  if (ferror (stdout) || fclose (stdout) == EOF)
    error (1, 0, "write error");

  exit (0);
}

static void
usage ()
{
  fprintf (stderr, "\
Usage: %s [-a 1|2] [-v 1|2] [-e empty-string] [-o field-list...] [-t char]\n\
       [-j[1|2] field] [-1 field] [-2 field] file1 file2\n",
	   program_name);
  exit (1);
}

/* error.c -- error handler for noninteractive utilities
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

/* David MacKenzie */


#include <stdarg.h>
#define VA_START(args, lastarg) va_start(args, lastarg)

/* Print the program name and error message MESSAGE, which is a printf-style
   format string with optional args.
   If ERRNUM is nonzero, print its corresponding system error message.
   Exit with status STATUS if it is nonzero. */
/* VARARGS */
void
error (int status, int errnum, char *message, ...)
{
  extern char *program_name;
  va_list args;

  fprintf (stderr, "%s: ", program_name);
  va_start (args, message);
  vfprintf (stderr, message, args);
  va_end (args);
  if (errnum)
    fprintf (stderr, ": %s", strerror (errnum));
  putc ('\n', stderr);
  fflush (stderr);
  if (status)
    exit (status);
}
