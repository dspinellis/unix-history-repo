/* cut - remove parts of lines of files
   Copyright (C) 1984 by David M. Ihnat
 
   This program is a total rewrite of the Bell Laboratories Unix(Tm)
   command of the same name, as of System V.  It contains no proprietary
   code, and therefore may be used without violation of any proprietary
   agreements whatsoever.  However, you will notice that the program is
   copyrighted by me.  This is to assure the program does *not* fall
   into the public domain.  Thus, I may specify just what I am now:
   This program may be freely copied and distributed, provided this notice
   remains; it may not be sold for profit without express written consent of
   the author.
   Please note that I recreated the behavior of the Unix(Tm) 'cut' command
   as faithfully as possible; however, I haven't run a full set of regression
   tests.  Thus, the user of this program accepts full responsibility for any
   effects or loss; in particular, the author is not responsible for any losses,
   explicit or incidental, that may be incurred through use of this program.

   I ask that any bugs (and, if possible, fixes) be reported to me when
   possible.  -David Ihnat (312) 784-4544 ignatz@homebru.chi.il.us

   POSIX changes, bug fixes, long-named options, and cleanup
   by David MacKenzie <djm@ai.mit.edu>.

   Options:
   --bytes=byte-list
   -b byte-list			Print only the bytes in positions listed
				in BYTE-LIST.
				Tabs and backspaces are treated like any
				other character; they take up 1 byte.

   --characters=character-list
   -c character-list		Print only characters in positions listed
				in CHARACTER-LIST.
				The same as -b for now, but
				internationalization will change that.
				Tabs and backspaces are treated like any
				other character; they take up 1 character.

   --fields=field-list
   -f field-list		Print only the fields listed in FIELD-LIST.
				Fields are separated by a TAB by default.

   --delimiter=delim
   -d delim			For -f, fields are separated by the first
				character in DELIM instead of TAB.

   -n				Do not split multibyte chars (no-op for now).

   --only-delimited
   -s				For -f, do not print lines that do not contain
				the field separator character.

   The BYTE-LIST, CHARACTER-LIST, and FIELD-LIST are one or more numbers
   or ranges separated by commas.  The first byte, character, and field
   are numbered 1.

   A FILE of `-' means standard input. */

#define _GNU_SOURCE
#include <ctype.h>
#ifndef isblank
#define isblank(c) ((c) == ' ' || (c) == '\t')
#endif
#include <stdio.h>
#include <getopt.h>
#include <sys/types.h>
#include "system.h"

#ifdef isascii
#define ISDIGIT(c) (isascii ((c)) && isdigit ((c)))
#else
#define ISDIGIT(c) (isdigit ((c)))
#endif

char *xmalloc ();
char *xrealloc ();
int set_fields ();
int cut_file ();
void cut_stream ();
void cut_bytes ();
void cut_fields ();
void enlarge_line ();
void error ();
void invalid_list ();
void usage ();

/* The number of elements allocated for the input line
   and the byte or field number.
   Enlarged as necessary. */
int line_size;

/* Processed output buffer. */
char *outbuf;

/* Where to save next char to output. */
char *outbufptr;

/* Raw line buffer for field mode. */
char *inbuf;

/* Where to save next input char. */
char *inbufptr;

/* What can be done about a byte or field. */
enum field_action
{
  field_omit,
  field_output
};

/* In byte mode, which bytes to output.
   In field mode, which `delim'-separated fields to output.
   Both bytes and fields are numbered starting with 1,
   so the first element of `fields' is unused. */
enum field_action *fields;

enum operating_mode
{
  undefined_mode,

  /* Output characters that are in the given bytes. */
  byte_mode,

  /* Output the given delimeter-separated fields. */
  field_mode
};

enum operating_mode operating_mode;

/* If nonzero,
   for field mode, do not output lines containing no delimeter characters. */
int delimited_lines_only;

/* The delimeter character for field mode. */
unsigned char delim;

/* Nonzero if we have ever read standard input. */
int have_read_stdin;

/* The name this program was run with. */
char *program_name;

struct option longopts[] =
{
  {"bytes", 1, 0, 'b'},
  {"characters", 1, 0, 'c'},
  {"fields", 1, 0, 'f'},
  {"delimiter", 1, 0, 'd'},
  {"only-delimited", 0, 0, 's'},
  {0, 0, 0, 0}
};

void
main (argc, argv)
     int argc;
     char **argv;
{
  int optc, exit_status = 0;

  program_name = argv[0];

  line_size = 512;
  operating_mode = undefined_mode;
  delimited_lines_only = 0;
  delim = '\0';
  have_read_stdin = 0;

  fields = (enum field_action *)
    xmalloc (line_size * sizeof (enum field_action));
  outbuf = (char *) xmalloc (line_size);
  inbuf = (char *) xmalloc (line_size);

  for (optc = 0; optc < line_size; optc++)
    fields[optc] = field_omit;

  while ((optc = getopt_long (argc, argv, "b:c:d:f:ns", longopts, (int *) 0))
	 != EOF)
    {
      switch (optc)
	{
	case 'b':
	case 'c':
	  /* Build the byte list. */
	  if (operating_mode != undefined_mode)
	    usage ();
	  operating_mode = byte_mode;
	  if (set_fields (optarg) == 0)
	    error (2, 0, "no fields given");
	  break;

	case 'f':
	  /* Build the field list. */
	  if (operating_mode != undefined_mode)
	    usage ();
	  operating_mode = field_mode;
	  if (set_fields (optarg) == 0)
	    error (2, 0, "no fields given");
	  break;

	case 'd':
	  /* New delimiter. */
	  if (optarg[0] == '\0')
	    error (2, 0, "no delimiter given");
	  if (optarg[1] != '\0')
	    error (2, 0, "delimiter must be a single character");
	  delim = optarg[0];
	  break;

	case 'n':
	  break;

	case 's':
	  delimited_lines_only++;
	  break;

	default:
	  usage ();
	}
    }

  if (operating_mode == undefined_mode)
    usage ();

  if ((delimited_lines_only || delim != '\0') && operating_mode != field_mode)
    usage ();

  if (delim == '\0')
    delim = '\t';

  if (optind == argc)
    exit_status |= cut_file ("-");
  else
    for (; optind < argc; optind++)
      exit_status |= cut_file (argv[optind]);

  if (have_read_stdin && fclose (stdin) == EOF)
    {
      error (0, errno, "-");
      exit_status = 1;
    }
  if (ferror (stdout) || fclose (stdout) == EOF)
    error (1, 0, "write error");

  exit (exit_status);
}

/* Select for printing the positions in `fields' that are listed in
   byte or field specification FIELDSTR.  FIELDSTR should be
   composed of one or more numbers or ranges of numbers, separated by
   blanks or commas.  Incomplete ranges may be given: `-m' means
   `1-m'; `n-' means `n' through end of line or last field.

   Return the number of fields selected. */

int
set_fields (fieldstr)
     char *fieldstr;
{
  int initial = 1;		/* Value of first number in a range. */
  int dash_found = 0;		/* Nonzero if a '-' is found in this field. */
  int value = 0;		/* If nonzero, a number being accumulated. */
  int fields_selected = 0;	/* Number of fields selected so far. */
  /* If nonzero, index of first field in a range that goes to end of line. */
  int eol_range_start = 0;

  for (;;)
    {
      if (*fieldstr == '-')
	{
	  /* Starting a range. */
	  if (dash_found)
	    invalid_list ();
	  dash_found++;
	  fieldstr++;

	  if (value)
	    {
	      if (value >= line_size)
		enlarge_line (value);
	      initial = value;
	      value = 0;
	    }
	  else
	    initial = 1;
	}
      else if (*fieldstr == ',' || isblank (*fieldstr) || *fieldstr == '\0')
	{
	  /* Ending the string, or this field/byte sublist. */
	  if (dash_found)
	    {
	      dash_found = 0;

	      /* A range.  Possibilites: -n, m-n, n-.
		 In any case, `initial' contains the start of the range. */
	      if (value == 0)
		{
		  /* `n-'.  From `initial' to end of line. */
		  eol_range_start = initial;
		  fields_selected++;
		}
	      else
		{
		  /* `m-n' or `-n' (1-n). */
		  if (value < initial)
		    invalid_list ();

		  if (value >= line_size)
		    enlarge_line (value);

		  /* Is there already a range going to end of line? */
		  if (eol_range_start != 0)
		    {
		      /* Yes.  Is the new sequence already contained
			 in the old one?  If so, no processing is
			 necessary. */
		      if (initial < eol_range_start)
			{
			  /* No, the new sequence starts before the
			     old.  Does the old range going to end of line
			     extend into the new range?  */
			  if (eol_range_start < value)
			    /* Yes.  Simply move the end of line marker. */
			    eol_range_start = initial;
			  else
			    {
			      /* No.  A simple range, before and disjoint from
				 the range going to end of line.  Fill it. */
			      for (; initial <= value; initial++)
				fields[initial] = field_output;
			    }

			  /* In any case, some fields were selected. */
			  fields_selected++;
			}
		    }
		  else
		    {
		      /* There is no range going to end of line. */
		      for (; initial <= value; initial++)
			fields[initial] = field_output;
		      fields_selected++;
		    }
		  value = 0;
		}
	    }
	  else if (value != 0)
	    {
	      /* A simple field number, not a range. */
	      if (value >= line_size)
		enlarge_line (value);

	      fields[value] = field_output;
	      value = 0;
	      fields_selected++;
	    }

	  if (*fieldstr == '\0')
	    {
	      /* If there was a range going to end of line, fill the
		 array from the end of line point.  */
	      if (eol_range_start)
		for (initial = eol_range_start; initial < line_size; initial++)
		  fields[initial] = field_output;

	      return fields_selected;
	    }

	  fieldstr++;
	}
      else if (ISDIGIT (*fieldstr))
	{
	  value = 10 * value + *fieldstr - '0';
	  fieldstr++;
	}
      else
	invalid_list ();
    }
}

/* Process file FILE to standard output.
   Return 0 if successful, 1 if not. */

int
cut_file (file)
     char *file;
{
  FILE *stream;

  if (!strcmp (file, "-"))
    {
      have_read_stdin = 1;
      stream = stdin;
    }
  else
    {
      stream = fopen (file, "r");
      if (stream == NULL)
	{
	  error (0, errno, "%s", file);
	  return 1;
	}
    }

  cut_stream (stream);

  if (ferror (stream))
    {
      error (0, errno, "%s", file);
      return 1;
    }
  if (!strcmp (file, "-"))
    clearerr (stream);		/* Also clear EOF. */
  else if (fclose (stream) == EOF)
    {
      error (0, errno, "%s", file);
      return 1;
    }
  return 0;
}

void
cut_stream (stream)
     FILE *stream;
{
  if (operating_mode == byte_mode)
    cut_bytes (stream);
  else
    cut_fields (stream);
}

/* Print the file open for reading on stream STREAM
   with the bytes marked `field_omit' in `fields' removed from each line. */

void
cut_bytes (stream)
     FILE *stream;
{
  register int c;		/* Each character from the file. */
  int doneflag = 0;		/* Nonzero if EOF reached. */
  int char_count;		/* Number of chars in the line so far. */

  while (doneflag == 0)
    {
      /* Start processing a line. */
      outbufptr = outbuf;
      char_count = 0;

      do
	{
	  c = getc (stream);
	  if (c == EOF)
	    {
	      doneflag++;
	      break;
	    }

	  /* If this character is to be sent, stow it in the outbuffer. */

	  if (++char_count == line_size - 1)
	    enlarge_line (char_count);

	  if (fields[char_count] == field_output || c == '\n')
	    *outbufptr++ = c;
	}
      while (c != '\n');

      if (char_count)
	fwrite (outbuf, sizeof (char), outbufptr - outbuf, stdout);
    }
}

/* Print the file open for reading on stream STREAM
   with the fields marked `field_omit' in `fields' removed from each line.
   All characters are initially stowed in the raw input buffer, until
   at least one field has been found. */

void
cut_fields (stream)
     FILE *stream;
{
  register int c;		/* Each character from the file. */
  int doneflag = 0;		/* Nonzero if EOF reached. */
  int char_count;		/* Number of chars in line before any delim. */
  int fieldfound;		/* Nonzero if any fields to print found. */
  int curr_field;		/* Current index in `fields'. */

  while (doneflag == 0)
    {
      char_count = 0;
      fieldfound = 0;
      curr_field = 1;
      outbufptr = outbuf;
      inbufptr = inbuf;

      do
	{
	  c = getc (stream);
	  if (c == EOF)
	    {
	      doneflag++;
	      break;
	    }

	  if (fields[curr_field] == field_output && c != '\n')
	    {
	      /* Working on a field.  It, and its terminating
		 delimiter, go only into the processed buffer. */
	      fieldfound = 1;
	      if (outbufptr - outbuf == line_size - 2)
		enlarge_line (outbufptr - outbuf);
	      *outbufptr++ = c;
	    }
	  else if (fieldfound == 0)
	    {
	      if (++char_count == line_size - 1)
		enlarge_line (char_count);
	      *inbufptr++ = c;
	    }

	  if (c == delim && ++curr_field == line_size - 1)
	    enlarge_line (curr_field);
	}
      while (c != '\n');

      if (fieldfound)
	{
	  /* Something was found. Print it. */
	  if (outbufptr[-1] == delim)
	    --outbufptr;	/* Suppress trailing delimiter. */

	  fwrite (outbuf, sizeof (char), outbufptr - outbuf, stdout);
	  if (c == '\n')
	    putc (c, stdout);
	}
      else if (!delimited_lines_only && char_count)
	/* A line with some characters, no delimiters, and no
	   suppression.  Print it. */
	fwrite (inbuf, sizeof (char), inbufptr - inbuf, stdout);
    }
}

/* Extend the buffers to accomodate at least NEW_SIZE characters. */

void
enlarge_line (new_size)
     int new_size;
{
  char *newp;
  int i;

  new_size += 256;		/* Leave some room to grow. */

  fields = (enum field_action *)
    xrealloc (fields, new_size * sizeof (enum field_action));

  newp = (char *) xrealloc (outbuf, new_size);
  outbufptr += newp - outbuf;
  outbuf = newp;

  newp = (char *) xrealloc (inbuf, new_size);
  inbufptr += newp - inbuf;
  inbuf = newp;

  for (i = line_size; i < new_size; i++)
    fields[i] = field_omit;
  line_size = new_size;
}

void
invalid_list ()
{
  error (2, 0, "invalid byte or field list");
}

void
usage ()
{
  fprintf (stderr, "\
Usage: %s {-b byte-list,--bytes=byte-list} [-n] [file...]\n\
       %s {-c character-list,--characters=character-list} [file...]\n\
       %s {-f field-list,--fields=field-list} [-d delim] [-s]\n\
       [--delimiter=delim] [--only-delimited] [file...]\n",
	   program_name, program_name, program_name);
  exit (2);
}
