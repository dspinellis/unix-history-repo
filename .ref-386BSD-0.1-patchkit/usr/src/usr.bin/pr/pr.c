/* pr -- convert text files for printing.
   Copyright (C) 1988, 1991 Free Software Foundation, Inc.

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

/*  Author: Pete TerMaat.  */

/* Things to watch: Sys V screws up on ...
   pr -n -3 -s: /usr/dict/words
   pr -m -o10 -n /usr/dict/words{,,,}
   pr -6 -a -n -o5 /usr/dict/words

   Ideas:

   Keep a things_to_do list of functions to call when we know we have
   something to print.  Cleaner than current series of checks.

   Improve the printing of control prefixes.


   Options:

   +PAGE	Begin output at page PAGE of the output.

   -COLUMN	Produce output that is COLUMN columns wide and print
		columns down.

   -a		Print columns across rather than down.  The input
		one
		two
		three
		four
		will be printed as
		one	two	three
		four

   -b		Balance columns on the last page.

   -c		Print unprintable characters as control prefixes.
		Control-g is printed as ^G.

   -d		Double space the output.

   -e[c[k]]	Expand tabs to spaces on input.  Optional argument C
		is the input tab character. (Default is `\t'.)  Optional
		argument K is the input tab character's width.  (Default is 8.)

   -F
   -f		Use formfeeds instead of newlines to separate pages.

   -h header	Replace the filename in the header with the string HEADER.

   -i[c[k]]	Replace spaces with tabs on output.  Optional argument
		C is the output tab character.  (Default is `\t'.)  Optional
		argument K is the output tab character's width.  (Default
		is 8.)

   -l lines	Set the page length to LINES.  Default is 66.

   -m		Print files in parallel.

   -n[c[k]]	Precede each column with a line number.
		(With parallel files, precede each line with a line
		number.)  Optional argument C is the character to print
		after each number.  (Default `\t'.)  Optional argument
		K is the number of digits per line number.  (Default 5.)

   -o offset	Offset each line with a margin OFFSET spaces wide.
		Total page width is the size of this offset plus the
		width set with `-w'.

   -r		Ignore files that can't be opened.

   -s[c]	Separate each line with a character.  Optional argument C is
		the character to be used.  Default is `\t'.

   -t		Do not print headers or footers.

   -v		Print unprintable characters as escape sequences.
		Control-G becomes \007.

   -w width	Set the page width to WIDTH characters. */

#include <stdio.h>
/*#include "getopt.h"*/
#include <ctype.h>
#include <sys/types.h>
#include <time.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <fcntl.h>

#ifdef isascii
#define ISPRINT(c) (isascii (c) && isprint (c))
#define ISDIGIT(c) (isascii (c) && isdigit (c))
#else
#define ISPRINT(c) isprint (c)
#define ISDIGIT(c) isdigit (c)
#endif

int char_to_clump ();
int read_line ();
int print_page ();
int print_stored ();
void error (int, int, char *, ...);
int open_file ();
int skip_to_page ();
void getoptarg ();
void usage ();
void print_files ();
void init_header ();
void init_store_cols ();
void store_columns ();
void balance ();
void store_char ();
void pad_down ();
void read_rest_of_line ();
void print_char ();
void cleanup ();

#ifndef TRUE
#define TRUE	1
#define FALSE	0
#endif

/* Used with start_position in the struct COLUMN described below.
   If start_position == ANYWHERE, we aren't truncating columns and
   can begin printing a column anywhere.  Otherwise we must pad to
   the horizontal position start_position. */
#define ANYWHERE	0

/* Each column has one of these structures allocated for it.
   If we're only dealing with one file, fp is the same for all
   columns.

   The general strategy is to spend time setting up these column
   structures (storing columns if necessary), after which printing
   is a matter of flitting from column to column and calling
   print_func.

   Parallel files, single files printing across in multiple
   columns, and single files printing down in multiple columns all
   fit the same printing loop.

   print_func		Function used to print lines in this column.
			If we're storing this column it will be
			print_stored(), Otherwise it will be read_line().

   char_func		Function used to process characters in this column.
			If we're storing this column it will be store_char(),
			otherwise it will be print_char().

   current_line		Index of the current entry in line_vector, which
			contains the index of the first character of the
			current line in buff[].

   lines_stored		Number of lines in this column which are stored in
			buff.

   lines_to_print	If we're storing this column, lines_to_print is
			the number of stored_lines which remain to be
			printed.  Otherwise it is the number of lines
			we can print without exceeding lines_per_body.

   start_position	The horizontal position we want to be in before we
			print the first character in this column.

   numbered		True means precede this column with a line number. */

typedef struct
{
  FILE *fp;			/* File pointer for this column. */
  enum
  {
    OPEN,
    ON_HOLD,			/* Hit a form feed. */
    CLOSED
  } status;			/* Status of the file pointer. */
  int (*print_func) ();		/* Func to print lines in this col. */
  void (*char_func) ();		/* Func to print/store chars in this col. */
  int current_line;		/* Index of current place in line_vector. */
  int lines_stored;		/* Number of lines stored in buff. */
  int lines_to_print;		/* No. lines stored or space left on page. */
  int start_position;		/* Horizontal position of first char. */
  int numbered;
} COLUMN;

#define NULLCOL (COLUMN *)0

COLUMN *column_vector;

/* When printing a single file in multiple downward columns,
   we store the leftmost columns contiguously in buff.
   To print a line from buff, get the index of the first char
   from line_vector[i], and print up to line_vector[i+1]. */
char *buff;

/* Index of the position in buff where the next character
   will be stored. */
int buff_current;

/* The number of characters in buff.
   Used for allocation of buff and to detect overflow of buff. */
int chars_per_buff;

/* Array of indices into buff.
   Each entry is an index of the first character of a line.
   This is used when storing lines to facilitate shuffling when
   we do column balancing on the last page. */
int *line_vector;

/* Array of horizonal positions.
   For each line in line_vector, end_vector[line] is the horizontal
   position we are in after printing that line.  We keep track of this
   so that we know how much we need to pad to prepare for the next
   column. */
int *end_vector;

/* (-m) True means we're printing multiple files in parallel. */
int parallel_files = FALSE;

/* (-[0-9]+) True when we're given an option explicitly specifying
   number of columns.  Used to detect when this option is used with -m. */
int explicit_columns = FALSE;

/* (-t) True means we're printing headers and footers. */
int extremities = TRUE;

/* True means we need to print a header as soon as we know we've got input
   to print after it. */
int print_a_header;

/* (-h) True means we're using the standard header rather than a
   customized one specified by the -h flag. */
int standard_header = TRUE;

/* (-f) True means use formfeeds instead of newlines to separate pages. */
int use_form_feed = FALSE;

/* True means we haven't encountered any filenames in the argument list. */
int input_is_stdin = TRUE;

/* True means we have read the standard input. */
int have_read_stdin = FALSE;

/* True means the -a flag has been given. */
int print_across_flag = FALSE;

/* True means we're printing one file in multiple (>1) downward columns. */
int storing_columns = TRUE;

/* (-b) True means balance columns on the last page as Sys V does. */
int balance_columns = FALSE;

/* (-l) Number of lines on a page, including header and footer lines. */
int lines_per_page = 66;

/* Number of lines in the header and footer can be reset to 0 using
   the -t flag. */
int lines_per_header = 5;
int lines_per_body;
int lines_per_footer = 5;

/* (-w) Width in characters of the page.  Does not include the width of
   the margin. */
int chars_per_line = 72;

/* Number of characters in a column.  Based on the gutter and page widths. */
int chars_per_column;

/* (-e) True means convert tabs to spaces on input. */
int untabify_input = FALSE;

/* (-e) The input tab character. */
char input_tab_char = '\t';

/* (-e) Tabstops are at chars_per_tab, 2*chars_per_tab, 3*chars_per_tab, ...
   where the leftmost column is 1. */
int chars_per_input_tab = 8;

/* (-i) True means convert spaces to tabs on output. */
int tabify_output = FALSE;

/* (-i) The output tab character. */
char output_tab_char = '\t';

/* (-i) The width of the output tab. */
int chars_per_output_tab = 8;

/* This is used to keep track of white space.  When we hit a
   nonspace character after some whitespace, we print whitespace, tabbing
   if necessary to get to output_position + spaces_not_printed. */
int spaces_not_printed;

/* Number of spaces between columns (though tabs can be used when possible to
   use up the equivalent amount of space).  Not sure if this is worth making
   a flag for.  BSD uses 0, Sys V uses 1.  Sys V looks better. */
int chars_per_gutter = 1;

/* (-o) Number of spaces in the left margin (tabs used when possible). */
int chars_per_margin = 0;

/* Position where the next character will fall.
   Leftmost position is 0 + chars_per_margin.
   Rightmost position is chars_per_margin + chars_per_line - 1
   This is important for converting spaces to tabs on output. */
int output_position;

/* Horizontal position relative to the current file.
   (output_position depends on where we are on the page;
   input_position depends on where we are in the file.)
   Important for converting tabs to spaces on input. */
int input_position;

/* Count number of failed opens so we can exit with non-zero
   status if there were any.  */
int failed_opens = 0;

/* The horizontal position we'll be at after printing a tab character
   of width c_ from the position h_. */
#define pos_after_tab(c_,h_) h_ - h_ % c_ + c_

/* The number of spaces taken up if we print a tab character with width
   c_ from position h_. */
#define tab_width(c_,h_) - h_ % c_ + c_

/* (-NNN) Number of columns of text to print. */
int columns = 1;

/* (+NNN) Page number on which to begin printing. */
int first_page = 1;

/* Number of files open (not closed, not on hold). */
int ready_to_read = 0;

/* Number of columns with either an open file or stored lines. */
int ready_to_print = 0;

/* Current page number.  Displayed in header. */
int page_number;

/* Current line number.  Displayed when -n flag is specified.

   When printing files in parallel (-m flag), line numbering is as follows:
   1	foo	goo	moo
   2	hoo	too	zoo

   When printing files across (-a flag), ...
   1	foo	2	moo	3	goo
   4	hoo	3	too	6	zoo

   Otherwise, line numbering is as follows:
   1	foo	3	goo	5	too
   2	moo	4	hoo	6	zoo */
int line_number;

/* (-n) True means lines should be preceded by numbers. */
int numbered_lines = FALSE;

/* True means print a number as soon as we know we'll be printing
   from the current column. */
int print_a_number;

/* (-n) Character which follows each line number. */
char number_separator = '\t';

/* (-n) Width in characters of a line number. */
int chars_per_number = 5;

/* Used when widening the first column to accommodate numbers -- only
   needed when printing files in parallel.  Includes width of both the
   number and the number_separator. */
int number_width;

/* Buffer sprintf uses to format a line number. */
char *number_buff;

/* (-v) True means unprintable characters are printed as escape sequences.
   control-g becomes \007. */
int use_esc_sequence = FALSE;

/* (-c) True means unprintable characters are printed as control prefixes.
   control-g becomes ^G. */
int use_cntrl_prefix = FALSE;

/* (-d) True means output is double spaced. */
int double_space = FALSE;

/* Number of files opened initially in init_files().  Should be one
   unless we're printing multiple files in parallel. */
int total_files = 0;

/* (-r) True means don't complain if we can't open a file. */
int ignore_failed_opens = FALSE;

/* (-s) True means we separate columns with a specified character. */
int use_column_separator = FALSE;

/* Character used to separate columns if the the -s flag has been specified. */
char column_separator = '\t';

/* Number of separator characters waiting to be printed as soon as we know
   that we have any input remaining to be printed. */
int separators_not_printed;

/* Position we need to pad to, as soon as we know that we have input
   remaining to be printed. */
int padding_not_printed;

/* True means we should pad the end of the page. Remains false until we
   know we have a page to print. */
int print_vertical_padding;

/* (-h) String of characters used in place of the filename in the header. */
char *custom_header;

/* String containing the date, filename or custom header, and "Page ". */
char *header;

int *clump_buff;

/* True means we truncate lines longer than chars_per_column. */
int truncate_lines = FALSE;

/* The name under which this program was invoked. */
char *program_name;

void
main (argc, argv)
     int argc;
     char **argv;
{
  int c;
  char *s;
  int files = 0;
  char **file_names, **file_name_vector;
  int accum = 0;

  program_name = argv[0];

  file_name_vector = (char **) malloc (argc * sizeof (char **));
  file_names = file_name_vector;

  for (;;)
    {
      c = getopt (argc, argv, "-0123456789abcde::fFh:i::l:mn::o:rs::tvw:");

      if (c == 1)		/* Non-option argument. */
	{
	  s = optarg;
	  if (*s == '+')
	    {
	      if (!ISDIGIT (*++s))
		usage ("`+' requires a numeric argument");
	      first_page = atoi (s);
	    }
	  else
	    {
	      *file_names++ = optarg;
	      ++files;
	    }
	}
      else if (files > 0)
	{
	  if (parallel_files && explicit_columns)
	    error (1, 0,
"Cannot specify number of columns when printing in parallel.");

	  if (parallel_files && print_across_flag)
	    error (1, 0,
"Cannot specify both printing across and printing in parallel.");

	  if (parallel_files)
	    print_files (files, file_name_vector);
	  else
	    {
	      file_names = file_name_vector;
	      while (files--)
		print_files (1, file_names++);
	    }

	  input_is_stdin = FALSE;
	  file_names = file_name_vector;
	  files = 0;
	  cleanup ();
	}

      if (ISDIGIT (c))
	{
	  accum = accum * 10 + c - '0';
	  continue;
	}
      else
	{
	  if (accum > 0)
	    {
	      columns = accum;
	      explicit_columns = TRUE;
	    }
	  accum = 0;
	}

      switch (c)
	{
	case 'a':
	  print_across_flag = TRUE;
	  storing_columns = FALSE;
	  break;
	case 'b':
	  balance_columns = TRUE;
	  break;
	case 'c':
	  use_cntrl_prefix = TRUE;
	  break;
	case 'd':
	  double_space = TRUE;
	  break;
	case 'e':
	  if (optarg)
	    getoptarg (optarg, 'e', &input_tab_char,
		       &chars_per_input_tab);
	  /* Could check tab width > 0. */
	  untabify_input = TRUE;
	  break;
	case 'f':
	case 'F':
	  use_form_feed = TRUE;
	  break;
	case 'h':
	  custom_header = optarg;
	  standard_header = FALSE;
	  break;
	case 'i':
	  if (optarg)
	    getoptarg (optarg, 'i', &output_tab_char,
		       &chars_per_output_tab);
	  /* Could check tab width > 0. */
	  tabify_output = TRUE;
	  break;
	case 'l':
	  lines_per_page = atoi (optarg);
	  break;
	case 'm':
	  parallel_files = TRUE;
	  storing_columns = FALSE;
	  break;
	case 'n':
	  numbered_lines = TRUE;
	  if (optarg)
	    getoptarg (optarg, 'n', &number_separator,
		       &chars_per_number);
	  break;
	case 'o':
	  chars_per_margin = atoi (optarg);
	  break;
	case 'r':
	  ignore_failed_opens = TRUE;
	  break;
	case 's':
	  use_column_separator = TRUE;
	  if (optarg)
	    {
	      s = optarg;
	      column_separator = *s;
	      if (*++s)
		{
		  fprintf (stderr, "\
%s: extra characters in the argument to the `-s' option: `%s'\n",
			   program_name, s);
		  usage ((char *) 0);
		}
	    }
	  break;
	case 't':
	  extremities = FALSE;
	  break;
	case 'v':
	  use_esc_sequence = TRUE;
	  break;
	case 'w':
	  chars_per_line = atoi (optarg);
	  break;
	case '?':
	  usage ((char *) 0);
	  break;
	}

      if (c == EOF)
	break;
    }

  if (input_is_stdin)
    print_files (0, (char **) 0);

  if (have_read_stdin && fclose (stdin) == EOF)
    error (1, errno, "-");
  if (ferror (stdout) || fclose (stdout) == EOF)
    error (1, errno, "write error");
  if (failed_opens > 0)
    exit(1);
  exit (0);
}

/* Parse options of the form -scNNN.

   Example: -nck, where 'n' is the option, c is the optional number
   separator, and k is the optional width of the field used when printing
   a number. */

void
getoptarg (arg, switch_char, character, number)
     char *arg, switch_char, *character;
     int *number;
{
  if (!ISDIGIT (*arg))
    *character = *arg++;
  if (*arg)
    {
      if (ISDIGIT (*arg))
	*number = atoi (arg);
      else
	{
	  fprintf (stderr, "\
%s: extra characters in the argument to the `-%c' option: `%s'\n",
		   program_name, switch_char, arg);
	  usage ((char *) 0);
	}
    }
}

/* Set parameters related to formatting. */

void
init_parameters (number_of_files)
     int number_of_files;
{
  int chars_used_by_number = 0;

  lines_per_body = lines_per_page - lines_per_header - lines_per_footer;
  if (lines_per_body <= 0)
    extremities = FALSE;
  if (extremities == FALSE)
    lines_per_body = lines_per_page;

  if (double_space)
    lines_per_body = lines_per_body / 2;

  /* If input is stdin, cannot print parallel files.  BSD dumps core
     on this. */
  if (number_of_files == 0)
    parallel_files = FALSE;

  if (parallel_files)
    columns = number_of_files;

  /* Tabification is assumed for multiple columns. */
  if (columns > 1)
    {
      if (!use_column_separator)
	truncate_lines = TRUE;

      untabify_input = TRUE;
      tabify_output = TRUE;
    }
  else
    storing_columns = FALSE;

  if (numbered_lines)
    {
      if (number_separator == input_tab_char)
	{
	  number_width = chars_per_number +
	    tab_width (chars_per_input_tab,
		       (chars_per_margin + chars_per_number));
	}
      else
	number_width = chars_per_number + 1;
      /* The number is part of the column width unless we are
         printing files in parallel. */
      if (parallel_files)
	chars_used_by_number = number_width;
    }

  chars_per_column = (chars_per_line - chars_used_by_number -
		      (columns - 1) * chars_per_gutter) / columns;

  if (chars_per_column < 1)
    error (1, 0, "page width too narrow");

  if (numbered_lines)
    {
      if (number_buff != (char *) 0)
	free (number_buff);
      number_buff = (char *)
	malloc (2 * chars_per_number * sizeof (char));
    }

  /* Pick the maximum between the tab width and the width of an
     escape sequence. */
  if (clump_buff != (int *) 0)
    free (clump_buff);
  clump_buff = (int *) malloc ((chars_per_input_tab > 4
				 ? chars_per_input_tab : 4) * sizeof (int));
}

/* Open the necessary files,
   maintaining a COLUMN structure for each column.

   With multiple files, each column p has a different p->fp.
   With single files, each column p has the same p->fp.
   Return 1 if (number_of_files > 0) and no files can be opened,
   0 otherwise.  */

int
init_fps (number_of_files, av)
     int number_of_files;
     char **av;
{
  int i, files_left;
  COLUMN *p;
  FILE *firstfp;

  total_files = 0;

  if (column_vector != NULLCOL)
    free ((char *) column_vector);
  column_vector = (COLUMN *) malloc (columns * sizeof (COLUMN));

  if (parallel_files)
    {
      files_left = number_of_files;
      for (p = column_vector; files_left--; ++p, ++av)
	{
	  if (open_file (*av, p) == 0)
	    {
	      --p;
	      --columns;
	    }
	}
      if (columns == 0)
	return 1;
      init_header ("", -1);
    }
  else
    {
      p = column_vector;
      if (number_of_files > 0)
	{
	  if (open_file (*av, p) == 0)
	    return 1;
	  init_header (*av, fileno (p->fp));
	}
      else
	{
	  p->fp = stdin;
	  have_read_stdin = TRUE;
	  p->status = OPEN;
	  ++total_files;
	  init_header ("", -1);
	}

      firstfp = p->fp;
      for (i = columns - 1, ++p; i; --i, ++p)
	{
	  p->fp = firstfp;
	  p->status = OPEN;
	}
    }
  ready_to_read = total_files;
  return 0;
}

/* Determine print_func and char_func, the functions
   used by each column for printing and/or storing.

   Determine the horizontal position desired when we begin
   printing a column (p->start_position). */

void
init_funcs ()
{
  int i, h, h_next;
  COLUMN *p;

  h = chars_per_margin;

  if (use_column_separator)
    h_next = ANYWHERE;
  else
    {
      /* When numbering lines of parallel files, we enlarge the
         first column to accomodate the number.  Looks better than
         the Sys V approach. */
      if (parallel_files && numbered_lines)
	h_next = h + chars_per_column + number_width;
      else
	h_next = h + chars_per_column;
    }

  /* This loop takes care of all but the rightmost column. */

  for (p = column_vector, i = 1; i < columns; ++p, ++i)
    {
      if (storing_columns)	/* One file, multi columns down. */
	{
	  p->char_func = store_char;
	  p->print_func = print_stored;
	}
      else
	/* One file, multi columns across; or parallel files.  */
	{
	  p->char_func = print_char;
	  p->print_func = read_line;
	}

      /* Number only the first column when printing files in
         parallel. */
      p->numbered = numbered_lines && (!parallel_files || i == 1);
      p->start_position = h;

      /* If we're using separators, all start_positions are
         ANYWHERE, except the first column's start_position when
         using a margin. */

      if (use_column_separator)
	{
	  h = ANYWHERE;
	  h_next = ANYWHERE;
	}
      else
	{
	  h = h_next + chars_per_gutter;
	  h_next = h + chars_per_column;
	}
    }

  /* The rightmost column.

     Doesn't need to be stored unless we intend to balance
     columns on the last page. */
  if (storing_columns && balance_columns)
    {
      p->char_func = store_char;
      p->print_func = print_stored;
    }
  else
    {
      p->char_func = print_char;
      p->print_func = read_line;
    }

  p->numbered = numbered_lines && (!parallel_files || i == 1);
  p->start_position = h;
}

/* Open a file.  Return nonzero if successful, zero if failed. */

int
open_file (name, p)
     char *name;
     COLUMN *p;
{
  if (!strcmp (name, "-"))
    {
      p->fp = stdin;
      have_read_stdin = 1;
    }
  else
    p->fp = fopen (name, "r");
  if (p->fp == NULL)
    {
      ++failed_opens;
      if (!ignore_failed_opens)
	error (0, errno, "%s", name);
      return 0;
    }
  p->status = OPEN;
  ++total_files;
  return 1;
}

/* Close the file in P.

   If we aren't dealing with multiple files in parallel, we change
   the status of all columns in the column list to reflect the close. */

void
close_file (p)
     COLUMN *p;
{
  COLUMN *q;
  int i;

  if (p->status == CLOSED)
    return;
  if (ferror (p->fp))
    error (1, errno, "read error"); /* Where's that filename kept? */
  if (p->fp != stdin && fclose (p->fp) == EOF)
    error (1, errno, "read error"); /* Where's that filename kept? */

  if (!parallel_files)
    {
      for (q = column_vector, i = columns; i; ++q, --i)
	{
	  q->status = CLOSED;
	  if (q->lines_stored == 0)
	    {
	      if (ready_to_print > 0)
		--ready_to_print;
	      q->lines_to_print = 0;
	    }
	}
    }
  else
    {
      p->status = CLOSED;
      p->lines_to_print = 0;
    }

  --ready_to_read;
}

/* Put a file on hold until we start a new page,
   since we've hit a form feed.

   If we aren't dealing with parallel files, we must change the
   status of all columns in the column list. */

void
hold_file (p)
     COLUMN *p;
{
  COLUMN *q;
  int i;

  if (!parallel_files)
    for (q = column_vector, i = columns; i; ++q, --i)
      q->status = ON_HOLD;
  else
    p->status = ON_HOLD;
  p->lines_to_print = 0;
  --ready_to_read;
}

/* Undo hold_file -- go through the column list and change any
   ON_HOLD columns to OPEN.  Used at the end of each page. */

void
reset_status ()
{
  int i = columns;
  COLUMN *p;

  for (p = column_vector; i; --i, ++p)
    if (p->status == ON_HOLD)
      {
	p->status = OPEN;
	ready_to_read++;
      }
}

/* Print a single file, or multiple files in parallel.

   Set up the list of columns, opening the necessary files.
   Allocate space for storing columns, if necessary.
   Skip to first_page, if user has asked to skip leading pages.
   Determine which functions are appropriate to store/print lines
   in each column.
   Print the file(s). */

void
print_files (number_of_files, av)
     int number_of_files;
     char **av;
{
  init_parameters (number_of_files);
  if (init_fps (number_of_files, av))
    return;
  if (storing_columns)
    init_store_cols ();

  if (first_page > 1)
    {
      if (!skip_to_page (first_page))
	return;
      else
	page_number = first_page;
    }
  else
    page_number = 1;

  init_funcs ();

  line_number = 1;
  while (print_page ())
    ;
}

/* Generous estimate of number of characters taken up by "Jun  7 00:08 " and
   "Page NNNNN". */
#define CHARS_FOR_DATE_AND_PAGE	50

/* Initialize header information.
   If DESC is non-negative, it is a file descriptor open to
   FILENAME for reading.

   Allocate space for a header string,
   Determine the time, insert file name or user-specified string.

   It might be nice to have a "blank headers" option, since
   pr -h "" still prints the date and page number. */

void
init_header (filename, desc)
     char *filename;
     int desc;
{
  int chars_per_header;
  char *f = filename;
  char *t, *middle;
  struct stat st;

  if (filename == 0)
    f = "";

  /* If parallel files or standard input, use current time. */
  if (desc < 0 || !strcmp (filename, "-") || fstat (desc, &st))
    st.st_mtime = time ((time_t *) 0);
  t = ctime (&st.st_mtime);

  t[16] = '\0';			/* Mark end of month and time string. */
  t[24] = '\0';			/* Mark end of year string. */

  middle = standard_header ? f : custom_header;

  chars_per_header = strlen (middle) + CHARS_FOR_DATE_AND_PAGE + 1;
  if (header != (char *) 0)
    free (header);
  header = (char *) malloc (chars_per_header * sizeof (char));

  sprintf (header, "%s %s  %s Page", &t[4], &t[20], middle);
}

/* Set things up for printing a page

   Scan through the columns ...
     Determine which are ready to print
       (i.e., which have lines stored or open files)
     Set p->lines_to_print appropriately
       (to p->lines_stored if we're storing, or lines_per_body
       if we're reading straight from the file)
     Keep track of this total so we know when to stop printing */

void
init_page ()
{
  int j;
  COLUMN *p;

  ready_to_print = 0;

  if (storing_columns)
    {
      store_columns ();
      for (j = columns - 1, p = column_vector; j; --j, ++p)
	if ((p->lines_to_print = p->lines_stored) != 0)
	  ++ready_to_print;

      /* Last column. */
      if (balance_columns)
	{
	  if ((p->lines_to_print = p->lines_stored) != 0)
	    ++ready_to_print;
	}
      /* Since we're not balancing columns, we don't need to store
         the rightmost column.   Read it straight from the file. */
      else
	{
	  if (p->status == OPEN)
	    {
	      p->lines_to_print = lines_per_body;
	      ++ready_to_print;
	    }
	  else
	    p->lines_to_print = 0;
	}
    }
  else
    for (j = columns, p = column_vector; j; --j, ++p)
      if (p->status == OPEN)
	{
	  p->lines_to_print = lines_per_body;
	  ++ready_to_print;
	}
      else
	p->lines_to_print = 0;
}

/* Print one page.

   As long as there are lines left on the page and columns ready to print,
     Scan across the column list
       if the column has stored lines or the file is open
         pad to the appropriate spot
         print the column
   pad the remainder of the page with \n or \f as requested
   reset the status of all files -- any files which where on hold because
     of formfeeds are now put back into the lineup. */

int
print_page ()
{
  int j;
  int lines_left;
  COLUMN *p;

  /* Used as an accumulator (with | operator) of successive values of
     print_vertical_padding.  The trick is to set print_vertical_padding
     to zero before each run through the inner loop, then after that
     loop, it tells us whether a line was actually printed (whether a
     newline needs to be output -- or two for double spacing).  But those
     values have to be accumulated (in pvp) so we can invoke pad_down
     properly after the outer loop completes. */
  int pvp;

  init_page ();

  if (!ready_to_print)
    return FALSE;

  if (extremities)
    print_a_header = TRUE;

  /* Don't pad unless we know a page was printed. */
  print_vertical_padding = FALSE;
  pvp = FALSE;

  lines_left = double_space ? 2 * lines_per_body : lines_per_body;

  for (; lines_left > 0 && ready_to_print > 0; --lines_left)
    {
      output_position = 0;
      spaces_not_printed = 0;
      separators_not_printed = 0;
      print_vertical_padding = 0;

      for (j = 1, p = column_vector; j <= columns; ++j, ++p)
	{
	  input_position = 0;
	  if (p->lines_to_print > 0)
	    {
	      padding_not_printed = p->start_position;

	      if (!(p->print_func) (p))
		read_rest_of_line (p);
	      pvp |= print_vertical_padding;

	      if (use_column_separator)
		++separators_not_printed;

	      if (--p->lines_to_print <= 0)
		if (--ready_to_print <= 0)
		  break;
	    }
	}

      if (print_vertical_padding)
	putchar ('\n');

      if (double_space && pvp && extremities)
	{
	  putchar ('\n');
	  --lines_left;
	}
    }

  print_vertical_padding = pvp;

  if (print_vertical_padding && extremities)
    pad_down (lines_left + lines_per_footer);

  reset_status ();		/* Change ON_HOLD to OPEN. */

  return TRUE;			/* More pages to go. */
}

/* Allocate space for storing columns.

   This is necessary when printing multiple columns from a single file.
   Lines are stored consecutively in buff, separated by '\0'.
   (We can't use a fixed offset since with the '-s' flag lines aren't
   truncated.)

   We maintain a list (line_vector) of pointers to the beginnings
   of lines in buff.  We allocate one more than the number of lines
   because the last entry tells us the index of the last character,
   which we need to know in order to print the last line in buff. */

void
init_store_cols ()
{
  int total_lines = lines_per_body * columns;
  int chars_if_truncate = total_lines * (chars_per_column + 1);

  if (line_vector != (int *) 0)
    free ((int *) line_vector);
  line_vector = (int *) malloc ((total_lines + 1) * sizeof (int *));

  if (end_vector != (int *) 0)
    free ((int *) end_vector);
  end_vector = (int *) malloc (total_lines * sizeof (int *));

  if (buff != (char *) 0)
    free (buff);
  chars_per_buff = use_column_separator ? 2 * chars_if_truncate
    : chars_if_truncate;	/* Tune this. */
  buff = (char *) malloc (chars_per_buff * sizeof (char));
}

/* Store all but the rightmost column.
   (Used when printing a single file in multiple downward columns)

   For each column
     set p->current_line to be the index in line_vector of the
       first line in the column
     For each line in the column
       store the line in buff
       add to line_vector the index of the line's first char
    buff_start is the index in buff of the first character in the
     current line. */

void
store_columns ()
{
  int i, j;
  int line = 0;
  int buff_start;
  int last_col;			/* The rightmost column which will be saved in buff */
  COLUMN *p;

  buff_current = 0;
  buff_start = 0;

  if (balance_columns)
    last_col = columns;
  else
    last_col = columns - 1;

  for (i = 1, p = column_vector; i <= last_col; ++i, ++p)
    p->lines_stored = 0;

  for (i = 1, p = column_vector; i <= last_col && ready_to_read; ++i, ++p)
    {
      p->current_line = line;
      for (j = lines_per_body; j && ready_to_read; --j)

	if (p->status == OPEN)	/* Redundant.  Clean up. */
	  {
	    input_position = 0;

	    if (!read_line (p, i))
	      read_rest_of_line (p);

	    if (p->status == OPEN
		|| buff_start != buff_current)
	      {
		++p->lines_stored;
		line_vector[line] = buff_start;
		end_vector[line++] = input_position;
		buff_start = buff_current;
	      }
	  }
    }

  /* Keep track of the location of the last char in buff. */
  line_vector[line] = buff_start;

  if (balance_columns && p->lines_stored != lines_per_body)
    balance (line);
}

void
balance (total_stored)
     int total_stored;
{
  COLUMN *p;
  int i, lines;
  int first_line = 0;

  for (i = 1, p = column_vector; i <= columns; ++i, ++p)
    {
      lines = total_stored / columns;
      if (i <= total_stored % columns)
	++lines;

      p->lines_stored = lines;
      p->current_line = first_line;

      first_line += lines;
    }
}

/* Store a character in the buffer. */

void
store_char (c)
     int c;
{
  if (buff_current >= chars_per_buff)
    {
      /* May be too generous. */
      chars_per_buff = 2 * chars_per_buff;
      buff = (char *) realloc (buff, chars_per_buff * sizeof (char));
    }
  buff[buff_current++] = (char) c;
}

void
number (p)
     COLUMN *p;
{
  int i;
  char *s;

  sprintf (number_buff, "%*d", chars_per_number, line_number++);
  s = number_buff;
  for (i = chars_per_number; i > 0; i--)
    (p->char_func) ((int) *s++);

  if (number_separator == input_tab_char)
    {
      i = number_width - chars_per_number;
      while (i-- > 0)
	(p->char_func) ((int) ' ');
    }
  else
    (p->char_func) ((int) number_separator);

  if (truncate_lines && !parallel_files)
    input_position += number_width;
}

/* Print (or store) padding until the current horizontal position
   is position. */

void
pad_across_to (position)
     int position;
{
  register int h = output_position;

  if (tabify_output)
    spaces_not_printed = position - output_position;
  else
    {
      while (++h <= position)
	putchar (' ');
      output_position = position;
    }
}

/* Pad to the bottom of the page.

   If the user has requested a formfeed, use one.
   Otherwise, use newlines. */

void
pad_down (lines)
     int lines;
{
  register int i;

  if (use_form_feed)
    putchar ('\f');
  else
    for (i = lines; i; --i)
      putchar ('\n');
}

/* Read the rest of the line.

   Read from the current column's file until an end of line is
   hit.  Used when we've truncated a line and we no longer need
   to print or store its characters. */

void
read_rest_of_line (p)
     COLUMN *p;
{
  register int c;
  FILE *f = p->fp;

  while ((c = getc (f)) != '\n')
    {
      if (c == '\f')
	{
	  hold_file (p);
	  break;
	}
      else if (c == EOF)
	{
	  close_file (p);
	  break;
	}
    }
}

/* If we're tabifying output,

   When print_char encounters white space it keeps track
   of our desired horizontal position and delays printing
   until this function is called. */

void
print_white_space ()
{
  register int h_new;
  register int h_old = output_position;
  register int goal = h_old + spaces_not_printed;

  while (goal - h_old > 1
	  && (h_new = pos_after_tab (chars_per_output_tab, h_old)) <= goal)
    {
      putchar (output_tab_char);
      h_old = h_new;
    }
  while (++h_old <= goal)
    putchar (' ');

  output_position = goal;
  spaces_not_printed = 0;
}

/* Print column separators.

   We keep a count until we know that we'll be printing a line,
   then print_separators() is called. */

void
print_separators ()
{
  for (; separators_not_printed > 0; --separators_not_printed)
    print_char (column_separator);
}

/* Print (or store, depending on p->char_func) a clump of N
   characters. */

void
print_clump (p, n, clump)
     COLUMN *p;
     int n;
     int *clump;
{
  while (n--)
    (p->char_func) (*clump++);
}

/* Print a character.

   If we're tabifying, all tabs have been converted to spaces by
   process_char().  Keep a count of consecutive spaces, and when
   a nonspace is encountered, call print_white_space() to print the
   required number of tabs and spaces. */

void
print_char (c)
     int c;
{
  if (tabify_output)
    {
      if (c == ' ')
	{
	  ++spaces_not_printed;
	  return;
	}
      else if (spaces_not_printed > 0)
	print_white_space ();

      /* Nonprintables are assumed to have width 0, except '\b'. */
      if (!ISPRINT (c))
	{
	  if (c == '\b')
	    --output_position;
	}
      else
	++output_position;
    }
  putchar (c);
}

/* Skip to page PAGE before printing. */

int
skip_to_page (page)
     int page;
{
  int n, i, j;
  COLUMN *p;

  for (n = 1; n < page; ++n)
    {
      for (i = 1; i <= lines_per_body; ++i)
	{
	  for (j = 1, p = column_vector; j <= columns; ++j, ++p)
	    read_rest_of_line (p);
	}
      reset_status ();
    }
  return ready_to_read > 0;
}

/* Print a header.

   Formfeeds are assumed to use up two lines at the beginning of
   the page. */

void
print_header ()
{
  if (!use_form_feed)
    fprintf (stdout, "\n\n");

  output_position = 0;
  pad_across_to (chars_per_margin);
  print_white_space ();

  fprintf (stdout, "%s %d\n\n\n", header, page_number++);

  print_a_header = FALSE;
  output_position = 0;
}

/* Print (or store, if p->char_func is store_char()) a line.

   Read a character to determine whether we have a line or not.
   (We may hit EOF, \n, or \f)

   Once we know we have a line,
     set print_vertical_padding = TRUE, meaning it's safe
       to pad down at the end of the page, since we do have a page.
       print a header if needed.
     pad across to padding_not_printed if needed.
     print any separators which need to be printed.
     print a line number if it needs to be printed.

   Print the clump which corresponds to the first character.

   Enter a loop and keep printing until an end of line condition
     exists, or until we exceed chars_per_column.

   Return FALSE if we exceed chars_per_column before reading
     an end of line character, TRUE otherwise. */

int
read_line (p)
     COLUMN *p;
{
  register int c, chars;
  int last_input_position;

  c = getc (p->fp);

  last_input_position = input_position;
  switch (c)
    {
    case '\f':
      hold_file (p);
      return TRUE;
    case EOF:
      close_file (p);
      return TRUE;
    case '\n':
      break;
    default:
      chars = char_to_clump (c);
    }

  if (truncate_lines && input_position > chars_per_column)
    {
      input_position = last_input_position;
      return FALSE;
    }

  if (p->char_func != store_char)
    {
      print_vertical_padding = TRUE;

      if (print_a_header)
	print_header ();

      if (padding_not_printed != ANYWHERE)
	{
	  pad_across_to (padding_not_printed);
	  padding_not_printed = ANYWHERE;
	}

      if (use_column_separator)
	print_separators ();
    }

  if (p->numbered)
    number (p);

  if (c == '\n')
    return TRUE;

  print_clump (p, chars, clump_buff);

  for (;;)
    {
      c = getc (p->fp);

      switch (c)
	{
	case '\n':
	  return TRUE;
	case '\f':
	  hold_file (p);
	  return TRUE;
	case EOF:
	  close_file (p);
	  return TRUE;
	}

      last_input_position = input_position;
      chars = char_to_clump (c);
      if (truncate_lines && input_position > chars_per_column)
	{
	  input_position = last_input_position;
	  return FALSE;
	}

      print_clump (p, chars, clump_buff);
    }
}

/* Print a line from buff.

   If this function has been called, we know we have something to
   print.  Therefore we set print_vertical_padding to TRUE, print
   a header if necessary, pad across if necessary, and print
   separators if necessary.

   Return TRUE, meaning there is no need to call read_rest_of_line. */

int
print_stored (p)
     COLUMN *p;
{
  int line = p->current_line++;
  register char *first = &buff[line_vector[line]];
  register char *last = &buff[line_vector[line + 1]];

  print_vertical_padding = TRUE;

  if (print_a_header)
    print_header ();

  if (padding_not_printed != ANYWHERE)
    {
      pad_across_to (padding_not_printed);
      padding_not_printed = ANYWHERE;
    }

  if (use_column_separator)
    print_separators ();

  while (first != last)
    print_char (*first++);

  if (spaces_not_printed == 0)
    output_position = p->start_position + end_vector[line];

  return TRUE;
}

/* Convert a character to the proper format and return the number of
   characters in the resulting clump.  Increment input_position by
   the width of the clump.

   Tabs are converted to clumps of spaces.
   Nonprintable characters may be converted to clumps of escape
   sequences or control prefixes.

   Note: the width of a clump is not necessarily equal to the number of
   characters in clump_buff.  (e.g, the width of '\b' is -1, while the
   number of characters is 1.) */

int
char_to_clump (c)
     int c;
{
  register int *s = clump_buff;
  register int i;
  char esc_buff[4];
  int width;
  int chars;

  if (c == input_tab_char)
    {
      width = tab_width (chars_per_input_tab, input_position);

      if (untabify_input)
	{
	  for (i = width; i; --i)
	    *s++ = ' ';
	  chars = width;
	}
      else
	{
	  *s = c;
	  chars = 1;
	}

    }
  else if (!ISPRINT (c))
    {
      if (use_esc_sequence)
	{
	  width = 4;
	  chars = 4;
	  *s++ = '\\';
	  sprintf (esc_buff, "%03o", c);
	  for (i = 0; i <= 2; ++i)
	    *s++ = (int) esc_buff[i];
	}
      else if (use_cntrl_prefix)
	{
	  if (c < 0200)
	    {
	      width = 2;
	      chars = 2;
	      *s++ = '^';
	      *s++ = c ^ 0100;
	    }
	  else
	    {
	      width = 4;
	      chars = 4;
	      *s++ = '\\';
	      sprintf (esc_buff, "%03o", c);
	      for (i = 0; i <= 2; ++i)
		*s++ = (int) esc_buff[i];
	    }
	}
      else if (c == '\b')
	{
	  width = -1;
	  chars = 1;
	  *s = c;
	}
      else
	{
	  width = 0;
	  chars = 1;
	  *s = c;
	}
    }
  else
    {
      width = 1;
      chars = 1;
      *s = c;
    }

  input_position += width;
  return chars;
}

/* We've just printed some files and need to clean up things before
   looking for more options and printing the next batch of files.

   Free everything we've malloc'ed, except `header'. */

void
cleanup ()
{
  if (number_buff)
    free (number_buff);
  if (clump_buff)
    free (clump_buff);
  if (column_vector)
    free (column_vector);
  if (line_vector)
    free (line_vector);
  if (end_vector)
    free (end_vector);
  if (buff)
    free (buff);
}

/* Complain, print a usage message, and die. */

void
usage (reason)
     char *reason;
{
  if (reason)
    fprintf (stderr, "%s: %s\n", program_name, reason);

  fprintf (stderr, "\
Usage: %s [+PAGE] [-COLUMN] [-abcdfFmrtv] [-e[in-tab-char[in-tab-width]]]\n\
       [-h header] [-i[out-tab-char[out-tab-width]]] [-l page-length]\n\
       [-n[number-separator[digits]]] [-o left-margin]\n\
       [-s[column-separator]] [-w page-width] [file...]\n",
	   program_name);
  exit (2);
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
