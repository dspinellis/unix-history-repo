/* nl -- number lines of files
   Copyright (C) 1989, 1992 Free Software Foundation, Inc.

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

/* Written by Scott Bartram (nancy!scott@uunet.uu.net)
   Revised by David MacKenzie (djm@ai.mit.edu) */

#include <stdio.h>
#include <sys/types.h>
#include <getopt.h>
#include <regex.h>
#include "linebuffer.h"
#ifdef __386BSD__
#undef	RE_DUP_MAX
#include "system.h"
/* XXX need to resolve the conflict in RE_DUP_MAX definitions. */
#undef	RE_DUP_MAX
#define RE_DUP_MAX  ((1 << 15) - 1) 
#else
#include "system.h"
#endif /*  __386BSD__ */

#ifndef TRUE
#define TRUE   1
#define FALSE  0
#endif

/* Line-number formats. */
enum number_format
{
  FORMAT_RIGHT_NOLZ,		/* Right justified, no leading zeroes.  */
  FORMAT_RIGHT_LZ,		/* Right justified, leading zeroes.  */
  FORMAT_LEFT			/* Left justified, no leading zeroes.  */
};

/* Default section delimiter characters.  */
#define DEFAULT_SECTION_DELIMITERS  "\\:"

/* Types of input lines: either one of the section delimiters,
   or text to output. */
enum section
{
  Header, Body, Footer, Text
};

/* Format of body lines (-b).  */
char *body_type = "t";

/* Format of header lines (-h).  */
char *header_type = "n";

/* Format of footer lines (-f).  */
char *footer_type = "n";

/* Format currently being used (body, header, or footer).  */
char *current_type;

/* Regex for body lines to number (-bp).  */
struct re_pattern_buffer body_regex;

/* Regex for header lines to number (-hp).  */
struct re_pattern_buffer header_regex;

/* Regex for footer lines to number (-fp).  */
struct re_pattern_buffer footer_regex;

/* Pointer to current regex, if any.  */
struct re_pattern_buffer *current_regex = NULL;

/* Separator string to print after line number (-s).  */
char *separator_str = "\t";

/* Input section delimiter string (-d).  */
char *section_del = DEFAULT_SECTION_DELIMITERS;

/* Header delimiter string.  */
char *header_del = NULL;

/* Header section delimiter length.  */
int header_del_len;

/* Body delimiter string.  */
char *body_del = NULL;

/* Body section delimiter length.  */
int body_del_len;

/* Footer delimiter string.  */
char *footer_del = NULL;

/* Footer section delimiter length.  */
int footer_del_len;

/* Input buffer.  */
struct linebuffer line_buf;

/* printf format string for line number.  */
char *print_fmt;

/* printf format string for unnumbered lines.  */
char *print_no_line_fmt = NULL;

/* Starting line number on each page (-v).  */
int page_start = 1;

/* Line number increment (-i).  */
int page_incr = 1;

/* If TRUE, reset line number at start of each page (-p).  */
int reset_numbers = TRUE;

/* Number of blank lines to consider to be one line for numbering (-l).  */
int blank_join = 1;

/* Width of line numbers (-w).  */
int lineno_width = 6;

/* Line number format (-n).  */
enum number_format lineno_format = FORMAT_RIGHT_NOLZ;

/* Current print line number.  */
int line_no;

/* The name this program was run with. */
char *program_name;

/* Nonzero if we have ever read standard input. */
int have_read_stdin;

enum section check_section ();
char *xmalloc ();
char *xrealloc ();
int build_type_arg ();
int nl_file ();
void usage ();
void process_file ();
void proc_header ();
void proc_body ();
void proc_footer ();
void proc_text ();
void print_lineno ();
void build_print_fmt ();
void error ();

struct option longopts[] =
{
  {"header-numbering", 1, NULL, 'h'},
  {"body-numbering", 1, NULL, 'b'},
  {"footer-numbering", 1, NULL, 'f'},
  {"first-page", 1, NULL, 'v'},
  {"page-increment", 1, NULL, 'i'},
  {"no-renumber", 0, NULL, 'p'},
  {"join-blank-lines", 1, NULL, 'l'},
  {"number-separator", 1, NULL, 's'},
  {"number-width", 1, NULL, 'w'},
  {"number-format", 1, NULL, 'n'},
  {"section-delimiter", 1, NULL, 'd'},
  {NULL, 0, NULL, 0}
};

void
main (argc, argv)
     int argc;
     char **argv;
{
  int c, exit_status = 0;

  program_name = argv[0];
  have_read_stdin = 0;

  while ((c = getopt_long (argc, argv, "h:b:f:v:i:pl:s:w:n:d:", longopts,
			   (int *) 0)) != EOF)
    {
      switch (c)
	{
	case 'h':
	  if (build_type_arg (&header_type, &header_regex) != TRUE)
	    usage ();
	  break;
	case 'b':
	  if (build_type_arg (&body_type, &body_regex) != TRUE)
	    usage ();
	  break;
	case 'f':
	  if (build_type_arg (&footer_type, &footer_regex) != TRUE)
	    usage ();
	  break;
	case 'v':
	  page_start = atoi (optarg);
	  break;
	case 'i':
	  page_incr = atoi (optarg);
	  if (page_incr < 1)
	    page_incr = 1;
	  break;
	case 'p':
	  reset_numbers = FALSE;
	  break;
	case 'l':
	  blank_join = atoi (optarg);
	  break;
	case 's':
	  separator_str = optarg;
	  break;
	case 'w':
	  lineno_width = atoi (optarg);
	  if (lineno_width < 1)
	    lineno_width = 1;
	  break;
	case 'n':
	  switch (*optarg)
	    {
	    case 'l':
	      if (optarg[1] == 'n')
		lineno_format = FORMAT_LEFT;
	      else
		usage ();
	      break;
	    case 'r':
	      switch (optarg[1])
		{
		case 'n':
		  lineno_format = FORMAT_RIGHT_NOLZ;
		  break;
		case 'z':
		  lineno_format = FORMAT_RIGHT_LZ;
		  break;
		default:
		  usage ();
		  break;
		}
	      break;
	    default:
	      usage ();
	      break;
	    }
	  break;
	case 'd':
	  section_del = optarg;
	  break;
	default:
	  usage ();
	  break;
	}
    }

  /* Initialize the section delimiters.  */
  c = strlen (section_del);

  header_del_len = c * 3;
  header_del = xmalloc (header_del_len + 1);
  strcat (strcat (strcpy (header_del, section_del), section_del), section_del);

  body_del_len = c * 2;
  body_del = xmalloc (body_del_len + 1);
  strcat (strcpy (body_del, section_del), section_del);

  footer_del_len = c;
  footer_del = xmalloc (footer_del_len + 1);
  strcpy (footer_del, section_del);

  /* Initialize the input buffer.  */
  initbuffer (&line_buf);

  /* Initialize the printf format for unnumbered lines. */
  c = strlen (separator_str);
  print_no_line_fmt = xmalloc (lineno_width + c + 1);
  memset (print_no_line_fmt, ' ', lineno_width + c);
  print_no_line_fmt[lineno_width + c] = '\0';

  line_no = page_start;
  current_type = body_type;
  current_regex = &body_regex;
  build_print_fmt ();

  /* Main processing. */

  if (optind == argc)
    exit_status |= nl_file ("-");
  else
    for (; optind < argc; optind++)
      exit_status |= nl_file (argv[optind]);

  if (have_read_stdin && fclose (stdin) == EOF)
    {
      error (0, errno, "-");
      exit_status = 1;
    }
  if (ferror (stdout) || fclose (stdout) == EOF)
    error (1, 0, "write error");

  exit (exit_status);
}

/* Process file FILE to standard output.
   Return 0 if successful, 1 if not. */

int
nl_file (file)
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

  process_file (stream);

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

/* Read and process the file pointed to by FP. */

void
process_file (fp)
     FILE *fp;
{
  while (readline (&line_buf, fp))
    {
      switch ((int) check_section ())
	{
	case Header:
	  proc_header ();
	  break;
	case Body:
	  proc_body ();
	  break;
	case Footer:
	  proc_footer ();
	  break;
	case Text:
	  proc_text ();
	  break;
	}
    }
}

/* Return the type of line in `line_buf'. */

enum section
check_section ()
{
  if (line_buf.length < 2 || memcmp (line_buf.buffer, section_del, 2))
    return Text;
  if (line_buf.length == header_del_len
      && !memcmp (line_buf.buffer, header_del, header_del_len))
    return Header;
  if (line_buf.length == body_del_len
      && !memcmp (line_buf.buffer, body_del, body_del_len))
    return Body;
  if (line_buf.length == footer_del_len
      && !memcmp (line_buf.buffer, footer_del, footer_del_len))
    return Footer;
  return Text;
}

/* Switch to a header section. */

void
proc_header ()
{
  current_type = header_type;
  current_regex = &header_regex;
  if (reset_numbers)
    line_no = page_start;
  putchar ('\n');
}

/* Switch to a body section. */

void
proc_body ()
{
  current_type = body_type;
  current_regex = &body_regex;
  putchar ('\n');
}

/* Switch to a footer section. */

void
proc_footer ()
{
  current_type = footer_type;
  current_regex = &footer_regex;
  putchar ('\n');
}

/* Process a regular text line in `line_buf'. */

void
proc_text ()
{
  static int blank_lines = 0;	/* Consecutive blank lines so far. */

  switch (*current_type)
    {
    case 'a':
      if (blank_join > 1)
	{
	  if (line_buf.length || ++blank_lines == blank_join)
	    {
	      print_lineno ();
	      blank_lines = 0;
	    }
	  else
	    printf (print_no_line_fmt);
	}
      else
	print_lineno ();
      break;
    case 't':
      if (line_buf.length)
	print_lineno ();
      else
	printf (print_no_line_fmt);
      break;
    case 'n':
      printf (print_no_line_fmt);
      break;
    case 'p':
      if (re_search (current_regex, line_buf.buffer, line_buf.length,
		     0, line_buf.length, (struct re_registers *) 0) < 0)
	printf (print_no_line_fmt);
      else
	print_lineno ();
      break;
    }
  fwrite (line_buf.buffer, sizeof (char), line_buf.length, stdout);
  putchar ('\n');
}

/* Print and increment the line number. */

void
print_lineno ()
{
  printf (print_fmt, line_no);
  line_no += page_incr;
}

/* Build the printf format string, based on `lineno_format'. */

void
build_print_fmt ()
{
  /* 12 = 10 chars for lineno_width, 1 for %, 1 for \0.  */
  print_fmt = xmalloc (strlen (separator_str) + 12);
  switch (lineno_format)
    {
    case FORMAT_RIGHT_NOLZ:
      sprintf (print_fmt, "%%%dd%s", lineno_width, separator_str);
      break;
    case FORMAT_RIGHT_LZ:
      sprintf (print_fmt, "%%0%dd%s", lineno_width, separator_str);
      break;
    case FORMAT_LEFT:
      sprintf (print_fmt, "%%-%dd%s", lineno_width, separator_str);
      break;
    }
}

/* Set the command line flag TYPEP and possibly the regex pointer REGEXP,
   according to `optarg'.  */

int
build_type_arg (typep, regexp)
     char **typep;
     struct re_pattern_buffer *regexp;
{
  char *errmsg;
  int rval = TRUE;
  int optlen;

  switch (*optarg)
    {
    case 'a':
    case 't':
    case 'n':
      *typep = optarg;
      break;
    case 'p':
      *typep = optarg++;
      optlen = strlen (optarg);
      regexp->allocated = optlen * 2;
      regexp->buffer = (unsigned char *) xmalloc (regexp->allocated);
      regexp->translate = NULL;
      regexp->fastmap = xmalloc (256);
      regexp->fastmap_accurate = 0;
      errmsg = (char *) re_compile_pattern (optarg, optlen, regexp);
      if (errmsg)
	error (1, 0, "%s", errmsg);
      break;
    default:
      rval = FALSE;
      break;
    }
  return rval;
}

/* Print a usage message and quit. */

void
usage ()
{
  fprintf (stderr, "\
Usage: %s [-h header-style] [-b body-style] [-f footer-style] [-p] [-d cc]\n\
       [-v start-number] [-i increment] [-l lines] [-s line-separator]\n\
       [-w line-no-width] [-n {ln,rn,rz}] [--header-numbering=style]\n\
       [--body-numbering=style] [--footer-numbering=style]\n\
       [--first-page=number] [--page-increment=number] [--no-renumber]\n\
       [--join-blank-lines=number] [--number-separator=string]\n\
       [--number-width=number] [--number-format={ln,rn,rz}]\n\
       [--section-delimiter=cc] [file...]\n",
	   program_name);
  exit (2);
}
