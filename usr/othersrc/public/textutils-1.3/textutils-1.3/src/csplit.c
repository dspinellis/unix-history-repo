/* csplit - split a file into sections determined by context lines
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

/* Written by Stuart Kemp, cpsrk@groper.jcu.edu.au.
   Modified by David MacKenzie, djm@gnu.ai.mit.edu. */

#include <stdio.h>
#include <getopt.h>
#include <ctype.h>
#include <sys/types.h>
#include <signal.h>
#include "regex.h"
#ifdef __386BSD__
#undef	RE_DUP_MAX
#include "system.h"
/* XXX need to resolve the conflict in RE_DUP_MAX definitions. */
#undef	RE_DUP_MAX
#define RE_DUP_MAX  ((1 << 15) - 1) 
#else
#include "system.h"
#endif /*  __386BSD__ */

#if !defined(USG) && !defined(STDC_HEADERS)
char *memchr ();
#endif

#ifdef STDC_HEADERS
#include <stdlib.h>
#else
char *malloc ();
char *realloc ();
#endif

void error ();

void cleanup ();
void close_output_file ();
void create_output_file ();
void save_line_to_file ();
void usage ();

#ifndef TRUE
#define FALSE 0
#define TRUE 1
#endif

/* Increment size of area for control records. */
#define ALLOC_SIZE 20

/* The default prefix for output file names. */
#define DEFAULT_PREFIX	"xx"

typedef int boolean;

/* A compiled pattern arg. */
struct control
{
  char *regexpr;		/* Non-compiled regular expression. */
  struct re_pattern_buffer re_compiled;	/* Compiled regular expression. */
  int offset;			/* Offset from regexp to split at. */
  int lines_required;		/* Number of lines required. */
  int repeat;			/* Repeat count. */
  int argnum;			/* ARGV index. */
  boolean ignore;		/* If true, produce no output (for regexp). */
};

/* Initial size of data area in buffers. */
#define START_SIZE	8191

/* Increment size for data area. */
#define INCR_SIZE	2048

/* Number of lines kept in each node in line list. */
#define CTRL_SIZE	80

#ifdef DEBUG
/* Some small values to test the algorithms. */
#define START_SIZE	200
#define INCR_SIZE	10
#define CTRL_SIZE	1
#endif

/* A string with a length count. */
struct cstring
{
  int len;
  char *str;
};

/* Pointers to the beginnings of lines in the buffer area.
   These structures are linked together if needed. */
struct line
{
  unsigned used;		/* Number of offsets used in this struct. */
  unsigned insert_index;	/* Next offset to use when inserting line. */
  unsigned retrieve_index;	/* Next index to use when retrieving line. */
  struct cstring starts[CTRL_SIZE]; /* Lines in the data area. */
  struct line *next;		/* Next in linked list. */
};

/* The structure to hold the input lines.
   Contains a pointer to the data area and a list containing
   pointers to the individual lines. */
struct buffer_record
{
  unsigned bytes_alloc;		/* Size of the buffer area. */
  unsigned bytes_used;		/* Bytes used in the buffer area. */
  unsigned start_line;		/* First line number in this buffer. */
  unsigned first_available;	/* First line that can be retrieved. */
  unsigned num_lines;		/* Number of complete lines in this buffer. */
  char *buffer;			/* Data area. */
  struct line *line_start;	/* Head of list of pointers to lines. */
  struct line *curr_line;	/* The line start record currently in use. */
  struct buffer_record *next;
};

/* Input file descriptor. */
int input_desc = 0;

/* List of available buffers. */
struct buffer_record *free_list = NULL;

/* Start of buffer list. */
struct buffer_record *head = NULL;

/* Partially read line. */
char *hold_area = NULL;

/* Number of chars in `hold_area'. */
unsigned hold_count = 0;

/* Number of the last line in the buffers. */
unsigned last_line_number = 0;

/* Number of the line currently being examined. */
unsigned current_line = 0;

/* Number of the last line in the input file. */
unsigned last_line_in_file = 0;

/* If TRUE, we have read EOF. */
boolean have_read_eof = FALSE;

/* Name of output files. */
char *filename_space = NULL;

/* Prefix part of output file names. */
char *prefix = NULL;

/* Number of digits to use in output file names. */
int digits = 2;

/* Number of files created so far. */
unsigned files_created = 0;

/* Number of bytes written to current file. */
unsigned bytes_written;

/* Output file pointer. */
FILE *output_stream = NULL;

/* Perhaps it would be cleaner to pass arg values instead of indexes. */
char **global_argv;

/* If TRUE, do not print the count of bytes in each output file. */
boolean suppress_count;

/* If TRUE, remove output files on error. */
boolean remove_files;

/* The compiled pattern arguments, which determine how to split
   the input file. */
struct control *controls;

/* Number of elements in `controls'. */
unsigned control_used;

/* The name this program was run with. */
char *program_name;

/* Allocate N bytes of memory dynamically, with error checking.  */

char *
xmalloc (n)
     unsigned n;
{
  char *p;

  p = malloc (n);
  if (p == NULL)
    {
      error (0, 0, "virtual memory exhausted");
      cleanup ();
    }
  return p;
}

/* Change the size of an allocated block of memory P to N bytes,
   with error checking.
   If P is NULL, run xmalloc.
   If N is 0, run free and return NULL.  */

char *
xrealloc (p, n)
     char *p;
     unsigned n;
{
  if (p == NULL)
    return xmalloc (n);
  if (n == 0)
    {
      free (p);
      return 0;
    }
  p = realloc (p, n);
  if (p == NULL)
    {
      error (0, 0, "virtual memory exhausted");
      cleanup ();
    }
  return p;
}

/* Keep track of NUM chars of a partial line in buffer START.
   These chars will be retrieved later when another large buffer is read.
   It is not necessary to create a new buffer for these chars; instead,
   we keep a pointer to the existing buffer.  This buffer *is* on the
   free list, and when the next buffer is obtained from this list
   (even if it is this one), these chars will be placed at the
   start of the new buffer. */

void
save_to_hold_area (start, num)
     char *start;
     unsigned num;
{
  hold_area = start;
  hold_count = num;
}

/* Read up to MAX chars from the input stream into DEST.
   Return the number of chars read. */

int
read_input (dest, max)
     char *dest;
     unsigned max;
{
  int bytes_read;

  if (max == 0)
    return 0;

  bytes_read = read (input_desc, dest, max);

  if (bytes_read == 0)
    have_read_eof = TRUE;

  if (bytes_read < 0)
    {
      error (0, errno, "read error");
      cleanup ();
    }

  return bytes_read;
}

/* Initialize existing line record P. */

void
clear_line_control (p)
     struct line *p;
{
  p->used = 0;
  p->insert_index = 0;
  p->retrieve_index = 0;
}

/* Initialize all line records in B. */

void
clear_all_line_control (b)
     struct buffer_record *b;
{
  struct line *l;

  for (l = b->line_start; l; l = l->next)
    clear_line_control (l);
}

/* Return a new, initialized line record. */

struct line *
new_line_control ()
{
  struct line *p;

  p = (struct line *) xmalloc (sizeof (struct line));

  p->next = NULL;
  clear_line_control (p);

  return p;
}

/* Record LINE_START, which is the address of the start of a line
   of length LINE_LEN in the large buffer, in the lines buffer of B. */

void
keep_new_line (b, line_start, line_len)
     struct buffer_record *b;
     char *line_start;
     int line_len;
{
  struct line *l;

  /* If there is no existing area to keep line info, get some. */
  if (b->line_start == NULL)
    b->line_start = b->curr_line = new_line_control ();

  /* If existing area for lines is full, get more. */
  if (b->curr_line->used == CTRL_SIZE)
    {
      b->curr_line->next = new_line_control ();
      b->curr_line = b->curr_line->next;
    }

  l = b->curr_line;

  /* Record the start of the line, and update counters. */
  l->starts[l->insert_index].str = line_start;
  l->starts[l->insert_index].len = line_len;
  l->used++;
  l->insert_index++;
}

/* Scan the buffer in B for newline characters
   and record the line start locations and lengths in B.
   Return the number of lines found in this buffer.

   There may be an incomplete line at the end of the buffer;
   a pointer is kept to this area, which will be used when
   the next buffer is filled. */

unsigned
record_line_starts (b)
     struct buffer_record *b;
{
  char *line_start;		/* Start of current line. */
  char *line_end;		/* End of each line found. */
  unsigned bytes_left;		/* Length of incomplete last line. */
  unsigned lines;		/* Number of lines found. */
  unsigned line_length;		/* Length of each line found. */

  if (b->bytes_used == 0)
    return 0;

  lines = 0;
  line_start = b->buffer;
  bytes_left = b->bytes_used;

  for (;;)
    {
      line_end = memchr (line_start, '\n', bytes_left);
      if (line_end == NULL)
	break;
      line_length = line_end - line_start + 1;
      keep_new_line (b, line_start, line_length);
      bytes_left -= line_length;
      line_start = line_end + 1;
      lines++;
    }

  /* Check for an incomplete last line. */
  if (bytes_left)
    {
      if (have_read_eof)
	{
	  keep_new_line (b, line_start, bytes_left);
	  lines++;
	  last_line_in_file = last_line_number + lines;
	}
      else
	save_to_hold_area (line_start, bytes_left);
    }

  b->num_lines = lines;
  b->first_available = b->start_line = last_line_number + 1;
  last_line_number += lines;

  return lines;
}

/* Return a new buffer with room to store SIZE bytes, plus
   an extra byte for safety. */

struct buffer_record *
create_new_buffer (size)
     unsigned size;
{
  struct buffer_record *new_buffer;

  new_buffer = (struct buffer_record *)
    xmalloc (sizeof (struct buffer_record));

  new_buffer->buffer = (char *) xmalloc (size + 1);

  new_buffer->bytes_alloc = size;
  new_buffer->line_start = new_buffer->curr_line = NULL;

  return new_buffer;
}

/* Return a new buffer of at least MINSIZE bytes.  If a buffer of at
   least that size is currently free, use it, otherwise create a new one. */

struct buffer_record *
get_new_buffer (min_size)
     unsigned min_size;
{
  struct buffer_record *p, *q;
  struct buffer_record *new_buffer; /* Buffer to return. */
  unsigned alloc_size;		/* Actual size that will be requested. */

  alloc_size = START_SIZE;
  while (min_size > alloc_size)
    alloc_size += INCR_SIZE;

  if (free_list == NULL)
    new_buffer = create_new_buffer (alloc_size);
  else
    {
      /* Use first-fit to find a buffer. */
      p = new_buffer = NULL;
      q = free_list;

      do
	{
	  if (q->bytes_alloc >= min_size)
	    {
	      if (p == NULL)
		free_list = q->next;
	      else
		p->next = q->next;
	      break;
	    }
	  p = q;
	  q = q->next;
	}
      while (q);

      new_buffer = (q ? q : create_new_buffer (alloc_size));

      new_buffer->curr_line = new_buffer->line_start;
      clear_all_line_control (new_buffer);
    }

  new_buffer->num_lines = 0;
  new_buffer->bytes_used = 0;
  new_buffer->start_line = new_buffer->first_available = last_line_number + 1;
  new_buffer->next = NULL;

  return new_buffer;
}

/* Add buffer BUF to the list of free buffers. */

void
free_buffer (buf)
     struct buffer_record *buf;
{
  buf->next = free_list;
  free_list = buf;
}

/* Append buffer BUF to the linked list of buffers that contain
   some data yet to be processed. */

void
save_buffer (buf)
     struct buffer_record *buf;
{
  struct buffer_record *p;

  buf->next = NULL;
  buf->curr_line = buf->line_start;

  if (head == NULL)
    head = buf;
  else
    {
      for (p = head; p->next; p = p->next)
	/* Do nothing. */ ;
      p->next = buf;
    }
}

/* Fill a buffer of input.

   Set the initial size of the buffer to a default.
   Fill the buffer (from the hold area and input stream)
   and find the individual lines.
   If no lines are found (the buffer is too small to hold the next line),
   release the current buffer (whose contents would have been put in the
   hold area) and repeat the process with another large buffer until at least
   one entire line has been read.

   Return TRUE if a new buffer was obtained, otherwise false
   (in which case end-of-file must have been encountered). */

boolean
load_buffer ()
{
  struct buffer_record *b;
  unsigned bytes_wanted = START_SIZE; /* Minimum buffer size. */
  unsigned bytes_avail;		/* Size of new buffer created. */
  unsigned lines_found;		/* Number of lines in this new buffer. */
  char *p;			/* Place to load into buffer. */

  if (have_read_eof)
    return FALSE;

  /* We must make the buffer at least as large as the amount of data
     in the partial line left over from the last call. */
  if (bytes_wanted < hold_count)
    bytes_wanted = hold_count;

  do
    {
      b = get_new_buffer (bytes_wanted);
      bytes_avail = b->bytes_alloc; /* Size of buffer returned. */
      p = b->buffer;

      /* First check the `holding' area for a partial line. */
      if (hold_count)
	{
	  if (p != hold_area)
	    bcopy (hold_area, p, hold_count);
	  p += hold_count;
	  b->bytes_used += hold_count;
	  bytes_avail -= hold_count;
	  hold_count = 0;
	}

      b->bytes_used += (unsigned) read_input (p, bytes_avail);

      lines_found = record_line_starts (b);
      bytes_wanted = b->bytes_alloc + INCR_SIZE;
      if (!lines_found)
	free_buffer (b);
    }
  while (!lines_found && !have_read_eof);

  if (lines_found)
    save_buffer (b);

  return lines_found != 0;
}

/* Return the line number of the first line that has not yet been retrieved. */

unsigned
get_first_line_in_buffer ()
{
  if (head == NULL && !load_buffer ())
    error (1, errno, "input disappeared");

  return head->first_available;
}

/* Return a pointer to the logical first line in the buffer and make the 
   next line the logical first line.
   Return NULL if there is no more input. */

struct cstring *
remove_line ()
{
  struct cstring *line;		/* Return value. */
  unsigned line_got;		/* Number of the line retrieved. */
  struct line *l;		/* For convenience. */

  if (head == NULL && !load_buffer ())
    return NULL;

  if (current_line < head->first_available)
    current_line = head->first_available;

  line_got = head->first_available++;

  l = head->curr_line;

  line = &l->starts[l->retrieve_index];

  /* Advance index to next line. */
  if (++l->retrieve_index == l->used)
    {
      /* Go on to the next line record. */
      head->curr_line = l->next;
      if (head->curr_line == NULL || head->curr_line->used == 0)
	{
	  /* Go on to the next data block. */
	  struct buffer_record *b = head;
	  head = head->next;
	  free_buffer (b);
	}
    }

  return line;
}

/* Search the buffers for line LINENUM, reading more input if necessary.
   Return a pointer to the line, or NULL if it is not found in the file. */

struct cstring *
find_line (linenum)
     unsigned linenum;
{
  struct buffer_record *b;

  if (head == NULL && !load_buffer ())
    return NULL;

  if (linenum < head->start_line)
    return NULL;

  for (b = head;;)
    {
      if (linenum < b->start_line + b->num_lines)
	{
	  /* The line is in this buffer. */
	  struct line *l;
	  unsigned offset;	/* How far into the buffer the line is. */

	  l = b->line_start;
	  offset = linenum - b->start_line;
	  /* Find the control record. */
	  while (offset >= CTRL_SIZE)
	    {
	      l = l->next;
	      offset -= CTRL_SIZE;
	    }
	  return &l->starts[offset];
	}
      if (b->next == NULL && !load_buffer ())
	return NULL;
      b = b->next;		/* Try the next data block. */
    }
}

/* Return TRUE if at least one more line is available for input. */

boolean
no_more_lines ()
{
  return (find_line (current_line + 1) == NULL) ? TRUE : FALSE;
}

/* Set the name of the input file to NAME and open it. */

void
set_input_file (name)
     char *name;
{
  if (!strcmp (name, "-"))
    input_desc = 0;
  else
    {
      input_desc = open (name, O_RDONLY);
      if (input_desc < 0)
	error (1, errno, "%s", name);
    }
}

/* Write all lines from the beginning of the buffer up to, but
   not including, line LAST_LINE, to the current output file.
   If IGNORE is TRUE, do not output lines selected here.
   ARGNUM is the index in ARGV of the current pattern. */

void
write_to_file (last_line, ignore, argnum)
     unsigned last_line;
     boolean ignore;
     int argnum;
{
  struct cstring *line;
  unsigned first_line;		/* First available input line. */
  unsigned lines;		/* Number of lines to output. */
  unsigned i;

  first_line = get_first_line_in_buffer ();

  if (first_line > last_line)
    {
      error (0, 0, "%s: line number out of range", global_argv[argnum]);
      cleanup ();
    }

  lines = last_line - first_line;

  for (i = 0; i < lines; i++)
    {
      line = remove_line ();
      if (line == NULL)
	{
	  error (0, 0, "%s: line number out of range", global_argv[argnum]);
	  cleanup ();
	}
      if (!ignore)
	save_line_to_file (line);
    }
}

/* Output any lines left after all regexps have been processed. */

void
dump_rest_of_file ()
{
  struct cstring *line;

  while ((line = remove_line ()) != NULL)
    save_line_to_file (line);
}

/* Handle an attempt to read beyond EOF under the control of record P,
   on iteration REPETITION if nonzero. */

void
handle_line_error (p, repetition)
     struct control *p;
     int repetition;
{
  fprintf (stderr, "%s: `%d': line number out of range",
	   program_name, p->lines_required);
  if (repetition)
    fprintf (stderr, " on repetition %d\n", repetition);
  else
    fprintf (stderr, "\n");

  cleanup ();
}

/* Determine the line number that marks the end of this file,
   then get those lines and save them to the output file.
   P is the control record.
   REPETITION is the repetition number. */

void
process_line_count (p, repetition)
     struct control *p;
     int repetition;
{
  unsigned linenum;
  unsigned last_line_to_save = p->lines_required * (repetition + 1);
  struct cstring *line;

  create_output_file ();

  linenum = get_first_line_in_buffer ();

  /* Check for requesting a line that has already been written out.
     If this ever happens, it's due to a bug in csplit. */
  if (linenum >= last_line_to_save)
    handle_line_error (p, repetition);

  while (linenum++ < last_line_to_save)
    {
      line = remove_line ();
      if (line == NULL)
	handle_line_error (p, repetition);
      save_line_to_file (line);
    }

  close_output_file ();

  /* Ensure that the line number specified is not 1 greater than
     the number of lines in the file. */
  if (no_more_lines ())
    handle_line_error (p, repetition);
}

void
regexp_error (p, repetition, ignore)
     struct control *p;
     int repetition;
     boolean ignore;
{
  fprintf (stderr, "%s: `%s': match not found",
	   program_name, global_argv[p->argnum]);

  if (repetition)
    fprintf (stderr, " on repetition %d\n", repetition);
  else
    fprintf (stderr, "\n");

  if (!ignore)
    {
      dump_rest_of_file ();
      close_output_file ();
    }
  cleanup ();
}

/* Read the input until a line matches the regexp in P, outputting
   it unless P->IGNORE is TRUE.
   REPETITION is this repeat-count; 0 means the first time. */

void
process_regexp (p, repetition)
     struct control *p;
     int repetition;
{
  struct cstring *line;		/* From input file. */
  register unsigned line_len;	/* To make "$" in regexps work. */
  unsigned break_line;		/* First line number of next file. */
  boolean ignore = p->ignore;	/* If TRUE, skip this section. */
  int ret;

  if (!ignore)
    create_output_file ();

  /* If there is no offset for the regular expression, or
     it is positive, then it is not necessary to buffer the lines. */

  if (p->offset >= 0)
    {
      for (;;)
	{
	  line = find_line (++current_line);
	  if (line == NULL)
	    regexp_error (p, repetition, ignore);
	  line_len = line->len;
	  if (line->str[line_len - 1] == '\n')
	    line_len--;
	  ret = re_search (&p->re_compiled, line->str, line_len,
			   0, line_len, (struct re_registers *) 0);
	  if (ret == -2)
	    {
	      error (0, 0, "error in regular expression search");
	      cleanup ();
	    }
	  if (ret == -1)
	    {
	      line = remove_line ();
	      if (!ignore)
		save_line_to_file (line);
	    }
	  else
	    break;
	}
    }
  else
    {
      /* Buffer the lines. */
      for (;;)
	{
	  line = find_line (++current_line);
	  if (line == NULL)
	    regexp_error (p, repetition, ignore);
	  line_len = line->len;
	  if (line->str[line_len - 1] == '\n')
	    line_len--;
	  ret = re_search (&p->re_compiled, line->str, line_len,
			   0, line_len, (struct re_registers *) 0);
	  if (ret == -2)
	    {
	      error (0, 0, "error in regular expression search");
	      cleanup ();
	    }
	  if (ret >= 0)
	    break;
	}
    }

  /* Account for any offset from this regexp. */
  break_line = current_line + p->offset;

  write_to_file (break_line, ignore, p->argnum);

  if (!ignore)
    close_output_file ();

  current_line = break_line;
}

/* Split the input file according to the control records we have built. */

void
split_file ()
{
  register int i, j;

  for (i = 0; i < control_used; i++)
    {
      if (controls[i].regexpr)
	{
	  for (j = 0; j <= controls[i].repeat; j++)
	    process_regexp (&controls[i], j);
	}
      else
	{
	  for (j = 0; j <= controls[i].repeat; j++)
	    process_line_count (&controls[i], j);
	}
    }

  create_output_file ();
  dump_rest_of_file ();
  close_output_file ();
}

/* Return the name of output file number NUM. */

char *
make_filename (num)
     int num;
{
  sprintf (filename_space, "%s%0*d", prefix, digits, num);
  return filename_space;
}

/* Create the next output file. */

void
create_output_file ()
{
  char *name;

  name = make_filename (files_created);
  output_stream = fopen (name, "w");
  if (output_stream == NULL)
    {
      error (0, errno, "%s", name);
      cleanup ();
    }
  files_created++;
  bytes_written = 0;
}

/* Delete all the files we have created. */

void
delete_all_files ()
{
  int i;
  char *name;

  for (i = 0; i < files_created; i++)
    {
      name = make_filename (i);
      if (unlink (name))
	error (0, errno, "%s", name);
    }
}

/* Close the current output file and print the count
   of characters in this file. */

void
close_output_file ()
{
  if (output_stream)
    {
      if (fclose (output_stream) == EOF)
	{
	  error (0, errno, "write error");
	  cleanup ();
	}
      if (!suppress_count)
	fprintf (stdout, "%d\n", bytes_written);
      output_stream = NULL;
    }
}

/* Optionally remove files created so far; then exit.
   Called when an error detected. */

void
cleanup ()
{
  if (output_stream)
    close_output_file ();

  if (remove_files)
    delete_all_files ();

  exit (1);
}

/* Save line LINE to the output file and
   increment the character count for the current file. */

void
save_line_to_file (line)
     struct cstring *line;
{
  fwrite (line->str, sizeof (char), line->len, output_stream);
  bytes_written += line->len;
}

/* Return a new, initialized control record. */

struct control *
new_control_record ()
{
  static unsigned control_allocated = 0; /* Total space allocated. */
  register struct control *p;

  if (control_allocated == 0)
    {
      control_allocated = ALLOC_SIZE;
      controls = (struct control *)
	xmalloc (sizeof (struct control) * control_allocated);
    }
  else if (control_used == control_allocated)
    {
      control_allocated += ALLOC_SIZE;
      controls = (struct control *)
	xrealloc (controls, sizeof (struct control) * control_allocated);
    }
  p = &controls[control_used++];
  p->regexpr = NULL;
  p->repeat = 0;
  p->lines_required = 0;
  p->offset = 0;
  return p;
}

/* Convert string NUM to an integer and put the value in *RESULT.
   Return a TRUE if the string consists entirely of digits,
   FALSE if not. */

boolean
string_to_number (result, num)
     int *result;
     char *num;
{
  register char ch;
  register int val = 0;

  if (*num == '\0')
    return FALSE;

  while (ch = *num++)
    {
      if (!isdigit (ch))
	return FALSE;
      val = val * 10 + ch - '0';
    }

  *result = val;
  return TRUE;
}

/* Check if there is a numeric offset after a regular expression.
   STR is the entire command line argument.
   ARGNUM is the index in ARGV of STR.
   P is the control record for this regular expression.
   NUM is the numeric part of STR. */

void
check_for_offset (argnum, p, str, num)
     int argnum;
     struct control *p;
     char *str;
     char *num;
{
  if (*num != '-' && *num != '+')
    error (1, 0, "%s: `+' or `-' expected after delimeter", str);

  if (!string_to_number (&p->offset, num + 1))
    error (1, 0, "%s: integer expected after `%c'", str, *num);

  if (*num == '-')
    p->offset = -p->offset;
}

/* Given that the first character of command line arg STR is '{',
   make sure that the rest of the string is a valid repeat count
   and store its value in P.
   ARGNUM is the ARGV index of STR. */

void
parse_repeat_count (argnum, p, str)
     int argnum;
     struct control *p;
     char *str;
{
  char *end;

  end = str + strlen (str) - 1;
  if (*end != '}')
    error (1, 0, "%s: `}' is required in repeat count", str);
  *end = '\0';

  if (!string_to_number (&p->repeat, str +  1))
    error (1, 0, "%s}: integer required between `{' and `}'",
	   global_argv[argnum]);

  *end = '}';
}

/* Extract the regular expression from STR and check for a numeric offset.
   STR should start with the regexp delimiter character.
   Return a new control record for the regular expression.
   ARGNUM is the ARGV index of STR.
   Unless IGNORE is TRUE, mark these lines for output. */

struct control *
extract_regexp (argnum, ignore, str)
     int argnum;
     boolean ignore;
     char *str;
{
  int len;			/* Number of chars in this regexp. */
  char delim = *str;
  char *closing_delim;
  struct control *p;
  char *err;

  closing_delim = rindex (str + 1, delim);
  if (closing_delim == NULL)
    error (1, 0, "%s: closing delimeter `%c' missing", str, delim);

  len = closing_delim - str - 1;
  p = new_control_record ();
  p->argnum = argnum;
  p->ignore = ignore;

  p->regexpr = (char *) xmalloc ((unsigned) (len + 1));
  strncpy (p->regexpr, str + 1, len);
  p->re_compiled.allocated = len * 2;
  p->re_compiled.buffer = (unsigned char *) xmalloc (p->re_compiled.allocated);
  p->re_compiled.fastmap = xmalloc (256);
  p->re_compiled.translate = 0;
  err = (char *) re_compile_pattern (p->regexpr, len, &p->re_compiled);
  if (err)
    {
      error (0, 0, "%s: invalid regular expression: %s", str, err);
      cleanup ();
    }

  if (closing_delim[1])
    check_for_offset (argnum, p, str, closing_delim + 1);

  return p;
}

/* Extract the break patterns from args START through ARGC - 1 of ARGV.
   After each pattern, check if the next argument is a repeat count. */

void
parse_patterns (argc, start, argv)
     int argc;
     int start;
     char **argv;
{
  int i;			/* Index into ARGV. */
  struct control *p;		/* New control record created. */

  for (i = start; i < argc; i++)
    {
      if (*argv[i] == '/' || *argv[i] == '%')
	{
	  p = extract_regexp (i, *argv[i] == '%', argv[i]);
	}
      else
	{
	  p = new_control_record ();
	  p->argnum = i;
	  if (!string_to_number (&p->lines_required, argv[i]))
	    error (1, 0, "%s: invalid pattern", argv[i]);
	}

      if (i + 1 < argc && *argv[i + 1] == '{')
	{
	  /* We have a repeat count. */
	  i++;
	  parse_repeat_count (i, p, argv[i]);
	}
    }
}

void
interrupt_handler ()
{
  error (0, 0, "interrupted");
  cleanup ();
}

struct option longopts[] =
{
  {"digits", 1, NULL, 'n'},
  {"quiet", 0, NULL, 's'},
  {"silent", 0, NULL, 's'},
  {"keep-files", 0, NULL, 'k'},
  {"prefix", 1, NULL, 'f'},
  {NULL, 0, NULL, 0}
};

void
main (argc, argv)
     int argc;
     char **argv;
{
  int optc;
#ifdef _POSIX_VERSION
  struct sigaction oldact, newact;
#endif				/* _POSIX_VERSION */

  program_name = argv[0];
  global_argv = argv;
  controls = NULL;
  control_used = 0;
  suppress_count = FALSE;
  remove_files = TRUE;
  prefix = DEFAULT_PREFIX;

#ifdef _POSIX_VERSION
  newact.sa_handler = interrupt_handler;
  sigemptyset (&newact.sa_mask);
  newact.sa_flags = 0;

  sigaction (SIGHUP, NULL, &oldact);
  if (oldact.sa_handler != SIG_IGN)
    sigaction (SIGHUP, &newact, NULL);

  sigaction (SIGINT, NULL, &oldact);
  if (oldact.sa_handler != SIG_IGN)
    sigaction (SIGINT, &newact, NULL);

  sigaction (SIGQUIT, NULL, &oldact);
  if (oldact.sa_handler != SIG_IGN)
    sigaction (SIGQUIT, &newact, NULL);

  sigaction (SIGTERM, NULL, &oldact);
  if (oldact.sa_handler != SIG_IGN)
    sigaction (SIGTERM, &newact, NULL);
#else				/* !_POSIX_VERSION */
  if (signal (SIGHUP, SIG_IGN) != SIG_IGN)
    signal (SIGHUP, interrupt_handler);
  if (signal (SIGINT, SIG_IGN) != SIG_IGN)
    signal (SIGINT, interrupt_handler);
  if (signal (SIGQUIT, SIG_IGN) != SIG_IGN)
    signal (SIGQUIT, interrupt_handler);
  if (signal (SIGTERM, SIG_IGN) != SIG_IGN)
    signal (SIGTERM, interrupt_handler);
#endif

  while ((optc = getopt_long (argc, argv, "f:kn:s", longopts, (int *) 0))
	 != EOF)
    switch (optc)
      {
      case 'f':
	prefix = optarg;
	break;

      case 'k':
	remove_files = FALSE;
	break;

      case 'n':
	if (!string_to_number (&digits, optarg))
	  error (1, 0, "%s: invalid number", optarg);
	break;

      case 's':
	suppress_count = TRUE;
	break;

      default:
	usage ();
      }

  if (optind >= argc - 1)
    usage ();

  filename_space = (char *) xmalloc (strlen (prefix) + digits + 2);

  set_input_file (argv[optind++]);

  parse_patterns (argc, optind, argv);

  split_file ();

  if (close (input_desc) < 0)
    {
      error (0, errno, "read error");
      cleanup ();
    }

  exit (0);
}

void
usage ()
{
  fprintf (stderr, "\
Usage: %s [-sk] [-f prefix] [-n digits] [--prefix=prefix]\n\
       [--digits=digits] [--quiet] [--silent] [--keep-files] file pattern...\n",
	   program_name);
  exit (1);
}
