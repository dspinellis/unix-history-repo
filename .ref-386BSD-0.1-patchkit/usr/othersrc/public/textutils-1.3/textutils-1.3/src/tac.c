/* tac - concatenate and print files in reverse
   Copyright (C) 1988, 1989, 1990, 1991 Free Software Foundation, Inc.

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

/* Written by Jay Lepreau (lepreau@cs.utah.edu).
   GNU enhancements by David MacKenzie (djm@ai.mit.edu). */

/* Copy each FILE, or the standard input if none are given or when a
   FILE name of "-" is encountered, to the standard output with the
   order of the records reversed.  The records are separated by
   instances of a string, or a newline if none is given.  By default, the
   separator string is attached to the end of the record that it
   follows in the file.

   Options:
   -b, --before			The separator is attached to the beginning
				of the record that it precedes in the file.
   -r, --regex			The separator is a regular expression.
   -s, --separator=separator	Use SEPARATOR as the record separator.

   To reverse a file byte by byte, use (in bash, ksh, or sh):
tac -r -s '.\|
' file */

#include <stdio.h>
#include <getopt.h>
#include <sys/types.h>
#include <signal.h>
#include <regex.h>
#ifdef __386BSD__
#undef	RE_DUP_MAX
#include "system.h"
/* XXX need to resolve the conflict in RE_DUP_MAX definitions. */
#undef	RE_DUP_MAX
#define RE_DUP_MAX  ((1 << 15) - 1) 
#else
#include "system.h"
#endif /*  __386BSD__ */

#ifndef STDC_HEADERS
char *malloc ();
char *realloc ();
#endif

/* The number of bytes per atomic read. */
#define INITIAL_READSIZE 8192

/* The number of bytes per atomic write. */
#define WRITESIZE 8192

char *mktemp ();

SIGTYPE cleanup ();
int tac ();
int tac_file ();
int tac_stdin ();
char *xmalloc ();
char *xrealloc ();
void output ();
void error ();
void save_stdin ();
void xwrite ();

/* The name this program was run with. */
char *program_name;

/* The string that separates the records of the file. */
char *separator;

/* If nonzero, print `separator' along with the record preceding it
   in the file; otherwise with the record following it. */
int separator_ends_record;

/* 0 if `separator' is to be matched as a regular expression;
   otherwise, the length of `separator', used as a sentinel to
   stop the search. */
int sentinel_length;

/* The length of a match with `separator'.  If `sentinel_length' is 0,
   `match_length' is computed every time a match succeeds;
   otherwise, it is simply the length of `separator'. */
int match_length;

/* The input buffer. */
char *buffer;

/* The number of bytes to read at once into `buffer'. */
unsigned read_size;

/* The size of `buffer'.  This is read_size * 2 + sentinel_length + 2.
   The extra 2 bytes allow `past_end' to have a value beyond the
   end of `buffer' and `match_start' to run off the front of `buffer'. */
unsigned buffer_size;

/* The compiled regular expression representing `separator'. */
static struct re_pattern_buffer compiled_separator;

struct option longopts[] =
{
  {"before", 0, &separator_ends_record, 0},
  {"regex", 0, &sentinel_length, 0},
  {"separator", 1, NULL, 's'},
  {NULL, 0, NULL, 0}
};

void
main (argc, argv)
     int argc;
     char **argv;
{
  char *error_message;		/* Return value from re_compile_pattern. */
  int optc, errors;
  int have_read_stdin = 0;

  program_name = argv[0];
  errors = 0;
  separator = "\n";
  sentinel_length = 1;
  separator_ends_record = 1;

  while ((optc = getopt_long (argc, argv, "brs:", longopts, (int *) 0))
	 != EOF)
    {
      switch (optc)
	{
	case 0:
	  break;
	case 'b':
	  separator_ends_record = 0;
	  break;
	case 'r':
	  sentinel_length = 0;
	  break;
	case 's':
	  separator = optarg;
	  if (*separator == 0)
	    error (1, 0, "separator cannot be empty");
	  break;
	default:
	  fprintf (stderr, "\
Usage: %s [-br] [-s separator] [--before] [--regex] [--separator=separator]\n\
       [file...]\n",
		   program_name);
	  exit (1);
	}
    }

  if (sentinel_length == 0)
    {
      compiled_separator.allocated = 100;
      compiled_separator.buffer = (unsigned char *)
	xmalloc (compiled_separator.allocated);
      compiled_separator.fastmap = xmalloc (256);
      compiled_separator.translate = 0;
      error_message = re_compile_pattern (separator, strlen (separator),
					  &compiled_separator);
      if (error_message)
	error (1, 0, "%s", error_message);
    }
  else
    match_length = sentinel_length = strlen (separator);

  read_size = INITIAL_READSIZE;
  /* A precaution that will probably never be needed. */
  while (sentinel_length * 2 >= read_size)
    read_size *= 2;
  buffer_size = read_size * 2 + sentinel_length + 2;
  buffer = xmalloc (buffer_size);
  if (sentinel_length)
    {
      strcpy (buffer, separator);
      buffer += sentinel_length;
    }
  else
    ++buffer;

  if (optind == argc)
    {
      have_read_stdin = 1;
      errors = tac_stdin ();
    }
  else
    for (; optind < argc; ++optind)
      {
	if (strcmp (argv[optind], "-") == 0)
	  {
	    have_read_stdin = 1;
	    errors |= tac_stdin ();
	  }
	else
	  errors |= tac_file (argv[optind]);
      }

  /* Flush the output buffer. */
  output ((char *) NULL, (char *) NULL);

  if (have_read_stdin && close (0) < 0)
    error (1, errno, "-");
  if (close (1) < 0)
    error (1, errno, "write error");
  exit (errors);
}

/* The name of a temporary file containing a copy of pipe input. */
char *tempfile;

/* Print the standard input in reverse, saving it to temporary
   file `tempfile' first if it is a pipe.
   Return 0 if ok, 1 if an error occurs. */

int
tac_stdin ()
{
  /* Previous values of signal handlers. */
  SIGTYPE (*sigint) (), (*sighup) (), (*sigterm) ();
  int errors;
  struct stat stats;
#ifdef _POSIX_VERSION
    struct sigaction oldact, newact;
#endif				/* _POSIX_VERSION */

  /* No tempfile is needed for "tac < file".
     Use fstat instead of checking for errno == ESPIPE because
     lseek doesn't work on some special files but doesn't return an
     error, either. */
  if (fstat (0, &stats))
    {
      error (0, errno, "standard input");
      return 1;
    }
  if (S_ISREG (stats.st_mode))
    return tac (0, "standard input");

#ifdef _POSIX_VERSION
  newact.sa_handler = cleanup;
  sigemptyset (&newact.sa_mask);
  newact.sa_flags = 0;

  sigaction (SIGINT, NULL, &oldact);
  sigint = oldact.sa_handler;
  if (sigint != SIG_IGN)
    sigaction (SIGINT, &newact, NULL);

  sigaction (SIGHUP, NULL, &oldact);
  sighup = oldact.sa_handler;
  if (sighup != SIG_IGN)
    sigaction (SIGHUP, &newact, NULL);

  sigaction (SIGTERM, NULL, &oldact);
  sigterm = oldact.sa_handler;
  if (sigterm != SIG_IGN)
    sigaction (SIGTERM, &newact, NULL);
#else				/* !_POSIX_VERSION */
  sigint = signal (SIGINT, SIG_IGN);
  if (sigint != SIG_IGN)
    signal (SIGINT, cleanup);

  sighup = signal (SIGHUP, SIG_IGN);
  if (sighup != SIG_IGN)
    signal (SIGHUP, cleanup);

  sigterm = signal (SIGTERM, SIG_IGN);
  if (sigterm != SIG_IGN)
    signal (SIGTERM, cleanup);
#endif				/* _POSIX_VERSION */

  save_stdin ();

  errors = tac_file (tempfile);

  unlink (tempfile);

#ifdef _POSIX_VERSION
  newact.sa_handler = sigint;
  sigaction (SIGINT, &newact, NULL);
  newact.sa_handler = sighup;
  sigaction (SIGHUP, &newact, NULL);
  newact.sa_handler = sigterm;
  sigaction (SIGTERM, &newact, NULL);
#else				/* !_POSIX_VERSION */
  signal (SIGINT, sigint);
  signal (SIGHUP, sighup);
  signal (SIGTERM, sigterm);
#endif				/* _POSIX_VERSION */

  return errors;
}

/* Make a copy of the standard input in `tempfile'. */

void
save_stdin ()
{
  static char *template = NULL;
  static char *tempdir;
  int fd;
  int bytes_read;

  if (template == NULL)
    {
      tempdir = getenv ("TMPDIR");
      if (tempdir == NULL)
	tempdir = "/tmp";
      template = xmalloc (strlen (tempdir) + 11);
    }
  sprintf (template, "%s/tacXXXXXX", tempdir);
  tempfile = mktemp (template);

  fd = creat (tempfile, 0600);
  if (fd == -1)
    {
      error (0, errno, "%s", tempfile);
      cleanup ();
    }
  while ((bytes_read = read (0, buffer, read_size)) > 0)
    if (write (fd, buffer, bytes_read) != bytes_read)
      {
	error (0, errno, "%s", tempfile);
	cleanup ();
      }
  if (close (fd) < 0)
    {
      error (0, errno, "%s", tempfile);
      cleanup ();
    }
  if (bytes_read == -1)
    {
      error (0, errno, "read error");
      cleanup ();
    }
}

/* Print FILE in reverse.
   Return 0 if ok, 1 if an error occurs. */

int
tac_file (file)
     char *file;
{
  int fd, errors;

  fd = open (file, 0);
  if (fd == -1)
    {
      error (0, errno, "%s", file);
      return 1;
    }
  errors = tac (fd, file);
  if (close (fd) < 0)
    {
      error (0, errno, "%s", file);
      return 1;
    }
  return errors;
}

/* Print in reverse the file open on descriptor FD for reading FILE.
   Return 0 if ok, 1 if an error occurs. */

int
tac (fd, file)
     int fd;
     char *file;
{
  /* Pointer to the location in `buffer' where the search for
     the next separator will begin. */
  char *match_start;
  /* Pointer to one past the rightmost character in `buffer' that
     has not been printed yet. */
  char *past_end;
  unsigned saved_record_size;	/* Length of the record growing in `buffer'. */
  off_t file_pos;		/* Offset in the file of the next read. */
  /* Nonzero if `output' has not been called yet for any file.
     Only used when the separator is attached to the preceding record. */
  int first_time = 1;
  char first_char = *separator;	/* Speed optimization, non-regexp. */
  char *separator1 = separator + 1; /* Speed optimization, non-regexp. */
  int match_length1 = match_length - 1; /* Speed optimization, non-regexp. */
  struct re_registers regs;

  /* Find the size of the input file. */
  file_pos = lseek (fd, (off_t) 0, SEEK_END);
  if (file_pos < 1)
    return 0;			/* It's an empty file. */

  /* Arrange for the first read to lop off enough to leave the rest of the
     file a multiple of `read_size'.  Since `read_size' can change, this may
     not always hold during the program run, but since it usually will, leave
     it here for i/o efficiency (page/sector boundaries and all that).
     Note: the efficiency gain has not been verified. */
  saved_record_size = file_pos % read_size;
  if (saved_record_size == 0)
    saved_record_size = read_size;
  file_pos -= saved_record_size;
  /* `file_pos' now points to the start of the last (probably partial) block
     in the input file. */

  lseek (fd, file_pos, SEEK_SET);
  if (read (fd, buffer, saved_record_size) != saved_record_size)
    {
      error (0, 1, "%s", file);
      return 1;
    }

  match_start = past_end = buffer + saved_record_size;
  /* For non-regexp search, move past impossible positions for a match. */
  if (sentinel_length)
    match_start -= match_length1;

  for (;;)
    {
      /* Search backward from `match_start' - 1 to `buffer' for a match
	 with `separator'; for speed, use strncmp if `separator' contains no
	 metacharacters.
	 If the match succeeds, set `match_start' to point to the start of
	 the match and `match_length' to the length of the match.
	 Otherwise, make `match_start' < `buffer'. */
      if (sentinel_length == 0)
	{
	  int i = match_start - buffer;
	  int ret;

	  ret = re_search (&compiled_separator, buffer, i, i - 1, -i, &regs);
	  if (ret == -1)
	    match_start = buffer - 1;
	  else if (ret == -2)
	    {
	      error (0, 0, "error in regular expression search");
	      cleanup ();
	    }
	  else
	    {
	      match_start = buffer + regs.start[0];
	      match_length = regs.end[0] - regs.start[0];
	    }
	}
      else
	{
	  /* `match_length' is constant for non-regexp boundaries. */
	  while (*--match_start != first_char
		 || (match_length1 && strncmp (match_start + 1, separator1,
					       match_length1)))
	    /* Do nothing. */ ;
	}

      /* Check whether we backed off the front of `buffer' without finding
         a match for `separator'. */
      if (match_start < buffer)
	{
	  if (file_pos == 0)
	    {
	      /* Hit the beginning of the file; print the remaining record. */
	      output (buffer, past_end);
	      return 0;
	    }

	  saved_record_size = past_end - buffer;
	  if (saved_record_size > read_size)
	    {
	      /* `buffer_size' is about twice `read_size', so since
		 we want to read in another `read_size' bytes before
		 the data already in `buffer', we need to increase
		 `buffer_size'. */
	      char *newbuffer;
	      int offset = sentinel_length ? sentinel_length : 1;

	      read_size *= 2;
	      buffer_size = read_size * 2 + sentinel_length + 2;
	      newbuffer = xrealloc (buffer - offset, buffer_size) + offset;
	      /* Adjust the pointers for the new buffer location.  */
	      match_start += newbuffer - buffer;
	      past_end += newbuffer - buffer;
	      buffer = newbuffer;
	    }

	  /* Back up to the start of the next bufferfull of the file.  */
	  if (file_pos >= read_size)
	    file_pos -= read_size;
	  else
	    {
	      read_size = file_pos;
	      file_pos = 0;
	    }
	  lseek (fd, file_pos, SEEK_SET);

	  /* Shift the pending record data right to make room for the new. */
	  bcopy (buffer, buffer + read_size, saved_record_size);
	  past_end = buffer + read_size + saved_record_size;
	  /* For non-regexp searches, avoid unneccessary scanning. */
	  if (sentinel_length)
	    match_start = buffer + read_size;
	  else
	    match_start = past_end;

	  if (read (fd, buffer, read_size) != read_size)
	    {
	      error (0, errno, "%s", file);
	      return 1;
	    }
	}
      else
	{
	  /* Found a match of `separator'. */
	  if (separator_ends_record)
	    {
	      char *match_end = match_start + match_length;

	      /* If this match of `separator' isn't at the end of the
	         file, print the record. */
	      if (first_time == 0 || match_end != past_end)
		output (match_end, past_end);
	      past_end = match_end;
	      first_time = 0;
	    }
	  else
	    {
	      output (match_start, past_end);
	      past_end = match_start;
	    }
	  match_start -= match_length - 1;
	}
    }
}

/* Print the characters from START to PAST_END - 1.
   If START is NULL, just flush the buffer. */

void
output (start, past_end)
     char *start;
     char *past_end;
{
  static char buffer[WRITESIZE];
  static int bytes_in_buffer = 0;
  int bytes_to_add = past_end - start;
  int bytes_available = WRITESIZE - bytes_in_buffer;

  if (start == 0)
    {
      xwrite (1, buffer, bytes_in_buffer);
      bytes_in_buffer = 0;
      return;
    }
  
  /* Write out as many full buffers as possible. */
  while (bytes_to_add >= bytes_available)
    {
      bcopy (start, buffer + bytes_in_buffer, bytes_available);
      bytes_to_add -= bytes_available;
      start += bytes_available;
      xwrite (1, buffer, WRITESIZE);
      bytes_in_buffer = 0;
      bytes_available = WRITESIZE;
    }

  bcopy (start, buffer + bytes_in_buffer, bytes_to_add);
  bytes_in_buffer += bytes_to_add;
}

SIGTYPE
cleanup ()
{
  unlink (tempfile);
  exit (1);
}

void
xwrite (desc, buffer, size)
     int desc;
     char *buffer;
     int size;
{
  if (write (desc, buffer, size) != size)
    {
      error (0, errno, "write error");
      cleanup ();
    }
}

/* Allocate N bytes of memory dynamically, with error checking.  */

char *
xmalloc (n)
     unsigned n;
{
  char *p;

  p = malloc (n);
  if (p == 0)
    {
      error (0, 0, "virtual memory exhausted");
      cleanup ();
    }
  return p;
}

/* Change the size of memory area P to N bytes, with error checking. */

char *
xrealloc (p, n)
     char *p;
     unsigned n;
{
  p = realloc (p, n);
  if (p == 0)
    {
      error (0, 0, "virtual memory exhausted");
      cleanup ();
    }
  return p;
}
