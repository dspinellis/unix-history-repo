/* tail -- output last part of file(s)
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

/* Can display any amount of data, unlike the Unix version, which uses
   a fixed size buffer and therefore can only deliver a limited number
   of lines.

   Options:
   -b			Tail by N 512-byte blocks.
   -c, --bytes=N[bkm]	Tail by N bytes
			[or 512-byte blocks, kilobytes, or megabytes].
   -f, --follow		Loop forever trying to read more characters at the
			end of the file, on the assumption that the file
			is growing.  Ignored if reading from a pipe.
			Cannot be used if more than one file is given.
   -k			Tail by N kilobytes.
   -N, -l, -n, --lines=N	Tail by N lines.
   -m			Tail by N megabytes.
   -q, --quiet, --silent	Never print filename headers.
   -v, --verbose		Always print filename headers.

   If a number (N) starts with a `+', begin printing with the Nth item
   from the start of each file, instead of from the end.

   Reads from standard input if no files are given or when a filename of
   ``-'' is encountered.
   By default, filename headers are printed only more than one file
   is given.
   By default, prints the last 10 lines (tail -n 10).

   Original version by Paul Rubin <phr@ocf.berkeley.edu>.
   Extensions by David MacKenzie <djm@ai.mit.edu>. */

#include <stdio.h>
#include <getopt.h>
#include <ctype.h>
#include <sys/types.h>
/*#include "system.h"*/
#include <sys/file.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>

#ifdef isascii
#define ISDIGIT(c) (isascii ((c)) && isdigit ((c)))
#else
#define ISDIGIT(c) (isdigit ((c)))
#endif

/* Number of items to tail. */
#define DEFAULT_NUMBER 10

/* Size of atomic reads. */
#define BUFSIZE (512 * 8)

/* Number of bytes per item we are printing.
   If 0, tail in lines. */
int unit_size;

/* If nonzero, read from end of file until killed. */
int forever;

/* If nonzero, count from start of file instead of end. */
int from_start;

/* If nonzero, print filename headers. */
int print_headers;

/* When to print the filename banners. */
enum header_mode
{
  multiple_files, always, never
};

int file_lines ();
int pipe_bytes ();
int pipe_lines ();
int start_bytes ();
int start_lines ();
int tail ();
int tail_bytes ();
int tail_file ();
int tail_lines ();
long atou();
void dump_remainder ();
void error (int status, int errnum, char *message, ...);
void parse_unit ();
void usage ();
void write_header ();

/* The name this program was run with. */
char *program_name;

/* Nonzero if we have ever read standard input. */
int have_read_stdin;

struct option long_options[] =
{
  {"bytes", 1, NULL, 'c'},
  {"follow", 0, NULL, 'f'},
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
  /* If from_start, the number of items to skip before printing; otherwise,
     the number of items at the end of the file to print.  Initially, -1
     means the value has not been set. */
  long number = -1;
  int c;			/* Option character. */

  program_name = argv[0];
  have_read_stdin = 0;
  unit_size = 0;
  forever = from_start = print_headers = 0;

  if (argc > 1
      && ((argv[1][0] == '-' && ISDIGIT (argv[1][1]))
	  || (argv[1][0] == '+' && (ISDIGIT (argv[1][1]) || argv[1][1] == 0))))
    {
      /* Old option syntax: a dash or plus, one or more digits (zero digits
	 are acceptable with a plus), and one or more option letters. */
      if (argv[1][0] == '+')
	from_start = 1;
      if (argv[1][1] != 0)
	{
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

		case 'f':
		  forever = 1;
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
	}
      /* Make the options we just parsed invisible to getopt. */
      argv[1] = argv[0];
      argv++;
      argc--;
    }

  while ((c = getopt_long (argc, argv, "c:n:fqv", long_options, (int *) 0))
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
	  if (*optarg == '+')
	    {
	      from_start = 1;
	      ++optarg;
	    }
	  else if (*optarg == '-')
	    ++optarg;
	  number = atou (optarg);
	  if (number == -1)
	    error (1, 0, "invalid number `%s'", optarg);
	  break;

	case 'f':
	  forever = 1;
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

  /* To start printing with item `number' from the start of the file, skip
     `number' - 1 items.  `tail +0' is actually meaningless, but for Unix
     compatibility it's treated the same as `tail +1'. */
  if (from_start)
    {
      if (number)
	--number;
    }

  if (unit_size > 1)
    number *= unit_size;

  if (optind < argc - 1 && forever)
    error (1, 0, "cannot follow the ends of multiple files");

  if (header_mode == always
      || (header_mode == multiple_files && optind < argc - 1))
    print_headers = 1;

  if (optind == argc)
    exit_status |= tail_file ("-", number);

  for (; optind < argc; ++optind)
    exit_status |= tail_file (argv[optind], number);

  if (have_read_stdin && close (0) < 0)
    error (1, errno, "-");
  if (close (1) < 0)
    error (1, errno, "write error");
  exit (exit_status);
}

/* Display the last NUMBER units of file FILENAME.
   "-" for FILENAME means the standard input.
   Return 0 if successful, 1 if an error occurred. */

int
tail_file (filename, number)
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
      return tail (filename, 0, number);
    }
  else
    {
      fd = open (filename, O_RDONLY);
      if (fd >= 0)
	{
	  int errors;

	  if (print_headers)
	    write_header (filename);
	  errors = tail (filename, fd, number);
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
      write (1, "==> ", 4);
      first_file = 0;
    }
  else
    write (1, "\n==> ", 5);
  write (1, filename, strlen (filename));
  write (1, " <==\n", 5);
}

/* Display the last NUMBER units of file FILENAME, open for reading
   in FD.
   Return 0 if successful, 1 if an error occurred. */

int
tail (filename, fd, number)
     char *filename;
     int fd;
     long number;
{
  if (unit_size)
    return tail_bytes (filename, fd, number);
  else
    return tail_lines (filename, fd, number);
}

/* Display the last part of file FILENAME, open for reading in FD,
   using NUMBER characters.
   Return 0 if successful, 1 if an error occurred. */

int
tail_bytes (filename, fd, number)
     char *filename;
     int fd;
     long number;
{
  struct stat stats;

  /* Use fstat instead of checking for errno == ESPIPE because
     lseek doesn't work on some special files but doesn't return an
     error, either. */
  if (fstat (fd, &stats))
    {
      error (0, errno, "%s", filename);
      return 1;
    }

  if (from_start)
    {
      if (S_ISREG (stats.st_mode))
	lseek (fd, number, SEEK_SET);
      else if (start_bytes (filename, fd, number))
	return 1;
      dump_remainder (filename, fd);
    }
  else
    {
      if (S_ISREG (stats.st_mode))
	{
	  if (lseek (fd, 0L, SEEK_END) <= number)
	    /* The file is shorter than we want, or just the right size, so
	       print the whole file. */
	    lseek (fd, 0L, SEEK_SET);
	  else
	    /* The file is longer than we want, so go back. */
	    lseek (fd, -number, SEEK_END);
	  dump_remainder (filename, fd);
	}
      else
	return pipe_bytes (filename, fd, number);
    }
  return 0;
}

/* Display the last part of file FILENAME, open for reading on FD,
   using NUMBER lines.
   Return 0 if successful, 1 if an error occurred. */

int
tail_lines (filename, fd, number)
     char *filename;
     int fd;
     long number;
{
  struct stat stats;
  long length;

  if (fstat (fd, &stats))
    {
      error (0, errno, "%s", filename);
      return 1;
    }

  if (from_start)
    {
      if (start_lines (filename, fd, number))
	return 1;
      dump_remainder (filename, fd);
    }
  else
    {
      if (S_ISREG (stats.st_mode))
	{
	  length = lseek (fd, 0L, SEEK_END);
	  if (length != 0 && file_lines (filename, fd, number, length))
	    return 1;
	  dump_remainder (filename, fd);
	}
      else
	return pipe_lines (filename, fd, number);
    }
  return 0;
}

/* Print the last NUMBER lines from the end of file FD.
   Go backward through the file, reading `BUFSIZE' bytes at a time (except
   probably the first), until we hit the start of the file or have
   read NUMBER newlines.
   POS starts out as the length of the file (the offset of the last
   byte of the file + 1).
   Return 0 if successful, 1 if an error occurred. */

int
file_lines (filename, fd, number, pos)
     char *filename;
     int fd;
     long number;
     long pos;
{
  char buffer[BUFSIZE];
  int bytes_read;
  int i;			/* Index into `buffer' for scanning. */

  if (number == 0)
    return 0;

  /* Set `bytes_read' to the size of the last, probably partial, buffer;
     0 < `bytes_read' <= `BUFSIZE'. */
  bytes_read = pos % BUFSIZE;
  if (bytes_read == 0)
    bytes_read = BUFSIZE;
  /* Make `pos' a multiple of `BUFSIZE' (0 if the file is short), so that all
     reads will be on block boundaries, which might increase efficiency. */
  pos -= bytes_read;
  lseek (fd, pos, SEEK_SET);
  bytes_read = read (fd, buffer, bytes_read);
  if (bytes_read == -1)
    {
      error (0, errno, "%s", filename);
      return 1;
    }

  /* Count the incomplete line on files that don't end with a newline. */
  if (bytes_read && buffer[bytes_read - 1] != '\n')
    --number;

  do
    {
      /* Scan backward, counting the newlines in this bufferfull. */
      for (i = bytes_read - 1; i >= 0; i--)
	{
	  /* Have we counted the requested number of newlines yet? */
	  if (buffer[i] == '\n' && number-- == 0)
	    {
	      /* If this newline wasn't the last character in the buffer,
	         print the text after it. */
	      if (i != bytes_read - 1)
		write (1, &buffer[i + 1], bytes_read - (i + 1));
	      return 0;
	    }
	}
      /* Not enough newlines in that bufferfull. */
      if (pos == 0)
	{
	  /* Not enough lines in the file; print the entire file. */
	  lseek (fd, 0L, SEEK_SET);
	  return 0;
	}
      pos -= BUFSIZE;
      lseek (fd, pos, SEEK_SET);
    }
  while ((bytes_read = read (fd, buffer, BUFSIZE)) > 0);
  if (bytes_read == -1)
    {
      error (0, errno, "%s", filename);
      return 1;
    }
  return 0;
}

/* Print the last NUMBER lines from the end of the standard input,
   open for reading as pipe FD.
   Buffer the text as a linked list of LBUFFERs, adding them as needed.
   Return 0 if successful, 1 if an error occured. */

int
pipe_lines (filename, fd, number)
     char *filename;
     int fd;
     long number;
{
  struct linebuffer
  {
    int nbytes, nlines;
    char buffer[BUFSIZE];
    struct linebuffer *next;
  };
  typedef struct linebuffer LBUFFER;
  LBUFFER *first, *last, *tmp;
  int i;			/* Index into buffers. */
  int total_lines = 0;		/* Total number of newlines in all buffers. */
  int errors = 0;

  first = last = (LBUFFER *) malloc (sizeof (LBUFFER));
  first->nbytes = first->nlines = 0;
  first->next = NULL;
  tmp = (LBUFFER *) malloc (sizeof (LBUFFER));

  /* Input is always read into a fresh buffer. */
  while ((tmp->nbytes = read (fd, tmp->buffer, BUFSIZE)) > 0)
    {
      tmp->nlines = 0;
      tmp->next = NULL;

      /* Count the number of newlines just read. */
      for (i = 0; i < tmp->nbytes; i++)
	if (tmp->buffer[i] == '\n')
	  ++tmp->nlines;
      total_lines += tmp->nlines;

      /* If there is enough room in the last buffer read, just append the new
         one to it.  This is because when reading from a pipe, `nbytes' can
         often be very small. */
      if (tmp->nbytes + last->nbytes < BUFSIZE)
	{
	  bcopy (tmp->buffer, &last->buffer[last->nbytes], tmp->nbytes);
	  last->nbytes += tmp->nbytes;
	  last->nlines += tmp->nlines;
	}
      else
	{
	  /* If there's not enough room, link the new buffer onto the end of
	     the list, then either free up the oldest buffer for the next
	     read if that would leave enough lines, or else malloc a new one.
	     Some compaction mechanism is possible but probably not
	     worthwhile. */
	  last = last->next = tmp;
	  if (total_lines - first->nlines > number)
	    {
	      tmp = first;
	      total_lines -= first->nlines;
	      first = first->next;
	    }
	  else
	    tmp = (LBUFFER *) malloc (sizeof (LBUFFER));
	}
    }
  if (tmp->nbytes == -1)
    {
      error (0, errno, "%s", filename);
      errors = 1;
      free ((char *) tmp);
      goto free_lbuffers;
    }

  free ((char *) tmp);

  /* This prevents a core dump when the pipe contains no newlines. */
  if (number == 0)
    goto free_lbuffers;

  /* Count the incomplete line on files that don't end with a newline. */
  if (last->buffer[last->nbytes - 1] != '\n')
    {
      ++last->nlines;
      ++total_lines;
    }

  /* Run through the list, printing lines.  First, skip over unneeded
     buffers. */
  for (tmp = first; total_lines - tmp->nlines > number; tmp = tmp->next)
    total_lines -= tmp->nlines;

  /* Find the correct beginning, then print the rest of the file. */
  if (total_lines > number)
    {
      char *cp;

      /* Skip `total_lines' - `number' newlines.  We made sure that
         `total_lines' - `number' <= `tmp->nlines'. */
      cp = tmp->buffer;
      for (i = total_lines - number; i; --i)
	while (*cp++ != '\n')
	  /* Do nothing. */ ;
      i = cp - tmp->buffer;
    }
  else
    i = 0;
  write (1, &tmp->buffer[i], tmp->nbytes - i);

  for (tmp = tmp->next; tmp; tmp = tmp->next)
    write (1, tmp->buffer, tmp->nbytes);

free_lbuffers:
  while (first)
    {
      tmp = first->next;
      free ((char *) first);
      first = tmp;
    }
  return errors;
}

/* Print the last NUMBER characters from the end of pipe FD.
   This is a stripped down version of pipe_lines.
   Return 0 if successful, 1 if an error occurred. */

int
pipe_bytes (filename, fd, number)
     char *filename;
     int fd;
     long number;
{
  struct charbuffer
  {
    int nbytes;
    char buffer[BUFSIZE];
    struct charbuffer *next;
  };
  typedef struct charbuffer CBUFFER;
  CBUFFER *first, *last, *tmp;
  int i;			/* Index into buffers. */
  int total_bytes = 0;		/* Total characters in all buffers. */
  int errors = 0;

  first = last = (CBUFFER *) malloc (sizeof (CBUFFER));
  first->nbytes = 0;
  first->next = NULL;
  tmp = (CBUFFER *) malloc (sizeof (CBUFFER));

  /* Input is always read into a fresh buffer. */
  while ((tmp->nbytes = read (fd, tmp->buffer, BUFSIZE)) > 0)
    {
      tmp->next = NULL;

      total_bytes += tmp->nbytes;
      /* If there is enough room in the last buffer read, just append the new
         one to it.  This is because when reading from a pipe, `nbytes' can
         often be very small. */
      if (tmp->nbytes + last->nbytes < BUFSIZE)
	{
	  bcopy (tmp->buffer, &last->buffer[last->nbytes], tmp->nbytes);
	  last->nbytes += tmp->nbytes;
	}
      else
	{
	  /* If there's not enough room, link the new buffer onto the end of
	     the list, then either free up the oldest buffer for the next
	     read if that would leave enough characters, or else malloc a new
	     one.  Some compaction mechanism is possible but probably not
	     worthwhile. */
	  last = last->next = tmp;
	  if (total_bytes - first->nbytes > number)
	    {
	      tmp = first;
	      total_bytes -= first->nbytes;
	      first = first->next;
	    }
	  else
	    {
	      tmp = (CBUFFER *) malloc (sizeof (CBUFFER));
	    }
	}
    }
  if (tmp->nbytes == -1)
    {
      error (0, errno, "%s", filename);
      errors = 1;
      free ((char *) tmp);
      goto free_cbuffers;
    }

  free ((char *) tmp);

  /* Run through the list, printing characters.  First, skip over unneeded
     buffers. */
  for (tmp = first; total_bytes - tmp->nbytes > number; tmp = tmp->next)
    total_bytes -= tmp->nbytes;

  /* Find the correct beginning, then print the rest of the file.
     We made sure that `total_bytes' - `number' <= `tmp->nbytes'. */
  if (total_bytes > number)
    i = total_bytes - number;
  else
    i = 0;
  write (1, &tmp->buffer[i], tmp->nbytes - i);

  for (tmp = tmp->next; tmp; tmp = tmp->next)
    write (1, tmp->buffer, tmp->nbytes);

free_cbuffers:
  while (first)
    {
      tmp = first->next;
      free ((char *) first);
      first = tmp;
    }
  return errors;
}

/* Skip NUMBER characters from the start of pipe FD, and print
   any extra characters that were read beyond that.
   Return 1 on error, 0 if ok.  */

int
start_bytes (filename, fd, number)
     char *filename;
     int fd;
     long number;
{
  char buffer[BUFSIZE];
  int bytes_read = 0;

  while (number > 0 && (bytes_read = read (fd, buffer, BUFSIZE)) > 0)
    number -= bytes_read;
  if (bytes_read == -1)
    {
      error (0, errno, "%s", filename);
      return 1;
    }
  else if (number < 0)
    write (1, &buffer[bytes_read + number], -number);
  return 0;
}

/* Skip NUMBER lines at the start of file or pipe FD, and print
   any extra characters that were read beyond that.
   Return 1 on error, 0 if ok.  */

int
start_lines (filename, fd, number)
     char *filename;
     int fd;
     long number;
{
  char buffer[BUFSIZE];
  int bytes_read = 0;
  int bytes_to_skip = 0;

  while (number && (bytes_read = read (fd, buffer, BUFSIZE)) > 0)
    {
      bytes_to_skip = 0;
      while (bytes_to_skip < bytes_read)
	if (buffer[bytes_to_skip++] == '\n' && --number == 0)
	  break;
    }
  if (bytes_read == -1)
    {
      error (0, errno, "%s", filename);
      return 1;
    }
  else if (bytes_to_skip < bytes_read)
    write (1, &buffer[bytes_to_skip], bytes_read - bytes_to_skip);
  return 0;
}

/* Display file FILENAME from the current position in FD
   to the end.  If `forever' is nonzero, keep reading from the
   end of the file until killed. */

void
dump_remainder (filename, fd)
     char *filename;
     int fd;
{
  char buffer[BUFSIZE];
  int bytes_read;

output:
  while ((bytes_read = read (fd, buffer, BUFSIZE)) > 0)
    write (1, buffer, bytes_read);
  if (bytes_read == -1)
    error (1, errno, "%s", filename);
  if (forever)
    {
      sleep (1);
      goto output;
    }
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
  unsigned long value;

  for (value = 0; ISDIGIT (*str); ++str)
    value = value * 10 + *str - '0';
  return *str ? -1 : value;
}

void
usage ()
{
  fprintf (stderr, "\
Usage: %s [-c [+]N[bkm]] [-n [+]N] [-fqv] [--bytes=[+]N[bkm]] [--lines=[+]N]\n\
       [--follow] [--quiet] [--silent] [--verbose] [file...]\n\
       %s [{-,+}Nbcfklmqv] [file...]\n", program_name, program_name);
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
