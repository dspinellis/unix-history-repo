/* cmp -- compare two files.
   Copyright (C) 1988, 1990, 1991 Free Software Foundation, Inc.

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

/* Differences from the Unix cmp:
   * 6 - 40 - oo times faster.
   * The file name `-' is always the standard input. If one file name
     is omitted, the standard input is used as well.
   * -c option to print the differing characters like `cat -t'
     (except that newlines are printed as `^J'), with or without -l.

   By Torbjorn Granlund and David MacKenzie. */

#include <stdio.h>
#include <getopt.h>
#include <sys/types.h>
#include "system.h"

#define max(h, i)	((h) > (i) ? (h) : (i))
#define min(l, o)	((l) < (o) ? (l) : (o))

char *xmalloc ();
int bcmp_cnt ();
int bcmp2 ();
int bread ();
int cmp ();
void printc ();
void error ();

/* Name under which this program was invoked.  */

char *program_name;

/* Filenames of the compared files.  */

char *file1;
char *file2;

/* File descriptors of the files.  */

int file1_desc;
int file2_desc;

/* Read buffers for the files.  */

char *buf1;
char *buf2;

/* Optimal block size for the files.  */

int buf_size;

/* Output format:
   type_first_diff
     to print the offset and line number of the first differing bytes
   type_all_diffs
     to print the (decimal) offsets and (octal) values of all differing bytes
   type_status
     to only return an exit status indicating whether the files differ */

enum
  {
    type_first_diff, type_all_diffs, type_status
  } comparison_type = type_first_diff;

/* If nonzero, print values of bytes quoted like cat -t does. */
int opt_print_chars = 0;

struct option long_options[] =
{
  {"show-chars", 0, &opt_print_chars, 1},
  {"silent", 0, NULL, 's'},
  {"quiet", 0, NULL, 's'},
  {"verbose", 0, NULL, 'l'},
  {NULL, 0, NULL, 0}
};

void
usage (reason)
     char *reason;
{
  if (reason != NULL)
    fprintf (stderr, "%s: %s\n", program_name, reason);

  fprintf (stderr, "\
Usage: %s [-cls] [--show-chars] [--verbose] [--silent] [--quiet]\n\
       file1 [file2]\n",
	   program_name);

  exit (2);
}

void
main (argc, argv)
     int argc;
     char *argv[];
{
  int c, opened = 0, exit_status;
  struct stat stat_buf1, stat_buf2;

  program_name = argv[0];

  /* If an argument is omitted, default to the standard input.  */

  file1 = "-";
  file2 = "-";
  file1_desc = 0;
  file2_desc = 0;

  /* Parse command line options.  */

  while ((c = getopt_long (argc, argv, "cls", long_options, (int *) 0)) != EOF)
    switch (c)
      {
      case 0:
	break;

      case 'c':
	opt_print_chars = 1;
	break;

      case 'l':
	comparison_type = type_all_diffs;
	break;

      case 's':
	comparison_type = type_status;
	break;

      default:
	usage ((char *) 0);
      }

  if (optind < argc)
    file1 = argv[optind++];

  if (optind < argc)
    file2 = argv[optind++];

  if (optind < argc)
    usage ("extra arguments");

  if (strcmp (file1, "-"))
    {
      opened = 1;
      file1_desc = open (file1, O_RDONLY);
      if (file1_desc < 0)
	{
	  if (comparison_type == type_status)
	    exit (2);
	  else
	    error (2, errno, "%s", file1);
	}
    }

  if (strcmp (file2, "-"))
    {
      opened = 1;
      file2_desc = open (file2, O_RDONLY);
      if (file2_desc < 0)
	{
	  if (comparison_type == type_status)
	    exit (2);
	  else
	    error (2, errno, "%s", file2);
	}
    }

  if (file1_desc == file2_desc)
    {
      if (opened)
	error (2, 0, "standard input is closed");
      else
	usage ("at least one filename should be specified");
    }

  if (fstat (file1_desc, &stat_buf1) < 0)
    error (2, errno, "%s", file1);
  if (fstat (file2_desc, &stat_buf2) < 0)
    error (2, errno, "%s", file2);

  /* If both the input descriptors are associated with plain files,
     we can make the job simpler in some cases.  */

  if (S_ISREG (stat_buf1.st_mode) && S_ISREG (stat_buf2.st_mode))
    {
      /* Find out if the files are links to the same inode, and therefore
	 identical.  */

      if (stat_buf1.st_dev == stat_buf2.st_dev
	  && stat_buf1.st_ino == stat_buf2.st_ino)
	exit (0);

      /* If output is redirected to "/dev/null", we may assume `-s'.  */

      if (comparison_type != type_status)
	{
	  struct stat sb;
	  dev_t nulldev;
	  ino_t nullino;

	  if (stat ("/dev/null", &sb) == 0)
	    {
	      nulldev = sb.st_dev;
	      nullino = sb.st_ino;
	      if (fstat (1, &sb) == 0
		  && sb.st_dev == nulldev && sb.st_ino == nullino)
		comparison_type = type_status;
	    }
	}

      /* If only a return code is needed, conclude that
	 the files differ if they have different sizes.  */

      if (comparison_type == type_status
	  && stat_buf1.st_size != stat_buf2.st_size)
	exit (1);
    }

  /* Get the optimal block size of the files.  */

  buf_size = max (ST_BLKSIZE (stat_buf1), ST_BLKSIZE (stat_buf2));

  /* Allocate buffers, with space for sentinels at the end.  */

  buf1 = xmalloc (buf_size + sizeof (long));
  buf2 = xmalloc (buf_size + sizeof (long));

  exit_status = cmp ();

  if (close (file1_desc) < 0)
    error (2, errno, "%s", file1);
  if (close (file2_desc) < 0)
    error (2, errno, "%s", file2);
  if (comparison_type != type_status
      && (ferror (stdout) || fclose (stdout) == EOF))
    error (2, errno, "write error");
  exit (exit_status);
}

/* Compare the two files already open on `file1_desc' and `file2_desc',
   using `buf1' and `buf2'.
   Return 0 if identical, 1 if different, >1 if error. */

int
cmp ()
{
  long line_number = 1;		/* Line number (1...) of first difference. */
  long char_number = 1;		/* Offset (1...) in files of 1st difference. */
  int read1, read2;		/* Number of bytes read from each file. */
  int first_diff;		/* Offset (0...) in buffers of 1st diff. */
  int smaller;			/* The lesser of `read1' and `read2'. */
  int ret = 0;

  do
    {
      read1 = bread (file1_desc, buf1, buf_size);
      if (read1 < 0)
	error (2, errno, "%s", file1);
      read2 = bread (file2_desc, buf2, buf_size);
      if (read2 < 0)
	error (2, errno, "%s", file2);

      /* Insert sentinels for the block compare.  */

      buf1[read1] = ~buf2[read1];
      buf2[read2] = ~buf1[read2];

      if (comparison_type == type_first_diff)
	{
	  int cnt;

	  /* If the line number should be written for differing files,
	     compare the blocks and count the number of newlines
	     simultaneously.  */
	  first_diff = bcmp_cnt (&cnt, buf1, buf2, '\n');
	  line_number += cnt;
	}
      else
	{
	  first_diff = bcmp2 (buf1, buf2);
	}

      if (comparison_type != type_all_diffs)
	char_number += first_diff;
      else
	smaller = min (read1, read2);

      if (first_diff < read1 && first_diff < read2)
	{
	  switch (comparison_type)
	    {
	    case type_first_diff:
	      /* This format is a proposed POSIX standard.  */
	      printf ("%s %s differ: char %ld, line %ld",
		      file1, file2, char_number, line_number);
	      if (opt_print_chars)
		{
		  printf (" is");
		  printf (" %3o ",
			  (unsigned) (unsigned char) buf1[first_diff]);
		  printc (stdout, 0,
			  (unsigned) (unsigned char) buf1[first_diff]);
		  printf (" %3o ",
			  (unsigned) (unsigned char) buf2[first_diff]);
		  printc (stdout, 0,
			  (unsigned) (unsigned char) buf2[first_diff]);
		}
	      putchar ('\n');
	      /* Fall through. */
	    case type_status:
	      return 1;
	    case type_all_diffs:
	      while (first_diff < smaller)
		{
		  if (buf1[first_diff] != buf2[first_diff])
		    {
		      if (opt_print_chars)
			{
			  printf ("%6ld", first_diff + char_number);
			  printf (" %3o ",
				  (unsigned) (unsigned char) buf1[first_diff]);
			  printc (stdout, 4,
				  (unsigned) (unsigned char) buf1[first_diff]);
			  printf (" %3o ",
				  (unsigned) (unsigned char) buf2[first_diff]);
			  printc (stdout, 0,
				  (unsigned) (unsigned char) buf2[first_diff]);
			  putchar ('\n');
			}
		      else
			/* This format is a proposed POSIX standard. */
			printf ("%6ld %3o %3o\n",
				first_diff + char_number,
				(unsigned) (unsigned char) buf1[first_diff],
				(unsigned) (unsigned char) buf2[first_diff]);
		    }
		  first_diff++;
		}
	      ret = 1;
	      break;
	    }
	}

      if (comparison_type == type_all_diffs)
	char_number += smaller;

      if (read1 != read2)
	{
	  switch (comparison_type)
	    {
	    case type_first_diff:
	    case type_all_diffs:
	      /* This format is a proposed POSIX standard. */
	      fprintf (stderr, "%s: EOF on %s\n",
		      program_name, read1 < read2 ? file1 : file2);
	      break;
	    case type_status:
	      break;
	    }
	  return 1;
	}
    }
  while (read1);
  return ret;
}

/* Compare two blocks of memory P1 and P2 until they differ,
   and count the number of occurences of the character C in the common
   part of P1 and P2.
   Assumes that P1 and P2 are aligned at long addresses!
   If the blocks are not guaranteed to be different, put sentinels at the ends
   of the blocks before calling this function.
   Return the offset of the first byte that differs.
   Place the count at the address pointed to by COUNT.  */

int
bcmp_cnt (count, p1, p2, c)
     int *count;
     char *p1, *p2;
     unsigned char c;
{
  long w;			/* Word for counting C. */
  long i1, i2;			/* One word from each buffer to compare. */
  long *p1i, *p2i;		/* Pointers into each buffer. */
  char *p1c, *p2c;		/* Pointers for finding exact address. */
  unsigned int cnt = 0;		/* Number of occurrences of C. */
  long cccc;			/* C, four times. */
  long m0, m1, m2, m3;		/* Bitmasks for counting C. */

  cccc = ((long) c << 24) | ((long) c << 16) | ((long) c << 8) | ((long) c);

  m0 = 0xff;
  m1 = 0xff00;
  m2 = 0xff0000;
  m3 = 0xff000000;

  p1i = (long *) p1;
  p2i = (long *) p2;

  /* Find the rough position of the first difference by reading long ints,
     not bytes.  */

  i1 = *p1i++;
  i2 = *p2i++;
  while (i1 == i2)
    {
      w = i1 ^ cccc;
      cnt += (w & m0) == 0;
      cnt += (w & m1) == 0;
      cnt += (w & m2) == 0;
      cnt += (w & m3) == 0;
      i1 = *p1i++;
      i2 = *p2i++;
    }

  /* Find out the exact differing position (endianess independant).  */

  p1c = (char *) (p1i - 1);
  p2c = (char *) (p2i - 1);
  while (*p1c == *p2c)
    {
      cnt += c == *p1c;
      p1c++;
      p2c++;
    }

  *count = cnt;
  return p1c - p1;
}

/* Compare two blocks of memory P1 and P2 until they differ.
   Assumes that P1 and P2 are aligned at long addresses!
   If the blocks are not guaranteed to be different, put sentinels at the ends
   of the blocks before calling this function.
   Return the offset of the first byte that differs.  */

int
bcmp2 (p1, p2)
     char *p1, *p2;
{
  long *i1, *i2;
  char *c1, *c2;

  /* Find the rough position of the first difference by reading long ints,
     not bytes.  */

  for (i1 = (long *) p1, i2 = (long *) p2; *i1++ == *i2++;)
    ;

  /* Find out the exact differing position (endianess independant).  */

  for (c1 = (char *) (i1 - 1), c2 = (char *) (i2 - 1); *c1 == *c2; c1++, c2++)
    ;

  return c1 - p1;
}

/* Read NCHARS bytes from descriptor FD into BUF.
   Return the number of characters successfully read.  */

int
bread (fd, buf, nchars)
     int fd;
     char *buf;
     int nchars;
{
  char *bp = buf;
  int nread;

  for (;;)
    {
      nread = read (fd, bp, nchars);
      if (nread < 0)
	return -1;
      bp += nread;
      if (nread == nchars || nread == 0)
	break;
      nchars -= nread;
    }
  return bp - buf;
}

/* Print character C on stream FS, making nonvisible characters
   visible by quoting like cat -t does.
   Pad with spaces on the right to WIDTH characters.  */

void
printc (fs, width, c)
     FILE *fs;
     int width;
     unsigned c;
{
  if (c >= 128)
    {
      putc ('M', fs);
      putc ('-', fs);
      c -= 128;
      width -= 2;
    }
  if (c < 32)
    {
      putc ('^', fs);
      c += 64;
      --width;
    }
  else if (c == 127)
    {
      putc ('^', fs);
      c = '?';
      --width;
    }

  putc (c, fs);
  while (--width > 0)
    putc (' ', fs);
}
