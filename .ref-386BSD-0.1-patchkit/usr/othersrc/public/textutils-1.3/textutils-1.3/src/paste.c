/* paste - merge lines of files
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
   Please note that I recreated the behavior of the Unix(Tm) 'paste' command
   as faithfully as possible, with minor exceptions; however,
   I haven't run a full set of regression tests.  Thus, the user of
   this program accepts full responsibility for any effects or loss;
   in particular, the author is not responsible for any losses,
   explicit or incidental, that may be incurred through use of this program.
 
   I ask that any bugs (and, if possible, fixes) be reported to me when
   possible.  -David Ihnat (312) 784-4544 ignatz@homebru.chi.il.us

   The list of valid escape sequences has been expanded over the Unix
   version, to include \b, \f, \r, and \v.
 
   POSIX changes, bug fixes, long-named options, and cleanup
   by David MacKenzie <djm@ai.mit.edu>.
 
   Options:
   --serial
   -s				Paste one file at a time rather than
				one line from each file.
   --delimiters=delim-list
   -d delim-list		Consecutively use the characters in
				DELIM-LIST instead of tab to separate
				merged lines.  When DELIM-LIST is exhausted,
				start again at its beginning.
   A FILE of `-' means standard input.
   If no FILEs are given, standard input is used. */

#include <stdio.h>
#include <getopt.h>
#include <sys/types.h>
#include "system.h"

char *collapse_escapes ();
char *xmalloc ();
char *xrealloc ();
int paste_parallel ();
int paste_serial ();
void error ();
void usage ();

/* Indicates that no delimiter should be added in the current position. */
#define EMPTY_DELIM '\0'

/* Element marking a file that has reached EOF and been closed. */
#define	CLOSED ((FILE *) -1)

/* Element marking end of list of open files. */
#define ENDLIST ((FILE *) -2)

/* Name this program was run with. */
char *program_name;

/* If nonzero, we have read standard input at some point. */
int have_read_stdin;

/* If nonzero, merge subsequent lines of each file rather than
   corresponding lines from each file in parallel. */
int serial_merge;

/* The delimeters between lines of input files (used cyclically). */
char *delims;

/* A pointer to the character after the end of `delims'. */
char *delim_end;

struct option longopts[] =
{
  {"serial", 0, 0, 's'},
  {"delimiters", 1, 0, 'd'},
  {0, 0, 0, 0}
};

void
main (argc, argv)
     int argc;
     char **argv;
{
  int optc, exit_status;
  char default_delims[2];

  program_name = argv[0];
  have_read_stdin = 0;
  serial_merge = 0;
  delims = default_delims;
  strcpy (delims, "\t");

  while ((optc = getopt_long (argc, argv, "d:s", longopts, (int *) 0))
	 != EOF)
    {
      switch (optc)
	{
	case 'd':
	  /* Delimiter character(s). */
	  if (optarg[0] == '\0')
	    optarg = "\\0";
	  delims = optarg;
	  break;

	case 's':
	  serial_merge++;
	  break;

	default:
	  usage ();
	}
    }

  if (optind == argc)
    argv[argc++] = "-";

  delim_end = collapse_escapes (delims);

  if (!serial_merge)
    exit_status = paste_parallel (argc - optind, &argv[optind]);
  else
    exit_status = paste_serial (argc - optind, &argv[optind]);
  if (have_read_stdin && fclose (stdin) == EOF)
    error (1, errno, "-");
  if (ferror (stdout) || fclose (stdout) == EOF)
    error (1, errno, "write error");
  exit (exit_status);
}

/* Replace backslash representations of special characters in
   STRPTR with their actual values.
   The set of possible backslash characters has been expanded beyond
   that recognized by the Unix version.

   Return a pointer to the character after the new end of STRPTR. */

char *
collapse_escapes (strptr)
     char *strptr;
{
  register char *strout;

  strout = strptr;		/* Start at the same place, anyway. */

  while (*strptr)
    {
      if (*strptr != '\\')	/* Is it an escape character? */
	*strout++ = *strptr++;	/* No, just transfer it. */
      else
	{
	  switch (*++strptr)
	    {
	    case '0':
	      *strout++ = EMPTY_DELIM;
	      break;

	    case 'b':
	      *strout++ = '\b';
	      break;

	    case 'f':
	      *strout++ = '\f';
	      break;

	    case 'n':
	      *strout++ = '\n';
	      break;

	    case 'r':
	      *strout++ = '\r';
	      break;

	    case 't':
	      *strout++ = '\t';
	      break;

	    case 'v':
	      *strout++ = '\v';
	      break;

	    default:
	      *strout++ = *strptr;
	      break;
	    }
	  strptr++;
	}
    }
  return strout;
}

/* Perform column paste on the NFILES files named in FNAMPTR.
   Return 0 if no errors, 1 if one or more files could not be
   opened or read. */

int
paste_parallel (nfiles, fnamptr)
     int nfiles;
     char **fnamptr;
{
  int errors = 0;		/* 1 if open or read errors occur. */
  /* Number of files for which space is allocated in `delbuf' and `fileptr'.
     Enlarged as necessary. */
  int file_list_size = 12;
  int chr;			/* Input character. */
  int line_length;		/* Number of chars in line. */
  int somedone;			/* 0 if all files empty for this line. */
  /* If all files are just ready to be closed, or will be on this
     round, the string of delimiters must be preserved.
     delbuf[0] through delbuf[file_list_size]
     store the delimiters for closed files. */
  char *delbuf;
  int delims_saved;		/* Number of delims saved in `delbuf'. */
  register char *delimptr;	/* Cycling pointer into `delims'. */
  FILE **fileptr;		/* Streams open to the files to process. */
  int files_open;		/* Number of files still open to process. */
  int i;			/* Loop index. */
  int opened_stdin = 0;		/* Nonzero if any fopen got fd 0. */

  delbuf = (char *) xmalloc (file_list_size + 2);
  fileptr = (FILE **) xmalloc ((file_list_size + 1) * sizeof (FILE *));

  /* Attempt to open all files.  This could be expanded to an infinite
     number of files, but at the (considerable) expense of remembering
     each file and its current offset, then opening/reading/closing.  */

  for (files_open = 0; files_open < nfiles; ++files_open)
    {
      if (files_open == file_list_size - 2)
	{
	  file_list_size += 12;
	  delbuf = (char *) xrealloc (delbuf, file_list_size + 2);
	  fileptr = (FILE **) xrealloc (fileptr, (file_list_size + 1)
					* sizeof (FILE *));
	}
      if (!strcmp (fnamptr[files_open], "-"))
	{
	  have_read_stdin = 1;
	  fileptr[files_open] = stdin;
	}
      else
	{
	  fileptr[files_open] = fopen (fnamptr[files_open], "r");
	  if (fileptr[files_open] == NULL)
	    error (1, errno, "%s", fnamptr[files_open]);
	  else if (fileno (fileptr[files_open]) == 0)
	    opened_stdin = 1;
	}
    }

  fileptr[files_open] = ENDLIST;

  if (opened_stdin && have_read_stdin)
    error (1, 0, "standard input is closed");

  /* Read a line from each file and output it to stdout separated by a
     delimiter, until we go through the loop without successfully
     reading from any of the files. */

  while (files_open)
    {
      /* Set up for the next line. */
      somedone = 0;
      delimptr = delims;
      delims_saved = 0;

      for (i = 0; fileptr[i] != ENDLIST && files_open; i++)
	{
	  line_length = 0;	/* Clear so we can easily detect EOF. */
	  if (fileptr[i] != CLOSED)
	    {
	      chr = getc (fileptr[i]);
	      if (chr != EOF && delims_saved)
		{
		  fwrite (delbuf, sizeof (char), delims_saved, stdout);
		  delims_saved = 0;
		}

	      while (chr != EOF)
		{
		  line_length++;
		  if (chr == '\n')
		    break;
		  putc (chr, stdout);
		  chr = getc (fileptr[i]);
		}
	    }

	  if (line_length == 0)
	    {
	      /* EOF, read error, or closed file.
		 If an EOF or error, close the file and mark it in the list. */
	      if (fileptr[i] != CLOSED)
		{
		  if (ferror (fileptr[i]))
		    {
		      error (0, errno, "%s", fnamptr[i]);
		      errors = 1;
		    }
		  if (fileptr[i] == stdin)
		    clearerr (fileptr[i]); /* Also clear EOF. */
		  else if (fclose (fileptr[i]) == EOF)
		    {
		      error (0, errno, "%s", fnamptr[i]);
		      errors = 1;
		    }
		      
		  fileptr[i] = CLOSED;
		  files_open--;
		}

	      if (fileptr[i + 1] == ENDLIST)
		{
		  /* End of this output line.
		     Is this the end of the whole thing? */
		  if (somedone)
		    {
		      /* No.  Some files were not closed for this line. */
		      if (delims_saved)
			{
			  fwrite (delbuf, sizeof (char), delims_saved, stdout);
			  delims_saved = 0;
			}
		      putc ('\n', stdout);
		    }
		  continue;	/* Next read of files, or exit. */
		}
	      else
		{
		  /* Closed file; add delimiter to `delbuf'. */
		  if (*delimptr != EMPTY_DELIM)
		    delbuf[delims_saved++] = *delimptr;
		  if (++delimptr == delim_end)
		    delimptr = delims;
		}
	    }
	  else
	    {
	      /* Some data read. */
	      somedone++;

	      /* Except for last file, replace last newline with delim. */
	      if (fileptr[i + 1] != ENDLIST)
		{
		  if (chr != '\n')
		    putc (chr, stdout);
		  if (*delimptr != EMPTY_DELIM)
		    putc (*delimptr, stdout);
		  if (++delimptr == delim_end)
		    delimptr = delims;
		}
	      else
		putc (chr, stdout);
	    }
	}
    }
  return errors;
}

/* Perform serial paste on the NFILES files named in FNAMPTR.
   Return 0 if no errors, 1 if one or more files could not be
   opened or read. */

int
paste_serial (nfiles, fnamptr)
     int nfiles;
     char **fnamptr;
{
  int errors = 0;		/* 1 if open or read errors occur. */
  register int charnew, charold; /* Current and previous char read. */
  register char *delimptr;	/* Current delimiter char. */
  register FILE *fileptr;	/* Open for reading current file. */

  for (; nfiles; nfiles--, fnamptr++)
    {
      if (!strcmp (*fnamptr, "-"))
	{
	  have_read_stdin = 1;
	  fileptr = stdin;
	}
      else
	{
	  fileptr = fopen (*fnamptr, "r");
	  if (fileptr == NULL)
	    {
	      error (0, errno, "%s", *fnamptr);
	      errors = 1;
	      continue;
	    }
	}

      delimptr = delims;	/* Set up for delimiter string. */

      charold = getc (fileptr);
      if (charold != EOF)
	{
	  /* `charold' is set up.  Hit it!
	     Keep reading characters, stashing them in `charnew';
	     output `charold', converting to the appropriate delimiter
	     character if needed.  After the EOF, output `charold'
	     if it's a newline; otherwise, output it and then a newline. */

	  while ((charnew = getc (fileptr)) != EOF)
	    {
	      /* Process the old character. */
	      if (charold == '\n')
		{
		  if (*delimptr != EMPTY_DELIM)
		    putc (*delimptr, stdout);

		  if (++delimptr == delim_end)
		    delimptr = delims;
		}
	      else
		putc (charold, stdout);

	      charold = charnew;
	    }

	  /* Hit EOF.  Process that last character. */
	  putc (charold, stdout);
	}

      if (charold != '\n')
	putc ('\n', stdout);

      if (ferror (fileptr))
	{
	  error (0, errno, "%s", *fnamptr);
	  errors = 1;
	}
      if (fileptr == stdin)
	clearerr (fileptr);	/* Also clear EOF. */
      else if (fclose (fileptr) == EOF)
	{
	  error (0, errno, "%s", *fnamptr);
	  errors = 1;
	}
    }
  return errors;
}

void
usage ()
{
  fprintf (stderr, "\
Usage: %s [-s] [-d delim-list] [--serial] [--delimiters=delim-list]\n\
       [file...]\n",
	   program_name);
  exit (1);
}
