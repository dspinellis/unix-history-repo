/*
 * GNU test program (ksb and mjb)
 *
 * Modified to run with the GNU shell Apr 25, 1988 by bfox.
 *
 *???	-G	file is owned by same gid; the effective gid is checked
 * Chet Ramey, CWRU 3/23/89
 *
 * Fixed a BSD dependency (_doprnt()) in the port to AIX 2.2
 * Chet Ramey, CWRU 5/3/89
 */

/* Copyright (C) 1987,1989 Free Software Foundation, Inc.

This file is part of GNU Bash, the Bourne Again SHell.

Bash is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 1, or (at your option) any later
version.

Bash is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with Bash; see the file COPYING.  If not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. */

#include <stdio.h>
#include <varargs.h>

#ifndef SONY
#include <fcntl.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>

#ifndef R_OK
#define R_OK 4
#define W_OK 2
#define X_OK 1
#define F_OK 0
#endif

#ifndef lint
static char *rcsid = "$Id: gtest.c,v 1.10 88/07/02 13:34:45 afb Exp Locker: afb $";
#endif

/* The following few defines control the truth and false output of each stage.
   TRUE and FALSE are what we use to compute the final output value.
   SHELL_BOOLEAN is the form which returns truth or falseness in shell terms.
   TRUTH_OR is how to do logical or with TRUE and FALSE.
   TRUTH_AND is how to do logical and with TRUE and FALSE..
   Default is TRUE = 1, FALSE = 0, TRUTH_OR = a | b, TRUTH_AND = a & b,
    SHELL_BOOLEAN = (!value). */
#define TRUE 1
#define FALSE 0
#define SHELL_BOOLEAN(value) (!(value))
#define TRUTH_OR(a, b) ((a) | (b))
#define TRUTH_AND(a, b) ((a) & (b))


/* Define STANDALONE to get the /bin/test version.  Otherwise, we are
   making this for the shell. */
/* #define STANDALONE */

#ifdef STANDALONE
#define test_exit(val) exit (val)
#else
#include <setjmp.h>
jmp_buf test_exit_buf;
int test_error_return = 0;
#define test_exit(val) test_error_return = val, longjmp (test_exit_buf, 1)
#endif /* STANDALONE */

#ifdef SYSV
int sys_v = 1;
#else
int sys_v = 0;
#endif

static int pos;			/* position in list			*/
static int argc;		/* number of args from command line	*/
static char **argv;		/* the argument list			*/

static void
test_syntax_error (format, arg)
     char *format, *arg;
{
  (void) fprintf (stderr, "%s: ", argv[0]);
  (void) fprintf (stderr, format, arg);
  test_exit (SHELL_BOOLEAN (FALSE));
}

test_io_error (name)
     char *name;
{
  extern int errno;
  int old_errno = errno;
  fprintf (stderr, "%s: ", argv[0]);
  errno = old_errno;
  perror (name);
  test_exit (SHELL_BOOLEAN (FALSE));
}

/*
 * advance - increment our position in the argument list.  Check that
 *	we're not past the end of the argument list.  This check is
 *	supressed if the argument is FALSE.  made a macro for efficiency.
 */
#ifndef lint
#define advance(f)	(++pos, f && (pos < argc ? 0 : beyond()))
#endif

#if !defined(advance)
static int
advance (f)
     int f;
{
  ++pos;
  if (f && pos >= argc)
    beyond ();
}

#endif

#define unary_advance() (advance (1),++pos)

/*
 * beyond - call when we're beyond the end of the argument list (an
 *	error condition)
 */
static int
beyond ()
{
  test_syntax_error ("argument expected\n", (char *)NULL);
}

/*
 * int_expt_err - when an integer argument was expected, but something else
 * 	was found.
 */
static void
int_expt_err (pch)
     char *pch;
{
  test_syntax_error ("integer expression expected %s\n", pch);
}

/*
 * isint - is the argument whose position in the argument vector is 'm' an
 * 	integer? Convert it for me too, returning it's value in 'pl'.
 */
static int
isint (m, pl)
     int m;
     long *pl;
{
  extern long atol ();
  register char *pch;

  pch = argv[m];

  /* Skip leading whitespace characters. */
  while (*pch == '\t' || *pch == ' ')
    pch++;

  /* accept negative numbers but not '-' alone */
  if ('-' == *pch)
    if ('\000' == *++pch)
      return 0;

  while ('\000' != *pch)
    {
      switch (*pch)
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
	  break;
	default:
	  return (FALSE);
	}
      ++pch;
    }
  *pl = atol (argv[m]);
  return (TRUE);
}

/*
 * age_of - find the age of the given file.  Return YES or NO depending
 *	on whether the file exists, and if it does, fill in the age with
 *	the modify time.
 */
static int
age_of (posit, age)
     int posit;
     long *age;
{
  struct stat stat_buf;

  if (stat (argv[posit], &stat_buf) < 0)
    {
      return (FALSE);
    }
  *age = stat_buf.st_mtime;
  return (TRUE);
}

/*
 * term - parse a term and return 1 or 0 depending on whether the term
 *	evaluates to true or false, respectively.
 *
 * term ::=
 *	'-'('h'|'d'|'f'|'r'|'s'|'w'|'c'|'b'|'p'|'u'|'g'|'k') filename
 *	'-'('L'|'x') filename
 * 	'-t' [ int ]
 *	'-'('z'|'n') string
 *	string
 *	string ('!='|'=') string
 *	<int> '-'(eq|ne|le|lt|ge|gt) <int>
 *	file '-'(nt|ot|ef) file
 *	'(' <expr> ')'
 * int ::=
 *	'-l' string
 *	positive and negative integers
 */
static int
term ()
{
  int expr ();
  auto struct stat stat_buf, stat_spare;
  auto long int l, r;
  auto int l_is_l, r_is_l; /* are the left and right integer
			    * expressions of the form '-l string'?
			    */
  auto int value, fd;

  if (pos >= argc)
    beyond ();

  /* Deal with leading "not". */
  if (pos < argc && '!' == argv[pos][0] && '\000' == argv[pos][1])
    {
      advance (1);

      /* This has to be rewritten to handle the TRUTH and FALSE stuff. */
      return (!term ());
    }
  
  if ('(' == argv[pos][0] && '\000' == argv[pos][1])
    {
      advance (1);
      value = expr ();
      if (')' != argv[pos][0] || '\000' != argv[pos][1])
	test_syntax_error ("argument expected, found %s\n", argv[pos]);
      advance (0);
      return (TRUE == (value));
    }
  /* are there enough arguments left that this could be dyadic? */
  if (pos + 3 <= argc)
    {
      register int op;
      if ('-' == argv[pos][0] && 'l' == argv[pos][1] &&
	  '\000' == argv[pos][2])
	{
	  l_is_l = 1;
	  op = pos + 2;
	  advance (0);
	}
      else
	{
	  l_is_l = 0;
	  op = pos + 1;
	}
      if ('-' == argv[op + 1][0] && 'l' == argv[op + 1][1]
	  && '\000' == argv[op + 1][2])
	{
	  r_is_l = 1;
	  advance (0);
	}
      else
	r_is_l = 0;

      if ('-' == argv[op][0])
	{
	  /* check for eq, nt, and stuff */
	  switch (argv[op][1])
	    {
	    default:
	      break;
	    case 'l':
	      if ('t' == argv[op][2] && '\000' == argv[op][3])
		{
		  /* lt */
		  if (l_is_l)
		    l = strlen (argv[op - 1]);
		  else
		    {
		      if (!isint (op - 1, &l))
			int_expt_err ("before -lt");
		    }

		  if (r_is_l)
		    r = strlen (argv[op + 2]);
		  else
		    {
		      if (!isint (op + 1, &r))
			int_expt_err ("after -lt");
		    }
		  pos += 3;
		  return (TRUE == (l < r));
		}

	      if ('e' == argv[op][2] && '\000' == argv[op][3])
		{
		  /* le */
		  if (l_is_l)
		    l = strlen (argv[op - 1]);
		  else
		    {
		      if (!isint (op - 1, &l))
			int_expt_err ("before -le");
		    }
		  if (r_is_l)
		    r = strlen (argv[op + 2]);
		  else
		    {
		      if (!isint (op + 1, &r))
			int_expt_err ("after -le");
		    }
		  pos += 3;
		  return (TRUE == (l <= r));
		}
	      break;

	    case 'g':

	      if ('t' == argv[op][2] && '\000' == argv[op][3])
		{
		  /* gt integer greater than */
		  if (l_is_l)
		    l = strlen (argv[op - 1]);
		  else
		    {
		      if (!isint (op - 1, &l))
			int_expt_err ("before -gt");
		    }
		  if (r_is_l)
		    r = strlen (argv[op + 2]);
		  else
		    {
		      if (!isint (op + 1, &r))
			int_expt_err ("after -gt");
		    }
		  pos += 3;
		  return (TRUE == (l > r));
		}

	      if ('e' == argv[op][2] && '\000' == argv[op][3])
		{
		  /* ge - integer greater than or equal to */
		  if (l_is_l)
		    l = strlen (argv[op - 1]);
		  else
		    {
		      if (!isint (op - 1, &l))
			int_expt_err ("before -ge");
		    }
		  if (r_is_l)
		    r = strlen (argv[op + 2]);
		  else
		    {
		      if (!isint (op + 1, &r))
			int_expt_err ("after -ge");
		    }
		  pos += 3;
		  return (TRUE == (l >= r));
		}
	      break;

	    case 'n':
	      if ('t' == argv[op][2] && '\000' == argv[op][3])
		{
		  /* nt - newer than */
		  pos += 3;
		  if (l_is_l || r_is_l)
		    test_syntax_error ("-nt does not accept -l\n",
				       (char *)NULL);
		  if (age_of (op - 1, &l) && age_of (op + 1, &r))
		    return (TRUE == (l > r));
		  else
		    return (FALSE);
		}

	      if ('e' == argv[op][2] && '\000' == argv[op][3])
		{
		  /* ne - integer not equal */
		  if (l_is_l)
		    l = strlen (argv[op - 1]);
		  else
		    {
		      if (!isint (op - 1, &l))
			int_expt_err ("before -ne");
		    }
		  if (r_is_l)
		    r = strlen (argv[op + 2]);
		  else
		    {
		      if (!isint (op + 1, &r))
			int_expt_err ("after -ne");
		    }
		  pos += 3;
		  return (TRUE == (l != r));
		}
	      break;

	    case 'e':
	      if ('q' == argv[op][2] && '\000' == argv[op][3])
		{
		  /* eq - integer equal */
		  if (l_is_l)
		    l = strlen (argv[op - 1]);
		  else
		    {
		      if (!isint (op - 1, &l))
			int_expt_err ("before -eq");
		    }
		  if (r_is_l)
		    r = strlen (argv[op + 2]);
		  else
		    {
		      if (!isint (op + 1, &r))
			int_expt_err ("after -eq");
		    }
		  pos += 3;
		  return (TRUE == (l == r));
		}

	      if ('f' == argv[op][2] && '\000' == argv[op][3])
		{
		  /* ef - hard link? */
		  pos += 3;
		  if (l_is_l || r_is_l)
		    test_syntax_error ("-ef does not accept -l\n",
				       (char *)NULL);
		  if (stat (argv[op - 1], &stat_buf) < 0)
		    return (FALSE);
		  if (stat (argv[op + 1], &stat_spare) < 0)
		    return (FALSE);
		  return (TRUE ==
			  (stat_buf.st_dev == stat_spare.st_dev &&
			   stat_buf.st_ino == stat_spare.st_ino));
		}
	      break;

	    case 'o':
	      if ('t' == argv[op][2] && '\000' == argv[op][3])
		{
		  /* ot - older than */
		  pos += 3;
		  if (l_is_l || r_is_l)
		    test_syntax_error ("-nt does not accept -l\n",
				       (char *)NULL);
		  if (age_of (op - 1, &l) && age_of (op + 1, &r))
		    return (TRUE == (l < r));
		  return (FALSE);
		}
	      break;
	    }
	}

      if ('=' == argv[op][0] && '\000' == argv[op][1])
	{
	  value = (0 == strcmp (argv[pos], argv[pos + 2]));
	  pos += 3;
	  return (TRUE == value);
	}
      if ('!' == argv[op][0] && '=' == argv[op][1] && '\000' == argv[op][2])
	{
	  value = 0 != strcmp (argv[pos], argv[pos + 2]);
	  pos += 3;
	  return (TRUE == value);
	}
    }

  /* Might be a switch type argument */
  if ('-' == argv[pos][0] && '\000' == argv[pos][2] /* && pos < argc-1 */ )
    {
      switch (argv[pos][1])
	{
	default:
	  break;

	  /* All of the following unary operators use unary_advance (), which
	     checks to make sure that there is an argument, and then advances
	     pos right past it.  This means that pos - 1 is the location of the
	     argument. */

	case 'r':		/* file is readable? */
	  unary_advance ();
	  value = -1 != access (argv[pos - 1], R_OK);
	  return (TRUE == value);

	case 'w':		/* File is writeable? */
	  unary_advance ();
	  value = -1 != access (argv[pos - 1], W_OK);
	  return (TRUE == value);

	case 'x':		/* File is executable? */
	  unary_advance ();
	  value = -1 != access (argv[pos - 1], X_OK);
	  return (TRUE == value);

	case 'O':		/* File is owned by you? */
	  unary_advance ();
	  if (stat (argv[pos - 1], &stat_buf) < 0)
	    return (FALSE);

	  return (TRUE == (geteuid () == stat_buf.st_uid));

	case 'f':		/* File is a file? */
	  unary_advance ();
	  if (stat (argv[pos - 1], &stat_buf) < 0)
	    return (FALSE);

	  /*
	   * Under SYSV, -f is true if the given file exists
	   * and is a regular file.  Other places, this checks
	   * to see if the given file is not a directory.
	   */
	  if (sys_v)
	    return (TRUE == ((S_IFREG == (stat_buf.st_mode & S_IFMT)) ||
			     (0 == (stat_buf.st_mode & S_IFMT))));
	  else
	    return (TRUE == (S_IFDIR != (stat_buf.st_mode & S_IFMT)));

	case 'd':		/* File is a directory? */
	  unary_advance ();
	  if (stat (argv[pos - 1], &stat_buf) < 0)
	    return (FALSE);

	  return (TRUE == (S_IFDIR == (stat_buf.st_mode & S_IFMT)));

	case 's':		/* File has something in it? */
	  unary_advance ();
	  if (stat (argv[pos - 1], &stat_buf) < 0)
	    return (FALSE);

	  return (TRUE == (stat_buf.st_size > (off_t) 0));

#ifdef S_IFSOCK
	case 'S':		/* File is a socket? */
	  unary_advance ();
	  if (stat (argv[pos - 1], &stat_buf) < 0)
	    return (FALSE);

	  return (TRUE == (S_IFSOCK == (stat_buf.st_mode & S_IFMT)));
#endif

	case 'c': /* File is character special? */
	  unary_advance ();
	  if (stat (argv[pos - 1], &stat_buf) < 0)
	    return (FALSE);

	  return (TRUE == (S_IFCHR == (stat_buf.st_mode & S_IFMT)));

	case 'b':		/* File is block special? */
	  unary_advance ();
	  if (stat (argv[pos - 1], &stat_buf) < 0)
	    return (FALSE);

	  return (TRUE == (S_IFBLK == (stat_buf.st_mode & S_IFMT)));

	case 'p':		/* File is a named pipe? */
	  unary_advance ();
#ifndef S_IFIFO
	  return (FALSE);
#else
	  if (stat (argv[pos - 1], &stat_buf) < 0)
	    return (FALSE);
	  return (TRUE == (S_IFIFO == (stat_buf.st_mode & S_IFMT)));
#endif /* S_IFIFO */

	case 'L':		/* Same as -h  */
	  /*FALLTHROUGH*/

	case 'h':		/* File is a symbolic link? */
	  unary_advance ();
#ifndef S_IFLNK
	  return (FALSE);
#else
	  if (lstat (argv[pos - 1], &stat_buf) < 0)
	    return (FALSE);

	  return (TRUE == (S_IFLNK == (stat_buf.st_mode & S_IFMT)));
#endif /* S_IFLNK */

	case 'u':		/* File is setuid? */
	  unary_advance ();
	  if (stat (argv[pos - 1], &stat_buf) < 0)
	    return (FALSE);

	  return (TRUE == (0 != (stat_buf.st_mode & S_ISUID)));

	case 'g':		/* File is setgid? */
	  unary_advance ();
	  if (stat (argv[pos - 1], &stat_buf) < 0)
	    return (FALSE);

	  return (TRUE == (0 != (stat_buf.st_mode & S_ISGID)));

	case 'k':		/* File has sticky bit set? */
	  unary_advance ();
	  if (stat (argv[pos - 1], &stat_buf) < 0)
	    return (FALSE);
	  return (TRUE == (0 != (stat_buf.st_mode & S_ISVTX)));

	case 't':		/* File (fd) is a terminal?  (fd) defaults to stdout. */
	  advance (0);
	  if (pos < argc && isint (pos, &r))
	    {
	      advance (0);
	      return (TRUE == (isatty ((int) r)));
	    }
	  return (TRUE == (isatty (1)));

	case 'n':		/* True if arg has some length. */
	  unary_advance ();
	  return (TRUE == (0 != strlen (argv[pos - 1])));
	  break;

	case 'z':		/* True if arg has no length. */
	  unary_advance ();
	  return (TRUE == (0 == strlen (argv[pos - 1])));
	}
    }
  value = 0 != strlen (argv[pos]);
  advance (0);
  return value;
}

/*
 * and:
 *	and '-a' term
 *	term
 */
static int
and ()
{
  auto int value;

  value = term ();
  while (pos < argc && '-' == argv[pos][0] && 'a' == argv[pos][1] && '\000' == argv[pos][2])
    {
      advance (0);
      value = TRUTH_AND (value, term ());
    }
  return (TRUE == value);
}

/*
 * or:
 *	or '-o' and
 *	and
 */
static int
or ()
{
  auto int value;

  value = and ();
  while (pos < argc && '-' == argv[pos][0] && 'o' == argv[pos][1] && '\000' == argv[pos][2])
    {
      advance (0);
      value = TRUTH_OR (value, and ());
    }
  return (TRUE == value);
}

/*
 * expr:
 *	or
 */
int
expr ()
{
  auto int value;

  if (pos >= argc)
    beyond ();

  value = FALSE;

  return value ^ or ();		/* Same with this. */
}

/*
 * [:
 *	'[' expr ']'
 * test:
 *	test expr
 */
int
#ifdef STANDALONE
main (margc, margv)
#else
test_command (margc, margv)
#endif /* STANDALONE */
     int margc;
     char **margv;
{
  auto int value;
  int expr ();
#ifndef STANDALONE
  int code = setjmp (test_exit_buf);

  if (code)
    return (test_error_return);
#endif /* STANDALONE */

  argv = margv;

  if (margv[0] && strcmp (margv[0], "[") == 0)
    {
      --margc;

      if (margc < 2)
	test_exit (SHELL_BOOLEAN (FALSE));

      if (margv[margc] && strcmp (margv[margc], "]") != 0)
	test_syntax_error ("missing `]'\n", (char *)NULL);
    }

  argc = margc;
  pos = 1;

  if (pos >= argc)
    test_exit (SHELL_BOOLEAN (FALSE));

  value = expr ();
  if (pos != argc)
    test_syntax_error ("too many arguments\n", (char *)NULL);

  test_exit (SHELL_BOOLEAN (value));
}
