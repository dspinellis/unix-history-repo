/* GNU test program (ksb and mjb) */

/* Modified to run with the GNU shell Apr 25, 1988 by bfox. */

/* Copyright (C) 1987, 1988, 1989, 1990, 1991 Free Software Foundation, Inc.

   This file is part of GNU Bash, the Bourne Again SHell.

   Bash is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 2, or (at your option) any later
   version.

   Bash is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License along
   with Bash; see the file COPYING.  If not, write to the Free Software
   Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. */

/* Define STANDALONE to get the /bin/test version.  Otherwise, you get 
   the shell builtin version. */
/* #define STANDALONE */

#include <stdio.h>
#include <sys/types.h>

#if defined (STANDALONE)
#  if defined (HAVE_UNISTD_H)
#    include <unistd.h>
#  endif /* HAVE_UNISTD_H */
#else /* !STANDALONE */
#  include "shell.h"
#endif /* !STANDALONE */

#if !defined (_POSIX_VERSION)
#  include <sys/file.h>
#endif /* !_POSIX_VERSION */

#include <errno.h>
#if !defined (errno)
extern int errno;
#endif /* !errno */

#if !defined (STANDALONE)
#  include "posixstat.h"
#  include "filecntl.h"
#else /* STANDALONE */
#  include "system.h"
#  if !defined (S_IXUGO)
#    define S_IXUGO 0111
#  endif
#endif /* STANDALONE */

#if !defined (STREQ)
#  define STREQ(a, b) ((a)[0] == (b)[0] && strcmp (a, b) == 0)
#endif /* !STREQ */

#if !defined (member)
#  define member(c, s) (int)((c) ? index ((s), (c)) : 0)
#endif /* !member */

#if defined (STANDALONE) && (defined (USG) || defined (STDC_HEADERS))
#  if !defined (index)
#    define index strchr
#    define rindex strrchr
#  endif /* !index */
#endif /* STANDALONE && (USG || STDC_HEADERS) */

/* Make gid_t and uid_t mean something for non-posix systems. */
#if !defined (_POSIX_VERSION)
#  if !defined (gid_t)
#    define gid_t int
#  endif
#  if !defined (uid_t)
#    define uid_t int
#  endif
#endif /* !_POSIX_VERSION */

extern gid_t getgid (), getegid ();
extern uid_t geteuid ();

#if !defined (R_OK)
#define R_OK 4
#define W_OK 2
#define X_OK 1
#define F_OK 0
#endif /* R_OK */

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

#if defined (STANDALONE)
#  define test_exit(val) exit (val)
#else
   static jmp_buf test_exit_buf;
   static int test_error_return = 0;
#  define test_exit(val) test_error_return = val, longjmp (test_exit_buf, 1)
#endif /* STANDALONE */

static int pos;		/* The offset of the current argument in ARGV. */
static int argc;	/* The number of arguments present in ARGV. */
static char **argv;	/* The argument list. */

static int unop ();
static int binop ();
static int unary_operator ();
static int binary_operator ();
static int two_arguments ();
static int three_arguments ();
static int posixtest ();

static int expr ();
static int term ();
static int and ();
static int or ();

static void
test_syntax_error (format, arg)
     char *format, *arg;
{
  fprintf (stderr, "%s: ", argv[0]);
  fprintf (stderr, format, arg);
  fflush (stderr);
  test_exit (SHELL_BOOLEAN (FALSE));
}

/* A wrapper for stat () which disallows pathnames that are empty strings. */
static int
test_stat (path, finfo)
     char *path;
     struct stat *finfo;
{
  if (*path == '\0')
    {
      errno = ENOENT;
      return (-1);
    }
  return (stat (path, finfo));
}

/* Do the same thing access(2) does, but use the effective uid and gid,
   and don't make the mistake of telling root that any file is
   executable. */
static int
eaccess (path, mode)
     char *path;
     int mode;
{
  extern int group_member ();
  struct stat st;
  static int euid = -1;

  if (test_stat (path, &st) < 0)
    return (-1);

  if (euid == -1)
    euid = geteuid ();

  if (euid == 0)
    {
      /* Root can read or write any file. */
      if (mode != X_OK)
	return (0);

      /* Root can execute any file that has any one of the execute
	 bits set. */
      if (st.st_mode & S_IXUGO)
	return (0);
    }

  if (st.st_uid == euid)        /* owner */
    mode <<= 6;
  else if (group_member (st.st_gid))
    mode <<= 3;

  if (st.st_mode & mode)
    return (0);

  return (-1);
}

#if defined (HAVE_GETGROUPS)
/* The number of groups that this user is a member of. */
static int ngroups = 0;
static gid_t *group_array = (gid_t *)NULL;
static int default_group_array_size = 0;
#endif /* HAVE_GETGROUPS */

/* Return non-zero if GID is one that we have in our groups list. */
int
group_member (gid)
     gid_t gid;
{
#if !defined (HAVE_GETGROUPS)
  return ((gid == getgid ()) || (gid == getegid ()));
#else
  register int i;

  /* getgroups () returns the number of elements that it was able to
     place into the array.  We simply continue to call getgroups ()
     until the number of elements placed into the array is smaller than
     the physical size of the array. */

  while (ngroups == default_group_array_size)
    {
      default_group_array_size += 64;

      group_array = (gid_t *)
	xrealloc (group_array,
		  default_group_array_size * sizeof (gid_t));

      ngroups = getgroups (default_group_array_size, group_array);
    }

  /* In case of error, the user loses. */
  if (ngroups < 0)
    return (0);

  /* Search through the list looking for GID. */
  for (i = 0; i < ngroups; i++)
    if (gid == group_array[i])
      return (1);

  return (0);
#endif /* HAVE_GETGROUPS */
}

/* Increment our position in the argument list.  Check that we're not
   past the end of the argument list.  This check is supressed if the
   argument is FALSE.  Made a macro for efficiency. */
#if !defined (lint)
#define advance(f)	(++pos, f && (pos < argc ? 0 : beyond()))
#endif

#if !defined (advance)
static int
advance (f)
     int f;
{
  ++pos;

  if (f && pos >= argc)
    beyond ();
}
#endif /* advance */

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

/* Syntax error for when an integer argument was expected, but
   something else was found. */
static void
integer_expected_error (pch)
     char *pch;
{
  test_syntax_error ("integer expression expected %s\n", pch);
}

/* Return non-zero if the characters pointed to by STRING constitute a
   valid number.  Stuff the converted number into RESULT if RESULT is
   a non-null pointer to a long. */
static int
isint (string, result)
     register char *string;
     long *result;
{
  int sign;
  long value;

  sign = 1;
  value = 0;

  if (result)
    *result = 0;

  /* Skip leading whitespace characters. */
  while (whitespace (*string))
    string++;

  if (!*string)
    return (0);

  /* We allow leading `-' or `+'. */
  if (*string == '-' || *string == '+')
    {
      if (!digit (string[1]))
	return (0);

      if (*string == '-')
	sign = -1;

      string++;
    }

  while (digit (*string))
    {
      if (result)
	value = (value * 10) + digit_value (*string);
      string++;
    }

  /* Skip trailing whitespace, if any. */
  while (whitespace (*string))
    string++;

  /* Error if not at end of string. */
  if (*string)
    return (0);

  if (result)
    {
      value *= sign;
      *result = value;
    }

  return (1);
}

/* Find the modification time of FILE, and stuff it into AGE, a pointer
   to a long.  Return non-zero if successful, else zero. */
static int
age_of (filename, age)
     char *filename;
     long *age;
{
  struct stat finfo;

  if (test_stat (filename, &finfo) < 0)
    return (0);

  if (age)
    *age = finfo.st_mtime;

  return (1);
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
  int value;

  if (pos >= argc)
    beyond ();

  /* Deal with leading "not"'s. */
  if ('!' == argv[pos][0] && '\000' == argv[pos][1])
    {
      value = FALSE;
      while (pos < argc && '!' == argv[pos][0] && '\000' == argv[pos][1])
	{
	  advance (1);
	  value ^= (TRUE);
	}

      return (value ^ (term ()));
    }

  /* A paren-bracketed argument. */  
  if (argv[pos][0] == '(' && !argv[pos][1])
    {
      advance (1);
      value = expr ();
      if (argv[pos][0] != ')' || argv[pos][1])
	test_syntax_error ("')' expected, found %s\n", argv[pos]);
      advance (0);
      return (TRUE == (value));
    }

  /* are there enough arguments left that this could be dyadic? */
  if (((pos + 3 <= argc) && binop (argv[pos + 1])) ||
      ((pos + 4 <= argc && STREQ (argv[pos], "-l") && binop (argv[pos + 2]))))
    value = binary_operator ();

  /* Might be a switch type argument */
  else if ('-' == argv[pos][0] && 0 == argv[pos][2])
    {
      if (unop (argv[pos][1]))
	value = unary_operator ();
      else
	test_syntax_error ("%s: unary operator expected\n", argv[pos]);
    }
  else
    {
      value = (argv[pos][0] != '\0');
      advance (0);
    }

  return (value);
}

static int
binary_operator ()
{
  register int op;
  struct stat stat_buf, stat_spare;
  long int l, r, value;
  /* Are the left and right integer expressions of the form '-l string'? */
  int l_is_l, r_is_l;

  if (strcmp (argv[pos], "-l") == 0)
    {
      l_is_l = 1;
      op = pos + 2;

      /* Make sure that OP is still a valid binary operator. */
      if ((op >= argc - 1) || (binop (argv[op]) == 0))
	test_syntax_error ("%s: binary operator expected\n", argv[op]);

      advance (0);
    }
  else
    {
      l_is_l = 0;
      op = pos + 1;
    }

  if ((op < argc - 2) && (strcmp (argv[op + 1], "-l") == 0))
    {
      r_is_l = 1;
      advance (0);
    }
  else
    r_is_l = 0;

  if (argv[op][0] == '-')
    {
      /* check for eq, nt, and stuff */
      switch (argv[op][1])
	{
	default:
	  break;

	case 'l':
	  if (argv[op][2] == 't' && !argv[op][3])
	    {
	      /* lt */
	      if (l_is_l)
		l = strlen (argv[op - 1]);
	      else
		{
		  if (!isint (argv[op - 1], &l))
		    integer_expected_error ("before -lt");
		}

	      if (r_is_l)
		r = strlen (argv[op + 2]);
	      else
		{
		  if (!isint (argv[op + 1], &r))
		    integer_expected_error ("after -lt");
		}
	      pos += 3;
	      return (TRUE == (l < r));
	    }

	  if (argv[op][2] == 'e' && !argv[op][3])
	    {
	      /* le */
	      if (l_is_l)
		l = strlen (argv[op - 1]);
	      else
		{
		  if (!isint (argv[op - 1], &l))
		    integer_expected_error ("before -le");
		}
	      if (r_is_l)
		r = strlen (argv[op + 2]);
	      else
		{
		  if (!isint (argv[op + 1], &r))
		    integer_expected_error ("after -le");
		}
	      pos += 3;
	      return (TRUE == (l <= r));
	    }
	  break;

	case 'g':
	  if (argv[op][2] == 't' && !argv[op][3])
	    {
	      /* gt integer greater than */
	      if (l_is_l)
		l = strlen (argv[op - 1]);
	      else
		{
		  if (!isint (argv[op - 1], &l))
		    integer_expected_error ("before -gt");
		}
	      if (r_is_l)
		r = strlen (argv[op + 2]);
	      else
		{
		  if (!isint (argv[op + 1], &r))
		    integer_expected_error ("after -gt");
		}
	      pos += 3;
	      return (TRUE == (l > r));
	    }

	  if (argv[op][2] == 'e' && !argv[op][3])
	    {
	      /* ge - integer greater than or equal to */
	      if (l_is_l)
		l = strlen (argv[op - 1]);
	      else
		{
		  if (!isint (argv[op - 1], &l))
		    integer_expected_error ("before -ge");
		}
	      if (r_is_l)
		r = strlen (argv[op + 2]);
	      else
		{
		  if (!isint (argv[op + 1], &r))
		    integer_expected_error ("after -ge");
		}
	      pos += 3;
	      return (TRUE == (l >= r));
	    }
	  break;

	case 'n':
	  if (argv[op][2] == 't' && !argv[op][3])
	    {
	      /* nt - newer than */
	      pos += 3;
	      if (l_is_l || r_is_l)
		test_syntax_error ("-nt does not accept -l\n", (char *)NULL);
	      if (age_of (argv[op - 1], &l) && age_of (argv[op + 1], &r))
		return (TRUE == (l > r));
	      else
		return (FALSE);
	    }

	  if (argv[op][2] == 'e' && !argv[op][3])
	    {
	      /* ne - integer not equal */
	      if (l_is_l)
		l = strlen (argv[op - 1]);
	      else
		{
		  if (!isint (argv[op - 1], &l))
		    integer_expected_error ("before -ne");
		}
	      if (r_is_l)
		r = strlen (argv[op + 2]);
	      else
		{
		  if (!isint (argv[op + 1], &r))
		    integer_expected_error ("after -ne");
		}
	      pos += 3;
	      return (TRUE == (l != r));
	    }
	  break;

	case 'e':
	  if (argv[op][2] == 'q' && !argv[op][3])
	    {
	      /* eq - integer equal */
	      if (l_is_l)
		l = strlen (argv[op - 1]);
	      else
		{
		  if (!isint (argv[op - 1], &l))
		    integer_expected_error ("before -eq");
		}
	      if (r_is_l)
		r = strlen (argv[op + 2]);
	      else
		{
		  if (!isint (argv[op + 1], &r))
		    integer_expected_error ("after -eq");
		}
	      pos += 3;
	      return (TRUE == (l == r));
	    }

	  if (argv[op][2] == 'f' && !argv[op][3])
	    {
	      /* ef - hard link? */
	      pos += 3;
	      if (l_is_l || r_is_l)
		test_syntax_error ("-ef does not accept -l\n", (char *)NULL);
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
		test_syntax_error ("-nt does not accept -l\n", (char *)NULL);
	      if (age_of (argv[op - 1], &l) && age_of (argv[op + 1], &r))
		return (TRUE == (l < r));
	      return (FALSE);
	    }
	  break;
	}
      test_syntax_error ("unknown binary operator", argv[op]);
    }

  if (argv[op][0] == '=' && !argv[op][1])
    {
      value = (strcmp (argv[pos], argv[pos + 2]) == 0);
      pos += 3;
      return (TRUE == value);
    }

  if (strcmp (argv[op], "!=") == 0)
    {
      value = (strcmp (argv[pos], argv[pos + 2]) != 0);
      pos += 3;
      return (TRUE == value);
    }
}

static int
unary_operator ()
{
  long r, value;
  struct stat stat_buf;

  switch (argv[pos][1])
    {
    default:
      return (FALSE);

      /* All of the following unary operators use unary_advance (), which
	 checks to make sure that there is an argument, and then advances
	 pos right past it.  This means that pos - 1 is the location of the
	 argument. */

    case 'a':			/* file exists in the file system? */
    case 'e':
      unary_advance ();
      value = -1 != test_stat (argv[pos - 1], &stat_buf);
      return (TRUE == value);

    case 'r':			/* file is readable? */
      unary_advance ();
      value = -1 != eaccess (argv[pos - 1], R_OK);
      return (TRUE == value);

    case 'w':			/* File is writeable? */
      unary_advance ();
      value = -1 != eaccess (argv[pos - 1], W_OK);
      return (TRUE == value);

    case 'x':			/* File is executable? */
      unary_advance ();
      value = -1 != eaccess (argv[pos - 1], X_OK);
      return (TRUE == value);

    case 'O':			/* File is owned by you? */
      unary_advance ();
      if (test_stat (argv[pos - 1], &stat_buf) < 0)
	return (FALSE);

      return (TRUE == (geteuid () == stat_buf.st_uid));

    case 'G':			/* File is owned by your group? */
      unary_advance ();
      if (test_stat (argv[pos - 1], &stat_buf) < 0)
	return (FALSE);

      return (TRUE == (getegid () == stat_buf.st_gid));

    case 'f':			/* File is a file? */
      unary_advance ();
      if (test_stat (argv[pos - 1], &stat_buf) < 0)
	return (FALSE);

      /* Under POSIX, -f is true if the given file exists
	 and is a regular file. */
      return (TRUE == ((S_ISREG (stat_buf.st_mode)) ||
		       (0 == (stat_buf.st_mode & S_IFMT))));

    case 'd':			/* File is a directory? */
      unary_advance ();
      if (test_stat (argv[pos - 1], &stat_buf) < 0)
	return (FALSE);

      return (TRUE == (S_ISDIR (stat_buf.st_mode)));

    case 's':			/* File has something in it? */
      unary_advance ();
      if (test_stat (argv[pos - 1], &stat_buf) < 0)
	return (FALSE);

      return (TRUE == (stat_buf.st_size > (off_t) 0));

    case 'S':			/* File is a socket? */
#if !defined (S_ISSOCK)
      return (FALSE);
#else
      unary_advance ();

      if (test_stat (argv[pos - 1], &stat_buf) < 0)
	return (FALSE);

      return (TRUE == (S_ISSOCK (stat_buf.st_mode)));
#endif				/* S_ISSOCK */

    case 'c':			/* File is character special? */
      unary_advance ();
      if (test_stat (argv[pos - 1], &stat_buf) < 0)
	return (FALSE);

      return (TRUE == (S_ISCHR (stat_buf.st_mode)));

    case 'b':			/* File is block special? */
      unary_advance ();
      if (test_stat (argv[pos - 1], &stat_buf) < 0)
	return (FALSE);

      return (TRUE == (S_ISBLK (stat_buf.st_mode)));

    case 'p':			/* File is a named pipe? */
      unary_advance ();
#ifndef S_ISFIFO
      return (FALSE);
#else
      if (test_stat (argv[pos - 1], &stat_buf) < 0)
	return (FALSE);
      return (TRUE == (S_ISFIFO (stat_buf.st_mode)));
#endif				/* S_ISFIFO */

    case 'L':			/* Same as -h  */
      /*FALLTHROUGH*/

    case 'h':			/* File is a symbolic link? */
      unary_advance ();
#ifndef S_ISLNK
      return (FALSE);
#else
      /* An empty filename is not a valid pathname. */
      if ((argv[pos - 1][0] == '\0') ||
	  (lstat (argv[pos - 1], &stat_buf) < 0))
	return (FALSE);

      return (TRUE == (S_ISLNK (stat_buf.st_mode)));
#endif				/* S_IFLNK */

    case 'u':			/* File is setuid? */
      unary_advance ();
      if (test_stat (argv[pos - 1], &stat_buf) < 0)
	return (FALSE);

      return (TRUE == (0 != (stat_buf.st_mode & S_ISUID)));

    case 'g':			/* File is setgid? */
      unary_advance ();
      if (test_stat (argv[pos - 1], &stat_buf) < 0)
	return (FALSE);

      return (TRUE == (0 != (stat_buf.st_mode & S_ISGID)));

    case 'k':			/* File has sticky bit set? */
      unary_advance ();
      if (test_stat (argv[pos - 1], &stat_buf) < 0)
	return (FALSE);
#if !defined (S_ISVTX)
      /* This is not Posix, and is not defined on some Posix systems. */
      return (FALSE);
#else
      return (TRUE == (0 != (stat_buf.st_mode & S_ISVTX)));
#endif

    case 't':	/* File (fd) is a terminal?  (fd) defaults to stdout. */
      advance (0);
      if (pos < argc && isint (argv[pos], &r))
	{
	  advance (0);
	  return (TRUE == (isatty ((int) r)));
	}
      return (TRUE == (isatty (1)));

    case 'n':			/* True if arg has some length. */
      unary_advance ();
      return (TRUE == (argv[pos - 1][0] != 0));

    case 'z':			/* True if arg has no length. */
      unary_advance ();
      return (TRUE == (argv[pos - 1][0] == '\0'));
    }
}
	
/*
 * and:
 *	term
 *	term '-a' and
 */
static int
and ()
{
  int value;

  value = term ();
  while ((pos < argc) && strcmp (argv[pos], "-a") == 0)
    {
      advance (0);
      value = TRUTH_AND (value, and ());
    }
  return (TRUE == value);
}

/*
 * or:
 *	and
 *	and '-o' or
 */
static int
or ()
{
  int value;

  value = and ();

  while ((pos < argc) && strcmp (argv[pos], "-o") == 0)
    {
      advance (0);
      value = TRUTH_OR (value, or ());
    }

  return (TRUE == value);
}

/*
 * expr:
 *	or
 */
static int
expr ()
{
  if (pos >= argc)
    beyond ();

  return (FALSE ^ (or ()));		/* Same with this. */
}

/* Return TRUE if S is one of the test command's binary operators. */
static int
binop (s)
     char *s;
{
  return ((STREQ (s,   "=")) || (STREQ (s,  "!=")) || (STREQ (s, "-nt")) ||
	  (STREQ (s, "-ot")) || (STREQ (s, "-ef")) || (STREQ (s, "-eq")) ||
	  (STREQ (s, "-ne")) || (STREQ (s, "-lt")) || (STREQ (s, "-le")) ||
	  (STREQ (s, "-gt")) || (STREQ (s, "-ge")));
}

/* Return non-zero if OP is one of the test command's unary operators. */
static int
unop (op)
     int op;
{
  return (member (op, "abcdefgkLhprsStuwxOGnz"));
}

static int
two_arguments ()
{
  int value;

  if (STREQ (argv[pos], "!"))
    value = strlen (argv[pos + 1]) == 0;
  else if ((argv[pos][0] == '-') && (argv[pos][2] == '\0'))
    {
      if (unop (argv[pos][1]))
	value = unary_operator ();
      else
	test_syntax_error ("%s: unary operator expected\n", argv[pos]);
    }
  else
    beyond ();
  return (value);
}

static int
three_arguments ()
{
  int value;

  if (STREQ (argv[pos], "!"))
    {
      advance (1);
      value = !two_arguments ();
    }
  else if (binop (argv[pos+1]))
    {
      value = binary_operator ();
      pos = argc;
    }
  else if ((STREQ (argv[pos+1], "-a")) || (STREQ (argv[pos+1], "-o")) ||
	   (argv[pos][0] == '('))
    value = expr ();
  else
    test_syntax_error ("%s: binary operator expected\n", argv[pos+1]);
  return (value);
}

/* This is an implementation of a Posix.2 proposal by David Korn. */
static int
posixtest ()
{
  int value;

  switch (argc - 1)	/* one extra passed in */
    {
      case 0:
	value = FALSE;
	pos = argc;
	break;

      case 1:
	value = strlen (argv[1]) != 0;
	pos = argc;
	break;

      case 2:
	value = two_arguments ();
	pos = argc;
	break;

      case 3:
	value = three_arguments ();
	break;

      case 4:
	if (STREQ (argv[pos], "!"))
	  {
	    advance (1);
	    value = !three_arguments ();
	    break;
	  }
	/* FALLTHROUGH */
      case 5:
      default:
	value = expr ();
    }

  return (value);
}

/*
 * [:
 *	'[' expr ']'
 * test:
 *	test expr
 */
int
#if defined (STANDALONE)
main (margc, margv)
#else
test_command (margc, margv)
#endif /* STANDALONE */
     int margc;
     char **margv;
{
  auto int value;
  int expr ();

#if !defined (STANDALONE)
  int code;

  code = setjmp (test_exit_buf);

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

  value = posixtest ();

  if (pos != argc)
    test_syntax_error ("too many arguments\n", (char *)NULL);

  test_exit (SHELL_BOOLEAN (value));
}
