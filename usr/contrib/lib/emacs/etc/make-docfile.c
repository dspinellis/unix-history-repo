/* Generate doc-string file for GNU Emacs from source files.
   Copyright (C) 1985, 1986 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* The arguments given to this program are all the C and Lisp source files
 of GNU Emacs.  .elc and .el and .c files are allowed.
 A .o file can also be specified; the .c file it was made from is used.
 This helps the makefile pass the correct list of files.

 The results, which go to standard output or to a file
 specified with -a or -o (-a to append, -o to start from nothing),
 are entries containing function or variable names and their documentation.
 Each entry starts with a ^_ character.
 Then comes F for a function or V for a variable.
 Then comes the function or variable name, terminated with a newline.
 Then comes the documentation for that function or variable.
 */

#include <stdio.h>

FILE *outfile;

main (argc, argv)
     int argc;
     char **argv;
{
  int i;
  int err_count = 0;

  outfile = stdout;

  /* If first two args are -o FILE, output to FILE.  */
  i = 1;
  if (argc > i + 1 && !strcmp (argv[i], "-o"))
    {
      outfile = fopen (argv[i + 1], "w");
      i += 2;
    }
  if (argc > i + 1 && !strcmp (argv[i], "-a"))
    {
      outfile = fopen (argv[i + 1], "a");
      i += 2;
    }

  for (; i < argc; i++)
    err_count += scan_file (argv[i]);	/* err_count seems to be {mis,un}used */
#ifndef VMS
  exit (err_count);			/* see below - shane */
#endif VMS
}

/* Read file FILENAME and output its doc strings to stdout.  */
/* Return 1 if file is not found, 0 if it is found.  */

scan_file (filename)
     char *filename;
{
  int len = strlen (filename);
  if (!strcmp (filename + len - 4, ".elc"))
    return scan_lisp_file (filename);
  else if (!strcmp (filename + len - 3, ".el"))
    return scan_lisp_file (filename);
  else
    return scan_c_file (filename);
}

char buf[128];

/* Skip a C string from INFILE,
 and return the character that follows the closing ".
 If printflag is positive, output string contents to stdout.
 If it is negative, store contents in buf.
 Convert escape sequences \n and \t to newline and tab;
 discard \ followed by newline.  */

read_c_string (infile, printflag)
     FILE *infile;
     int printflag;
{
  register int c;
  char *p = buf;

  c = getc (infile);
  while (c != EOF)
    {
      while (c != '"' && c != EOF)
	{
	  if (c == '\\')
	    {
	      c = getc (infile);
	      if (c == '\n')
		{
		  c = getc (infile);
		  continue;
		}
	      if (c == 'n')
		c = '\n';
	      if (c == 't')
		c = '\t';
	    }
	  if (printflag > 0)
	    putc (c, outfile);
	  else if (printflag < 0)
	    *p++ = c;
	  c = getc (infile);
	}
      c = getc (infile);
      if (c != '"')
	break;
      if (printflag > 0)
	putc (c, outfile);
      else if (printflag < 0)
	*p++ = c;
      c = getc (infile);
    }

  if (printflag < 0)
    *p = 0;

  return c;
}

/* Read through a c file.  If a .o file is named,
 the corresponding .c file is read instead.
 Looks for DEFUN constructs such as are defined in ../src/lisp.h.
 Accepts any word starting DEF... so it finds DEFSIMPLE and DEFPRED.  */

scan_c_file (filename)
     char *filename;
{
  FILE *infile;
  register int c;
  register int commas;
  register int defunflag;
  register int defvarflag;
  
  if (filename[strlen (filename) - 1] == 'o')
    filename[strlen (filename) - 1] = 'c';

  infile = fopen (filename, "r");

  /* No error if non-ex input file */
  if (infile == NULL)
    {
      perror (filename);
      return 0;
    }

  c = '\n';
  while (!feof (infile))
    {
      if (c != '\n')
	{
	  c = getc (infile);
	  continue;
	}
      c = getc (infile);
      if (c == ' ')
	{
	  while (c == ' ')
	    c = getc (infile);
	  if (c != 'D')
	    continue;
	  c = getc (infile);
	  if (c != 'E')
	    continue;
	  c = getc (infile);
	  if (c != 'F')
	    continue;
	  c = getc (infile);
	  if (c != 'V')
	    continue;
	  defvarflag = 1;
	  defunflag = 0;
	  c = getc (infile);
	}
      else if (c == 'D')
	{
	  c = getc (infile);
	  if (c != 'E')
	    continue;
	  c = getc (infile);
	  if (c != 'F')
	    continue;
	  c = getc (infile);
	  defunflag = c == 'U';
	  defvarflag = 0;
	}
      else continue;

      while (c != '(')
	{
	  if (c < 0)
	    return 0;
	  c = getc (infile);
	}

      c = getc (infile);
      if (c != '"')
	continue;
      c = read_c_string (infile, -1);

      if (defunflag)
	commas = 5;
      else if (defvarflag)
	commas = 1;
      else  /* For DEFSIMPLE and DEFPRED */
	commas = 2;

      while (commas)
	{
	  if (c == ',') commas --;
	  if (c < 0)
	    return 0;
	  c = getc (infile);
	}
      while (c == ' ' || c == '\n' || c == '\t')
	c = getc (infile);
      if (c == '"')
	c = read_c_string (infile, 0);
      while (c != ',')
	c = getc (infile);
      c = getc (infile);
      while (c == ' ' || c == '\n' || c == '\t')
	c = getc (infile);

      if (c == '"')
	{
	  putc (037, outfile);
	  putc (defvarflag ? 'V' : 'F', outfile);
	  fprintf (outfile, "%s\n", buf);
	  read_c_string (infile, 1);
	}
    }
  fclose (infile);
  return 0;
}

/* Read a file of Lisp code, compiled or interpreted.
 Looks for
  (defun NAME ARGS DOCSTRING ...)
  (autoload 'NAME FILE DOCSTRING ...)
  (defvar NAME VALUE DOCSTRING)
  (defconst NAME VALUE DOCSTRING)
 starting in column zero.
 ARGS, FILE or VALUE is ignored.  We do not know how to parse Lisp code
 so we use a kludge to skip them:
  In a function definition, the form of ARGS of FILE is known, and we
  can skip it.
  In a variable definition, we use a formatting convention:
  the DOCSTRING, if present, must be followed by a closeparen and a newline,
  and no newline must appear between the defvar or defconst and the docstring,
  The only source file that must follow this convention is loaddefs.el;
  aside from that, it is always the .elc file that we look at, and
  they are no problem because byte-compiler output follows this convention.
 The NAME and DOCSTRING are output.
 NAME is preceded by `F' for a function or `V' for a variable.
 An entry is output only if DOCSTRING has \ newline just after the opening "
 */

scan_lisp_file (filename)
     char *filename;
{
  FILE *infile;
  register int c;
  register int commas;
  register char *p;
  int defvarflag;

  infile = fopen (filename, "r");
  if (infile == NULL)
    {
      perror (filename);
      return 0;				/* No error */
    }

  c = '\n';
  while (!feof (infile))
    {
      if (c != '\n')
	{
	  c = getc (infile);
	  continue;
	}
      c = getc (infile);
      if (c != '(')
	continue;
      c = getc (infile);
      if (c == 'a')
	{
	  c = getc (infile);
	  if (c != 'u')
	    continue;
	  c = getc (infile);
	  if (c != 't')
	    continue;
	  c = getc (infile);
	  if (c != 'o')
	    continue;
	  c = getc (infile);
	  if (c != 'l')
	    continue;
	  c = getc (infile);
	  if (c != 'o')
	    continue;
	  c = getc (infile);
	  if (c != 'a')
	    continue;
	  c = getc (infile);
	  if (c != 'd')
	    continue;

	  c = getc (infile);
	  while (c == ' ')
	    c = getc (infile);

	  if (c == '\'')
	    {
	      c = getc (infile);
	    }
	  else
	    {
	      if (c != '(')
		continue;
	      c = getc (infile);
	      if (c != 'q')
		continue;
	      c = getc (infile);
	      if (c != 'u')
		continue;
	      c = getc (infile);
	      if (c != 'o')
		continue;
	      c = getc (infile);
	      if (c != 't')
		continue;
	      c = getc (infile);
	      if (c != 'e')
		continue;
	      c = getc (infile);
	      if (c != ' ')
		continue;
	      while (c == ' ')
		c = getc (infile);
	    }

	  p = buf;
	  while (c != ' ' && c != ')')
	    {
	      if (c == EOF)
		return 1;
	      if (c == '\\')
		c = getc (infile);
	      *p++ = c;
	      c = getc (infile);
	    }
	  *p = 0;

	  while (c != '"')
	    {
	      if (c == EOF)
		return 1;
	      c = getc (infile);
	    }
	  c = read_c_string (infile, 0);
	}
      else if (c == 'd')
	{
	  c = getc (infile);
	  if (c != 'e')
	    continue;
	  c = getc (infile);
	  if (c != 'f')
	    continue;
	  c = getc (infile);
	  if (c == 'u')
	    {
	      c = getc (infile);
	      if (c != 'n')
		continue;
	      defvarflag = 0;
	    }
	  else if (c == 'v')
	    {
	      c = getc (infile);
	      if (c != 'a')
		continue;
	      c = getc (infile);
	      if (c != 'r')
		continue;
	      defvarflag = 1;
	    }
	  else if (c == 'c')
	    {
	      c = getc (infile);
	      if (c != 'o')
		continue;
	      c = getc (infile);
	      if (c != 'n')
		continue;
	      c = getc (infile);
	      if (c != 's')
		continue;
	      c = getc (infile);
	      if (c != 't')
		continue;
	      defvarflag = 1;
	    }
	  else
	    continue;

	  /* Now we have seen "defun" or "defvar" or "defconst".  */

	  while (c != ' ' && c != '\n' && c != '\t')
	    c = getc (infile);

	  while (c == ' ' || c == '\n' || c == '\t')
	    c = getc (infile);

	  /* Read and store name of function or variable being defined
	     Discard backslashes that are for quoting.  */
	  p = buf;
	  while (c != ' ' && c != '\n' && c != '\t')
	    {
	      if (c == '\\')
		c = getc (infile);
	      *p++ = c;
	      c = getc (infile);
	    }
	  *p = 0;

	  while (c == ' ' || c == '\n' || c == '\t')
	    c = getc (infile);

	  if (! defvarflag)
	    {
	      /* A function: */
	      /* Skip the arguments: either "nil" or a list in parens */
	      if (c == 'n')
		{
		  while (c != ' ' && c != '\n' && c != '\t')
		    c = getc (infile);
		}
	      else
		{
		  while (c != '(')
		    c = getc (infile);
		  while (c != ')')
		    c = getc (infile);
		}
	      c = getc (infile);
	    }
	  else
	    {
	      /* A variable:  */

	      /* Skip until the first newline; remember
		 the two previous characters.  */
	      char c1 = 0, c2 = 0;

	      while (c != '\n' && c >= 0)
		{
		  c2 = c1;
		  c1 = c;
		  c = getc (infile);
		}

	      /* If two previous characters were " and \,
		 this is a doc string.  Otherwise, there is none.  */
	      if (c2 == '"' && c1 == '\\')
		{
		  putc (037, outfile);
		  putc ('V', outfile);
		  fprintf (outfile, "%s\n", buf);
		  read_c_string (infile, 1);
		}
	      continue;
	    }
	}
      else
	continue;

      /* Here for a function definition.
	 We have skipped the file name or arguments
	 and arrived at where the doc string is,
	 if there is a doc string.  */

      /* Skip whitespace */

      while (c == ' ' || c == '\n' || c == '\t')
	c = getc (infile);

      /* " followed by \ and newline means a doc string we should gobble */
      if (c != '"')
	continue;
      c = getc (infile);
      if (c != '\\')
	continue;
      c = getc (infile);
      if (c != '\n')
	continue;

      putc (037, outfile);
      putc ('F', outfile);
      fprintf (outfile, "%s\n", buf);
      read_c_string (infile, 1);
    }
  fclose (infile);
  return 0;
}
