/* Generate doc-string file for GNU Emacs from source files.
   Copyright (C) 1985 Richard M. Stallman.

This file is part of GNU Emacs.

GNU Emacs is distributed in the hope that it will be useful,
but without any warranty.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.

Everyone is granted permission to copy, modify and redistribute
GNU Emacs, but only under the conditions described in the
document "GNU Emacs copying permission notice".   An exact copy
of the document is supposed to have been given to you along with
GNU Emacs so that you can know how you may redistribute it all.
It should be in a file named COPYING.  Among other things, the
copyright notice and this notice must be preserved on all copies.  */

/* The arguments given to this program are all the C and Lisp source files
 of GNU Emacs.  .elc and .el and .c files are allowed.
 A .o file can also be specified; the .c file it was made from is used.
 This helps the makefile pass the correct list of files.

 The results, printed on standard output, are entries containing
 function names and their documentation.
 Each entry starts with a ^_ character.
 Then comes the function name, terminated with a newline.
 Then comes the documentation for that function.
 */

#include <stdio.h>

main (argc, argv)
     int argc;
     char **argv;
{
  int i;
  int err_count = 0;

  for (i = 1; i < argc; i++)
    err_count += scan_file (argv[i]);	/* err_count seems to be {mis,un}used */
  exit (err_count);			/* see below - shane */
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
  while (1)
    {
      while (c != '"')
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
	    putchar (c);
	  else if (printflag < 0)
	    *p++ = c;
	  c = getc (infile);
	}
      c = getc (infile);
      if (c != '"')
	break;
      if (printflag > 0)
	putchar (c);
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
      if (c != 'D')
	continue;
      c = getc (infile);
      if (c != 'E')
	continue;
      c = getc (infile);
      if (c != 'F')
	continue;
      c = getc (infile);
      defunflag = c == 'U';

      while (c != '(')
	c = getc (infile);

      c = getc (infile);
      if (c != '"')
	continue;
      c = read_c_string (infile, -1, 0);

      if (defunflag)
	commas = 5;
      else
	commas = 2;

      while (commas)
	{
	  if (c == ',') commas --;
	  c = getc (infile);
	}
      while (c == ' ' || c == '\n' || c == '\t')
	c = getc (infile);
      if (c == '"')
	c = read_c_string (infile, 0, 0);
      while (c != ',')
	c = getc (infile);
      c = getc (infile);
      while (c == ' ' || c == '\n' || c == '\t')
	c = getc (infile);

      if (c == '"')
	{
	  putchar (037);
	  puts (buf);
	  read_c_string (infile, 1, 0);
	}
    }
  fclose (infile);
/* return 1; /* - This says there was an error to caller - breaks make - shane */
  return 0;  /* - So I changed it to this instead. */
}

/* Read a file of Lisp code, compiled or interpreted.
 Looks for
  (defun NAME ARGS DOCSTRING ...)  or
  (autoload 'NAME FILE DOCSTRING ...)
 either one starting in column zero.
 ARGS or FILE is ignored;
 the NAME and DOCSTRING are output.
 An entry is output only if DOCSTRING has \ newline just after the opening "
 */

scan_lisp_file (filename)
     char *filename;
{
  FILE *infile;
  register int c;
  register int commas;
  register char *p;
  
  infile = fopen (filename, "r");
  if (infile == NULL)
    {
      perror (infile);
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

	  while (c != '\'')
	    c = getc (infile);
	  c = getc (infile);

	  p = buf;
	  while (c != ' ')
	    {
	      *p++ = c;
	      c = getc (infile);
	    }
	  *p = 0;

	  while (c != '"')
	    c = getc (infile);
	  c = read_c_string (infile, 0, 0);
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
	  if (c != 'u')
	    continue;
	  c = getc (infile);
	  if (c != 'n')
	    continue;

	  /* Recognize anything that starts with "defun" */
	  while (c != ' ' && c != '\n' && c != '\t')
	    c = getc (infile);

	  while (c == ' ' || c == '\n' || c == '\t')
	    c = getc (infile);

	  /* Store name of function being defined. */
	  p = buf;
	  while (c != ' ' && c != '\n' && c != '\t')
	    {
	      *p++ = c;
	      c = getc (infile);
	    }
	  *p = 0;

	  while (c == ' ' || c == '\n' || c == '\t')
	    c = getc (infile);

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
	continue;

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

      putchar (037);
      puts (buf);
      read_c_string (infile, 1, 0);
    }
  fclose (infile);
/* return 1; /* - This says there was an error to caller - breaks make - shane */
  return 0;  /* - So I changed it to this instead. */
}
