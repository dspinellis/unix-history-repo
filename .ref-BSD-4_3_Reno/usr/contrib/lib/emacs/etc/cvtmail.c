/* Copyright (C) 1985 Free Software Foundation
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

/* cvtmail:
 * Program to convert oldstyle goslings emacs mail directories into
 * gnu-rmail format.  Program expects a directory called Messages to
 * exist in your home directory, containing individual mail messages in
 * separate files in the standard gosling emacs mail reader format.
 *
 * Program takes one argument: an output file.  THis file will contain
 * all the messages in Messages directory, in berkeley mail format.
 * If no output file is mentioned, messages are put in ~/OMAIL.
 *
 * In order to get rmail to read the messages, the resulting file must
 * be mv'ed to ~/mbox, and then have rmail invoked on them.
 * 
 * Author: Larry Kolodney, 1985

 * RMS, 2 Sept 85: Removed fix maximums on file name sizes.
 */


#include <stdio.h>


main (argc, argv)
     int argc;
     char *argv[];
{
  char *hd;
  char *md;
  char *mdd;
  char *mfile;
  char *cf;
  int cflen;
  FILE *mddf;
  FILE *mfilef;
  FILE *cff;
  char pre[10], post[100];
  char name[14];
  int c;

  hd = (char *) getenv ("HOME");

  md = (char *) xmalloc (strlen (hd) + 10);
  strcpy (md, hd);
  strcat (md, "/Messages");

  mdd = (char *) xmalloc (strlen (md) + 11);
  strcpy (mdd, md);
  strcat (mdd, "/Directory");

  cflen = 100;
  cf = (char *) xmalloc (cflen);

  mddf = fopen (mdd, "r");
  if (argc > 1)
    mfilef = fopen (argv[1], "w");
  else
    {
      mfile = (char *) xmalloc (strlen (hd) + 7);
      strcpy (mfile, hd);
      strcat (mfile, "/OMAIL");
      mfilef = fopen (mfile, "w");
    }
  skip_to_lf (mddf);
  while (fscanf (mddf, "%4c%14[0123456789]", pre, name) != EOF)
    {
      if (cflen < strlen (md) + strlen (name) + 2)
	{
	  cflen = strlen (md) + strlen (name) + 2;
	  cf = (char *) xrealloc (cf, cflen);
	}
      strcpy (cf, md);
      strcat (cf,"/");
      strcat (cf, name);
      cff = fopen (cf, "r");
      while ((c = getc(cff)) != EOF)
	putc (c, mfilef);
      putc ('\n', mfilef);
      skip_to_lf (mddf);
     fclose (cff);
    }
  fclose (mddf);
  fclose (mfilef);    
  return 0;
}

skip_to_lf (stream)
     FILE *stream;
{
  register int c;
  while ((c = getc(stream)) != '\n')
    ;
}

int
xmalloc (size)
     int size;
{
  int result = malloc (size);
  if (!result)
    fatal ("virtual memory exhausted", 0);
  return result;
}

int
xrealloc (ptr, size)
     char *ptr;
     int size;
{
  int result = realloc (ptr, size);
  if (!result)
    fatal ("virtual memory exhausted");
  return result;
}

/* Print error message and exit.  */

fatal (s1, s2)
     char *s1, *s2;
{
  error (s1, s2);
  exit (1);
}

error (s1, s2)
     char *s1, *s2;
{
  printf ("cvtmail: ");
  printf (s1, s2);
  printf ("\n");
}
