/* Demangler filter for GNU C++ 
   Copyright (C) 1989 Free Software Foundation, Inc.
   written by James Clark (jjc@jclark.uucp)
   
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 1, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <stdio.h>
#include <string.h>
#include <ctype.h>

char *malloc ();
char *realloc ();

#ifndef __STDC__
#define const
#endif

#ifdef __STDC__
extern char *cplus_demangle (const char *);
#else
extern char *cplus_demangle ();
#endif

void fatal ();

static char *prog_name;

#define SYMBOL_MAX 1024

#define SYMBOL_CHAR(c) (isascii(c) && (isalnum(c) || (c) == '_' || (c) == '$'))

int
main (argc, argv)
     int argc;
     char **argv;
{
  char buf[SYMBOL_MAX+1];
  int c;

  prog_name = argv[0];

  c = getchar ();
  while (c != EOF)
    {
      int i = 0;
      while (c != EOF && !SYMBOL_CHAR (c))
	{
	  putchar (c);
	  c = getchar ();
	}
      while (i < SYMBOL_MAX && c != EOF && SYMBOL_CHAR (c))
	{
	  buf[i++] = c;
	  c = getchar ();
	}
      buf[i] = '\0';
      if (i == SYMBOL_MAX)
	{
	  fputs (buf, stdout);
	  while (c != EOF && SYMBOL_CHAR (c))
	    {
	      putchar (c);
	      c = getchar ();
	    }
	}
      else
	{
	  char *result;
	  if (i > 0 && (result = cplus_demangle (buf)) != NULL)
	    {
	      fputs (result, stdout);
	      free (result);
	    }
	  else
	    fputs (buf, stdout);
	}
    }
  return 0;
}

char *
xmalloc (n)
     int n;
{
  char *tem = malloc (n);
  if (tem == NULL)
    fatal ("out of memory");
  return tem;
}

char *
xrealloc (p, n)
     char *p;
     int n;
{
  char *tem = realloc (p, n);
  if (tem == NULL)
    fatal ("out of memory");
  return tem;
}

void
fatal (message)
     const char *message;
{
  fprintf (stderr, "%s: %s\n", prog_name, message);
  exit (1);
}
