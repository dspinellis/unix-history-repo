/* braces.c -- code for doing word expansion in curly braces. */

/* Copyright (C) 1987,1991 Free Software Foundation, Inc.

   This file is part of GNU Bash, the Bourne Again SHell.

   Bash is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 1, or (at your option)
   any later version.

   Bash is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with Bash; see the file COPYING.  If not, write to the Free
   Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. */

/* Stuff in curly braces gets expanded after variable and command
   substitution, but before filename globbing.

   (Actually, this should be true for the sake of efficiency, but it
   isn't because of quoting hacks.  Once I rebuild quoting it will be
   true. */

#if defined (SHELL)
#include "shell.h"
#endif /* SHELL */

#include "general.h"
#define brace_whitespace(c) (!(c) || (c) == ' ' || (c) == '\t' || (c) == '\n')

#if !defined (NULL)
#define NULL (char *)0x0
#endif /* NULL */

/* Basic idea:

   Segregate the text into 3 sections: preamble (stuff before an open brace),
   postamble (stuff after the matching close brace) and amble (stuff after
   preamble, and before postamble).  Expand amble, and then tack on the
   expansions to preamble.  Expand postamble, and tack on the expansions to
   the result so far.
 */

/* The character which is used to separate arguments. */
int brace_arg_separator = ',';

static int brace_gobbler ();
static char **expand_amble (), **array_concat (), **copy_array ();

/* Return an array of strings; the brace expansion of TEXT. */
char **
brace_expand (text)
     char *text;
{
  register int start;
  char *preamble, *postamble, *amble;
  char **tack, **result;
  int i, c;

  /* Find the text of the preamble. */
  i = 0;
  c = brace_gobbler (text, &i, '{');

  preamble = (char *)xmalloc (i + 1);
  strncpy (preamble, text, i);
  preamble[i] = '\0';

  result = (char **)xmalloc (2 * sizeof (char *));
  result[0] = preamble;
  result[1] = (char *)NULL;
  
  /* Special case.  If we never found an exciting character, then
     the preamble is all of the text, so just return that. */
  if (c != '{')
    return (result);

  /* Find the amble.  This is the stuff inside this set of braces. */
  start = ++i;
  c = brace_gobbler (text, &i, '}');

  /* What if there isn't a matching close brace? */
  if (!c)
    {
#if defined (SHELL)
      register int j;

      /* Well, if we found BRACE_ARG_SEPARATOR between START and I,
	 then this should be an error.  Otherwise, it isn't. */
      for (j = start; j < i; j++)
	if (text[j] == brace_arg_separator)
	  {
	    free_array (result);
	    report_error ("Missing `}'");
	    throw_to_top_level ();
	  }
#endif
      free (preamble);		/* Same as result[0]; see initialization. */
      result[0] = savestring (text);
      return (result);
    }

  amble = (char *)xmalloc (1 + (i - start));
  strncpy (amble, &text[start], (i - start));
  amble[i - start] = '\0';

#if defined (SHELL)
  /* If the amble does not contain BRACE_ARG_SEPARATOR, then just return
     without doing any expansion.  */
  if (index (amble, brace_arg_separator) == NULL)
    {
      free (amble);
      free (preamble);
      result[0] = savestring (text);
      return (result);
    }
#endif /* SHELL */

  postamble = &text[i + 1];

  tack = expand_amble (amble);
  result = array_concat (result, tack);
  free (amble);
  free_array (tack);

  tack = brace_expand (postamble);
  result = array_concat (result, tack);
  free_array (tack);

  return (result);
}

/* Expand the text found inside of braces.  We simply try to split the
   text at BRACE_ARG_SEPARATORs into separate strings.  We then brace
   expand each slot which needs it, until there are no more slots which
   need it. */
static char **
expand_amble (text)
     char *text;
{
  char **result, **partial;
  char *tem;
  int start, i, c;

  result = (char **)NULL;

  for (start = 0, i = 0, c = 1; c; start = ++i)
    {
      c = brace_gobbler (text, &i, brace_arg_separator);
      tem = (char *)xmalloc (1 + (i - start));
      strncpy (tem, &text[start], (i - start));
      tem[i- start] = '\0';

      partial = brace_expand (tem);

      if (!result)
	result = partial;
      else
	{
	  register int lr = array_len (result);
	  register int lp = array_len (partial);
	  register int j;

	  result = (char **)xrealloc (result, (1 + lp + lr) * sizeof (char *));

	  for (j = 0; j < lp; j++)
	    result[lr + j] = partial[j];

	  result[lr + j] = (char *)NULL;
	  free (partial);
	}
      free (tem);
    }
  return (result);
}

/* Start at INDEX, and skip characters in TEXT. Set INDEX to the
   index of the character matching SATISFY.  This understands about
   quoting.  Return the character that caused us to stop searching;
   this is either the same as SATISFY, or 0. */
static int
brace_gobbler (text, index, satisfy)
     char *text;
     int *index;
     int satisfy;
{
  register int i, c, quoted, level;

  level = quoted = 0;

  for (i = *index; c = text[i]; i++)
    {
      if (quoted)
	{
	  if ((quoted == '\\') || (c == quoted))
	    quoted = 0;
	  continue;
	}

      if (c == '"' || c == '\'' || c == '\\')
	{
	  quoted = c;
	  continue;
	}
      
      if (c == satisfy && !level && !quoted)
	{
	  /* We ignore an open brace surrounded by whitespace, and also
	     an open brace followed immediately by a close brace, that
	     was preceded with whitespace.  */
	  if (c == '{' &&
	      ((!i || brace_whitespace (text[i - 1])) &&
	       (brace_whitespace (text[i + 1]) || text[i + 1] == '}')))
	    continue;
#ifdef SHELL
	  /* If this is being compiled as part of bash, ignore the `{'
	     in a `${}' construct */
	  if ((c != '{') || !i || (text[i - 1] != '$'))
#endif
	    break;
	}

      if (c == '{')
	level++;
      else if (c == '}' && level)
	level--;
    }

  *index = i;
  return (c);
}

/* Return a new array of strings which is the result of appending each
   string in ARR2 to each string in ARR1.  The resultant array is
   len (arr1) * len (arr2) long.  For convenience, ARR1 (and its contents)
   are free ()'ed.  ARR1 can be NULL, in that case, a new version of ARR2
   is returned. */
static char **
array_concat (arr1, arr2)
     char **arr1, **arr2;
{
  register int i, j, len, len1, len2;
  register char **result;

  if (!arr1)
    return (copy_array (arr2));

  if (!arr2)
    return (arr1);

  len1 = array_len (arr1);
  len2 = array_len (arr2);

  result = (char **)xmalloc ((1 + (len1 * len2)) * sizeof (char *));

  len = 0;
  for (i = 0; i < len1; i++)
    {
      int strlen_1 = strlen (arr1[i]);

      for (j = 0; j < len2; j++)
	{
	  result[len] =
	    (char *)xmalloc (1 + strlen_1 + strlen (arr2[j]));
	  strcpy (result[len], arr1[i]);
	  strcpy (result[len] + strlen_1, arr2[j]);
	  len++;
	}
      free (arr1[i]);
    }
  free (arr1);

  result[len] = (char *)NULL;
  return (result);
}

static char **
copy_array (array)
     char **array;
{
  register int i;
  char **new;

  if (!array)
    return (array);

  new = (char **)xmalloc ((1 + array_len (array)) * sizeof (char *));

  for (i = 0; array[i]; i++)
    new[i] = savestring (array[i]);

  new[i] = (char *)NULL;

  return (new);
}
  
#if defined (TEST)
#include <stdio.h>

fatal_error (format, arg1, arg2)
     char *format, *arg1, *arg2;
{
  report_error (format, arg1, arg2);
  exit (1);
}

report_error (format, arg1, arg2)
     char *format, *arg1, *arg2;
{
  fprintf (stderr, format, arg1, arg2);
  fprintf (stderr, "\n");
}

main ()
{
  char example[256];

  for (;;)
    {
      char **result;
      int i;

      fprintf (stderr, "brace_expand> ");

      if ((!fgets (example, 256, stdin)) ||
	  (strncmp (example, "quit", 4) == 0))
	break;

      if (strlen (example))
	example[strlen (example) - 1] = '\0';

      result = brace_expand (example);

      for (i = 0; result[i]; i++)
	printf ("%s\n", result[i]);

      free_array (result);
    }
}

/*
 * Local variables:
 * compile-command: "gcc -g -Bstatic -DTEST -o brace_expand braces.c general.o"
 * end:
 */

#endif	/* TEST */
