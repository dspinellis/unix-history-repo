/* substitutions.c -- The part of the shell that does parameter,
   command, and globbing substitutions. */

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
#include <sys/types.h>
#include <pwd.h>
#include <signal.h>
#include <string.h>
#include "shell.h"
#include "flags.h"
#include "alias.h"
#include "jobs.h"
#include "filecntl.h"
#include <readline/history.h>
#include <glob/fnmatch.h>

/* The size that strings change by. */
#define DEFAULT_ARRAY_SIZE 512

/* How to quote and dequote the character C. */
#define QUOTE_CHAR(c)   ((unsigned char)(c) | 0x80)
#define DEQUOTE_CHAR(c) ((unsigned char)(c) & 0x7f)
#define QUOTED_CHAR(c)  ((unsigned char)(c) > 0x7f)

/* Process ID of the last command executed within command substitution. */
pid_t last_command_subst_pid = NO_PID;

/* Some forward declarations. */

extern WORD_LIST *expand_string (), *expand_word (), *list_string ();
extern char *string_list ();
extern WORD_DESC *make_word ();
extern WORD_DESC *copy_word ();
extern WORD_LIST *copy_word_list();

static WORD_LIST *expand_string_internal (), *expand_words_internal ();
static char *quote_string (), *dequote_string ();
static int unquoted_substring ();
static void quote_list ();

/* **************************************************************** */
/*								    */
/*			Utility Functions			    */
/*								    */
/* **************************************************************** */

/* Cons a new string from STRING starting at START and ending at END,
   not including END. */
char *
substring (string, start, end)
     char *string;
     int start, end;
{
  register int len = end - start;
  register char *result = (char *)xmalloc (len + 1);

  strncpy (result, string + start, len);
  result[len] = '\0';
  return (result);
}

/* Just like string_extract, but doesn't hack backslashes or any of
   that other stuff. */
char *
string_extract_verbatim (string, sindex, charlist)
     char *string, *charlist;
     int *sindex;
{
  register int i = *sindex;
  int c;
  char *temp;

  while ((c = string[i]) && (!member (c, charlist))) i++;
  temp = (char *)xmalloc (1 + (i - *sindex));
  strncpy (temp, string + (*sindex), i - (*sindex));
  temp[i - (*sindex)] = '\0';
  *sindex = i;
  return (temp);
}

/* Extract a substring from STRING, starting at SINDEX and ending with
   one of the characters in CHARLIST.  Don't make the ending character
   part of the string.  Leave SINDEX pointing at the ending character.
   Understand about backslashes in the string. */
char *
string_extract (string, sindex, charlist)
     char *string, *charlist;
     int *sindex;
{
  register int c, i = *sindex;
  char *temp;

  while (c = string[i]) {
    if (c == '\\')
      if (string[i + 1])
	i++;
      else
	break;
    else
      if (member (c, charlist))
	break;
    i++;
  }
  temp = (char *)xmalloc (1 + (i - *sindex));
  strncpy (temp, string + (*sindex), i - (*sindex));
  temp[i - (*sindex)] = '\0';
  *sindex = i;
  return (temp);
}

/* Remove backslashes which are quoting backquotes from STRING.  Modifies
   STRING, and returns a pointer to it. */
char *
de_backslash (string)
     char *string;
{
  register int i, l = strlen (string);

  for (i = 0; i < l; i++)
    if (string[i] == '\\' && (string[i + 1] == '`' || string[i + 1] == '\\' ||
			      string[i + 1] == '$'))
      strcpy (&string[i], &string[i + 1]);
  return (string);
}

/* Replace instances of \! in a string with !. */
void
unquote_bang (string)
     char *string;
{
  register int i, j;
  register char *temp;

  temp = (char *)alloca (1 + strlen (string));

  for (i = 0, j = 0; (temp[j] = string[i]); i++, j++)
    {
      if (string[i] == '\\' && string[i + 1] == '!')
	{
	  temp[j] = '!';
	  i++;
	}
    }
  strcpy (string, temp);
}

/* Extract the $( construct in STRING, and return a new string.
   Start extracting at (SINDEX) as if we had just seen "$(".
   Make (SINDEX) get the position just after the matching ")". */
char *
extract_command_subst (string, sindex)
     char *string;
     int *sindex;
{
  char *extract_delimited_string ();

  return (extract_delimited_string (string, sindex, "$(", "(", ")"));
}

/* Extract the $[ construct in STRING, and return a new string.
   Start extracting at (SINDEX) as if we had just seen "$[".
   Make (SINDEX) get the position just after the matching "]".

   Strictly speaking, according to the letter of POSIX.2, arithmetic
   substitutions cannot be nested.  This code allows nesting, however,
   and it is fully implemented. */
char *
extract_arithmetic_subst (string, sindex)
     char *string;
     int *sindex;
{
  char *extract_delimited_string ();

  return (extract_delimited_string (string, sindex, "$[", "[", "]"));
}

/* Extract and create a new string from the contents of STRING, a
   character string delimited with OPENER and CLOSER.  SINDEX is
   the address of an int describing the current offset in STRING;
   it should point to just after the first OPENER found.  On exit,
   SINDEX gets the position just after the matching CLOSER.  If
   OPENER is more than a single character, ALT_OPENER, if non-null,
   contains a character string that can also match CLOSER and thus
   needs to be skipped. */
char *
extract_delimited_string (string, sindex, opener, alt_opener, closer)
     char *string;
     int *sindex;
     char *opener, *alt_opener, *closer;
{
  register int i, c, l;
  int pass_character, nesting_level;
  int delimiter, delimited_nesting_level;
  int len_closer, len_opener, len_alt_opener;
  char *result;

  len_opener = strlen (opener);
  len_alt_opener = alt_opener ? strlen (alt_opener) : 0;
  len_closer = strlen (closer);

  pass_character = delimiter = delimited_nesting_level = 0;

  nesting_level = 1;

  for (i = *sindex; c = string[i]; i++)
    {
      if (pass_character)
	{
	  pass_character = 0;
	  continue;
	}

      if (c == '\\')
	{
	  if ((delimiter == '"') &&
	      (member (string[i + 1], slashify_in_quotes)))
	    {
	      pass_character++;
	      continue;
	    }
	}

      if (!delimiter || delimiter == '"')
	{
	  if (strncmp (string + i, opener, len_opener) == 0)
	    {
	      if (!delimiter)
		nesting_level++;
	      else
		delimited_nesting_level++;

	      i += len_opener - 1;
	      continue;
	    }

	  if (len_alt_opener &&
	      strncmp (string + i, alt_opener, len_alt_opener) == 0)
	    {
	      if (!delimiter)
		nesting_level++;
	      else
		delimited_nesting_level++;

	      i += len_alt_opener - 1;
	      continue;
	    }

	  if (strncmp (string + i, closer, len_closer) == 0)
	    {
	      i += len_closer - 1;

	      if (delimiter && delimited_nesting_level)
		delimited_nesting_level--;

	      if (!delimiter)
		{
		  nesting_level--;
		  if (nesting_level == 0)
		    break;
		}
	    }
	}

      if (delimiter)
	{
	  if (c == delimiter || delimiter == '\\')
	    delimiter = 0;
	  continue;
	}
      else
	{
	  if (c == '"' || c == '\'' || c == '\\')
	    delimiter = c;
	}
    }

  l = i - *sindex;
  result = (char *)xmalloc (1 + l);
  strncpy (result, &string[*sindex], l);
  result[l] = '\0';
  *sindex = i;

  if (!c && (delimiter || nesting_level))
    {
      report_error ("bad substitution: %s%s", opener, result);
      free (result);
      longjmp (top_level, DISCARD);
    }
  return (result);
}

/* An artifact for extracting the contents of a quoted string.  Since the
   string is about to be evaluated, we pass everything through, and only
   strip backslash before backslash or quote. */
/* This is a mini state machine. */
char *
string_extract_double_quoted (string, sindex)
     char *string;
     int *sindex;
{
  register int c, j, i;
  char *temp;			/* The new string we return. */
  int pass_next, backquote;	/* State variables for the machine. */

  pass_next = backquote = 0;
  temp = (char *)xmalloc (1 + (strlen (string) - *sindex));

  for (j = 0, i = *sindex; c = string[i]; i++)
    {
      /* Process a character that was quoted by a backslash. */
      if (pass_next)
	{
	  /* Posix.2 sez:

	     ``The backslash shall retain its special meaning as an escape
	     character only when followed by one of the characters:
	     	$	`	"	\	<newline>''.

	     We handle the double quotes here.  expand_word_internal handles
	     the rest. */
	  if (c != '"')
	    temp[j++] = '\\';
          temp[j++] = c;
          pass_next = 0;
          continue;
	}

      /* A backslash protects the next character.  The code just above
         handles preserving the backslash in front of any character but
         a double quote. */
      if (c == '\\')
	{
	  pass_next++;
	  continue;
	}

      /* Inside backquotes, ``the portion of the quoted string from the
	 initial backquote and the characters up to the next backquote
	 that is not preceded by a backslash, having escape characters
	 removed, defines that command''. */
      if (backquote)
        {
          if (c == '`')
            backquote = 0;
	  temp[j++] = c;
	  continue;
        }

      if (c == '`')
        {
          temp[j++] = c;
          backquote++;
          continue;
        }

      /* Pass everything between `$(' and the matching `)' through verbatim. */
      if (c == '$' && string[i + 1] == '(')
	{
	  register int t;
	  int si;
	  char *ret;

	  si = i + 2;
	  ret = extract_delimited_string (string, &si, "$(", "(", ")");

	  temp[j++] = '$';
	  temp[j++] = '(';

	  for (t = 0; ret[t]; t++)
	    temp[j++] = ret[t];

	  i = si;
	  temp[j++] = string[i];
	  free (ret);
	  continue;
	}

      /* An unescaped double quote serves to terminate the string. */
      if (c == '"')
        break;

      /* Add the character to the quoted string we're accumulating. */
      temp[j++] = c;
    }
  temp[j] = '\0';
  *sindex = i;
  return (temp);
}

/* Extract the name of the variable to bind to from the assignment string. */
char *
assignment_name (string)
     char *string;
{
  int offset = assignment (string);
  char *temp;
  if (!offset) return (char *)NULL;
  temp = (char *)xmalloc (offset + 1);
  strncpy (temp, string, offset);
  temp[offset] = '\0';
  return (temp);
}

/* Return a single string of all the words in LIST.  SEP is the separator
   to put between individual elements of LIST in the output string. */

static char *
string_list_internal (list, sep)
     WORD_LIST *list;
     char *sep;
{
  char *result = (char *)NULL;
  int sep_len;

  sep_len = strlen (sep);

  while (list)
    {
      /* Can't simply let xrealloc malloc the bytes for us the first time
         because of the strcat (result, ...) -- we need to make sure result
         is initialized to null after being allocated initially. */
      if (!result)
	result = savestring ("");

      result = (char *)xrealloc
	(result, 2 + sep_len + strlen (result) + strlen (list->word->word));
      strcat (result, list->word->word);
      if (list->next)
	strcat (result, sep);
      list = list->next;
    }
  return (result);
}

/* Return a single string of all the words present in LIST, separating
   each word with a space. */
char *
string_list (list)
     WORD_LIST *list;
{
  return (string_list_internal (list, " "));
}

/* Return a single string of all the words present in LIST, obeying the
   quoting rules for "$*", to wit: (P1003.2, draft 11, 3.5.2, "If the
   expansion [of $*] appears within a double quoted string, it expands
   to a single field with the value of each parameter separated by the
   first character of the IFS variable, or by a <space> if IFS is unset
   [or null]." */

char *
string_list_dollar_star (list)
     WORD_LIST *list;
{
  char *ifs = get_string_value ("IFS");
  char sep[2];

  if (!ifs)
    sep[0] = ' ';
  else if (!*ifs)
    sep[0] = '\0';
  else
    sep[0] = *ifs;

  sep[1] = '\0';

  return (string_list_internal (list, sep));
}

/* Return the list of words present in STRING.  Separate the string into
   words at any of the characters found in SEPARATORS.  If QUOTED is
   non-zero then word in the list will have its quoted flag set, otherwise
   the quoted flag is left as make_word () deemed fit.

   This obeys the P1003.2 draft 11 word splitting semantics.  If `separators'
   is exactly <space><tab><newline>, then the splitting algorithm is that of
   the Bourne shell, which treats any sequence of characters from `separators'
   as a delimiter.  If IFS is unset, which results in `separators' being set
   to "", no splitting occurs.  If separators has some other value, the
   following rules are applied (`IFS white space' means zero or more
   occurrences of <space>, <tab>, or <newline>, as long as those characters
   are in `separators'):

	1) IFS white space is ignored at the start and the end of the
	   string.
	2) Each occurrence of a character in `separators' that is not
	   IFS white space, along with any adjacent occurrences of
	   IFS white space delimits a field.
	3) Any nonzero-length sequence of IFS white space delimits a field.
   */

/* BEWARE!  list_string strips null arguments.  Don't call it twice and
   expect to have "" preserved! */

/* Is C a quoted NULL character? */
#define QUOTED_NULL(c) ((unsigned char)(c) == (unsigned char)0x80)

/* Perform quoted null character removal on STRING. */
void
remove_quoted_nulls (string)
     char *string;
{
  register char *s;

  for (s = string; s && *s; s++)
    {
      if (QUOTED_NULL (*s))
	{
	  strcpy (s, s + 1);
	  s--;
	}
    }
}

/* Perform quoted null character removal on each element of LIST.
   This modifies LIST. */
word_list_remove_quoted_nulls (list)
     WORD_LIST *list;
{
  register WORD_LIST *t;

  t = list;

  while (t)
    {
      remove_quoted_nulls (t->word->word);
      t = t->next;
    }
}

/* This performs word splitting and quoted null character removal on
   STRING. */

#define issep(c)	(member ((c), separators))
#define spctabnl(c)	((c) == ' '|| (c) == '\t' || (c) == '\n')

WORD_LIST *
list_string (string, separators, quoted)
     register char *string, *separators;
     int quoted;
{
  WORD_LIST *result = (WORD_LIST *)NULL;
  char *current_word = (char *)NULL, *s;
  int sindex = 0;
  int sh_style_split;

  if (!string || !*string)
    return ((WORD_LIST *)NULL);

  sh_style_split = separators && *separators && (!strcmp (separators, " \t\n"));

  /* Remove sequences of whitespace at the beginning and end of STRING, as
     long as those characters appear in IFS. */
  for (s = string; *s && spctabnl (*s) && issep (*s); s++);
  if (!*s)
    return ((WORD_LIST *)NULL);
  string = s;
  s += strlen (s) - 1;
  for ( ; s > string && *s && spctabnl (*s) & issep (*s); s--);
  if (!*s)
    return ((WORD_LIST *)NULL);
  *++s = '\0';

  /* OK, now STRING points to a word that does not begin with white space.
     The splitting algorithm is:
     	extract a word, stopping at a separator
     	skip sequences of spc, tab, or nl as long as they are separators
     This obeys the field splitting rules in Posix.2 draft 11.x. */

  while (string[sindex])
    {
      current_word = string_extract_verbatim (string, &sindex, separators);
      if (!current_word)
        break;

      /* If we have a quoted empty string, add a quoted null argument.  We
	 want to preserve the quoted null character iff this is a quoted
	 empty string; otherwise the quoted null characters are removed
	 below. */
      if (QUOTED_NULL (current_word[0]) && current_word[1] == '\0')
	{
	  WORD_DESC *t = make_word (" ");
	  t->quoted++;
	  t->word[0] = (unsigned char)QUOTE_CHAR ('\0');
	  result = make_word_list (t, result);
	}

      /* If we have something, then add it regardless. */
      else if (strlen (current_word))
	{
	  register char *temp_string;

	  /* Perform quoted null character removal on the current word. */
 	  for (temp_string = current_word; *temp_string; temp_string++)
	    if (QUOTED_NULL (*temp_string))
	      {
		strcpy (temp_string, temp_string + 1);
		temp_string--;
	      }

	  result = make_word_list (make_word (current_word), result);
	  if (quoted)
	    result->word->quoted++;
	}

      /* If we're not doing sequences of separators in the traditional
	 Bourne shell style, then add a quoted null argument. */

      else if (!sh_style_split && !spctabnl (string[sindex]))
	{
	  result = make_word_list (make_word (""), result);
	  result->word->quoted++;
	}

      free (current_word);

      /* Move past the current separator character. */
      if (string[sindex])
        sindex++;

      /* Now skip sequences of space, tab, or newline characters if they are
         in the list of separators. */
      while (string[sindex] &&
	     spctabnl (string[sindex]) &&
	     issep (string[sindex]))
	sindex++;

    }
  return (WORD_LIST *)reverse_list (result);
}

/* Given STRING, an assignment string, get the value of the right side
   of the `=', and bind it to the left side.  If EXPAND is true, then
   perform parameter expansion, command substitution, and arithmetic
   expansion on the right-hand side.  Perform tilde expansion in any
   case.  Do not perform word splitting on the result of expansion. */
do_assignment_internal (string, expand)
     char *string;
     int expand;
{
  int offset = assignment (string);
  char *name = savestring (string);
  char *value = (char *)NULL;
  SHELL_VAR *entry = (SHELL_VAR *)NULL;

  if (name[offset] == '=')
    {
      char *tilde_expand (), *string_list ();
      WORD_LIST *list, *expand_string_unsplit ();
      char *temp;

      name[offset] = 0;
      temp = name + offset + 1;

      if (expand)
	{
	  if (index (temp, '~'))
	    temp = tilde_expand (temp);
	  else
	    temp = savestring (temp);

	  list = expand_string_unsplit (temp, 0);

	  if (list)
	    {
	      value = string_list (list);
	      dispose_words (list);
	    }
	  free (temp);
	}
      else
	value = savestring (temp);
    }

  if (!value)
    value = savestring ("");

  entry = bind_variable (name, value);

  if (echo_command_at_execute)
    {
      extern char *indirection_level_string ();
      fprintf (stderr, "%s%s=%s\n", indirection_level_string (), name, value);
    }

  /* Yes, here is where the special shell variables get tested for.
     Don't ask me, I just work here.  This is really stupid.  I would
     swear, but I've decided that that is an impolite thing to do in
     source that is to be distributed around the net, even if this code
     is totally brain-damaged. */

  /* if (strcmp (name, "PATH") == 0) Yeeecchhh!!!*/
  stupidly_hack_special_variables (name);

  if (entry)
    entry->attributes &= ~att_invisible;
  if (value)
    free (value);
  free (name);
}

/* Perform the assignment statement in STRING, and expand the
   right side by doing command and parameter expansion. */
do_assignment (string)
     char *string;
{
  do_assignment_internal (string, 1);
}

/* Given STRING, an assignment string, get the value of the right side
   of the `=', and bind it to the left side.  Do not do command and
   parameter substitution on the right hand side. */
do_assignment_no_expand (string)
     char *string;
{
  do_assignment_internal (string, 0);
}

/* Most of the substitutions must be done in parallel.  In order
   to avoid using tons of unclear goto's, I have some functions
   for manipulating malloc'ed strings.  They all take INDEX, a
   pointer to an integer which is the offset into the string
   where manipulation is taking place.  They also take SIZE, a
   pointer to an integer which is the current length of the
   character array for this string. */

/* Append SOURCE to TARGET at INDEX.  SIZE is the current amount
   of space allocated to TARGET.  SOURCE can be NULL, in which
   case nothing happens.  Gets rid of SOURCE by free ()ing it.
   Returns TARGET in case the location has changed. */
char *
sub_append_string (source, target, index, size)
     char *source, *target;
     int *index, *size;
{
  if (source)
    {
      while ((int)strlen (source) >= (int)(*size - *index))
	target = (char *)xrealloc (target, *size += DEFAULT_ARRAY_SIZE);

      strcat (target, source);
      *index += strlen (source);

      free (source);
    }
  return (target);
}

/* Append the textual representation of NUMBER to TARGET.
   INDEX and SIZE are as in SUB_APPEND_STRING. */
char *
sub_append_number (number, target, index, size)
     int number, *index, *size;
     char *target;
{
  char *temp = (char *)xmalloc (10);
  sprintf (temp, "%d", number);
  return (sub_append_string (temp, target, index, size));
}

/* Return the word list that corresponds to `$*'. */
WORD_LIST *
list_rest_of_args ()
{
  register WORD_LIST *list = (WORD_LIST *)NULL;
  register WORD_LIST *args = rest_of_args;
  int i;

  for (i = 1; i < 10; i++)
    if (dollar_vars[i])
      list = make_word_list (make_word (dollar_vars[i]), list);
  while (args)
    {
      list = make_word_list (make_word (args->word->word), list);
      args = args->next;
    }
  return ((WORD_LIST *)reverse_list (list));
}

/* Make a single large string out of the dollar digit variables,
   and the rest_of_args.  If DOLLAR_STAR is 1, then obey the special
   case of "$*" with respect to IFS. */
char *
string_rest_of_args (dollar_star)
     int dollar_star;
{
  register WORD_LIST *list = list_rest_of_args ();
  char *string;

  if (!dollar_star)
    string = string_list (list);
  else
    string = string_list_dollar_star (list);

  dispose_words (list);
  return (string);
}

/***************************************************
 *						   *
 *	   Functions to Expand a String		   *
 *						   *
 ***************************************************/

/* Perform parameter expansion, command substitution, and arithmetic
   expansion on STRING, as if it were a word.  Leave the result quoted. */
static WORD_LIST *
expand_string_internal (string, quoted)
     char *string;
     int quoted;
{
  WORD_DESC *make_word (), *temp = make_word (string);
  WORD_LIST *tresult, *expand_word_internal ();

  tresult  = expand_word_internal (temp, quoted, (int *)NULL, (int *)NULL);
  dispose_word (temp);
  return (tresult);
}

/* Expand STRING by performing parameter expansion, command substitution,
   and arithmetic expansion.  Dequote the resulting WORD_LIST before
   returning it, but do not perform word splitting.  The call to
   remove_quoted_nulls () is in here because word splitting normally
   takes care of quote removal. */
WORD_LIST *
expand_string_unsplit (string, quoted)
     char *string;
     int quoted;
{
  WORD_LIST *value = expand_string_internal (string, quoted);

  if (value && value->word)
    remove_quoted_nulls (value->word->word);

  if (value)
    dequote_list (value);
  return (value);
}

/* Expand STRING just as if you were expanding a word.  This also returns
   a list of words.  Note that filename globbing is *NOT* done for word
   or string expansion, just when the shell is expanding a command.  This
   does parameter expansion, command substitution, arithmetic expansion,
   and word splitting.  Dequote the resultant WORD_LIST before returning. */
WORD_LIST *
expand_string (string, quoted)
     char *string;
     int quoted;
{
  WORD_LIST *value = expand_string_internal (string, quoted);
  WORD_LIST *result, *word_list_split ();

  result = word_list_split (value);
  dispose_words (value);
  if (result)
    dequote_list (result);
  return (result);
}

/* Expand STRING just as if you were expanding a word, but do not dequote
   the resultant WORD_LIST.  This is called only from within this file,
   and is used to correctly preserve quoted characters when expanding
   things like ${1+"$@"}.  This does parameter expansion, command
   subsitution, arithmetic expansion, and word splitting. */
static WORD_LIST *
expand_string_leave_quoted (string, quoted)
     char *string;
     int quoted;
{
  WORD_LIST *tlist  = expand_string_internal (string, quoted);
  WORD_LIST *tresult, *word_list_split ();

  tresult = word_list_split (tlist);
  dispose_words (tlist);
  return (tresult);
}

/***************************************************
 *						   *
 *	Functions to handle quoting chars	   *
 *						   *
 ***************************************************/

/* I'm going to have to rewrite expansion because filename globbing is
   beginning to make the entire arrangement ugly.  I'll do this soon. */
dequote_list (list)
     register WORD_LIST *list;
{
  register char *s;

  while (list)
    {
      s = dequote_string (list->word->word);
      free (list->word->word);
      list->word->word = s;
      list = list->next;
    }
}

/* Quote the string S.  Return a new string. */
static char *
quote_string (s)
     char *s;
{
  unsigned char *result;

  /* If S is an empty string then simply create a string consisting of a
     quoted null. */
  if (s[0] == '\0')
    {
      result = (unsigned char *)xmalloc (2);
      result[0] = (unsigned char)QUOTE_CHAR ('\0');
      result[1] = '\0';
    }
  else
    {
      register unsigned char *t;
      result = (unsigned char *)savestring (s);
      for (t = result; t && *t ; t++)
	*t |= 0x80;
    }
  return ((char *)result);
}

/* De-quoted quoted characters in string s. */
static char *
dequote_string (s)
     char *s;
{
  register unsigned char *t;
  unsigned char *result;

  result = (unsigned char *)savestring (s);
  for (t = result; t && *t ; t++)
    *t = DEQUOTE_CHAR (*t);

  return ((char *)result);
}

/* Quote the entire WORD_LIST list. */
static void
quote_list (list)
     WORD_LIST *list;
{
  register WORD_LIST *w;

  for (w = list; w; w = w->next)
    {
      char *t = w->word->word;
      w->word->word = quote_string (t);
      free (t);
      w->word->quoted = 1;
    }
}

/* **************************************************************** */
/*								    */
/*		    Functions for Removing Patterns		    */
/*								    */
/* **************************************************************** */

/* Remove the portion of PARAM matched by PATTERN according to OP, where OP
   can have one of 4 values:
	RP_LONG_LEFT	remove longest matching portion at start of PARAM
	RP_SHORT_LEFT	remove shortest matching portion at start of PARAM
	RP_LONG_RIGHT	remove longest matching portion at end of PARAM
	RP_SHORT_RIGHT	remove shortest matching portion at end of PARAM
*/

#define RP_LONG_LEFT	1
#define RP_SHORT_LEFT	2
#define RP_LONG_RIGHT	3
#define RP_SHORT_RIGHT	4

static char *
remove_pattern (param, pattern, op)
     char *param, *pattern;
     int op;
{
  register int len = param ? strlen (param) : 0;
  register char *end = param + len;
  register char *p, *ret, c;

  if (pattern == NULL || *pattern == '\0')	/* minor optimization */
    return (savestring (param));

  if (param == NULL || *param == '\0')
    return (param);

  switch (op)
    {
      case RP_LONG_LEFT:	/* remove longest match at start */
	for (p = end; p >= param; p--)
	  {
	    c = *p; *p = '\0';
	    if (fnmatch (pattern, param, 0) != FNM_NOMATCH)
	      {
		*p = c;
		return (savestring (p));
	      }
	    *p = c;
	  }
	break;

      case RP_SHORT_LEFT:	/* remove shortest match at start */
	for (p = param; p <= end; p++)
	  {
	    c = *p; *p = '\0';
	    if (fnmatch (pattern, param, 0) != FNM_NOMATCH)
	      {
		*p = c;
		return (savestring (p));
	      }
	    *p = c;
	  }
	break;

      case RP_LONG_RIGHT:	/* remove longest match at end */
	for (p = param; p <= end; p++)
	  {
	    if (fnmatch (pattern, p, 0) != FNM_NOMATCH)
	      {
		c = *p;
		*p = '\0';
		ret = savestring (param);
		*p = c;
		return (ret);
	      }
	  }
	break;

      case RP_SHORT_RIGHT:	/* remove shortest match at end */
	for (p = end; p >= param; p--)
	  {
	    if (fnmatch (pattern, p, 0) != FNM_NOMATCH)
	      {
		c = *p;
		*p = '\0';
		ret = savestring (param);
		*p = c;
		return (ret);
	      }
	  }
	break;
    }
  return (savestring (param));	/* no match, return original string */
}

/*******************************************
 *					   *
 *	Functions to expand WORD_DESCs	   *
 *					   *
 *******************************************/

/* Expand WORD, performing word splitting on the result.  This does
   parameter expansion, command substitution, arithmetic expansion,
   word splitting, and quote removal. */

WORD_LIST *
expand_word (word, quoted)
     WORD_DESC *word;
     int quoted;
{
  WORD_LIST *word_list_split (), *expand_word_internal ();
  WORD_LIST *result, *tresult;

  tresult = expand_word_internal (word, quoted, (int *)NULL, (int *)NULL);
  result = word_list_split (tresult);
  dispose_words (tresult);
  if (result)
    dequote_list (result);
  return (result);
}

/* Expand WORD, but do not perform word splitting on the result.  This
   does parameter expansion, command substitution, arithmetic expansion,
   and quote removal. */
WORD_LIST *
expand_word_no_split (word, quoted)
     WORD_DESC *word;
     int quoted;
{
  WORD_LIST *expand_word_internal ();
  WORD_LIST *result;

  result = expand_word_internal (word, quoted, (int *)NULL, (int *)NULL);
  if (result)
    dequote_list (result);
  return (result);
}

WORD_LIST *
expand_word_leave_quoted (word, quoted)
     WORD_DESC *word;
     int quoted;
{
  WORD_LIST *expand_word_internal (), *result;

  result = expand_word_internal (word, quoted, (int *)NULL, (int *)NULL);
  return (result);
}

/* Return the value of a positional parameter.  This handles values > 10. */
char *
get_dollar_var_value (ind)
     int ind;
{
  char *temp;

  if (ind < 10)
    {
      if (dollar_vars[ind])
	temp = savestring (dollar_vars[ind]);
      else
	temp = (char *)NULL;
    }
  else	/* We want something like ${11} */
    {
      WORD_LIST *p = rest_of_args;

      ind -= 10;
      while (p && ind--)
	p = p->next;
      if (p)
	temp = savestring (p->word->word);
      else
	temp = (char *)NULL;
    }
  return (temp);
}

/* Perform command substitution on STRING.  This returns a string,
   possibly quoted. */
static char *
command_substitute (string, quoted)
     char *string;
     int quoted;
{
  pid_t pid, old_pid;
  int fildes[2];
  char *istring = (char *)NULL;
  int istring_index, istring_size, c = 1;
  extern int interactive, last_command_exit_value;

  istring_index = istring_size = 0;

  /* Don't fork () if there is no need to.  In the case of no command to
     run, just return NULL. */
  if (!string || !*string)
    return ((char *)NULL);

  /* Pipe the output of executing STRING into the current shell. */
  if (pipe (fildes) < 0)
    {
      report_error ("Can't make pipes for command substitution!");
      goto error_exit;
    }
	  
  old_pid = last_made_pid;
#if defined (JOB_CONTROL)
  {
    pid_t old_pipeline_pgrp = pipeline_pgrp;    

    pipeline_pgrp = shell_pgrp;
    pid = make_child (savestring ("command substitution"), 0);

    stop_making_children ();
    pipeline_pgrp = old_pipeline_pgrp;
  }
#else   /* JOB_CONTROL */
  pid = make_child (savestring ("command substitution"), 0);
#endif  /* JOB_CONTROL */

  if (pid < 0)
    {
      report_error ("Can't make a child for command substitution!");
    error_exit:
      if (istring)
	free (istring);
      return ((char *)NULL);
    }

  if (pid == 0)
    {
#if defined (JOB_CONTROL)
      set_job_control (0);
#endif
      if (dup2 (fildes[1], 1) < 0)
	{
	  extern int errno;
	  report_error ("command_substitute: cannot duplicate pipe as fd 1: %s\n",
			strerror (errno));
	  exit (EXECUTION_FAILURE);
	}
      close (fildes[1]);
      /* If standard output is closed in the parent shell
	 (such as after `exec >&-'), file descriptor 1 will be
	 the lowest available file descriptor, and end up in
	 fildes[0].  This can happen for stdin and stderr as well,
	 but stdout is more important -- it will cause no output
	 to be generated from this command. */
      if (fildes[0] > 2)
	close (fildes[0]);
      interactive = 0;

      exit (parse_and_execute (string, "command substitution"));
    }
  else
    {
      FILE *istream;

      istream = fdopen (fildes[0], "r");

#if defined (JOB_CONTROL) && defined (PGRP_PIPE)
      close_pgrp_pipe ();
#endif /* JOB_CONTROL && PGRP_PIPE */

      close (fildes[1]);

      if (!istream)
	{
	  report_error ("Can't reopen pipe to command substitution");
	  goto error_exit;
	}

      /* Read the output of the command through the pipe. */
      while (1)
	{
#if defined (USG) || (defined (_POSIX_VERSION) && defined (Ultrix))
	  c = sysv_getc (istream);
#else
	  c = getc (istream);
#endif

	  if (c == EOF)
	    break;

	  /* Add the character to ISTRING. */
	  while (istring_index + 1 >= istring_size)
	    istring = (char *) xrealloc
	      (istring, istring_size += DEFAULT_ARRAY_SIZE);

	  if (quoted)
	    istring[istring_index++] = QUOTE_CHAR (c);
	  else
	    istring[istring_index++] = c;

	  istring[istring_index] = '\0';
	}

      fclose (istream);
      close (fildes[0]);

      last_command_exit_value = wait_for (pid);
      last_command_subst_pid = pid;
      last_made_pid = old_pid;

#if defined (JOB_CONTROL)
      /* If last_command_exit_value > 128, then the substituted command
	 was terminated by a signal.  If that signal was SIGINT, then send
	 SIGINT to ourselves.  This will break out of loops, for instance. */
      if (last_command_exit_value == (128 + SIGINT))
	kill (getpid (), SIGINT);

      /* wait_for gives the terminal back to shell_pgrp.  If some other
         process group should have it, give it away to that group here. */
      if (pipeline_pgrp != (pid_t)0)
	give_terminal_to (pipeline_pgrp);
#endif /* JOB_CONTROL */

      /* If we read no output, just return now and save ourselves some
	 trouble. */
      if (istring_index == 0)
	goto error_exit;

      /* Strip trailing newlines from the output of the command. */
      if (quoted)
	{
	  while (istring_index > 0 &&
	      DEQUOTE_CHAR (istring[istring_index - 1]) == '\n')
	    --istring_index;

	  istring[istring_index] = '\0';
	}
      else
	{
	  strip_trailing (istring, 1);
	  istring_index = strlen (istring);
	}

      return (istring);
    }
}

/********************************************************
 *							*
 *	Utility functions for parameter expansion	*
 *							*
 ********************************************************/

/* Handle removing a pattern from a string as a result of ${name%[%]value}
   or ${name#[#]value}. */
static char *
parameter_brace_remove_pattern (value, temp, c)
     char *value, *temp;
     int c;
{
  int pattern_specifier;
  WORD_LIST *l;
  char *pattern, *t;

  if (c == '#')
    {
      if (*value == '#')
	{
	  value++;
	  pattern_specifier = RP_LONG_LEFT;
	}
      else
	pattern_specifier = RP_SHORT_LEFT;
    }
  else	/* c == '%' */
    {
      if (*value == '%')
	{
	  value++;
	  pattern_specifier = RP_LONG_RIGHT;
	}
      else
	pattern_specifier = RP_SHORT_RIGHT;
    }

  l = expand_string (value, 0);
  pattern = (char *)string_list (l);
  dispose_words (l);
  t = remove_pattern (temp, pattern, pattern_specifier);
  free (pattern);
  return (t);
}

/* Parameter expand NAME, and return a new string which is the expansion,
   or NULL if there was no expansion.
   VAR_IS_SPECIAL is non-zero if NAME is one of the special variables in
   the shell, e.g., "@", "$", "*", etc.  QUOTED, if non-zero, means that
   NAME was found inside of a double-quoted expression. */
static char *
parameter_brace_expand_word (name, var_is_special, quoted)
     char *name;
     int var_is_special, quoted;
{
  char *temp = (char *)NULL;

  /* Handle multiple digit arguments, as in ${11}. */
  if (digit (*name))
    {
      int arg_index = atoi (name);

      temp = get_dollar_var_value (arg_index);
    }
  else if (var_is_special)      /* ${@} */
    {
      char *tt;
      WORD_LIST *l;

      tt = (char *)alloca (2 + strlen (name));
      tt[0] = '$'; tt[1] = '\0';
      strcat (tt, name);
      l = expand_string_leave_quoted (tt, quoted);
      temp = string_list (l);
      dispose_words (l);
    }
  else
    {
      SHELL_VAR *var = find_variable (name);

      if (var && !invisible_p (var) && (temp = value_cell (var)))
	temp = savestring (temp);
    }
  return (temp);
}

/* Expand the right side of a parameter expansion of the form ${NAMEcVALUE},
   depending on the value of C, the separating character.  C can be one of
   "-", "+", or "=". */
static char *
parameter_brace_expand_rhs (name, value, c, quoted)
     char *name, *value;
     int c;
{
  extern char *tilde_expand ();
  WORD_LIST *l;
  char *t, *t1, *temp;

  if (value[0] == '~' ||
      (index (value, '~') && unquoted_substring ("=~", value)))
    temp = tilde_expand (value);
  else
    temp = savestring (value);

  l = expand_string_leave_quoted (temp, quoted);
  free (temp);

  temp = (char *)string_list (l);
  dispose_words (l);

  if (c == '-' || c == '+')
    return (temp);

  /* c == '=' */
  if (temp)
    t = savestring (temp);
  else
    t = savestring ("");
  t1 = dequote_string (t);
  free (t);
  t = t1;
  bind_variable (name, t);
  free (t);
  return (temp);
}

/* Deal with the right hand side of a ${name:?value} expansion in the case
   that NAME is null or not set.  If VALUE is non-null it is expanded and
   used as the error message to print, otherwise a standard message is
   printed. */
static void
parameter_brace_expand_error (name, value)
     char *name, *value;
{
  extern int interactive;

  if (value && *value)
    {
      WORD_LIST *l = expand_string (value, 0);
      char *temp1 =  string_list (l);
      fprintf (stderr, "%s: %s\n", name, temp1 ? temp1 : value);
      if (temp1)
	free (temp1);
      dispose_words (l);
    }
  else
    report_error ("%s: parameter null or not set", name);

  /* Free the data we have allocated during this expansion, since we
     are about to longjmp out. */
  free (name);
  if (value)
    free (value);

  if (!interactive)
    longjmp (top_level, FORCE_EOF);
  else
    longjmp (top_level, DISCARD);
}

/* Handle the parameter brace expansion that requires us to return the
   length of a parameter. */
static int
parameter_brace_expand_length (name)
     char *name;
{
  char *t;
  int number = 0;

  if (name[1] == '\0')			/* ${#} */
    {
      WORD_LIST *l = list_rest_of_args ();
      number = list_length (l);
      dispose_words (l);
    }
  else if (name[1] != '*' && name[1] != '@')
    {
      number = 0;

      if (digit (name[1]))		/* ${#1} */
	{
	  if (t = get_dollar_var_value (atoi (&name[1])))
	    {
	      number = strlen (t);
	      free (t);
	    }
	}
      else				/* ${#PS1} */
	{
	  WORD_LIST *list;
	  char *newname;

	  newname = savestring (name);
	  newname[0] = '$';
	  list = expand_string (newname, 0);
	  t = string_list (list);
	  free (newname);
	  dispose_words (list);

	  if (t)
	    number = strlen (t);
	}
    }
  else					/* ${#@} and ${#*} */
    {
      if (t = string_rest_of_args (1))
	{
	  number = strlen (t);
	  free (t);
	}
    }
  return (number);
}

/* Make a word list which is the parameter and variable expansion,
   command substitution, arithmetic substitution, and quote removed
   expansion of WORD.  Return a pointer to a WORD_LIST which is the
   result of the expansion.  If WORD contains a null word, the word
   list returned is also null.

   QUOTED, when non-zero specifies that the text of WORD is treated
   as if it were surrounded by double quotes.
   CONTAINS_DOLLAR_AT and EXPANDED_SOMETHING are return values; when non-null
   they point to an integer value which receives information about expansion.
   CONTAINS_DOLLAR_AT gets non-zero if WORD contained "$@", else zero.
   EXPANDED_SOMETHING get non-zero if WORD contained any parameter expansions,
   else zero.

   This only does word splitting in the case of $@ expansion.  In that
   case, we split on ' '. */
WORD_LIST *
expand_word_internal (word, quoted, contains_dollar_at, expanded_something)
     WORD_DESC *word;
     int quoted;
     int *contains_dollar_at;
     int *expanded_something;
{
  extern char *itos ();
  extern int last_command_exit_value;

  /* The thing that we finally output. */
  WORD_LIST *result = (WORD_LIST *)NULL;

  /* The intermediate string that we build while expanding. */
  char *istring = (char *)xmalloc (DEFAULT_ARRAY_SIZE);

  /* The current size of the above object. */
  int istring_size = DEFAULT_ARRAY_SIZE;

  /* Index into ISTRING. */
  int istring_index = 0;

  /* Temporary string storage. */
  char *temp = (char *)NULL;

  /* The text of WORD. */
  register char *string = word->word;

  /* The index into STRING. */
  register int sindex = 0;

  /* This gets 1 if we see a $@ while quoted. */
  int quoted_dollar_at = 0;

  /* This gets 1 if we are to treat backslashes as if we are within double
     quotes, but not otherwise behave as if the word is quoted.  This is
     used for things like expansion of patterns in case statement pattern
     lists.  This is a private variable, but the incoming value of
     Q_KEEP_BACKSLASH is passed to recursive invocations of this function. */
  int preserve_backslashes = 0;

  register int c;		/* Current character. */
  int number;			/* Temporary number value. */
  int t_index;			/* For calls to string_extract_xxx. */
  extern int interactive;
  char *command_subst_result;	/* For calls to command_substitute (). */

  istring[0] = '\0';

  if (!string) goto final_exit;

  if (quoted & Q_KEEP_BACKSLASH)
    {
      preserve_backslashes = 1;
      quoted &= ~Q_KEEP_BACKSLASH;
    }

  if (contains_dollar_at)
    *contains_dollar_at = 0;

  /* Begin the expansion. */

  for (;;) {

    c = string[sindex];

    switch (c) {		/* Case on toplevel character. */

    case '\0':
      goto finished_with_string;

    case '$':

      if (expanded_something)
	*expanded_something = 1;

      c = string[++sindex];

      /* Do simple cases first. Switch on what follows '$'. */
      switch (c)
	{
	  /* $0 .. $9? */
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
	  if (dollar_vars[digit_value (c)])
	    temp = savestring (dollar_vars[digit_value (c)]);
	  else
	    temp = (char *)NULL;
	  goto dollar_add_string;

	case '$':		/* $$ -- pid of the invoking shell. */
	  {
	    extern int dollar_dollar_pid;
	    number = dollar_dollar_pid;
	  }
	add_number:
	  temp = itos (number);
	dollar_add_string:
	  if (string[sindex]) sindex++;

	  /* Add TEMP to ISTRING. */
	add_string:
	  istring =
	    sub_append_string (temp, istring, &istring_index, &istring_size);
	  break;

	  /* $# -- number of positional parameters. */
	case '#':
	  {
	    WORD_LIST *list = list_rest_of_args ();
	    number = list_length (list);
	    dispose_words (list);
	    goto add_number;
	  }

	  /* $? -- return value of the last synchronous command. */
	case '?':
	  number = last_command_exit_value;
	  goto add_number;

	  /* $- -- flags supplied to the shell on invocation or by `set'. */
	case '-':

	  temp = (char *)which_set_flags ();
	  goto dollar_add_string;

	  /* $! -- Pid of the last asynchronous command. */
	case '!':
	  {
	    number = (int)last_asynchronous_pid;

	    /* If no asynchronous pids have been created, echo nothing. */
	    if (number == (int)NO_PID)
	      {
		if (string[sindex])
		  sindex++;
		if (expanded_something)
		  *expanded_something = 0;
		break;
	      }
	    goto add_number;
	  }

	  /* The only difference between this and $@ is when the
	     arg is quoted. */
	case '*':		/* `$*' */
	  temp = string_rest_of_args (quoted);

	  /* In the case of a quoted string, quote the entire arg-list.
	     "$1 $2 $3". */
	  if (quoted && temp)
	    {
	      char *james_brown = temp;
	      temp = quote_string (temp);
	      free (james_brown);
	    }
	  goto dollar_add_string;

	  /* When we have "$@" what we want is "$1" "$2" "$3" ... This
	     means that we have to turn quoting off after we split into
	     the individually quoted arguments so that the final split
	     on the first character of $IFS is still done.  */
	case '@':		/* `$@' */
	  {
	    WORD_LIST *tlist = list_rest_of_args ();
	    if (quoted && tlist)
	      quote_list (tlist);

	    /* We want to flag the fact that we saw this.  We can't turn off
	       quoting entirely, because other characters in the string might
	       need it (consider "\"$@\""), but we need some way to signal
	       that the final split on the first character of $IFS should be
	       done, even though QUOTED is 1. */
	    if (quoted)
	      quoted_dollar_at = 1;
	    if (contains_dollar_at)
	      *contains_dollar_at = 1;
	    temp = string_list (tlist);
	    goto dollar_add_string;
	  }

	  /* ${[#]name[[:]#[#]%[%]-=?+[word]]} */
	case '{':
	  {
	    int check_nullness = 0;
	    int var_is_set = 0;
	    int var_is_null = 0;
	    int var_is_special = 0;
	    char *name, *value;

	    sindex++;
	    t_index = sindex;
	    name = string_extract (string, &t_index, "#%:-=?+}");

	    /* If the name really consists of a special variable, then
	       make sure that we have the entire name. */
	    if (sindex == t_index &&
		(string[sindex] == '-' ||
		 string[sindex] == '?' ||
		 string[sindex] == '#'))
	      {
		char *tt;
		t_index++;
		free (name);
		tt = (string_extract (string, &t_index, "#%:-=?+}"));
		name = (char *)xmalloc (2 + (strlen (tt)));
		*name = string[sindex];
		strcpy (name + 1, tt);
		free (tt);
	      }
	    sindex = t_index;

	    /* Find out what character ended the variable name.  Then
	       do the appropriate thing. */

	    if (c = string[sindex])
	      sindex++;

	    if (c == ':')
	      {
		check_nullness++;
		if (c = string[sindex])
		  sindex++;
	      }

	    /* Determine the value of this variable. */
	    if (digit (*name) ||
		(strlen (name) == 1 && member (*name, "#-?$!@*")))
	      var_is_special++;

	    /* Check for special expansion things. */
	    if (*name == '#')
	      {
		/* Handle ${#-} and ${#?}.  They return the lengths of
		   $- and $?, respectively. */
		if (string[sindex] == '}' &&
		    !name[1] &&
		    !check_nullness &&
		    (c == '-' || c == '?'))
		  {
		    char *s;

		    free (name);

		    if (c == '-')
		      s = (char *)which_set_flags ();
		    else
		      s = itos (last_command_exit_value);

		    number = s ? strlen (s) : 0;
		    if (s)
		      free (s);
		    goto add_number;
		  }

		/* Don't allow things like ${#:-foo} to go by; they are
		   errors.  If we are not pointing at the character just
		   after the closing brace, then we haven't gotten all of
		   the name.  Since it begins with a special character,
		   this is a bad substitution.  Explicitly check for ${#:},
		   which the rules do not catch. */
		if (string[sindex - 1] != '}' || member (c, "?-=+") ||
		    (string[sindex - 1] == '}' && !name[1] && c == '}' &&
		     check_nullness))
		  {
		    free (name);
		    name = string;
		    goto bad_substitution;
		  }

		number = parameter_brace_expand_length (name);
		/* We are pointing one character after the brace which
		   closes this expression.  Since the code at add_number
		   increments SINDEX, we back up a single character here. */
		sindex--;
		goto add_number;
	      }

	    /* ${@} is identical to $@. */
	    if (name[0] == '@' && name[1] == '\0')
	      {
		if (quoted)
		  quoted_dollar_at = 1;

		if (contains_dollar_at)
		  *contains_dollar_at = 1;
	      }

	    temp = parameter_brace_expand_word (name, var_is_special, quoted);

	    if (temp)
	      var_is_set++;

	    if (!var_is_set || !temp || !*temp)
	      var_is_null++;

	    if (!check_nullness)
	      var_is_null = 0;

	    /* Get the rest of the stuff inside the braces. */
	    if (c && c != '}')
	      {
		/* Scan forward searching for last `{'.  This is a hack,
		   it will always be a hack, and it always has been a hack. */
		t_index = sindex;
		value = extract_delimited_string (string, &t_index,
						  "{", (char *)NULL, "}");
		sindex = t_index;

		if (string[sindex] == '}')
		  sindex++;
		else
		  {
		    if (value)
		      free (value);

		    free (name);
		    name = string;
		    goto bad_substitution;
		  }
	      }
	    else
	      value = (char *)NULL;

	    /* Do the right thing based on which character ended the variable
	       name. */
	    switch (c)
	      {
	      case '\0':
	      bad_substitution:
		report_error ("%s: bad substitution", name ? name : "??");
		free (name);
		longjmp (top_level, DISCARD);

	      case '}':
		break;

	      case '#':		/* ${param#[#]pattern} */
	      case '%':		/* ${param%[%]pattern} */
		{
		  char *t;
		  if (!value || !*value || !temp || !*temp)
		    break;
		  t = parameter_brace_remove_pattern (value, temp, c);
		  free (temp);
		  free (value);
		  temp = t;
		}
		break;

	      case '-':
	      case '=':
	      case '?':
	      case '+':
		if (var_is_set && !var_is_null)
		  {
		    /* We don't want the value of the named variable for
		       anything, just the value of the right hand side. */
		    if (c == '+')
		      {
			if (temp)
			  free (temp);
			if (value)
			  temp = parameter_brace_expand_rhs (name, value, c, quoted);
			else
			  temp = (char *)NULL;
		      }
		    /* Otherwise do nothing.  Just use the value in temp. */
		  }
		else		/* var not set or var is null */
		  {
		    if (temp)
		      free (temp);
		    temp = (char *)NULL;
		    if (c == '=' && var_is_special)
		      {
			report_error ("$%s: cannot assign in this way", name);
			free (name);
			free (value);
			longjmp (top_level, DISCARD);
		      }
		    else if (c == '?')
		      parameter_brace_expand_error (name, value);
		    else if (c != '+')
		      temp =
			parameter_brace_expand_rhs (name, value, c, quoted);
		    free (value);
		  }
		break;
	      }			/* end case on closing character. */
	    free (name);
	    goto add_string;
	  }			/* end case '{' */
	  /* break; */

	case '(':		/* Do command or arithmetic substitution. */
	  /* We have to extract the contents of this paren substitution. */
	  {
	    char *extract_command_subst ();
	    int old_index = ++sindex;

	    temp = extract_command_subst (string, &old_index);
	    sindex = old_index;

	    /* For the Posix.2-style $(( )) form of arithmetic substitution,
	       extract the expression and pass it to the evaluator. */
	    if (temp && *temp == '(')
	      {
		char *t = temp + 1;
		int last = strlen (t) - 1;
		extern long evalexp ();

		if (t[last] != ')')
		  {
		    report_error ("%s: bad arithmetic substitution", temp);
		    free (temp);
		    /* XXX - these are mem leaks */
		    longjmp (top_level, DISCARD);
		  }

		/* Cut off ending `)' */
		t[last] = '\0';

		number = (int)evalexp (t);
		free (temp);
		goto add_number;
	      }

	    goto handle_command_substitution;
	  }

	  /* Do straight arithmetic substitution. */
	case '[':
	  /* We have to extract the contents of this
	     arithmetic substitution. */
	  {
	    char *extract_arithmetic_subst (), *t;
	    int old_index = ++sindex;
	    WORD_LIST *l;
	    extern long evalexp ();
	    extern char *this_command_name;

	    temp = extract_arithmetic_subst (string, &old_index);
	    sindex = old_index;

	    /* Do initial variable expansion. */
	    l = expand_string (temp, 1);
	    t = string_list (l);
	    dispose_words (l);

	    /* No error messages. */
	    this_command_name = (char *)NULL;
	    number = (int)evalexp (t);
	    free (t);

	    goto add_number;
	  }

	default:
	  {
	    /* Find the variable in VARIABLE_LIST. */
	    int old_index = sindex;
	    char *name;
	    SHELL_VAR *var;

	    temp = (char *)NULL;

	    for (;
		 (c = string[sindex]) &&
		 (isletter (c) || digit (c) || c == '_');
		 sindex++);
	    name = (char *)substring (string, old_index, sindex);

	    /* If this isn't a variable name, then just output the `$'. */
	    if (!name || !*name)
	      {
		free (name);
		temp = savestring ("$");
		if (expanded_something)
		  *expanded_something = 0;
		goto add_string;
	      }

	    /* If the variable exists, return its value cell. */
	    var = find_variable (name);

	    if (var && !invisible_p (var) && value_cell (var))
	      {
		temp = savestring (value_cell (var));
		free (name);
		goto add_string;
	      }
	    else
	      temp = (char *)NULL;

	    if (unbound_vars_is_error)
	      report_error ("%s: unbound variable", name);
	    else
	      goto add_string;

	    free (name);
	    longjmp (top_level, DISCARD);
	  }
	}
      break;			/* End case '$': */

    case '`':			/* Backquoted command substitution. */
      {
	sindex++;

	if (expanded_something)
	  *expanded_something = 1;

	t_index = sindex;
	temp = string_extract (string, &t_index, "`");
	sindex = t_index;
	de_backslash (temp);

      handle_command_substitution:
	command_subst_result = command_substitute (temp, quoted);

	if (temp)
	  free (temp);

	temp = command_subst_result;

	if (string[sindex])
	  sindex++;

	goto add_string;
      }

    case '\\':
      if (string[sindex + 1] == '\n')
	{
	  sindex += 2;
	  continue;
	}
      else
	{
	  char *slashify_chars = "";

	  c = string[++sindex];

	  if (quoted == Q_HERE_DOCUMENT)
	    slashify_chars = slashify_in_here_document;
	  else if (quoted == Q_DOUBLE_QUOTES)
	    slashify_chars = slashify_in_quotes;

	  if (preserve_backslashes || (quoted && !member (c, slashify_chars)))
	    {
	      temp = (char *)xmalloc (3);
	      temp[0] = '\\'; temp[1] = c; temp[2] = '\0';
	      if (c)
		sindex++;
	      goto add_string;
	    }
	  else
	    {
	      /* This character is quoted, so add it in quoted mode. */
	      c = QUOTE_CHAR (c);
	      goto add_character;
	    }
	}

    case '"':
      if (quoted)
	goto add_character;
      sindex++;
      {
	WORD_LIST *tresult = (WORD_LIST *)NULL;

	t_index = sindex;
	temp = string_extract_double_quoted (string, &t_index);
	sindex = t_index;

	if (string[sindex])
	  sindex++;

	if (temp && *temp)
	  {
	    int dollar_at_flag;
	    int quoting_flags = Q_DOUBLE_QUOTES;
	    WORD_DESC *temp_word = make_word (temp);

	    free (temp);

	    if (preserve_backslashes)
	      quoting_flags |= Q_KEEP_BACKSLASH;
	    tresult = expand_word_internal (temp_word, quoting_flags,
					    &dollar_at_flag, (int *)NULL);

	    dispose_word (temp_word);

	    if (!tresult && dollar_at_flag)
	      break;
	    /* If we get "$@", we know we have expanded something, so we
	       need to remember it for the final split on $IFS.  This is
	       a special case; it's the only case where a quoted string
	       can expand into more than one word.  It's going to come back
	       from the above call to expand_word_internal as a list with
	       a single word, in which all characters are quoted and
	       separated by blanks.  What we want to do is to turn it back
	       into a list for the next piece of code. */
	    dequote_list (tresult);
	    if (dollar_at_flag)
	      quoted_dollar_at++;
	    if (expanded_something)
	      *expanded_something = 1;
	  }
	else
	  {
	    /* What we have is "".  This is a minor optimization. */
	    free (temp);
	    tresult = (WORD_LIST *)NULL;
	  }

	/* The code above *might* return a list (consider the case of "$@",
	   where it returns "$1", "$2", etc.).  We can't throw away the rest
	   of the list, and we have to make sure each word gets added as
	   quoted.  We test on tresult->next:  if it is non-NULL, we quote
	   the whole list, save it to a string with string_list, and add that
	   string. We don't need to quote the results of this (and it would be
	   wrong, since that would quote the separators as well), so we go
	   directly to add_string. */
	if (tresult)
	  {
	    if (tresult->next)
	      {
		quote_list (tresult);
		temp = string_list (tresult);
		dispose_words (tresult);
		goto add_string;
	      }
	    else
	      {
		temp = savestring (tresult->word->word);
		dispose_words (tresult);
	      }
	  }
	else
	  temp = (char *)NULL;

      add_quoted_string:

	if (temp)
	  {
	    char *t = temp;
	    temp = quote_string (temp);
	    free (t);
	  }
	else
	  {
	    /* Add NULL arg. */
	    temp = savestring (" ");
	    temp[0] = (unsigned char)QUOTE_CHAR ('\0');
	  }
	goto add_string;
      }
      /* break; */

    case '\'':
      {
	if (!quoted)
	  {
	    sindex++;

	    t_index = sindex;
	    temp = string_extract_verbatim (string, &t_index, "'");
	    sindex = t_index;

	    if (string[sindex])
	      sindex++;

	    if (!*temp)
	      {
		free (temp);
		temp = (char *)NULL;
	      }

	    goto add_quoted_string;
	  }
	else
	  goto add_character;

	break;
      }

    default:

      /* This is the fix for " $@ " */
      if (quoted)
	c = QUOTE_CHAR (c);

add_character:
      while (istring_index + 1 >= istring_size)
	istring = (char *)
	  xrealloc (istring, istring_size += DEFAULT_ARRAY_SIZE);
      istring[istring_index++] = c;
      istring[istring_index] = '\0';

      /* Next character. */
      sindex++;
    }
  }

finished_with_string:
final_exit:
  /* OK, we're ready to return.  If we have a quoted string, and
     quoted_dollar_at is not set, we do no splitting at all; otherwise
     we split on ' '.  The routines that call this will handle what to
     do if nothing has been expanded. */
  if (istring)
    {
      WORD_LIST *temp_list;

      if (quoted_dollar_at)
	temp_list = list_string (istring, " ", quoted);
      else if (*istring)
	{
	  temp_list = make_word_list (make_word (istring), (WORD_LIST *)NULL);
	  temp_list->word->quoted = quoted;
	}
      else
	temp_list = (WORD_LIST *)NULL;
      free (istring);
      result = (WORD_LIST *)list_append (reverse_list (result), temp_list);
    }
  else
    result = (WORD_LIST *)NULL;

  return (result);
}

/* **************************************************************** */
/*                                                                  */
/*              Functions for Quote Removal			    */
/*                                                                  */
/* **************************************************************** */

/* Perform quote removal on STRING.  If QUOTED > 0, assume we are obeying the
   backslash quoting rules for within double quotes. */
char *
string_quote_removal (string, quoted)
     char *string;
     int quoted;
{
  char *r, *result_string, *temp, *temp1;
  int sindex, tindex, c;

  /* The result can be no longer than the original string. */
  r = result_string = xmalloc (strlen (string) + 1);
  sindex = 0;

  for (;;)
    {
      c = string[sindex];
      if (c == '\0')
	break;

      switch (c)
	{
	  case '\\':
	    c = string[++sindex];
	    if (quoted && !member (c, slashify_in_quotes))
	      {
		*r++ = '\\';
		*r++ = c;
	      }
	    else
	      *r++ = c;

	    sindex++;
	    break;
      
	  case '"':
	    tindex = ++sindex;
	    temp = string_extract_double_quoted (string, &tindex);
	    sindex = tindex;

	    if (string[sindex])
	      sindex++;

	    temp1 = string_quote_removal (temp, 1);  /* XXX is this needed? */

	    if (temp)
	      free (temp);

	    if (temp1)
	      {
		strcpy (r, temp1);
		r += strlen (r);
		free (temp1);
	      }
	    break;
	    
	  case '\'':
	    if (quoted)
	      {
		*r++ = c;
		sindex++;
	      }
	    else
	      {
		tindex = ++sindex;
		temp = string_extract_verbatim (string, &tindex, "'");
		sindex = tindex;

		if (string[sindex])
		  sindex++;

		if (temp)
		  {
		    strcpy (r, temp);
		    r += strlen (r);
		    free (temp);
		  }
	      }
	    break;
	  default:
	    *r++ = c;
	    sindex++;
	    break;
	}
    }
    *r = '\0';
    return (result_string);
}

/* Perform quote removal on word WORD.  This allocates and returns a new
   WORD_DESC *. */
WORD_DESC *
word_quote_removal (word, quoted)
     WORD_DESC *word;
     int quoted;
{
  WORD_DESC *w;
  char *t;

  t = string_quote_removal (word->word, quoted);
  w = make_word (t);
  return (w);
}

/* Perform quote removal on all words in LIST.  If QUOTED is non-zero,
   the members of the list are treated as if they are surrounded by
   double quotes.  Return a new list, or NULL if LIST is NULL. */
WORD_LIST *
word_list_quote_removal (list, quoted)
     WORD_LIST *list;
     int quoted;
{
  WORD_LIST *result = (WORD_LIST *)NULL, *t, *tresult;

  t = list;
  while (t)
    {
      tresult = (WORD_LIST *)xmalloc (sizeof (WORD_LIST));
      tresult->word = word_quote_removal (t->word, quoted);
      tresult->next = (WORD_LIST *)NULL;
      result = (WORD_LIST *) list_append (result, tresult);
      t = t->next;
    }
  return (result);
}

#if defined (NOTDEF)
/* Currently unused. */
/* Return 1 if CHARACTER appears in an unquoted portion of
   STRING.  Return 0 otherwise. */
static int
unquoted_member (character, string)
     int character;
     char *string;
{
  int sindex, tindex, c;
  char *temp;

  sindex = 0;

  while (c = string[sindex])
    {
      if (c == character)
	return (1);

      switch (c)
	{
	  case '\\':
	    sindex++;
	    if (string[sindex])
	      sindex++;
	    break;
      
	  case '"':
	  case '\'':

	    tindex = ++sindex;
	    if (c == '"')
	      temp = string_extract_double_quoted (string, &tindex);
	    else
	      temp = string_extract_verbatim (string, &tindex, "'");
	    sindex = tindex;

	    if (string[sindex])
	      sindex++;

	    if (temp)
	      free (temp);
	    break;
	    
	  default:
	    sindex++;
	    break;
	}
    }
  return (0);
}
#endif /* NOTDEF */

/* Return 1 if SUBSTR appears in an unquoted portion of STRING. */
static int
unquoted_substring (substr, string)
     char *substr, *string;
{
  int sindex, tindex, c, sublen;
  char *temp;

  if (!substr || !*substr)
    return (0);

  sublen = strlen (substr);
  sindex = 0;

  while (c = string[sindex])
    {
      if (c == *substr &&
	  strncmp (string + sindex, substr, sublen) == 0)
	return (1);

      switch (c)
	{
	  case '\\':
	    sindex++;

	    if (string[sindex])
	      sindex++;
	    break;
      
	  case '"':
	  case '\'':

	    tindex = ++sindex;
	    if (c == '"')
	      temp = string_extract_double_quoted (string, &tindex);
	    else
	      temp = string_extract_verbatim (string, &tindex, "'");
	    sindex = tindex;

	    if (string[sindex])
	      sindex++;

	    if (temp)
	      free (temp);

	    break;
	    
	  default:
	    sindex++;
	    break;
	}
    }
  return (0);
}

/*******************************************
 *					   *
 *    Functions to perform word splitting  *
 *					   *
 *******************************************/

/* This splits a single word into a WORD LIST on $IFS, but only if the word
   is not quoted.  list_string () performs quote removal for us, even if we
   don't do any splitting. */
WORD_LIST *
word_split (w)
     WORD_DESC *w;
{
  WORD_LIST *result;

  if (w)
    {
      SHELL_VAR *ifs = find_variable ("IFS");
      char *ifs_chars;

      /* If IFS is unset, it defaults to " \t\n". */
      if (ifs)
	ifs_chars = value_cell (ifs);
      else
	ifs_chars = " \t\n";

      if (w->quoted || !ifs_chars)
	ifs_chars = "";

      result = list_string (w->word, ifs_chars, w->quoted);
    }
  else
    result = (WORD_LIST *)NULL;
  return (result);
}

/* Perform word splitting on LIST and return the RESULT.  It is possible
   to return (WORD_LIST *)NULL. */
WORD_LIST *
word_list_split (list)
     WORD_LIST *list;
{
  WORD_LIST *result = (WORD_LIST *)NULL, *t, *tresult;

  t = list;
  while (t)
    {
      tresult = word_split (t->word);
      result = (WORD_LIST *) list_append (result, tresult);
      t = t->next;
    }
  return (result);
}

/**************************************************
 * 						  *
 *	Functions to expand an entire WORD_LIST	  *
 *						  *
 **************************************************/

/* Do all of the assignments in LIST up to a word which isn't an
   assignment. */
WORD_LIST *
get_rid_of_variable_assignments (list)
     WORD_LIST *list;
{
  WORD_LIST *orig = list;

  while (list)
    if (!list->word->assignment)
      {
	WORD_LIST *new_list = copy_word_list (list);
	dispose_words (orig);
	return (new_list);
      }
    else
      {
	do_assignment (list->word->word);
	list = list->next;
      }
  dispose_words (orig);
  return ((WORD_LIST *)NULL);
}

/* Check and handle the case where there are some variable assignments
   in LIST which go into the environment for this command. */
WORD_LIST *
get_rid_of_environment_assignments (list)
     WORD_LIST *list;
{
  register WORD_LIST *tlist = list;
  register WORD_LIST *new_list;

  while (tlist)
    {
      if (!tlist->word->assignment) goto make_assignments;
      tlist = tlist->next;
    }
  /* Since all of the assignments are variable assignments. */
  return (list);

make_assignments:
  tlist = list;
  while (tlist)
    {
      if (tlist->word->assignment)
	assign_in_env (tlist->word->word);
      else
	{
	  if (!place_keywords_in_env)
	    {
	      new_list = copy_word_list (tlist);
	      dispose_words (list);
	      return (new_list);
	    }
	}
      tlist = tlist->next;
    }

  /* We got all of the keywords assigned.  Now return the remainder
     of the words. */
  {
    register WORD_LIST *new_list = (WORD_LIST *)NULL;

    tlist = list;

    /* Skip the ones at the start. */
    while (tlist && tlist->word->assignment)
      tlist = tlist->next;

    /* If we placed all the keywords in the list into the environment,
       then remove them from the output list. */
    if (place_keywords_in_env)
      {
	while (tlist)
	  {
	    if (!tlist->word->assignment)
	      new_list = make_word_list (copy_word (tlist->word), new_list);
	    tlist = tlist->next;
	  }
	new_list = (WORD_LIST *)reverse_list (new_list);
      }
    else
      {
	/* Just copy the list. */
	new_list = copy_word_list (tlist);
      }
    dispose_words (list);
    return (new_list);
  }
}

/* Take the list of words in LIST and do the various substitutions.  Return
   a new list of words which is the expanded list, and without things like
   variable assignments. */
static WORD_LIST *expand_words_internal ();

WORD_LIST *
expand_words (list)
     WORD_LIST *list;
{
  return (expand_words_internal (list, 1));
}

/* Same as expand_words (), but doesn't hack variable or environment
   variables. */
WORD_LIST *
expand_words_no_vars (list)
     WORD_LIST *list;
{
  return (expand_words_internal (list, 0));
}

/* Non-zero means to allow unmatched globbed filenames to expand to
   a null file. */
static int allow_null_glob_expansion = 0;

/* The workhorse for expand_words () and expand_words_no_var ().
   First arg is LIST, a WORD_LIST of words.
   Second arg DO_VARS is non-zero if you want to do environment and
   variable assignments, else zero.

   This does all of the subsitutions: brace expansion, tilde expansion,
   parameter expansion, command substitution, arithmetic expansion,
   word splitting, and pathname expansion. */
static WORD_LIST *
expand_words_internal (list, do_vars)
     WORD_LIST *list;
     int do_vars;
{
  register WORD_LIST *tlist, *new_list = (WORD_LIST *)NULL;
  WORD_LIST *orig_list;
  extern int no_brace_expansion;

  tlist = (WORD_LIST *)copy_word_list (list);

  if (do_vars)
    {
      /* Handle the case where the arguments are assignments for
	 the environment of this command. */
      tlist = get_rid_of_environment_assignments (tlist);

      /* Handle the case where the arguments are all variable assignments. */
      tlist = get_rid_of_variable_assignments (tlist);
    }

  /* Begin expanding the words that remain.  The expansions take place on
     things that aren't really variable assignments. */

  if (!tlist)
    return ((WORD_LIST *)NULL);

  /* Do brace expansion on this word if there are any brace characters
     in the string. */
  if (!no_brace_expansion)
    {
      extern char **brace_expand ();
      register char **expansions;
      WORD_LIST *braces = (WORD_LIST *)NULL;
      int eindex;

      orig_list = tlist;

      while (tlist)
	{
	  /* Only do brace expansion if the word has a brace character.  If
	     not, just copy the word list element, add it to braces, and
	     continue.  In the common case, at least when running shell
	     scripts, this will degenerate to a bunch of calls to `index',
	     and then what is basically the body of copy_word_list. */
	  if (index (tlist->word->word, '{') != NULL)
	    {
	      expansions = brace_expand (tlist->word->word);

	      for (eindex = 0; expansions[eindex]; eindex++)
		{
		  braces = make_word_list (make_word (expansions[eindex]),
		  			   braces);
		  free (expansions[eindex]);
		}
	      free (expansions);
	    }
	  else
	    {
	      WORD_LIST *new = (WORD_LIST *)xmalloc (sizeof (WORD_LIST));
	      new->word = copy_word (tlist->word);
	      new->next = braces;
	      braces = new;
	    }

	  tlist = tlist->next;
	}
      dispose_words (orig_list);
      tlist = (WORD_LIST *)reverse_list (braces);
    }

  orig_list = tlist;

  /* We do tilde expansion all the time.  This is what 1003.2 says. */
  while (tlist)
    {
      register char *current_word;
      WORD_LIST *expanded, *t;
      int expanded_something = 0;

      current_word = tlist->word->word;

      if (current_word[0] == '~' ||
	  (index (current_word, '~') &&
	   unquoted_substring ("=~", current_word)))
	{
	  char *tilde_expand (), *tt;

	  tt = tlist->word->word;
	  tlist->word->word = tilde_expand (tt);
	  free (tt);
	}

      expanded = expand_word_internal
	(tlist->word, 0, (int *)NULL, &expanded_something);

      if (expanded_something)
	t = word_list_split (expanded);
      else
	{
	  /* If no parameter expansion, command substitution, or arithmetic
	     substitution took place, then do not do word splitting.  We
	     still have to remove quoted null characters from the result. */
	  word_list_remove_quoted_nulls (expanded);
	  t = copy_word_list (expanded);
	}

      new_list =
	(WORD_LIST *)list_append (reverse_list (t), new_list);

      dispose_words (expanded);

      tlist = tlist->next;
    }

  new_list = (WORD_LIST *)reverse_list (new_list);

  dispose_words (orig_list);

  /* Okay, we're almost done.  Now let's just do some filename
     globbing. */
  {
    char **shell_glob_filename (), **temp_list = (char **)NULL;
    register int list_index;
    WORD_LIST *glob_list;

    orig_list = (WORD_LIST *)NULL;
    tlist = new_list;

    if (!disallow_filename_globbing)
      {
	while (tlist)
	  {
	    /* If the word isn't quoted, then glob it. */
	    if (!tlist->word->quoted && glob_pattern_p (tlist->word->word, 0))
	      {
		temp_list = shell_glob_filename (tlist->word->word);

		/* Fix the hi-bits. (This is how we quoted
		   special characters.) */
		{
		  register char *t = dequote_string (tlist->word->word);
		  free (tlist->word->word);
		  tlist->word->word = t;
		}

		/* Handle error cases.
		   I don't think we should report errors like "No such file
		   or directory".  However, I would like to report errors
		   like "Read failed". */

#if defined (USE_GLOB_LIBRARY)
		if (!temp_list)
#else
		if (temp_list == (char **)-1)
#endif /* !USE_GLOB_LIBRARY */
		  {
		    /* file_error (tlist->word->word); */
		    /* A small memory leak, I think */
		    temp_list = (char **) xmalloc (sizeof (char *));
		    temp_list[0] = '\0';
		  }

#if !defined (USE_GLOB_LIBRARY)
		if (!temp_list)
		  abort ();
#endif /* !USE_GLOB_LIBRARY */

		/* Make the array into a word list. */
		glob_list = (WORD_LIST *)NULL;
		for (list_index = 0; temp_list[list_index]; list_index++)
		  glob_list =
		    make_word_list (make_word (temp_list[list_index]), glob_list);

		if (glob_list)
		  orig_list = (WORD_LIST *)list_append (glob_list, orig_list);
		else
		  if (!allow_null_glob_expansion)
		    orig_list =
		      make_word_list (copy_word (tlist->word), orig_list);
	      }
	    else
	      {
		/* Fix the hi-bits. (This is how we quoted special
		   characters.) */
		register char *t = dequote_string (tlist->word->word);
		free (tlist->word->word);
		tlist->word->word = t;
		orig_list = make_word_list (copy_word (tlist->word), orig_list);
	      }

	    free_array (temp_list);
	    temp_list = (char **)NULL;

	    tlist = tlist->next;
	  }
	dispose_words (new_list);
	new_list = orig_list;
      }
    else
      {
	/* Fix the hi-bits. (This is how we quoted special characters.) */
	register WORD_LIST *wl = new_list;
	register char *wp;
	while (wl)
	  {
	    wp = dequote_string (wl->word->word);
	    free (wl->word->word);
	    wl->word->word = wp;
	    wl = wl->next;
	  }
	return (new_list);
      }
  }
  return (WORD_LIST *)(reverse_list (new_list));
}

/* Call the glob library to do globbing on PATHNAME.
   PATHNAME can contain characters with the hi bit set; this indicates
   that the character is to be quoted.  We quote it here. */
char **
shell_glob_filename (pathname)
     char *pathname;
#if defined (USE_GLOB_LIBRARY)
{
  extern int glob_dot_filenames;
  register int i, j;
  char *temp, **return_value;
  glob_t filenames;
  int glob_flags;

  temp = (char *)alloca (1 + (2 * strlen (pathname)));

  for (i = j = 0; pathname[i]; i++, j++)
    {
      if (QUOTED_CHAR (pathname[i]))
	temp[j++] = '\\';

      temp[j] = DEQUOTE_CHAR (pathname[i]);
    }
  temp[j] = '\0';

  filenames.gl_offs = 0;

  glob_flags = glob_dot_filenames ? GLOB_PERIOD : 0;
  glob_flags |= (GLOB_ERR | GLOB_DOOFFS);

  i = glob (temp, glob_flags, (Function *)NULL, &filenames);

  if (i == GLOB_NOSPACE || i == GLOB_ABEND)
    return ((char **)NULL);

  if (i == GLOB_NOMATCH)
    filenames.gl_pathv[0] = (char *)NULL;

  return (filenames.gl_pathv);
}
#else /* !USE_GLOB_LIBRARY */
{
  extern char **glob_filename ();
  extern int glob_dot_filenames, noglob_dot_filenames;
  register int i, j;
  char *temp, **results;

  temp = (char *)alloca (1 + (2 * strlen (pathname)));

  noglob_dot_filenames = !glob_dot_filenames;

  for (i = j = 0; pathname[i]; i++, j++)
    {
      if (QUOTED_CHAR (pathname[i]))
	{
	  temp[j++] = '\\';
	  temp[j] = DEQUOTE_CHAR (pathname[i]);
	}
      else
	temp[j] = pathname[i];
    }
  temp[j] = '\0';

  results = glob_filename (temp);

  if (results && results != (char **)-1)
    sort_char_array (results);

  return (results);
}
#endif /* !USE_GLOB_LIBRARY */

/*************************************************
 *						 *
 *	Functions to manage special variables	 *
 *						 *
 *************************************************/

/* An alist of name.function for each special variable.  Most of the
   functions don't do much, and in fact, this would be faster with a
   switch statement, but by the end of this file, I am sick of switch
   statements. */

/* The functions that get called. */
int
  sv_path (), sv_mail (), sv_terminal (), sv_histsize (), sv_histfilesize (),
  sv_uids (), sv_ignoreeof (), sv_glob_dot_filenames (), sv_histchars (),
  sv_nolinks (), sv_hostname_completion_file (), sv_history_control (),
  sv_noclobber (), sv_allow_null_glob_expansion (),
  sv_command_oriented_history ();

#if defined (GETOPTS_BUILTIN)
int sv_optind (), sv_opterr ();
#endif /* GETOPTS_BUILTIN */

#if defined (JOB_CONTROL)
extern int sv_notify ();
#endif

struct name_and_function {
  char *name;
  Function *function;
} special_vars[] = {
  { "PATH", sv_path },
  { "MAIL", sv_mail },
  { "MAILPATH", sv_mail },
  { "MAILCHECK", sv_mail },
  { "TERMCAP", sv_terminal },
  { "TERM", sv_terminal },
  { "HISTSIZE", sv_histsize },
  { "HISTFILESIZE", sv_histfilesize },
  { "EUID", sv_uids},
  { "UID", sv_uids},
  { "IGNOREEOF", sv_ignoreeof },
  { "ignoreeof", sv_ignoreeof },
#if defined (GETOPTS_BUILTIN)
  { "OPTIND", sv_optind },
  { "OPTERR", sv_opterr },
#endif /* GETOPTS_BUILTIN */
#if defined (JOB_CONTROL)
  { "notify", sv_notify },
#endif  /* JOB_CONTROL */
  { "glob_dot_filenames", sv_glob_dot_filenames },
  { "allow_null_glob_expansion", sv_allow_null_glob_expansion },
  { "command_oriented_history", sv_command_oriented_history },
  { "histchars", sv_histchars },
  { "hostname_completion_file", sv_hostname_completion_file },
  { "history_control", sv_history_control },
  { "noclobber", sv_noclobber },
  { "nolinks", sv_nolinks },
  { (char *)0x00, (Function *)0x00 }
};

/* The variable in NAME has just had its state changed.  Check to see if it
   is one of the special ones where something special happens. */
stupidly_hack_special_variables (name)
     char *name;
{
  int i = 0;

  while (special_vars[i].name)
    {
      if (STREQ (special_vars[i].name, name))
	{
	  (*(special_vars[i].function)) (name);
	  return;
	}
      i++;
    }
}

/* Set/unset noclobber. */
sv_noclobber (name)
     char *name;
{
  extern int noclobber;

  if (find_variable (name))
    noclobber = 1;
  else
    noclobber = 0;
}

/* What to do just after the PATH variable has changed. */
sv_path (name)
     char *name;
{
  /* hash -r */
  WORD_LIST *args;

  args = make_word_list (make_word ("-r"), NULL);
  hash_builtin (args);
  dispose_words (args);
}

/* What to do just after one of the MAILxxxx variables has changed.  NAME
   is the name of the variable.  */
sv_mail (name)
     char *name;
{
  /* If the time interval for checking the files has changed, then
     reset the mail timer.  Otherwise, one of the pathname vars
     to the users mailbox has changed, so rebuild the array of
     filenames. */
  if (strcmp (name, "MAILCHECK") == 0)
    reset_mail_timer ();
  else
    {
      if ((strcmp (name, "MAIL") == 0) || (strcmp (name, "MAILPATH") == 0))
	{
	  free_mail_files ();
	  remember_mail_dates ();
	}
    }
}

/* What to do just after one of the TERMxxx variables has changed.
   If we are an interactive shell, then try to reset the terminal
   information in readline. */
sv_terminal (name)
     char *name;
{
  extern int interactive;

  if (interactive)
    rl_reset_terminal (get_string_value ("TERM"));
}

/* What to do after the HISTSIZE variable changes.
   If there is a value for this variable (and it is numeric), then stifle
   the history.  Otherwise, if there is NO value for this variable,
   unstifle the history. */
sv_histsize (name)
     char *name;
{
  char *temp = get_string_value (name);

  if (temp)
    {
      int num;
      if (sscanf (temp, "%d", &num) == 1)
	{
	  extern int history_lines_this_session;

	  stifle_history (num);
	  if (history_lines_this_session > where_history ())
	    history_lines_this_session = where_history ();
	}
    }
  else
    unstifle_history ();
}

/* What to do if the HISTFILESIZE variable changes. */
sv_histfilesize (name)
     char *name;
{
  char *temp = get_string_value (name);

  if (temp)
    {
      extern int history_lines_in_file;
      int num;
      if (sscanf (temp, "%d", &num) == 1)
	{
	  history_truncate_file (get_string_value ("HISTFILE"), num);
	  if (num <= history_lines_in_file)
	    history_lines_in_file = num;
	}
    }
}

/* A nit for picking at history saving.
   Value of 0 means save all lines parsed by the shell on the history.
   Value of 1 means save all lines that do not start with a space.
   Value of 2 means save all lines that do not match the last line saved. */
int history_control = 0;

/* What to do after the HISTORY_CONTROL variable changes. */
sv_history_control (name)
     char *name;
{
  char *temp = get_string_value (name);

  history_control = 0;

  if (temp && *temp)
    {
      if (strcmp (temp, "ignorespace") == 0)
	history_control = 1;
      else if (strcmp (temp, "ignoredups") == 0)
	history_control = 2;
    }
}

/* By default, every line is saved in the history individually.  I.e.,
   if the user enters:
	bash$ for i in a b c
        > do
        > echo $i
        > done
   Each line will be individually saved in the history. 
	bash$ history
	10  for i in a b c
        11  do
        12  echo $i
        13  done
        14  history
   If the variable command_oriented_history is set, multiple lines
   which form one command will be saved as one history entry.
	bash$ for i in a b c
        > do
        > echo $i
        > done
        bash$ history
	10  for i in a b c
    do
    echo $i
    done
        11  history
   The user can then recall the whole command all at once instead
   of just being able to recall one line at a time.
   */
int command_oriented_history = 0;

/* What to do after the COMMAND_ORIENTED_HISTORY variable changes. */
sv_command_oriented_history (name)
     char *name;
{
  if (find_variable (name) != (SHELL_VAR *)NULL)
    command_oriented_history = 1;
  else
    command_oriented_history = 0;
}

/* If the variable exists, then the value of it can be the number
   of times we actually ignore the EOF.  The default is small,
   (smaller than csh, anyway). */
sv_ignoreeof (name)
     char *name;
{
  extern int eof_encountered, eof_encountered_limit;
  char *temp = get_string_value (name);
  int new_limit;

  eof_encountered = 0;

  if (temp && (sscanf (temp, "%d", &new_limit) == 1))
    eof_encountered_limit = new_limit;
  else
    eof_encountered_limit = 10; /* csh uses 26. */
}

/* Control whether * matches .files in globbing.  Yechh. */
int glob_dot_filenames = 0;

sv_glob_dot_filenames (name)
     char *name;
{
  if (find_variable (name) != (SHELL_VAR *)NULL)
    glob_dot_filenames = 1;
  else
    glob_dot_filenames = 0;
}

/* Setting/unsetting of the history expansion character. */
char old_history_expansion_char = '!';
char old_history_comment_char = '#';
char old_history_subst_char = '^';

sv_histchars (name)
     char *name;
{
  extern char history_expansion_char;
  extern char history_comment_char;
  extern char history_subst_char;
  char *temp = get_string_value (name);

  if (temp)
    {
      old_history_expansion_char = history_expansion_char;
      history_expansion_char = *temp;

      if (temp[1])
	{
	  old_history_subst_char = history_subst_char;
	  history_subst_char = temp[1];

	  if (temp[2])
	    {
	      old_history_comment_char = history_comment_char;
	      history_comment_char = temp[2];
	    }
	}
    }
  else
    {
      history_expansion_char = '!';
      history_subst_char = '^';
      history_comment_char = '#';
    }
}

#if defined (JOB_CONTROL)
/* Job notification feature desired? */
sv_notify (name)
     char *name;
{
  extern int asynchronous_notification;

  if (get_string_value (name))
    asynchronous_notification = 1;
  else
    asynchronous_notification = 0;
}
#endif  /* JOB_CONTROL */

/* If the variable `nolinks' exists, it specifies that symbolic links are
   not to be followed in `cd' commands. */
sv_nolinks (name)
     char *name;
{
  extern int follow_symbolic_links;

  follow_symbolic_links = !find_variable (name);
}

/* Don't let users hack the user id variables. */
sv_uids (name)
     char *name;
{
  int uid = getuid ();
  int euid = geteuid ();
  char buff[10];
  register SHELL_VAR *v;

  sprintf (buff, "%d", uid);
  v = find_variable ("UID");
  if (v)
    v->attributes &= ~att_readonly;

  v = bind_variable ("UID", buff);
  v->attributes |= (att_readonly | att_integer);

  sprintf (buff, "%d", euid);
  v = find_variable ("EUID");
  if (v)
    v->attributes &= ~att_readonly;

  v = bind_variable ("EUID", buff);
  v->attributes |= (att_readonly | att_integer);
}

sv_hostname_completion_file (name)
     char *name;
{
  extern int hostname_list_initialized;

  hostname_list_initialized = 0;
}

sv_allow_null_glob_expansion (name)
     char *name;
{
  allow_null_glob_expansion = (int)find_variable (name);
}

#if defined (GETOPTS_BUILTIN)
sv_optind (name)
     char *name;
{
  char *tt = get_string_value ("OPTIND");
  int s = 0;

  if (tt && *tt)
    {
      s = atoi (tt);

      /* According to POSIX, setting OPTIND=1 resets the internal state
	 of getopt (). */
      if (s < 0 || s == 1)
	s = 0;
    }
  getopts_reset (s);
}

int
sv_opterr (name)
     char *name;
{
  char *tt = get_string_value ("OPTERR");
  int s = 1;
  extern int opterr;

  if (tt)
    s = atoi (tt);
  opterr = s;
  return (0);
}
#endif /* GETOPTS_BUILTIN */
