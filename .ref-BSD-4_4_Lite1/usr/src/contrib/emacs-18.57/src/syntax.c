/* GNU Emacs routines to deal with syntax tables; also word and list parsing.
   Copyright (C) 1985, 1987, 1990 Free Software Foundation, Inc.

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


#include "config.h"
#include <ctype.h>
#include "lisp.h"
#include "commands.h"
#include "buffer.h"
#include "syntax.h"

Lisp_Object Qsyntax_table_p;

DEFUN ("syntax-table-p", Fsyntax_table_p, Ssyntax_table_p, 1, 1, 0,
  "Return t if ARG is a syntax table.\n\
Any vector of 256 elements will do.")
  (obj)
     Lisp_Object obj;
{
  if (XTYPE (obj) == Lisp_Vector && XVECTOR (obj)->size == 0400)
    return Qt;
  return Qnil;
}

Lisp_Object
check_syntax_table (obj)
     Lisp_Object obj;
{
  register Lisp_Object tem;
  while (tem = Fsyntax_table_p (obj),
	 NULL (tem))
    obj = wrong_type_argument (Qsyntax_table_p, obj, 0);
  return obj;
}   


DEFUN ("syntax-table", Fsyntax_table, Ssyntax_table, 0, 0, 0,
  "Return the current syntax table.\n\
This is the one specified by the current buffer.")
  ()
{
  return current_buffer->syntax_table;
}

DEFUN ("standard-syntax-table", Fstandard_syntax_table,
   Sstandard_syntax_table, 0, 0, 0,
  "Return the standard syntax table.\n\
This is the one used for new buffers.")
  ()
{
  return Vstandard_syntax_table;
}

DEFUN ("copy-syntax-table", Fcopy_syntax_table, Scopy_syntax_table, 0, 1, 0,
  "Construct a new syntax table and return it.\n\
It is a copy of the TABLE, which defaults to the standard syntax table.")
  (table)
     Lisp_Object table;
{
  Lisp_Object size, val;
  XFASTINT (size) = 0400;
  XFASTINT (val) = 0;
  val = Fmake_vector (size, val);
  if (!NULL (table))
    table = check_syntax_table (table);
  else if (NULL (Vstandard_syntax_table))
    /* Can only be null during initialization */
    return val;
  else table = Vstandard_syntax_table;

  bcopy (XVECTOR (table)->contents,
	 XVECTOR (val)->contents, 0400 * sizeof (Lisp_Object));
  return val;
}

DEFUN ("set-syntax-table", Fset_syntax_table, Sset_syntax_table, 1, 1, 0,
  "Select a new syntax table for the current buffer.\n\
One argument, a syntax table.")
  (table)
     Lisp_Object table;
{
  table = check_syntax_table (table);
  current_buffer->syntax_table = table;
  /* Indicate that this buffer now has a specified syntax table.  */
  current_buffer->local_var_flags |= buffer_local_flags.syntax_table;
  return table;
}

/* Convert a letter which signifies a syntax code
 into the code it signifies.
 This is used by modify-syntax-entry, and other things. */

char syntax_spec_code[0400] =
  { 0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    (char) Swhitespace, 0377, (char) Sstring, 0377,
        (char) Smath, 0377, 0377, (char) Squote,
    (char) Sopen, (char) Sclose, 0377, 0377,
	0377, (char) Swhitespace, (char) Spunct, (char) Scharquote,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377,
	(char) Scomment, 0377, (char) Sendcomment, 0377,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,   /* @, A, ... */
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, (char) Sword,
    0377, 0377, 0377, 0377, (char) Sescape, 0377, 0377, (char) Ssymbol,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,   /* `, a, ... */
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, (char) Sword,
    0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377
  };

/* Indexed by syntax code, give the letter that describes it. */

char syntax_code_spec[13] =
  {
    ' ', '.', 'w', '_', '(', ')', '\'', '\"', '$', '\\', '/', '<', '>'
  };

DEFUN ("char-syntax", Fchar_syntax, Schar_syntax, 1, 1, 0,
  "Return the syntax code of CHAR, described by a character.\n\
For example, if CHAR is a word constituent, ?w is returned.\n\
The characters that correspond to various syntax codes\n\
are listed in the documentation of  modify-syntax-entry.")
  (ch)
     Lisp_Object ch;
{
  CHECK_NUMBER (ch, 0);
  return make_number (syntax_code_spec[(int) SYNTAX (0xFF & XINT (ch))]);
}

/* This comment supplies the doc string for modify-syntax-entry,
   for make-docfile to see.  We cannot put this in the real DEFUN
   due to limits in the Unix cpp.

DEFUN ("modify-syntax-entry", foo, bar, 0, 0, 0,
  "Set syntax for character CHAR according to string S.\n\
The syntax is changed only for table TABLE, which defaults to\n\
 the current buffer's syntax table.\n\
The first character of S should be one of the following:\n\
  Space    whitespace syntax.    w   word constituent.\n\
  _        symbol constituent.   .   punctuation.\n\
  (        open-parenthesis.     )   close-parenthesis.\n\
  \"        string quote.         \\   character-quote.\n\
  $        paired delimiter.     '   expression prefix operator.\n\
  <	   comment starter.	 >   comment ender.\n\
Only single-character comment start and end sequences are represented thus.\n\
Two-character sequences are represented as described below.\n\
The second character of S is the matching parenthesis,\n\
 used only if the first character is ( or ).\n\
Any additional characters are flags.\n\
Defined flags are the characters 1, 2, 3 and 4.\n\
 1 means C is the start of a two-char comment start sequence.\n\
 2 means C is the second character of such a sequence.\n\
 3 means C is the start of a two-char comment end sequence.\n\
 4 means C is the second character of such a sequence.")

*/

DEFUN ("modify-syntax-entry", Fmodify_syntax_entry, Smodify_syntax_entry, 2, 3, 
  /* I really don't know why this is interactive
     help-form should at least be made useful whilst reading the second arg
   */
  "cSet syntax for character: \nsSet syntax for %s to: ",
  0 /* See immediately above */)
  (c, newentry, syntax_table)
     Lisp_Object c, newentry, syntax_table;
{
  register unsigned char *p, match;
  register enum syntaxcode code;
  Lisp_Object val;

  CHECK_NUMBER (c, 0);
  CHECK_STRING (newentry, 1);
  if (NULL (syntax_table))
    syntax_table = current_buffer->syntax_table;
  else
    syntax_table = check_syntax_table (syntax_table);

  p = XSTRING (newentry)->data;
  code = (enum syntaxcode) syntax_spec_code[*p++];
  if (((int) code & 0377) == 0377)
    error ("invalid syntax description letter: %c", c);

  match = *p;
  if (match) p++;
  if (match == ' ') match = 0;

  XFASTINT (val) = (match << 8) + (int) code;
  while (*p)
    switch (*p++)
      {
      case '1':
	XFASTINT (val) |= 1 << 16;
	break;

      case '2':
	XFASTINT (val) |= 1 << 17;
	break;

      case '3':
	XFASTINT (val) |= 1 << 18;
	break;

      case '4':
	XFASTINT (val) |= 1 << 19;
	break;
      }
	
  XVECTOR (syntax_table)->contents[0xFF & XINT (c)] = val;

  return Qnil;
}

/* Dump syntax table to buffer in human-readable format */

describe_syntax (value)
    Lisp_Object value;
{
  register enum syntaxcode code;
  char desc, match, start1, start2, end1, end2;
  char str[2];

  Findent_to (make_number (16), make_number (1));

  if (XTYPE (value) != Lisp_Int)
    {
      InsStr ("invalid");
      return;
    }

  code = (enum syntaxcode) (XINT (value) & 0377);
  match = (XINT (value) >> 8) & 0377;
  start1 = (XINT (value) >> 16) & 1;
  start2 = (XINT (value) >> 17) & 1;
  end1 = (XINT (value) >> 18) & 1;
  end2 = (XINT (value) >> 19) & 1;

  if ((int) code < 0 || (int) code >= (int) Smax)
    {
      InsStr ("invalid");
      return;
    }
  desc = syntax_code_spec[(int) code];

  str[0] = desc, str[1] = 0;
  insert (str, 1);

  str[0] = match ? match : ' ';
  insert (str, 1);


  if (start1)
    insert ("1", 1);
  if (start2)
    insert ("2", 1);

  if (end1)
    insert ("3", 1);
  if (end2)
    insert ("4", 1);

  InsStr ("\twhich means: ");

#ifdef SWITCH_ENUM_BUG
  switch ((int) code)
#else
  switch (code)
#endif
    {
    case Swhitespace:
      InsStr ("whitespace"); break;
    case Spunct:
      InsStr ("punctuation"); break;
    case Sword:
      InsStr ("word"); break;
    case Ssymbol:
      InsStr ("symbol"); break;
    case Sopen:
      InsStr ("open"); break;
    case Sclose:
      InsStr ("close"); break;
    case Squote:
      InsStr ("quote"); break;
    case Sstring:
      InsStr ("string"); break;
    case Smath:
      InsStr ("math"); break;
    case Sescape:
      InsStr ("escape"); break;
    case Scharquote:
      InsStr ("charquote"); break;
    case Scomment:
      InsStr ("comment"); break;
    case Sendcomment:
      InsStr ("endcomment"); break;
    default:
      InsStr ("invalid");
      return;
    }

  if (match)
    {
      InsStr (", matches ");
      
      str[0] = match, str[1] = 0;
      insert (str, 1);
    }

  if (start1)
    InsStr (",\n\t  is the first character of a comment-start sequence");
  if (start2)
    InsStr (",\n\t  is the second character of a comment-start sequence");

  if (end1)
    InsStr (",\n\t  is the first character of a comment-end sequence");
  if (end2)
    InsStr (",\n\t  is the second character of a comment-end sequence");

  InsStr ("\n");
}

Lisp_Object
describe_syntax_1 (vector)
     Lisp_Object vector;
{
  struct buffer *old = current_buffer;
  set_buffer_internal (XBUFFER (Vstandard_output));
  describe_vector (vector, Qnil, describe_syntax, 0, Qnil);
  set_buffer_internal (old);
  return Qnil;
}

DEFUN ("describe-syntax", Fdescribe_syntax, Sdescribe_syntax, 0, 0, "",
  "Describe the syntax specifications in the syntax table.\n\
The descriptions are inserted in a buffer, which is selected so you can see it.")
  ()
{
  internal_with_output_to_temp_buffer
     ("*Help*", describe_syntax_1, current_buffer->syntax_table);

  return Qnil;
}

/* Return the position across `count' words from `from'.
   If that many words cannot be found before the end of the buffer, return 0.
   `count' negative means scan backward and stop at word beginning.  */

scan_words (from, count)
     register int from, count;
{
  register int beg = BEGV;
  register int end = ZV;

  immediate_quit = 1;
  QUIT;

  while (count > 0)
    {
      while (1)
	{
	  if (from == end)
	    {
	      immediate_quit = 0;
	      return 0;
	    }
	  if (SYNTAX(FETCH_CHAR (from)) == Sword)
	    break;
	  from++;
	}
      while (1)
	{
	  if (from == end) break;
	  if (SYNTAX(FETCH_CHAR (from)) != Sword)
	    break;
	  from++;
	}
      count--;
    }
  while (count < 0)
    {
      while (1)
	{
	  if (from == beg)
	    {
	      immediate_quit = 0;
	      return 0;
	    }
	  if (SYNTAX(FETCH_CHAR (from - 1)) == Sword)
	    break;
	  from--;
	}
      while (1)
	{
	  if (from == beg) break;
	  if (SYNTAX(FETCH_CHAR (from - 1)) != Sword)
	    break;
	  from--;
	}
      count++;
    }

  immediate_quit = 0;

  return from;
}

DEFUN ("forward-word", Fforward_word, Sforward_word, 1, 1, "p",
  "Move point forward ARG words (backward if ARG is negative).\n\
Normally returns t.\n\
If an edge of the buffer is reached, point is left there\n\
and nil is returned.")
  (count)
     Lisp_Object count;
{
  int val;
  CHECK_NUMBER (count, 0);

  if (!(val = scan_words (point, XINT (count))))
    {
      SET_PT (XINT (count) > 0 ? ZV : BEGV);
      return Qnil;
    }
  SET_PT (val);
  return Qt;
}

int parse_sexp_ignore_comments;

Lisp_Object
scan_lists (from, count, depth, sexpflag)
     register int from;
     int count, depth, sexpflag;
{
  Lisp_Object val;
  register int stop;
  register int c;
  char stringterm;
  int quoted;
  int mathexit = 0;
  register enum syntaxcode code;
  int min_depth = depth;    /* Err out if depth gets less than this. */

  if (depth > 0) min_depth = 0;

  immediate_quit = 1;
  QUIT;

  while (count > 0)
    {
      stop = ZV;
      while (from < stop)
	{
	  c = FETCH_CHAR (from);
	  code = SYNTAX(c);
	  from++;
	  if (from < stop && SYNTAX_COMSTART_FIRST (c)
	      && SYNTAX_COMSTART_SECOND (FETCH_CHAR (from))
	      && parse_sexp_ignore_comments)
	    code = Scomment, from++;

#ifdef SWITCH_ENUM_BUG
	  switch ((int) code)
#else
	  switch (code)
#endif
	    {
	    case Sescape:
	    case Scharquote:
	      if (from == stop) goto lose;
	      from++;
	      /* treat following character as a word constituent */
	    case Sword:
	    case Ssymbol:
	      if (depth || !sexpflag) break;
	      /* This word counts as a sexp; return at end of it. */
	      while (from < stop)
		{
#ifdef SWITCH_ENUM_BUG
		  switch ((int) SYNTAX(FETCH_CHAR (from)))
#else
		  switch (SYNTAX(FETCH_CHAR (from)))
#endif
		    {
		    case Scharquote:
		    case Sescape:
		      from++;
		      if (from == stop) goto lose;
		      break;
		    case Sword:
		    case Ssymbol:
		    case Squote:
		      break;
		    default:
		      goto done;
		    }
		  from++;
		}
	      goto done;

	    case Scomment:
	      if (!parse_sexp_ignore_comments) break;
	      while (1)
		{
		  if (from == stop) goto done;
		  if (SYNTAX (c = FETCH_CHAR (from)) == Sendcomment)
		    break;
		  from++;
		  if (from < stop && SYNTAX_COMEND_FIRST (c)
		       && SYNTAX_COMEND_SECOND (FETCH_CHAR (from)))
		    { from++; break; }
		}
	      break;

	    case Smath:
	      if (!sexpflag)
		break;
	      if (from != stop && c == FETCH_CHAR (from))
		from++;
	      if (mathexit)
		{
		  mathexit = 0;
		  goto close1;
		}
	      mathexit = 1;

	    case Sopen:
	      if (!++depth) goto done;
	      break;

	    case Sclose:
	    close1:
	      if (!--depth) goto done;
	      if (depth < min_depth)
		error ("Containing expression ends prematurely");
	      break;

	    case Sstring:
	      stringterm = FETCH_CHAR (from - 1);
	      while (1)
		{
		  if (from >= stop) goto lose;
		  if (FETCH_CHAR (from) == stringterm) break;
#ifdef SWITCH_ENUM_BUG
		  switch ((int) SYNTAX(FETCH_CHAR (from)))
#else
		  switch (SYNTAX(FETCH_CHAR (from)))
#endif
		    {
		    case Scharquote:
		    case Sescape:
		      from++;
		    }
		  from++;
		}
	      from++;
	      if (!depth && sexpflag) goto done;
	      break;
	    }
	}

      /* Reached end of buffer.  Error if within object, return nil if between */
      if (depth) goto lose;

      immediate_quit = 0;
      return Qnil;

      /* End of object reached */
    done:
      count--;
    }

  while (count < 0)
    {
      stop = BEGV;
      while (from > stop)
	{
	  from--;
	  if (quoted = char_quoted (from))
	    from--;
	  c = FETCH_CHAR (from);
	  code = SYNTAX (c);
	  if (from > stop && SYNTAX_COMEND_SECOND (c)
	      && SYNTAX_COMEND_FIRST (FETCH_CHAR (from - 1))
	      && !char_quoted (from - 1)
	      && parse_sexp_ignore_comments)
	    code = Sendcomment, from--;

#ifdef SWITCH_ENUM_BUG
	  switch ((int) (quoted ? Sword : code))
#else
	  switch (quoted ? Sword : code)
#endif
	    {
	    case Sword:
	    case Ssymbol:
	      if (depth || !sexpflag) break;
	      /* This word counts as a sexp; count object finished after passing it. */
	      while (from > stop)
		{
		  if (quoted = char_quoted (from - 1))
		    from--;
		  if (! (quoted || SYNTAX(FETCH_CHAR (from - 1)) == Sword ||
			 SYNTAX(FETCH_CHAR (from - 1)) == Ssymbol ||
			 SYNTAX(FETCH_CHAR (from - 1)) == Squote))
            	    goto done2;
		  from--;
		}
	      goto done2;

	    case Smath:
	      if (!sexpflag)
		break;
	      if (from != stop && c == FETCH_CHAR (from - 1))
		from--;
	      if (mathexit)
		{
		  mathexit = 0;
		  goto open2;
		}
	      mathexit = 1;

	    case Sclose:
	      if (!++depth) goto done2;
	      break;

	    case Sopen:
	    open2:
	      if (!--depth) goto done2;
	      if (depth < min_depth)
		error ("Containing expression ends prematurely");
	      break;

	    case Sendcomment:
	      if (!parse_sexp_ignore_comments) break;
	      if (from != stop) from--;
	      while (1)
		{
		  if (SYNTAX (c = FETCH_CHAR (from)) == Scomment)
		    break;
		  if (from == stop) goto done;
		  from--;
		  if (SYNTAX_COMSTART_SECOND (c)
		      && SYNTAX_COMSTART_FIRST (FETCH_CHAR (from))
		      && !char_quoted (from))
		    break;
		}
	      break;

	    case Sstring:
	      stringterm = FETCH_CHAR (from);
	      while (1)
		{
		  if (from == stop) goto lose;
		  if (!char_quoted (from - 1)
		      && stringterm == FETCH_CHAR (from - 1))
		    break;
		  from--;
		}
	      from--;
	      if (!depth && sexpflag) goto done2;
	      break;
	    }
	}

      /* Reached start of buffer.  Error if within object, return nil if between */
      if (depth) goto lose;

      immediate_quit = 0;
      return Qnil;

    done2:
      count++;
    }


  immediate_quit = 0;
  XFASTINT (val) = from;
  return val;

 lose:
  error ("Unbalanced parentheses");
  /* NOTREACHED */
}

char_quoted (pos)
     register int pos;
{
  register enum syntaxcode code;
  register int beg = BEGV;
  register int quoted = 0;

  while (pos > beg &&
	 ((code = SYNTAX (FETCH_CHAR (pos - 1))) == Scharquote
	  || code == Sescape))
    pos--, quoted = !quoted;
  return quoted;
}

DEFUN ("scan-lists", Fscan_lists, Sscan_lists, 3, 3, 0,
  "Scan from character number FROM by COUNT lists.\n\
Returns the character number of the position thus found.\n\
\n\
If DEPTH is nonzero, paren depth begins counting from that value,\n\
only places where the depth in parentheses becomes zero\n\
are candidates for stopping; COUNT such places are counted.\n\
Thus, a positive value for DEPTH means go out levels.\n\
\n\
Comments are ignored if parse-sexp-ignore-comments is non-nil.\n\
\n\
If the beginning or end of (the visible part of) the buffer is reached\n\
and the depth is wrong, an error is signaled.\n\
If the depth is right but the count is not used up, nil is returned.")
  (from, count, depth)
     Lisp_Object from, count, depth;
{
  CHECK_NUMBER (from, 0);
  CHECK_NUMBER (count, 1);
  CHECK_NUMBER (depth, 2);

  return scan_lists (XINT (from), XINT (count), XINT (depth), 0);
}

DEFUN ("scan-sexps", Fscan_sexps, Sscan_sexps, 2, 2, 0,
  "Scan from character number FROM by COUNT balanced expressions.\n\
Returns the character number of the position thus found.\n\
\n\
Comments are ignored if parse-sexp-ignore-comments is non-nil.\n\
\n\
If the beginning or end of (the visible part of) the buffer is reached\n\
in the middle of a parenthetical grouping, an error is signaled.\n\
If the beginning or end is reached between groupings but before count is used up,\n\
nil is returned.")
  (from, count)
     Lisp_Object from, count;
{
  CHECK_NUMBER (from, 0);
  CHECK_NUMBER (count, 1);

  return scan_lists (XINT (from), XINT (count), 0, 1);
}

DEFUN ("backward-prefix-chars", Fbackward_prefix_chars, Sbackward_prefix_chars,
  0, 0, 0,
  "Move point backward over any number of chars with syntax \"prefix\".")
  ()
{
  int beg = BEGV;
  int pos = point;

  while (pos > beg && !char_quoted (pos - 1) && SYNTAX (FETCH_CHAR (pos - 1)) == Squote)
    pos--;

  SET_PT (pos);

  return Qnil;
}

struct lisp_parse_state
  {
    int depth;		/* Depth at end of parsing */
    int instring;	/* -1 if not within string, else desired terminator. */
    int incomment;	/* Nonzero if within a comment at end of parsing */
    int quoted;		/* Nonzero if just after an escape char at end of parsing */
    int thislevelstart;	/* Char number of most recent start-of-expression at current level */
    int prevlevelstart; /* Char number of start of containing expression */
    int location;	/* Char number at which parsing stopped. */
    int mindepth;	/* Minimum depth seen while scanning.  */
  };

/* Parse forward from FROM to END,
   assuming that FROM is the start of a function, 
   and return a description of the state of the parse at END. */

struct lisp_parse_state val_scan_sexps_forward;

struct lisp_parse_state *
scan_sexps_forward (from, end, targetdepth, stopbefore, oldstate)
     register int from;
     int end, targetdepth, stopbefore;
     Lisp_Object oldstate;
{
  struct lisp_parse_state state;

  register enum syntaxcode code;
  struct level { int last, prev; };
  struct level levelstart[100];
  register struct level *curlevel = levelstart;
  struct level *endlevel = levelstart + 100;
  char prev;
  register int depth;	/* Paren depth of current scanning location.
			   level - levelstart equals this except
			   when the depth becomes negative.  */
  int mindepth;		/* Lowest DEPTH value seen.  */
  int start_quoted = 0;		/* Nonzero means starting after a char quote */
  Lisp_Object tem;

  immediate_quit = 1;
  QUIT;

  if (NULL (oldstate))
    {
      depth = 0;
      state.instring = -1;
      state.incomment = 0;
    }
  else
    {
      tem = Fcar (oldstate);
      if (!NULL (tem))
	depth = XINT (tem);
      else
	depth = 0;

      oldstate = Fcdr (oldstate);
      oldstate = Fcdr (oldstate);
      oldstate = Fcdr (oldstate);
      tem = Fcar (oldstate);
      state.instring = !NULL (tem) ? XINT (tem) : -1;

      oldstate = Fcdr (oldstate);
      tem = Fcar (oldstate);
      state.incomment = !NULL (tem);

      oldstate = Fcdr (oldstate);
      tem = Fcar (oldstate);
      start_quoted = !NULL (tem);
    }
  state.quoted = 0;
  mindepth = depth;

  curlevel->prev = -1;
  curlevel->last = -1;

  /* Enter the loop at a place appropriate for initial state. */

  if (state.incomment) goto startincomment;
  if (state.instring >= 0)
    {
      if (start_quoted) goto startquotedinstring;
      goto startinstring;
    }
  if (start_quoted) goto startquoted;

  while (from < end)
    {
      code = SYNTAX(FETCH_CHAR (from));
      from++;
      if (from < end && SYNTAX_COMSTART_FIRST (FETCH_CHAR (from - 1))
	   && SYNTAX_COMSTART_SECOND (FETCH_CHAR (from)))
	code = Scomment, from++;
#ifdef SWITCH_ENUM_BUG
      switch ((int) code)
#else
      switch (code)
#endif
	{
	case Sescape:
	case Scharquote:
	  if (stopbefore) goto stop;  /* this arg means stop at sexp start */
	  curlevel->last = from - 1;
	startquoted:
	  if (from == end) goto endquoted;
	  from++;
	  goto symstarted;
	  /* treat following character as a word constituent */
	case Sword:
	case Ssymbol:
	  if (stopbefore) goto stop;  /* this arg means stop at sexp start */
	  curlevel->last = from - 1;
	symstarted:
	  while (from < end)
	    {
#ifdef SWITCH_ENUM_BUG
	      switch ((int) SYNTAX(FETCH_CHAR (from)))
#else
	      switch (SYNTAX(FETCH_CHAR (from)))
#endif
		{
		case Scharquote:
		case Sescape:
		  from++;
		  if (from == end) goto endquoted;
		  break;
		case Sword:
		case Ssymbol:
		case Squote:
		  break;
		default:
		  goto symdone;
		}
	      from++;
	    }
	symdone:
	  curlevel->prev = curlevel->last;
	  break;

	case Scomment:
	  state.incomment = 1;
	startincomment:
	  while (1)
	    {
	      if (from == end) goto done;
	      if (SYNTAX (prev = FETCH_CHAR (from)) == Sendcomment)
		break;
	      from++;
	      if (from < end && SYNTAX_COMEND_FIRST (prev)
		   && SYNTAX_COMEND_SECOND (FETCH_CHAR (from)))
		{ from++; break; }
	    }
	  state.incomment = 0;
	  break;

	case Sopen:
	  if (stopbefore) goto stop;  /* this arg means stop at sexp start */
	  depth++;
	  /* curlevel++->last ran into compiler bug on Apollo */
	  curlevel->last = from - 1;
	  if (++curlevel == endlevel)
	    error ("Nesting too deep for parser");
	  curlevel->prev = -1;
	  curlevel->last = -1;
	  if (!--targetdepth) goto done;
	  break;

	case Sclose:
	  depth--;
	  if (depth < mindepth)
	    mindepth = depth;
	  if (curlevel != levelstart)
	    curlevel--;
	  curlevel->prev = curlevel->last;
	  if (!++targetdepth) goto done;
	  break;

	case Sstring:
	  if (stopbefore) goto stop;  /* this arg means stop at sexp start */
	  curlevel->last = from - 1;
	  state.instring = FETCH_CHAR (from - 1);
	startinstring:
	  while (1)
	    {
	      if (from >= end) goto done;
	      if (FETCH_CHAR (from) == state.instring) break;
#ifdef SWITCH_ENUM_BUG
	      switch ((int) SYNTAX(FETCH_CHAR (from)))
#else
	      switch (SYNTAX(FETCH_CHAR (from)))
#endif
		{
		case Scharquote:
		case Sescape:
		  from++;
		startquotedinstring:
		  if (from >= end) goto endquoted;
		}
	      from++;
	    }
	  state.instring = -1;
	  curlevel->prev = curlevel->last;
	  from++;
	  break;

	case Smath:
	  break;
	}
    }
  goto done;

 stop:   /* Here if stopping before start of sexp. */
  from--;    /* We have just fetched the char that starts it; */
  goto done; /* but return the position before it. */

 endquoted:
  state.quoted = 1;
 done:
  state.depth = depth;
  state.mindepth = mindepth;
  state.thislevelstart = curlevel->prev;
  state.prevlevelstart
    = (curlevel == levelstart) ? -1 : (curlevel - 1)->last;
  state.location = from;
  immediate_quit = 0;

  val_scan_sexps_forward = state;
  return &val_scan_sexps_forward;
}

/* This comment supplies the doc string for parse-partial-sexp,
   for make-docfile to see.  We cannot put this in the real DEFUN
   due to limits in the Unix cpp.

DEFUN ("parse-partial-sexp", Ffoo, Sfoo, 0, 0, 0,
  "Parse Lisp syntax starting at FROM until TO; return status of parse at TO.\n\
Parsing stops at TO or when certain criteria are met;\n\
 point is set to where parsing stops.\n\
If fifth arg STATE is omitted or nil,\n\
 parsing assumes that FROM is the beginning of a function.\n\
Value is a list of seven elements describing final state of parsing:\n\
 1. depth in parens.\n\
 2. character address of start of innermost containing list; nil if none.\n\
 3. character address of start of last complete sexp terminated.\n\
 4. non-nil if inside a string.\n\
    (it is the character that will terminate the string.)\n\
 5. t if inside a comment.\n\
 6. t if following a quote character.\n\
 7. the minimum paren-depth encountered during this scan.\n\
If third arg TARGETDEPTH is non-nil, parsing stops if the depth\n\
in parentheses becomes equal to TARGETDEPTH.\n\
Fourth arg STOPBEFORE non-nil means stop when come to\n\
 any character that starts a sexp.\n\
Fifth arg STATE is a seven-list like what this function returns.\n\
It is used to initialize the state of the parse.")

*/

DEFUN ("parse-partial-sexp", Fparse_partial_sexp, Sparse_partial_sexp, 2, 5, 0,
  0 /* See immediately above */)
  (from, to, targetdepth, stopbefore, oldstate)
     Lisp_Object from, to, targetdepth, stopbefore, oldstate;
{
  struct lisp_parse_state state;
  int target;

  if (!NULL (targetdepth))
    {
      CHECK_NUMBER (targetdepth, 3);
      target = XINT (targetdepth);
    }
  else
    target = -100000;		/* We won't reach this depth */

  validate_region (&from, &to);
  state = *scan_sexps_forward (XINT (from), XINT (to),
			       target, !NULL (stopbefore), oldstate);

  SET_PT (state.location);
  
  return Fcons (make_number (state.depth),
	   Fcons (state.prevlevelstart < 0 ? Qnil : make_number (state.prevlevelstart),
	     Fcons (state.thislevelstart < 0 ? Qnil : make_number (state.thislevelstart),
	       Fcons (state.instring >= 0 ? make_number (state.instring) : Qnil,
		 Fcons (state.incomment ? Qt : Qnil,
		   Fcons (state.quoted ? Qt : Qnil,
			  Fcons (make_number (state.mindepth), Qnil)))))));
}

init_syntax_once ()
{
  register int i;
  register struct Lisp_Vector *v;

  /* Set this now, so first buffer creation can refer to it. */
  /* Make it nil before calling copy-syntax-table
    so that copy-syntax-table will know not to try to copy from garbage */
  Vstandard_syntax_table = Qnil;
  Vstandard_syntax_table = Fcopy_syntax_table (Qnil);

  v = XVECTOR (Vstandard_syntax_table);

  for (i = 'a'; i <= 'z'; i++)
    XFASTINT (v->contents[i]) = (int) Sword;
  for (i = 'A'; i <= 'Z'; i++)
    XFASTINT (v->contents[i]) = (int) Sword;
  for (i = '0'; i <= '9'; i++)
    XFASTINT (v->contents[i]) = (int) Sword;
  XFASTINT (v->contents['$']) = (int) Sword;
  XFASTINT (v->contents['%']) = (int) Sword;

  XFASTINT (v->contents['(']) = (int) Sopen + (')' << 8);
  XFASTINT (v->contents[')']) = (int) Sclose + ('(' << 8);
  XFASTINT (v->contents['[']) = (int) Sopen + (']' << 8);
  XFASTINT (v->contents[']']) = (int) Sclose + ('[' << 8);
  XFASTINT (v->contents['{']) = (int) Sopen + ('}' << 8);
  XFASTINT (v->contents['}']) = (int) Sclose + ('{' << 8);
  XFASTINT (v->contents['"']) = (int) Sstring;
  XFASTINT (v->contents['\\']) = (int) Sescape;

  for (i = 0; i < 10; i++)
    XFASTINT (v->contents["_-+*/&|<>="[i]]) = (int) Ssymbol;

  for (i = 0; i < 12; i++)
    XFASTINT (v->contents[".,;:?!#@~^'`"[i]]) = (int) Spunct;
}

syms_of_syntax ()
{
  Qsyntax_table_p = intern ("syntax-table-p");
  staticpro (&Qsyntax_table_p);

  DEFVAR_BOOL ("parse-sexp-ignore-comments", &parse_sexp_ignore_comments,
    "Non-nil means forward-sexp, etc., should treat comments as whitespace.\n\
Non-nil works only when the comment terminator is something like *\/,\n\
and appears only when it ends a comment.\n\
If comments are terminated by newlines,\n\
you must make this variable nil.");

  defsubr (&Ssyntax_table_p);
  defsubr (&Ssyntax_table);
  defsubr (&Sstandard_syntax_table);
  defsubr (&Scopy_syntax_table);
  defsubr (&Sset_syntax_table);
  defsubr (&Schar_syntax);
  defsubr (&Smodify_syntax_entry);
  defsubr (&Sdescribe_syntax);

  defsubr (&Sforward_word);

  defsubr (&Sscan_lists);
  defsubr (&Sscan_sexps);
  defsubr (&Sbackward_prefix_chars);
  defsubr (&Sparse_partial_sexp);
}
