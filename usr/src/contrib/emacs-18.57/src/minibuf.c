/* Minibuffer input and completion.
   Copyright (C) 1985, 1986, 1990 Free Software Foundation, Inc.

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
#include "lisp.h"
#include "commands.h"
#include "buffer.h"
#include "window.h"
#include "syntax.h"
#include "dispextern.h"

#define min(a, b) ((a) < (b) ? (a) : (b))

/* List of buffers for use as minibuffers.
  The first element of the list is used for the outermost minibuffer invocation,
  the next element is used for a recursive minibuffer invocation, etc.
  The list is extended at the end as deeped minibuffer recursions are encountered. */
Lisp_Object Vminibuffer_list;

struct minibuf_save_data
  {
    char *prompt;
    int prompt_width;
    Lisp_Object help_form;
    Lisp_Object current_prefix_arg;
  };

int minibuf_save_vector_size;
struct minibuf_save_data *minibuf_save_vector;

/* Depth in minibuffer invocations.  */
int minibuf_level;

/* Nonzero means display completion help for invalid input.  */
int completion_auto_help;

/* Fread_minibuffer leaves the input, as a string, here.  */
Lisp_Object last_minibuf_string;

/* Nonzero means let functions called when within a minibuffer 
   invoke recursive minibuffers (to read arguments, or whatever).  */
int enable_recursive_minibuffers;

/* help-form is bound to this while in the minibuffer.  */
Lisp_Object Vminibuffer_help_form;

/* Nonzero means completion ignores case.  */
int completion_ignore_case;

Lisp_Object Quser_variable_p;

/* Width in columns of current minibuffer prompt.  */
extern int minibuf_prompt_width;

/* Actual minibuffer invocation. */

void read_minibuf_unwind ();
Lisp_Object get_minibuffer ();
Lisp_Object read_minibuf ();

Lisp_Object
read_minibuf (map, initial, prompt, expflag)
     Lisp_Object map;
     Lisp_Object initial;
     Lisp_Object prompt;
     int expflag;
{
  register Lisp_Object val;
  int count = specpdl_ptr - specpdl;
  struct gcpro gcpro1, gcpro2;

  if (XTYPE (prompt) != Lisp_String)
    prompt = build_string ("");

  /* Emacs in -batch mode calls minibuffer: print the prompt.  */
  if (noninteractive)
    printf ("%s", XSTRING (prompt)->data);

  if (!enable_recursive_minibuffers &&
      (EQ (selected_window, minibuf_window)))
    error ("Command attempted to use minibuffer while in minibuffer");

  if (minibuf_level == minibuf_save_vector_size)
    minibuf_save_vector =
     (struct minibuf_save_data *) xrealloc (minibuf_save_vector,
		      (minibuf_save_vector_size *= 2) * sizeof (struct minibuf_save_data)); 
  minibuf_save_vector[minibuf_level].prompt = minibuf_prompt;
  minibuf_save_vector[minibuf_level].prompt_width = minibuf_prompt_width;
  minibuf_prompt_width = 0;
  /* >> Why is this done this way rather than binding these variables? */
  minibuf_save_vector[minibuf_level].help_form = Vhelp_form;
  minibuf_save_vector[minibuf_level].current_prefix_arg = Vcurrent_prefix_arg;
  GCPRO2 (minibuf_save_vector[minibuf_level].help_form,
	  minibuf_save_vector[minibuf_level].current_prefix_arg);


  record_unwind_protect (Fset_window_configuration,
			 Fcurrent_window_configuration ());

  val = current_buffer->directory;
  Fset_buffer (get_minibuffer (minibuf_level));
  current_buffer->directory = val;

  Fset_window_buffer (minibuf_window, Fcurrent_buffer ());
  Fselect_window (minibuf_window);
  XFASTINT (XWINDOW (minibuf_window)->hscroll) = 0;

  Ferase_buffer ();
  minibuf_level++;
  record_unwind_protect (read_minibuf_unwind, Qnil);
  Vminibuf_scroll_window = Qnil;

  if (!NULL (initial))
    Finsert (1, &initial);

  minibuf_prompt = (char *) alloca (XSTRING (prompt)->size + 1);
  bcopy (XSTRING (prompt)->data, minibuf_prompt, XSTRING (prompt)->size + 1);
  echo_area_contents = 0;

  Vhelp_form = Vminibuffer_help_form;
  current_buffer->keymap = map;
  recursive_edit_1 ();

  /* If cursor is on the minibuffer line,
     show the user we have exited by putting it in column 0.  */
  if (cursor_vpos >= XFASTINT (XWINDOW (minibuf_window)->top)
      && !noninteractive)
    {
      cursor_hpos = 0;
      update_screen (1, 1);
    }

  /* Make minibuffer contents into a string */
  val = make_string (BEG_ADDR, Z - BEG);
  bcopy (GAP_END_ADDR, XSTRING (val)->data + GPT - BEG, Z - GPT);
  unbind_to (count);
  UNGCPRO;

  /* VAL is the string of minibuffer text.  */

  last_minibuf_string = val;

  /* If Lisp form desired instead of string, parse it */
  if (expflag)
    val = Fread (val);

  return val;
}

/* Return a buffer to be used as the minibuffer at depth `depth'.
 depth = 0 is the lowest allowed argument, and that is the value
 used for nonrecursive minibuffer invocations */

Lisp_Object
get_minibuffer (depth)
     int depth;
{
  Lisp_Object tail, num, buf;
  char name[14];
  extern Lisp_Object nconc2 ();

  XFASTINT (num) = depth;
  tail = Fnthcdr (num, Vminibuffer_list);
  if (NULL (tail))
    {
      tail = Fcons (Qnil, Qnil);
      Vminibuffer_list = nconc2 (Vminibuffer_list, tail);
    }
  buf = Fcar (tail);
  if (NULL (buf) || NULL (XBUFFER (buf)->name))
    {
      sprintf (name, " *Minibuf-%d*", depth);
      buf = Fget_buffer_create (build_string (name));
      XCONS (tail)->car = buf;
    }
  else
    reset_buffer (XBUFFER (buf));
  return buf;
}

/* This function is called on exiting minibuffer, whether normally or not,
 and it restores the current window, buffer, etc. */

void
read_minibuf_unwind ()
{
  /* Erase the minibuffer we were using at this level.  */
  Fset_buffer (XWINDOW (minibuf_window)->buffer);
  Ferase_buffer ();

  /* If this was a recursive minibuffer,
     tie the minibuffer window back to the outer level minibuffer buffer */
  minibuf_level--;
  /* Make sure minibuffer window is erased, not ignored */
  windows_or_buffers_changed++;
  XFASTINT (XWINDOW (minibuf_window)->last_modified) = 0;

  /* Restore prompt from outer minibuffer */
  minibuf_prompt = minibuf_save_vector[minibuf_level].prompt;
  minibuf_prompt_width = minibuf_save_vector[minibuf_level].prompt_width;
  Vhelp_form = minibuf_save_vector[minibuf_level].help_form;
  Vcurrent_prefix_arg = minibuf_save_vector[minibuf_level].current_prefix_arg;
}

DEFUN ("read-from-minibuffer", Fread_from_minibuffer, Sread_from_minibuffer, 1, 4, 0,
  "Read a string from the minibuffer, prompting with string PROMPT.\n\
If optional second arg INITIAL-CONTENTS is non-nil, it is a string\n\
  to be inserted into the minibuffer before reading input.\n\
Third arg KEYMAP is a keymap to use whilst reading; the default is\n\
  minibuffer-local-map.\n\
If fourth arg READ is non-nil, then interpret the result as a lisp object\n\
  and return that object  (ie  (car (read-from-string <input-string>)))")
  (prompt, initial_input, keymap, read)
     Lisp_Object prompt, initial_input, keymap, read;
{
  CHECK_STRING (prompt, 0);
  if (!NULL (initial_input))
    CHECK_STRING (initial_input, 1);
  if (NULL (keymap))
    keymap = Vminibuffer_local_map;
  else
    keymap = get_keymap (keymap,2);
  return read_minibuf (keymap, initial_input, prompt, !NULL(read));
}

DEFUN ("read-minibuffer", Fread_minibuffer, Sread_minibuffer, 1, 2, 0,
  "Return a Lisp object read using the minibuffer.\n\
Prompt with PROMPT.  If non-nil, optional second arg INITIAL-CONTENTS\n\
is a string to insert in the minibuffer before reading.")
  (prompt, initial_contents)
     Lisp_Object prompt, initial_contents;
{
  CHECK_STRING (prompt, 0);
  if (!NULL (initial_contents))
    CHECK_STRING (initial_contents, 1)
  return read_minibuf (Vminibuffer_local_map, initial_contents, prompt, 1);
}

DEFUN ("eval-minibuffer", Feval_minibuffer, Seval_minibuffer, 1, 2, 0,
  "Return value of Lisp expression read using the minibuffer.\n\
Prompt with PROMPT.  If non-nil, optional second arg INITIAL-CONTENTS\n\
is a string to insert in the minibuffer before reading.")
  (prompt, initial_contents)
     Lisp_Object prompt, initial_contents;
{
  return Feval (Fread_minibuffer (prompt, initial_contents));
}

/* Functions that use the minibuffer to read various things. */

DEFUN ("read-string", Fread_string, Sread_string, 1, 2, 0,
  "Read a string from the minibuffer, prompting with string PROMPT.\n\
If non-nil second arg INITIAL-INPUT is a string to insert before reading.")
  (prompt, initial_input)
     Lisp_Object prompt, initial_input;
{
  return Fread_from_minibuffer (prompt, initial_input, Qnil, Qnil);
}

DEFUN ("read-no-blanks-input", Fread_no_blanks_input, Sread_no_blanks_input, 2, 2, 0,
  "Args PROMPT and INIT, strings.  Read a string from the terminal, not allowing blanks.\n\
Prompt with PROMPT, and provide INIT as an initial value of the input string.")
  (prompt, init)
     Lisp_Object prompt, init;
{
  CHECK_STRING (prompt, 0);
  CHECK_STRING (init, 1);

  return read_minibuf (Vminibuffer_local_ns_map, init, prompt, 0);
}

DEFUN ("read-command", Fread_command, Sread_command, 1, 1, 0,
  "One arg PROMPT, a string.  Read the name of a command and return as a symbol.\n\
Prompts with PROMPT.")
  (prompt)
     Lisp_Object prompt;
{
  return Fintern (Fcompleting_read (prompt, Vobarray, Qcommandp, Qt, Qnil),
		  Qnil);
}

#ifdef NOTDEF
DEFUN ("read-function", Fread_function, Sread_function, 1, 1, 0,
  "One arg PROMPT, a string.  Read the name of a function and return as a symbol.\n\
Prompts with PROMPT.")
  (prompt)
     Lisp_Object prompt;
{
  return Fintern (Fcompleting_read (prompt, Vobarray, Qfboundp, Qt, Qnil),
		  Qnil);
}
#endif /* NOTDEF */

DEFUN ("read-variable", Fread_variable, Sread_variable, 1, 1, 0,
  "One arg PROMPT, a string.  Read the name of a user variable and return\n\
it as a symbol.  Prompts with PROMPT.\n\
A user variable is one whose documentation starts with a \"*\" character.")
  (prompt)
     Lisp_Object prompt;
{
  return Fintern (Fcompleting_read (prompt, Vobarray,
				    Quser_variable_p, Qt, Qnil),
		  Qnil);
}

DEFUN ("read-buffer", Fread_buffer, Sread_buffer, 1, 3, 0,
  "One arg PROMPT, a string.  Read the name of a buffer and return as a string.\n\
Prompts with PROMPT.\n\
Optional second arg is value to return if user enters an empty line.\n\
If optional third arg REQUIRE-MATCH is non-nil, only existing buffer names are allowed.")
  (prompt, def, require_match)
     Lisp_Object prompt, def, require_match;
{
  Lisp_Object tem;
  Lisp_Object args[3];
  struct gcpro gcpro1;

  if (XTYPE (def) == Lisp_Buffer)
    def = XBUFFER (def)->name;
  if (!NULL (def))
    {
      args[0] = build_string ("%s(default %s) ");
      args[1] = prompt;
      args[2] = def;
      prompt = Fformat (3, args);
    }
  GCPRO1 (def);
  tem = Fcompleting_read (prompt, Vbuffer_alist, Qnil, require_match, Qnil);
  UNGCPRO;
  if (XSTRING (tem)->size)
    return tem;
  return def;
}

DEFUN ("try-completion", Ftry_completion, Stry_completion, 2, 3, 0,
  "Return common substring of all completions of STRING in ALIST.\n\
Each car of each element of ALIST is tested to see if it begins with STRING.\n\
All that match are compared together; the longest initial sequence\n\
common to all matches is returned as a string.\n\
If there is no match at all, nil is returned.\n\
For an exact match, t is returned.\n\
\n\
ALIST can be an obarray instead of an alist.\n\
Then the print names of all symbols in the obarray are the possible matches.\n\
\n\
If optional third argument PREDICATE is non-nil,\n\
it is used to test each possible match.\n\
The match is a candidate only if PREDICATE returns non-nil.\n\
The argument given to PREDICATE is the alist element or the symbol from the obarray.")
  (string, alist, pred)
     Lisp_Object string, alist, pred;
{
  Lisp_Object bestmatch, tail, elt, eltstring;
  int bestmatchsize;
  int compare, matchsize;
  int list = CONSP (alist) || NULL (alist);
  int index, obsize;
  int matchcount = 0;
  Lisp_Object bucket, zero, end, tem;
  struct gcpro gcpro1, gcpro2, gcpro3;

  CHECK_STRING (string, 0);
  if (!list && XTYPE (alist) != Lisp_Vector)
    return call3 (alist, string, pred, Qnil);

  bestmatch = Qnil;

  if (list)
    tail = alist;
  else
    {
      index = 0;
      obsize = XVECTOR (alist)->size;
      bucket = XVECTOR (alist)->contents[index];
    }

  while (1)
    {
      /* Get the next element of the alist or obarray. */
      /* Exit the loop if the elements are all used up. */
      /* elt gets the alist element or symbol.
	 eltstring gets the name to check as a completion. */

      if (list)
	{
	  if (NULL (tail))
	    break;
	  elt = Fcar (tail);
	  eltstring = Fcar (elt);
	  tail = Fcdr (tail);
	}
      else
	{
	  if (XFASTINT (bucket) != 0)
	    {
	      elt = bucket;
	      eltstring = Fsymbol_name (elt);
	      if (XSYMBOL (bucket)->next)
		XSETSYMBOL (bucket, XSYMBOL (bucket)->next);
	      else
		XFASTINT (bucket) = 0;
	    }
	  else if (++index >= obsize)
	    break;
	  else
	    {
	      bucket = XVECTOR (alist)->contents[index];
	      continue;
	    }
	}

      /* Is this element a possible completion? */

      if (XTYPE (eltstring) == Lisp_String &&
	  XSTRING (string)->size <= XSTRING (eltstring)->size &&
	  0 > scmp (XSTRING (eltstring)->data, XSTRING (string)->data,
		    XSTRING (string)->size))
	{
	  /* Yes. */
	  /* Ignore this element if there is a predicate
	     and the predicate doesn't like it. */

	  if (!NULL (pred))
	    {
	      if (EQ (pred, Qcommandp))
		tem = Fcommandp (elt);
	      else
		{
		  GCPRO3 (string, eltstring, bestmatch);
		  tem = call1 (pred, elt);
		  UNGCPRO;
		}
	      if (NULL (tem)) continue;
	    }

	  /* Update computation of how much all possible completions match */

	  matchcount++;
	  if (NULL (bestmatch))
	    bestmatch = eltstring, bestmatchsize = XSTRING (eltstring)->size;
	  else
	    {
	      compare = min (bestmatchsize, XSTRING (eltstring)->size);
	      matchsize = scmp (XSTRING (bestmatch)->data,
				XSTRING (eltstring)->data,
				compare);
	      bestmatchsize = (matchsize >= 0) ? matchsize : compare;
	    }
	}
    }

  if (NULL (bestmatch))
    return Qnil;		/* No completions found */
  if (matchcount == 1 && bestmatchsize == XSTRING (string)->size)
    return Qt;

  XFASTINT (zero) = 0;		/* Else extract the part in which */
  XFASTINT (end) = bestmatchsize;	     /* all completions agree */
  return Fsubstring (bestmatch, zero, end);
}

/* Compare exactly LEN chars of strings at S1 and S2,
   ignoring case if appropriate.
   Return -1 if strings match,
   else number of chars that match at the beginning.  */

scmp (s1, s2, len)
     register char *s1, *s2;
     int len;
{
  register int l = len;

  if (completion_ignore_case)
    {
      while (l && downcase_table[*s1++] == downcase_table[*s2++])
	l--;
    }
  else
    {
      while (l && *s1++ == *s2++)
	l--;
    }
  if (l == 0)
    return -1;
  else return len - l;
}

DEFUN ("all-completions", Fall_completions, Sall_completions, 2, 3, 0,
  "Search for partial matches to STRING in ALIST.\n\
Each car of each element of ALIST is tested to see if it begins with STRING.\n\
The value is a list of all the strings from ALIST that match.\n\
ALIST can be an obarray instead of an alist.\n\
Then the print names of all symbols in the obarray are the possible matches.\n\
\n\
If optional third argument PREDICATE is non-nil,\n\
it is used to test each possible match.\n\
The match is a candidate only if PREDICATE returns non-nil.\n\
The argument given to PREDICATE is the alist element or the symbol from the obarray.")
  (string, alist, pred)
     Lisp_Object string, alist, pred;
{
  Lisp_Object tail, elt, eltstring;
  Lisp_Object allmatches;
  int list = CONSP (alist) || NULL (alist);
  int index, obsize;
  Lisp_Object bucket, tem;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  CHECK_STRING (string, 0);
  if (!list && XTYPE (alist) != Lisp_Vector)
    {
      return call3 (alist, string, pred, Qt);
    }
  allmatches = Qnil;

  /* If ALIST is not a list, set TAIL just for gc pro.  */
  tail = alist;
  if (! list)
    {
      index = 0;
      obsize = XVECTOR (alist)->size;
      bucket = XVECTOR (alist)->contents[index];
    }

  while (1)
    {
      /* Get the next element of the alist or obarray. */
      /* Exit the loop if the elements are all used up. */
      /* elt gets the alist element or symbol.
	 eltstring gets the name to check as a completion. */

      if (list)
	{
	  if (NULL (tail))
	    break;
	  elt = Fcar (tail);
	  eltstring = Fcar (elt);
	  tail = Fcdr (tail);
	}
      else
	{
	  if (XFASTINT (bucket) != 0)
	    {
	      elt = bucket;
	      eltstring = Fsymbol_name (elt);
	      if (XSYMBOL (bucket)->next)
		XSETSYMBOL (bucket, XSYMBOL (bucket)->next);
	      else
		XFASTINT (bucket) = 0;
	    }
	  else if (++index >= obsize)
	    break;
	  else
	    {
	      bucket = XVECTOR (alist)->contents[index];
	      continue;
	    }
	}

      /* Is this element a possible completion? */

      if (XTYPE (eltstring) == Lisp_String &&
	  XSTRING (string)->size <= XSTRING (eltstring)->size &&
	  XSTRING (eltstring)->data[0] != ' ' &&
	  0 > scmp (XSTRING (eltstring)->data, XSTRING (string)->data,
		    XSTRING (string)->size))
	{
	  /* Yes. */
	  /* Ignore this element if there is a predicate
	     and the predicate doesn't like it. */

	  if (!NULL (pred))
	    {
	      if (EQ (pred, Qcommandp))
		tem = Fcommandp (elt);
	      else
		{
		  GCPRO4 (tail, eltstring, allmatches, string);
		  tem = call1 (pred, elt);
		  UNGCPRO;
		}
	      if (NULL (tem)) continue;
	    }
	  /* Ok => put it on the list. */
	  allmatches = Fcons (eltstring, allmatches);
	}
    }

  return Fnreverse (allmatches);
}

Lisp_Object Vminibuffer_completion_table, Qminibuffer_completion_table;
Lisp_Object Vminibuffer_completion_predicate, Qminibuffer_completion_predicate;
Lisp_Object Vminibuffer_completion_confirm, Qminibuffer_completion_confirm;

DEFUN ("completing-read", Fcompleting_read, Scompleting_read, 2, 5, 0,
  "Read a string in the minibuffer, with completion.\n\
Args are PROMPT, TABLE, PREDICATE, REQUIRE-MATCH and INITIAL-INPUT.\n\
PROMPT is a string to prompt with; normally it ends in a colon and a space.\n\
TABLE is an alist whose elements' cars are strings, or an obarray (see try-completion).\n\
PREDICATE limits completion to a subset of TABLE; see try-completion for details.\n\
If REQUIRE-MATCH is non-nil, the user is not allowed to exit unless\n\
 the input is (or completes to) an element of TABLE.\n\
 If it is also not t, Return does not exit if it does non-null completion.\n\
If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.\n\
Case is ignored if ambient value of  completion-ignore-case  is non-nil.")
  (prompt, table, pred, require_match, init)
     Lisp_Object prompt, table, pred, require_match, init;
{
  Lisp_Object val;
  int count = specpdl_ptr - specpdl;
  specbind (Qminibuffer_completion_table, table);
  specbind (Qminibuffer_completion_predicate, pred);
  specbind (Qminibuffer_completion_confirm,
	    EQ (require_match, Qt) ? Qnil : Qt);
  val = read_minibuf (NULL (require_match)
		      ? Vminibuffer_local_completion_map
		      : Vminibuffer_local_must_match_map,
		      init, prompt, 0);
  unbind_to (count);
  return val;
}

temp_echo_area_contents (m)
     char *m;
{
  int osize = ZV;
  Lisp_Object oinhibit;
  oinhibit = Vinhibit_quit;

  SET_PT (osize);
  InsStr (m);
  SET_PT (osize);
  Vinhibit_quit = Qt;
  Fsit_for (make_number (2), Qnil);
  del_range (point, ZV);
  if (!NULL (Vquit_flag))
    {
      Vquit_flag = Qnil;
      unread_command_char = Ctl ('g');
    }
  Vinhibit_quit = oinhibit;
}

Lisp_Object Fminibuffer_completion_help ();

/* returns:
 * 0 no possible completion
 * 1 was already an exact and unique completion
 * 3 was already an exact completion
 * 4 completed to an exact completion
 * 5 some completion happened
 * 6 no completion happened
 */
int
do_completion ()
{
  Lisp_Object completion, tem;
  int completedp;

  completion = Ftry_completion (Fbuffer_string (), Vminibuffer_completion_table,
				Vminibuffer_completion_predicate);
  if (NULL (completion))
    {
      bell ();
      temp_echo_area_contents (" [No match]");
      return 0;
    }

  if (EQ (completion, Qt))	/* exact and unique match */
    return 1;

  /* compiler bug */
  tem = Fstring_equal (completion, Fbuffer_string());
  if (completedp = NULL (tem))
    {
      Ferase_buffer ();		/* Some completion happened */
      Finsert (1, &completion);
    }

  /* It did find a match.  Do we match some possibility exactly now? */
  if (CONSP (Vminibuffer_completion_table)
      || NULL (Vminibuffer_completion_table))
    tem = Fassoc (Fbuffer_string (), Vminibuffer_completion_table);
  else if (XTYPE (Vminibuffer_completion_table) == Lisp_Vector)
    {
      /* the primitive used by Fintern_soft */
      extern Lisp_Object oblookup ();

      tem = Fbuffer_string ();
      /* Bypass intern-soft as that loses for nil */
      tem = oblookup (Vminibuffer_completion_table,
		      XSTRING (tem)->data, XSTRING (tem)->size);
      if (XTYPE (tem) != Lisp_Symbol)
	tem = Qnil;
      else if (!NULL (Vminibuffer_completion_predicate))
	tem = call1 (Vminibuffer_completion_predicate, tem);
      else
	tem = Qt;
    }
  else
    tem = call3 (Vminibuffer_completion_table,
		 Fbuffer_string (),
		 Vminibuffer_completion_predicate,
		 Qlambda);

  if (NULL (tem))
    { /* not an exact match */
      if (completedp)
	return 5;
      else if (completion_auto_help)
	Fminibuffer_completion_help ();
      else
	temp_echo_area_contents (" [Next char not unique]");
      return 6;
    }
  else
    return (completedp ? 4 : 3);
}
  

DEFUN ("minibuffer-complete", Fminibuffer_complete, Sminibuffer_complete, 0, 0, "",
  "Complete the minibuffer contents as far as possible.")
  ()
{
  register int i = do_completion ();
  switch (i)
    {
    case 0:
      return Qnil;

    case 1:
      temp_echo_area_contents(" [Sole completion]");
      break;

    case 3:
      temp_echo_area_contents(" [Complete, but not unique]");
      break;
    }
  return Qt;
}

DEFUN ("minibuffer-complete-and-exit", Fminibuffer_complete_and_exit,
        Sminibuffer_complete_and_exit, 0, 0, "",
  "Complete the minibuffer contents, and maybe exit.\n\
Exit if the name is valid with no completion needed.\n\
If name was completed to a valid match,\n\
a repetition of this command will exit.")
  ()
{
  register int i;

  /* Allow user to specify null string */
  if (BEGV == ZV)
    goto exit;

  i = do_completion ();
  switch (i)
    {
    case 1:
    case 3:
      goto exit;

    case 4:
      if (!NULL (Vminibuffer_completion_confirm))
	{
	  temp_echo_area_contents(" [Confirm]");
	  return Qnil;
	}
      else
	goto exit;

    default:
      return Qnil;
    }
 exit:
  Fthrow (Qexit, Qnil);
  /* NOTREACHED */
}

DEFUN ("minibuffer-complete-word", Fminibuffer_complete_word, Sminibuffer_complete_word,
  0, 0, "",
  "Complete the minibuffer contents at most a single word.")
  ()
{
  Lisp_Object completion, tem;
  register int i;
  register unsigned char *completion_string;
  /* We keep calling Fbuffer_string
     rather than arrange for GC to hold onto a pointer to
     one of the strings thus made.  */

  completion = Ftry_completion (Fbuffer_string (),
				Vminibuffer_completion_table,
				Vminibuffer_completion_predicate);
  if (NULL (completion))
    {
      bell ();
      temp_echo_area_contents (" [No match]");
      return Qnil;
    }
  if (EQ (completion, Qt))
    return Qnil;

#if 0 /* How the below code used to look, for reference */
  tem = Fbuffer_string ();
  b = XSTRING (tem)->data;
  i = ZV - 1 - XSTRING (completion)->size;
  p = XSTRING (completion)->data;
  if (i > 0 ||
      0 <= scmp (b, p, ZV - 1))
    {
      i = 1;
      /* Set buffer to longest match of buffer tail and completion head. */
      while (0 <= scmp (b + i, p, ZV - 1 - i))
	i++;
      del_range (1, i + 1);
      SET_PT (ZV);
    }
#else /* Rewritten code */
  {
    register unsigned char *buffer_string;
    int buffer_length, completion_length;

    tem = Fbuffer_string ();
    buffer_string = XSTRING (tem)->data;
    completion_string = XSTRING (completion)->data;
    buffer_length = XSTRING (tem)->size; /* ie ZV - BEGV */
    completion_length = XSTRING (completion)->size;
    i = buffer_length - completion_length;
    /* Mly: I don't understand what this is supposed to do AT ALL */
    if (i > 0 ||
	0 <= scmp (buffer_string, completion_string, buffer_length))
      {
	/* Set buffer to longest match of buffer tail and completion head. */
	if (i <= 0) i = 1;
	buffer_string += i;
	buffer_length -= i;
	while (0 <= scmp (buffer_string++, completion_string, buffer_length--))
	  i++;
	del_range (1, i + 1);
	SET_PT (ZV);
      }
  }
#endif /* Rewritten code */
  i = ZV - BEGV;

  /* If completion finds next char not unique,
     consider adding a space or a hyphen */
  if (i == XSTRING (completion)->size)
    {
      tem = Ftry_completion (concat2 (Fbuffer_string (), build_string (" ")),
			     Vminibuffer_completion_table,
			     Vminibuffer_completion_predicate);
      if (XTYPE (tem) == Lisp_String)
	completion = tem;
      else
	{
	  tem = Ftry_completion (concat2 (Fbuffer_string (), build_string ("-")),
				 Vminibuffer_completion_table,
				 Vminibuffer_completion_predicate);
	  if (XTYPE (tem) == Lisp_String)
	    completion = tem;
	}
    }      

  /* Now find first word-break in the stuff found by completion.
     i gets index in string of where to stop completing.  */
  completion_string = XSTRING (completion)->data;

  for (; i < XSTRING (completion)->size; i++)
    if (SYNTAX (completion_string[i]) != Sword) break;
  if (i < XSTRING (completion)->size)
    i = i + 1;

  /* If got no characters, print help for user.  */

  if (i == ZV - BEGV)
    {
      if (completion_auto_help)
	Fminibuffer_completion_help ();
      return Qnil;
    }

  /* Otherwise insert in minibuffer the chars we got */

  Ferase_buffer ();
  insert (completion_string, i);
  return Qt;
}

DEFUN ("display-completion-list", Fdisplay_completion_list, Sdisplay_completion_list,
       1, 1, 0,
  "Display in a buffer the list of completions, COMPLETIONS.\n\
Each element may be just a symbol or string\n\
or may be a list of two strings to be printed as if concatenated.")
  (completions)
     Lisp_Object completions;
{
  register Lisp_Object tail, elt;
  register int i;
  struct buffer *old = current_buffer;
  /* No GCPRO needed, since (when it matters) every variable
     points to a non-string that is pointed to by COMPLETIONS.  */

  set_buffer_internal (XBUFFER (Vstandard_output));

  if (NULL (completions))
    InsStr ("There are no possible completions of what you have typed.");
  else
    {
      InsStr ("Possible completions are:");
      for (tail = completions, i = 0; !NULL (tail); tail = Fcdr (tail), i++)
	{
	  /* this needs fixing for the case of long completions
	     and/or narrow windows */
	  /* Sadly, the window it will appear in is not known
	     until after the text has been made. */
	  if (i & 1)
	    Findent_to (make_number (35), make_number (1));
	  else
	    Fterpri (Qnil);
	  elt = Fcar (tail);
	  if (CONSP (elt))
	    {
	      Fprinc (Fcar (elt), Qnil);
	      Fprinc (Fcar (Fcdr (elt)), Qnil);
	    }
	  else
	    Fprinc (elt, Qnil);
	}
    }
  set_buffer_internal (old);
  return Qnil;
}

DEFUN ("minibuffer-completion-help", Fminibuffer_completion_help, Sminibuffer_completion_help,
  0, 0, "",
  "Display a list of possible completions of the current minibuffer contents.")
  ()
{
  Lisp_Object completions;
  message ("Making completion list...");
  completions = Fall_completions (Fbuffer_string (), Vminibuffer_completion_table,
				  Vminibuffer_completion_predicate);
  echo_area_contents = 0;
  if (NULL (completions))
    {
      bell ();
      temp_echo_area_contents (" [No completions]");
    }
  else
    internal_with_output_to_temp_buffer (" *Completions*",
					 Fdisplay_completion_list,
					 Fsort (completions, Qstring_lessp));
  return Qnil;
}

DEFUN ("self-insert-and-exit", Fself_insert_and_exit, Sself_insert_and_exit, 0, 0, "",
  "Terminate minibuffer input.")
  ()
{
  self_insert_internal (last_command_char, 0);
  Fthrow (Qexit, Qnil);
}

DEFUN ("exit-minibuffer", Fexit_minibuffer, Sexit_minibuffer, 0, 0, "",
  "Terminate this minibuffer argument.")
  ()
{
  Fthrow (Qexit, Qnil);
}

DEFUN ("minibuffer-depth", Fminibuffer_depth, Sminibuffer_depth, 0, 0, 0,
  "Return current depth of activations of minibuffer, a nonnegative integer.")
  ()
{
  return make_number (minibuf_level);
}


init_minibuf_once ()
{
  Vminibuffer_list = Qnil;
  staticpro (&Vminibuffer_list);
}

syms_of_minibuf ()
{
  minibuf_level = 0;
  minibuf_prompt = 0;
  minibuf_save_vector_size = 5;
  minibuf_save_vector = (struct minibuf_save_data *) malloc (5 * sizeof (struct minibuf_save_data));

  Qminibuffer_completion_table = intern ("minibuffer-completion-table");
  staticpro (&Qminibuffer_completion_table);

  Qminibuffer_completion_confirm = intern ("minibuffer-completion-confirm");
  staticpro (&Qminibuffer_completion_confirm);

  Qminibuffer_completion_predicate = intern ("minibuffer-completion-predicate");
  staticpro (&Qminibuffer_completion_predicate);

  staticpro (&last_minibuf_string);
  last_minibuf_string = Qnil;

  Quser_variable_p = intern ("user-variable-p");
  staticpro (&Quser_variable_p);



  DEFVAR_BOOL ("completion-auto-help", &completion_auto_help,
    "*Non-nil means automatically provide help for invalid completion input.");
  completion_auto_help = 1;

  DEFVAR_BOOL ("completion-ignore-case", &completion_ignore_case,
    "Non-nil means don't consider case significant in completion.");
  completion_ignore_case = 0;

  DEFVAR_BOOL ("enable-recursive-minibuffers", &enable_recursive_minibuffers,
    "*Non-nil means to allow minibuffers to invoke commands which use\n\
recursive minibuffers.");
  enable_recursive_minibuffers = 0;

  DEFVAR_LISP ("minibuffer-completion-table", &Vminibuffer_completion_table,
    "Alist or obarray used for completion in the minibuffer.");
  Vminibuffer_completion_table = Qnil;

  DEFVAR_LISP ("minibuffer-completion-predicate", &Vminibuffer_completion_predicate,
    "Holds PREDICATE argument to completing-read.");
  Vminibuffer_completion_predicate = Qnil;

  DEFVAR_LISP ("minibuffer-completion-confirm", &Vminibuffer_completion_confirm,
    "Non-nil => demand confirmation of completion before exiting minibuffer.");
  Vminibuffer_completion_confirm = Qnil;

  DEFVAR_LISP ("minibuffer-help-form", &Vminibuffer_help_form,
    "Value that help-form takes on inside the minibuffer.");
  Vminibuffer_help_form = Qnil;

  defsubr (&Sread_from_minibuffer);
  defsubr (&Seval_minibuffer);
  defsubr (&Sread_minibuffer);
  defsubr (&Sread_string);
  defsubr (&Sread_command);
  defsubr (&Sread_variable);
  defsubr (&Sread_buffer);
  defsubr (&Sread_no_blanks_input);
  defsubr (&Sminibuffer_depth);

  defsubr (&Stry_completion);
  defsubr (&Sall_completions);
  defsubr (&Scompleting_read);
  defsubr (&Sminibuffer_complete);
  defsubr (&Sminibuffer_complete_word);
  defsubr (&Sminibuffer_complete_and_exit);
  defsubr (&Sdisplay_completion_list);
  defsubr (&Sminibuffer_completion_help);

  defsubr (&Sself_insert_and_exit);
  defsubr (&Sexit_minibuffer);

}

keys_of_minibuf ()
{
  ndefkey (Vminibuffer_local_map, Ctl ('g'), "abort-recursive-edit");
  ndefkey (Vminibuffer_local_map, Ctl ('m'), "exit-minibuffer");
  ndefkey (Vminibuffer_local_map, Ctl ('j'), "exit-minibuffer");

  ndefkey (Vminibuffer_local_ns_map, Ctl ('g'), "abort-recursive-edit");
  ndefkey (Vminibuffer_local_ns_map, Ctl ('m'), "exit-minibuffer");
  ndefkey (Vminibuffer_local_ns_map, Ctl ('j'), "exit-minibuffer");

  ndefkey (Vminibuffer_local_ns_map, ' ', "exit-minibuffer");
  ndefkey (Vminibuffer_local_ns_map, '\t', "exit-minibuffer");
  ndefkey (Vminibuffer_local_ns_map, '?', "self-insert-and-exit");

  ndefkey (Vminibuffer_local_completion_map, Ctl ('g'), "abort-recursive-edit");
  ndefkey (Vminibuffer_local_completion_map, Ctl ('m'), "exit-minibuffer");
  ndefkey (Vminibuffer_local_completion_map, Ctl ('j'), "exit-minibuffer");
  ndefkey (Vminibuffer_local_completion_map, '\t', "minibuffer-complete");
  ndefkey (Vminibuffer_local_completion_map, ' ', "minibuffer-complete-word");
  ndefkey (Vminibuffer_local_completion_map, '?', "minibuffer-completion-help");

  ndefkey (Vminibuffer_local_must_match_map, Ctl ('g'), "abort-recursive-edit");
  ndefkey (Vminibuffer_local_must_match_map, Ctl ('m'), "minibuffer-complete-and-exit");
  ndefkey (Vminibuffer_local_must_match_map, Ctl ('j'), "minibuffer-complete-and-exit");
  ndefkey (Vminibuffer_local_must_match_map, '\t', "minibuffer-complete");
  ndefkey (Vminibuffer_local_must_match_map, ' ', "minibuffer-complete-word");
  ndefkey (Vminibuffer_local_must_match_map, '?', "minibuffer-completion-help");
}
