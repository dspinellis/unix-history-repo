/* Minibuffer input and completion.
   Copyright (C) 1985 Richard M. Stallman.

This file is part of GNU Emacs.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU Emacs General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU Emacs, but only under the conditions described in the
GNU Emacs General Public License.   A copy of this license is
supposed to have been given to you along with GNU Emacs so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  */


#include <ctype.h>
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
  };

int minibuf_save_vector_size;
struct minibuf_save_data *minibuf_save_vector;

int auto_help;		/* Nonzero means display completion help for invalid input */

/* Fread_minibuffer leaves the input, as a string, here */
Lisp_Object last_minibuf_string;

/* Nonzero means let functions called when within a minibuffer 
   invoke recursive minibuffers (to read arguments, or whatever) */
int enable_recursive_minibuffers;

/* help-form is bound to this while in the minibuffer.  */

Lisp_Object Vminibuffer_help_form;

/* Nonzero means completion ignores case.  */

int completion_ignore_case;

Lisp_Object Quser_variable_p;

/* Width in columns of current minibuffer prompt.  */

extern int minibuf_prompt_width;

/* Actual minibuffer invocation. */

void read_minibuf_string_unwind ();
Lisp_Object get_minibuffer ();
Lisp_Object read_minibuf ();

Lisp_Object
read_minibuf_string (map, prefix, prompt)
     Lisp_Object map;
     Lisp_Object prefix;
     Lisp_Object prompt;
{
  return read_minibuf (map, prefix, prompt, 0);
}


Lisp_Object
read_minibuf (map, prefix, prompt, expflag)
     Lisp_Object map;
     Lisp_Object prefix;
     Lisp_Object prompt;
     int expflag;
{
  Lisp_Object val;
  int count = specpdl_ptr - specpdl;

  if (!enable_recursive_minibuffers &&
      (EQ (selected_window, minibuf_window)))
    error ("Command attempted to use minibuffer while in minibuffer");

  if (MinibufDepth == minibuf_save_vector_size)
    minibuf_save_vector =
     (struct minibuf_save_data *) xrealloc (minibuf_save_vector,
		      (minibuf_save_vector_size *= 2) * sizeof (struct minibuf_save_data)); 
  minibuf_save_vector[MinibufDepth].prompt = minibuf_prompt;
  minibuf_save_vector[MinibufDepth].help_form = Vhelp_form;
  minibuf_save_vector[MinibufDepth].prompt_width = minibuf_prompt_width;
  minibuf_prompt_width = 0;

  record_unwind_protect (save_window_restore, save_window_save ());

  val = bf_cur->directory;
  Fset_buffer (get_minibuffer (MinibufDepth + 1));
  bf_cur->directory = val;

  Fshow_buffer (minibuf_window, Fcurrent_buffer ());
  Fselect_window (minibuf_window);
  XFASTINT (XWINDOW (minibuf_window)->hscroll) = 0;

  Ferase_buffer ();
  MinibufDepth++;
  record_unwind_protect (read_minibuf_string_unwind, Qnil);

  if (!NULL (prefix))
    Finsert (1, &prefix);

  minibuf_prompt = (char *) alloca (XSTRING (prompt)->size + 1);
  bcopy (XSTRING (prompt)->data, minibuf_prompt, XSTRING (prompt)->size + 1);
  minibuf_message = 0;

  Vhelp_form = Vminibuffer_help_form;
  bf_cur->keymap = map;
  Frecursive_edit ();

  /* If cursor is on the minibuffer line,
     show the user we have exited by putting it in column 0.  */
  if (cursY >= XFASTINT (XWINDOW (minibuf_window)->top)
      && !noninteractive)
    {
      cursX = 0;
      update_screen (1, 1);
    }

  /* Make minibuffer contents into a string */
  val = make_string (&CharAt (1), bf_s1 + bf_s2);
  bcopy (bf_p2 + bf_s1 + 1,
	 XSTRING (val)->data + bf_s1,
	 bf_s2);

  last_minibuf_string = val;

  /* If Lisp form desired instead of string, read buffer contents */
  if (expflag)
    {
      SetPoint (1);
      val = Fread (Fcurrent_buffer ());
    }

  unbind_to (count);
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
read_minibuf_string_unwind ()
{
  Ferase_buffer ();

  /* If this was a recursive minibuffer,
     tie the minibuffer window back to the outer level minibuffer buffer */
  MinibufDepth--;
  /* Make sure minibuffer window is erased, not ignored */
  windows_or_buffers_changed++;
  XFASTINT (XWINDOW (minibuf_window)->last_modified) = 0;

  /* Restore prompt from outer minibuffer */
  minibuf_prompt = minibuf_save_vector[MinibufDepth].prompt;
  minibuf_prompt_width = minibuf_save_vector[MinibufDepth].prompt_width;
  Vhelp_form = minibuf_save_vector[MinibufDepth].help_form;
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

  return read_minibuf_string (Vminibuffer_local_ns_map, init, prompt);
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
  int list = LISTP (alist);
  int index, obsize;
  int matchcount = 0;
  Lisp_Object bucket, zero, end, tem;

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
	  if (XSYMBOL (bucket))
	    {
	      elt = bucket;
	      eltstring = Fsymbol_name (elt);
	      XSETSYMBOL (bucket, XSYMBOL (bucket)->next);
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
	  -1 == scmp (XSTRING (eltstring)->data, XSTRING (string)->data, XSTRING (string)->size))
	{
	  /* Yes. */
	  /* Ignore this element if there is a predicate and the predicate doesn't like it. */

	  if (!NULL (pred))
	    {
	      if (EQ (pred, Qcommandp))
		tem = Fcommandp (elt);
	      else
		{
		  tem = call1 (pred, elt);
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

/* Like strncmp but ignores case differences if appropriate.
   Also return value is different:
   -1 if strings match,
   else number of chars that match at the beginning.  */

#define cvt(c) (islower (c) ? c + 'A' - 'a' : c)

scmp (s1, s2, len)
     register char *s1, *s2;
     int len;
{
  register int l = len;

  if (completion_ignore_case)
    {
      while (l && *s1 && cvt (*s1) == cvt (*s2))
	{
	  l--;
	  s1++;
	  s2++;
	}
    }
  else
    {
      while (l && *s1 && *s1 == *s2)
	{
	  l--;
	  s1++;
	  s2++;
	}
    }
  if (l == 0 || (*s1 == 0 && *s2 == 0))
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
  int list = LISTP (alist);
  int index, obsize;
  Lisp_Object bucket, tem;

  CHECK_STRING (string, 0);
  if (!list && XTYPE (alist) != Lisp_Vector)
    {
      return call3 (alist, string, pred, Qt);
    }
  allmatches = Qnil;

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
	  if (XSYMBOL (bucket))
	    {
	      elt = bucket;
	      eltstring = Fsymbol_name (elt);
	      XSETSYMBOL (bucket, XSYMBOL (bucket)->next);
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
	  -1 == scmp (XSTRING (eltstring)->data, XSTRING (string)->data, XSTRING (string)->size))
	{
	  /* Yes. */
	  /* Ignore this element if there is a predicate and the predicate doesn't like it. */

	  if (!NULL (pred))
	    {
	      if (EQ (pred, Qcommandp))
		tem = Fcommandp (elt);
	      else
		tem = call1 (pred, elt);
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
  val = read_minibuf_string (NULL (require_match)
			     ? Vminibuffer_local_completion_map
			     : Vminibuffer_local_must_match_map,
			     init, prompt);
  unbind_to (count);
  return val;
}

temp_minibuf_message (m)
     char *m;
{
  int osize = NumCharacters + 1;
  Lisp_Object oinhibit;
  oinhibit = Vinhibit_quit;

  SetPoint (osize);
  InsStr (m);
  SetPoint (osize);
  Vinhibit_quit = Qt;
  Fsit_for (make_number (2));
  del_range (point, NumCharacters + 1);
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
  int completedp = 0;

  completion = Ftry_completion (Fbuffer_string (), Vminibuffer_completion_table,
				Vminibuffer_completion_predicate);
  if (NULL (completion))
    {
      Ding ();
      temp_minibuf_message (" [No match]");
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
  if (LISTP (Vminibuffer_completion_table))
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
      else if (auto_help)
	Fminibuffer_completion_help ();
      else
	temp_minibuf_message (" [Next char not unique]");
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
      temp_minibuf_message(" [Sole completion]");
      break;

    case 3:
      temp_minibuf_message(" [Complete, but not unique]");
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
  if (NumCharacters == 0)
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
	  temp_minibuf_message(" [Confirm]");
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
  register unsigned char *b;
  register unsigned char *p;
  register int i;

  /* We keep calling Fbuffer_string
     rather than arrange for GC to hold onto a pointer to
     one of the strings thus made.  */

  completion = Ftry_completion (Fbuffer_string (),
				Vminibuffer_completion_table,
				Vminibuffer_completion_predicate);
  if (NULL (completion))
    {
      Ding ();
      temp_minibuf_message (" [No match]");
      return Qnil;
    }
  if (EQ (completion, Qt))
    return Qnil;

  tem = Fbuffer_string ();
  b = XSTRING (tem)->data;
  i = NumCharacters - XSTRING (completion)->size;
  p = XSTRING (completion)->data;
  if (i > 0 ||
      0 <= scmp (b, p, NumCharacters))
    {
      i = 1;
      /* Set buffer to longest match of buffer tail and completion head. */
      while (0 <= scmp (b + i, p, NumCharacters - i))
	i++;
      del_range (1, i + 1);
      SetPoint (NumCharacters + 1);
    }

  i = NumCharacters;

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
  p = XSTRING (completion)->data;

  for (; i < XSTRING (completion)->size; i++)
    if (SYNTAX (p[i]) != Sword) break;
  if (i < XSTRING (completion)->size)
    i = i + 1;

  /* If got no characters, print help for user.  */

  if (i == NumCharacters)
    {
      if (auto_help)
	Fminibuffer_completion_help ();
      return Qnil;
    }

  /* Otherwise insert in minibuffer the chars we got */

  Ferase_buffer ();
  InsCStr (p, i);
  return Qt;
}

Lisp_Object
minibuffer_completion_help_1 (completions)
     Lisp_Object completions;
{
  register Lisp_Object tail;
  register int i;
  struct buffer *old = bf_cur;
  SetBfp (XBUFFER (Vstandard_output));

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
	  Fprinc (Fcar (tail), Qnil);
	}
    }
  SetBfp (old);
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
  minibuf_message = 0;
  if (NULL (completions))
    { Ding ();
      temp_minibuf_message (" [No completions]"); }
  else
    internal_with_output_to_temp_buffer (" *Completions*",
					 minibuffer_completion_help_1,
					 Fsort (completions, Qstring_lessp));
  return Qnil;
}

DEFUN ("self-insert-and-exit", Fself_insert_and_exit, Sself_insert_and_exit, 0, 0, "",
  "Terminate minibuffer input.")
  ()
{
  SelfInsert (last_command_char);
  Fthrow (Qexit, Qnil);
}

DEFUN ("exit-minibuffer", Fexit_minibuffer, Sexit_minibuffer, 0, 0, "",
  "Terminate this minibuffer argument.")
  ()
{
  Fthrow (Qexit, Qnil);
}

init_minibuf_once ()
{
  Vminibuffer_list = Qnil;
  staticpro (&Vminibuffer_list);
}

syms_of_minibuf ()
{
  MinibufDepth = 0;
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



  DefBoolVar ("completion-auto-help", &auto_help,
    "*Non-nil means automatically provide help for invalid completion input.");
  auto_help = 1;

  DefBoolVar ("completion-ignore-case", &completion_ignore_case,
    "Non-nil means don't consider case significant in completion.");
  completion_ignore_case = 0;

  DefBoolVar ("enable-recursive-minibuffers", &enable_recursive_minibuffers,
    "*Non-nil means to allow minibuffers to invoke commands which use\n\
recursive minibuffers.");
  enable_recursive_minibuffers = 0;

  DefLispVar ("minibuffer-completion-table", &Vminibuffer_completion_table,
    "Alist or obarray used for completion in the minibuffer.");
  Vminibuffer_completion_table = Qnil;

  DefLispVar ("minibuffer-completion-predicate", &Vminibuffer_completion_predicate,
    "Holds PREDICATE argument to completing-read.");
  Vminibuffer_completion_predicate = Qnil;

  DefLispVar ("minibuffer-completion-confirm", &Vminibuffer_completion_confirm,
    "Non-nil => demand confirmation of completion before exiting minibuffer.");
  Vminibuffer_completion_confirm = Qnil;

  DefLispVar ("minibuffer-help-form", &Vminibuffer_help_form,
    "Value that help-form takes on inside the minibuffer.");
  Vminibuffer_help_form = Qnil;

  defsubr (&Sread_from_minibuffer);
  defsubr (&Seval_minibuffer);
  defsubr (&Sread_minibuffer);
  defsubr (&Sread_string);
  defalias (&Sread_string, "read-input");
  defsubr (&Sread_command);
  defsubr (&Sread_variable);
  defsubr (&Sread_buffer);
  defsubr (&Sread_no_blanks_input);

  defsubr (&Stry_completion);
  defsubr (&Sall_completions);
  defsubr (&Scompleting_read);
  defsubr (&Sminibuffer_complete);
  defsubr (&Sminibuffer_complete_word);
  defsubr (&Sminibuffer_complete_and_exit);
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
