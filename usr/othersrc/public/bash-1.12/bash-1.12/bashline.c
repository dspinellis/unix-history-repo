/* bashline.c -- Bash's interface to the readline library. */

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

#include <stdio.h>
#include <readline/readline.h>
#include <readline/history.h>
#include "shell.h"
#include "builtins.h"

/* Functions bound to keys in Readline for Bash users. */
static void
  shell_expand_line (), insert_last_arg (), display_shell_version (),
  operate_and_get_next ();

/* Helper functions for Readline. */
static void
  bash_symbolic_link_hook (), filename_completion_ignore (), bash_push_line ();

static char
  **attempt_shell_completion (), *variable_completion_function (),
  *hostname_completion_function (), *command_word_completion_function ();

static void
  snarf_hosts_from_file (), add_host_name (), sort_hostname_list ();

/* Externally defined functions used by this file. */
extern char
  *get_string_value (), *filename_completion_function (),
  *username_completion_function ();

extern int show_shell_version ();

#define DYNAMIC_HISTORY_COMPLETION
#if defined (DYNAMIC_HISTORY_COMPLETION)
static void dynamic_complete_history ();
#endif /* DYNAMIC_HISTORY_COMPLETION */

/* SPECIFIC_COMPLETION_FUNCTIONS specifies that we have individual
   completion functions which indicate what type of completion should be
   done (at or before point) that can be bound to key sequences with
   the readline library. */
#define SPECIFIC_COMPLETION_FUNCTIONS

#if defined (SPECIFIC_COMPLETION_FUNCTIONS)
static void
  bash_specific_completion (),
  bash_complete_filename (), bash_possible_filename_completions (),
  bash_complete_filename_internal (),
  bash_complete_username (), bash_possible_username_completions (),
  bash_complete_username_internal (),
  bash_complete_hostname (), bash_possible_hostname_completions (),
  bash_complete_hostname_internal (),
  bash_complete_variable (), bash_possible_variable_completions (),
  bash_complete_variable_internal (),
  bash_complete_command (), bash_possible_command_completions (),
  bash_complete_command_internal ();
#endif /* SPECIFIC_COMPLETION_FUNCTIONS */

/* Non-zero once initalize_readline () has been called. */
int bash_readline_initialized = 0;

/* Called once from parse.y if we are going to use readline. */
initialize_readline ()
{
  if (bash_readline_initialized)
    return;

  rl_terminal_name = get_string_value ("TERM");
  rl_instream = stdin, rl_outstream = stderr;
  rl_special_prefixes = "$@%";

  /* Allow conditional parsing of the ~/.inputrc file. */
  rl_readline_name = "Bash";

  /* Bind up our special shell functions. */
  rl_add_defun
    ("shell-expand-line", (Function *)shell_expand_line, META(CTRL('E')));

  rl_add_defun
    ("insert-last-argument", (Function *)insert_last_arg, META('.'));

  rl_bind_key (META('_'), (Function *)insert_last_arg);

  rl_add_defun
    ("operate-and-get-next", (Function *)operate_and_get_next, CTRL('O'));

  rl_add_defun
    ("display-shell-version", (Function *)display_shell_version, -1);

  rl_bind_key_in_map
    (CTRL ('V'), (Function *)display_shell_version, emacs_ctlx_keymap);

  /* In Bash, the user can switch editing modes with "set -o [vi emacs]",
     so it is not necessary to allow C-M-j for context switching.  Turn
     off this occasionally confusing behaviour. */
  rl_unbind_key_in_map (CTRL('J'), emacs_meta_keymap);

#ifdef SPECIFIC_COMPLETION_FUNCTIONS
  rl_add_defun ("complete-filename", bash_complete_filename, META('/'));
  rl_add_defun ("possible-filename-completions",
		bash_possible_filename_completions, -1);
  rl_bind_key_in_map ('/', bash_possible_filename_completions,
		      emacs_ctlx_keymap);

  rl_add_defun ("complete-username", bash_complete_username, META('~'));
  rl_add_defun ("possible-username-completions",
		bash_possible_username_completions, -1);
  rl_bind_key_in_map ('~', bash_possible_username_completions,
		      emacs_ctlx_keymap);

  rl_add_defun ("complete-hostname", bash_complete_hostname, META('@'));
  rl_add_defun ("possible-hostname-completions",
		bash_possible_hostname_completions, -1);
  rl_bind_key_in_map ('@', bash_possible_hostname_completions,
		      emacs_ctlx_keymap);

  rl_add_defun ("complete-variable", bash_complete_variable, META('$'));
  rl_add_defun ("possible-variable-completions",
		bash_possible_variable_completions, -1);
  rl_bind_key_in_map ('$', bash_possible_variable_completions,
		      emacs_ctlx_keymap);

  rl_add_defun ("complete-command", bash_complete_command, META('!'));
  rl_add_defun ("possible-command-completions",
		bash_possible_command_completions, -1);
  rl_bind_key_in_map ('!', bash_possible_command_completions,
		      emacs_ctlx_keymap);

#endif  /* SPECIFIC_COMPLETION_FUNCTIONS */

#if defined (DYNAMIC_HISTORY_COMPLETION)
  rl_add_defun
    ("dynamic-complete-history", dynamic_complete_history, META(TAB));
#endif /* DYNAMIC_HISTORY_COMPLETION */

  /* Tell the completer that we want a crack first. */
  rl_attempted_completion_function = (Function *)attempt_shell_completion;

  /* Tell the completer that we might want to follow symbolic links. */
  rl_symbolic_link_hook = (Function *)bash_symbolic_link_hook;

  /* Tell the filename completer we want a chance to ignore some names. */
  rl_ignore_some_completions_function =
    (Function *)filename_completion_ignore;

  bash_readline_initialized = 1;
}

/* On Sun systems at least, rl_attempted_completion_function can end up
   getting set to NULL, and rl_completion_entry_function set to do command
   word completion if Bash is interrupted while trying to complete a command
   word.  This just resets all the completion functions to the right thing.
   It's called from throw_to_top_level(). */
bashline_reinitialize ()
{
  extern void tilde_initialize ();

  tilde_initialize ();
  rl_attempted_completion_function = (Function *)attempt_shell_completion;
  rl_symbolic_link_hook = (Function *)bash_symbolic_link_hook;
}

/* Contains the line to push into readline. */
static char *push_to_readline = (char *)NULL;

/* Push the contents of push_to_readline into the
   readline buffer. */
static void
bash_push_line ()
{
  if (push_to_readline)
    {
      rl_insert_text (push_to_readline);
      free (push_to_readline);
      push_to_readline = (char *)NULL;
    }
}

/* Call this to set the initial text for the next line to read
   from readline. */
int
bash_re_edit (line)
     char *line;
{
  if (push_to_readline)
    free (push_to_readline);

  push_to_readline = savestring (line);
  rl_startup_hook = (Function *)bash_push_line;

  return (0);
}

static void
display_shell_version (count, c)
     int count, c;
{
  crlf ();
  show_shell_version ();
  putc ('\r', rl_outstream);
  fflush (rl_outstream);
  rl_on_new_line ();
  rl_redisplay ();
}

/* **************************************************************** */
/*								    */
/*			     Readline Stuff			    */
/*								    */
/* **************************************************************** */

/* If the user requests hostname completion, then simply build a list
   of hosts, and complete from that forever more. */
#if !defined (ETCHOSTS)
#define ETCHOSTS "/etc/hosts"
#endif

/* The kept list of hostnames. */
static char **hostname_list = (char **)NULL;

/* The physical size of the above list. */
static int hostname_list_size = 0;

/* The length of the above list. */
static int hostname_list_length = 0;

/* Whether or not HOSTNAME_LIST has been initialized. */
int hostname_list_initialized = 0;

/* Non-zero means that HOSTNAME_LIST needs to be sorted. */
static int hostname_list_needs_sorting = 0;

/* Initialize the hostname completion table. */
static void
initialize_hostname_list ()
{
  char *temp = get_string_value ("hostname_completion_file");

  if (!temp)
    temp = ETCHOSTS;

  snarf_hosts_from_file (temp);
  sort_hostname_list ();
  if (hostname_list)
    hostname_list_initialized++;
}

/* Add NAME to the list of hosts. */
static void
add_host_name (name)
     char *name;
{
  if (hostname_list_length + 2 > hostname_list_size)
    {
      hostname_list = (char **)
	xrealloc (hostname_list,
		  (1 + (hostname_list_size += 100)) * sizeof (char *));
    }

  hostname_list[hostname_list_length] = savestring (name);
  hostname_list[++hostname_list_length] = (char *)NULL;
  hostname_list_needs_sorting++;
}

/* After you have added some names, you should sort the list of names. */
static void
sort_hostname_list ()
{
  if (hostname_list_needs_sorting && hostname_list)
    sort_char_array (hostname_list);
  hostname_list_needs_sorting = 0;
}

#define cr_whitespace(c) ((c) == '\r' || (c) == '\n' || whitespace(c))

static void
snarf_hosts_from_file (filename)
     char *filename;
{
  FILE *file = fopen (filename, "r");
  char *temp, buffer[256], name[256];
  register int i, start;

  if (!file)
    return;

  while (temp = fgets (buffer, 255, file))
    {
      /* Skip to first character. */
      for (i = 0; buffer[i] && cr_whitespace (buffer[i]); i++);

      /* If comment, ignore. */
      if (buffer[i] == '#')
	continue;

      /* If `preprocessor' directive, do the include. */
      if (strncmp (&buffer[i], "$include ", 9) == 0)
	{
	  char *includefile = &buffer[i + 9];
	  char *t;

	  /* Find start of filename. */
	  while (*includefile && whitespace (*includefile))
	    includefile++;

	  t = includefile;

	  /* Find end of filename. */
	  while (*t && !cr_whitespace (*t))
	    t++;

	  *t = '\0';

	  snarf_hosts_from_file (includefile);
	  continue;
	}

      /* Skip internet address. */
      for (; buffer[i] && !cr_whitespace (buffer[i]); i++);

      /* Gobble up names.  Each name is separated with whitespace. */
      while (buffer[i] && buffer[i] != '#')
	{
	  for (; i && cr_whitespace (buffer[i]); i++);
	  if (buffer[i] ==  '#')
	    continue;
	  for (start = i; buffer[i] && !cr_whitespace (buffer[i]); i++);
	  if ((i - start) == 0)
	    continue;
	  strncpy (name, buffer + start, i - start);
	  name[i - start] = '\0';
	  add_host_name (name);
	}
    }
  fclose (file);
}

/* Return a NULL terminated list of hostnames which begin with TEXT.
   Initialize the hostname list the first time if neccessary.
   The array is malloc ()'ed, but not the individual strings. */
static char **
hostnames_matching (text)
     char *text;
{
  register int i, len = strlen (text);
  register int begin, end;
  int last_search = -1;
  char **result = (char **)NULL;

  if (!hostname_list_initialized)
    {
      initialize_hostname_list ();

      if (!hostname_list_initialized)
	return ((char **)NULL);
    }

  sort_hostname_list ();

  /* The list is sorted.  Do a binary search on it for the first character
     in TEXT, and then grovel the names of interest. */
  begin = 0; end = hostname_list_length;

  /* Special case.  If TEXT consists of nothing, then the whole list is
     what is desired. */
  if (!*text)
    {
      result = (char **)xmalloc ((1 + hostname_list_length) * sizeof (char *));
      for (i = 0; i < hostname_list_length; i++)
	result[i] = hostname_list[i];
      result[i] = (char *)NULL;
      return (result);
    }

  /* Scan until found, or failure. */
  while (end != begin)
    {
      int r = 0;

      i = ((end - begin) / 2) + begin;
      if (i == last_search)
	break;

      if (hostname_list[i] &&
	  (r = strncmp (hostname_list[i], text, len)) == 0)
	{
	  while (strncmp (hostname_list[i], text, len) == 0 && i) i--;
	  if (strncmp (hostname_list[i], text, len) != 0) i++;

	  begin = i;
	  while (hostname_list[i] &&
		 strncmp (hostname_list[i], text, len) == 0) i++;
	  end = i;

	  result = (char **)xmalloc ((1 + (end - begin)) * sizeof (char *));
	  for (i = 0; i + begin < end; i++)
	    result[i] = hostname_list[begin + i];
	  result[i] = (char *)NULL;
	  return (result);
	}

      last_search = i;

      if (r < 0)
	begin = i;
      else
	end = i;
    }
  return ((char **)NULL);
}

/* This is a K*rn shell style insert-last-arg function.  The
   difference is that Bash puts stuff into the history file before
   expansion and file name generation, so we deal with exactly what the
   user typed.  Those wanting the other behavior, at least for the last
   arg, can use `$_'.  This also `knows' about how rl_yank_nth_arg treats
   `$'. */
static void
insert_last_arg (count, c)
     int count, c;
{
  extern int rl_explicit_arg;

  if (rl_explicit_arg)
    rl_yank_nth_arg (count, c);
  else
    rl_yank_nth_arg ('$', c);
}

/* The equivalent of the K*rn shell C-o operate-and-get-next-history-line
   editing command. */
static int saved_history_line_to_use = 0;

static void
set_saved_history ()
{
  HIST_ENTRY *h;

  if (saved_history_line_to_use)
    {
      if (history_set_pos (saved_history_line_to_use))
	{
	  h = current_history ();
	  if (h)
	    {
	      rl_insert_text (h->line);
	      /*
	       * Get rid of any undo list created by the previous insert,
	       * so the line won't totally be erased when the edits are
	       * undone (they will be normally, because this is a history
	       * line -- cf. readline.c: line 380 or so).
	       */
	      if (rl_undo_list)
		{
		  free_undo_list ();
		  rl_undo_list = (UNDO_LIST *)NULL;
		}
	    }
	}
    }
  saved_history_line_to_use = 0;
  rl_startup_hook = (Function *)NULL;
}  

static void
operate_and_get_next (count, c)
     int count, c;
{
  int where;
  extern int history_stifled, history_length, max_input_history;

  /* Accept the current line. */
  rl_newline ();	

  /* Find the current line, and find the next line to use. */
  where = where_history ();

  if (history_stifled && (history_length >= max_input_history))
    saved_history_line_to_use = where;
  else
    saved_history_line_to_use = where + 1;

  rl_startup_hook = (Function *)set_saved_history;
}

/* **************************************************************** */
/*								    */
/*			How To Do Shell Completion		    */
/*								    */
/* **************************************************************** */

/* Do some completion on TEXT.  The indices of TEXT in RL_LINE_BUFFER are
   at START and END.  Return an array of matches, or NULL if none. */
static char **
attempt_shell_completion (text, start, end)
     char *text;
     int start, end;
{
  int in_command_position = 0;
  char **matches = (char **)NULL;
  char *command_separator_chars = ";|&{(";

  /* Determine if this could be a command word.  It is if it appears at
     the start of the line (ignoring preceding whitespace), or if it
     appears after a character that separates commands.  It cannot be a
     command word if we aren't at the top-level prompt. */
  {
    register int ti = start - 1;

    while ((ti > -1) && (whitespace (rl_line_buffer[ti])))
      ti--;

    if (ti < 0)
      {
	extern char *current_prompt_string, *ps1_prompt;

	/* Only do command completion at the start of a line when we
	   are not prompting at top level. */
	if (current_prompt_string == ps1_prompt)
	  in_command_position++;
      }
    else
      {
	if (member (rl_line_buffer[ti], command_separator_chars))
	  in_command_position++;
      }
  }

  /* Variable name? */
  if (*text == '$')
    matches = completion_matches (text, variable_completion_function);

  /* If the word starts in `~', and there is no slash in the word, then
     try completing this word as a username. */
  if (!matches && *text == '~' && !index (text, '/'))
    matches = completion_matches (text, username_completion_function);

  /* Another one.  Why not?  If the word starts in '@', then look through
     the world of known hostnames for completion first. */
  if (!matches && *text == '@')
    matches = completion_matches (text, hostname_completion_function);

  /* And last, (but not least) if this word is in a command position, then
     complete over possible command names, including aliases, functions,
     and command names. */
  if (!matches && in_command_position && *text != '/')
    matches = completion_matches (text, command_word_completion_function);

  return (matches);
}

/* This is the function to call when the word to complete is at the start
   of a line.  It grovels $PATH, looking for commands that match.  It also
   scans aliases, function names, and the shell_builtin table. */
static char *
command_word_completion_function (hint_text, state)
     char *hint_text;
     int state;
{
  extern SHELL_VAR **all_visible_functions ();
  char *extract_colon_unit ();
  static char *hint = (char *)NULL;
  static char *path = (char *)NULL;
  static char *val = (char *)NULL;
  static char *filename_hint = (char *)NULL;
  static int path_index, hint_len, istate;
  static int mapping_over, local_index;
  static SHELL_VAR **varlist = (SHELL_VAR **)NULL;

  /* We have to map over the possibilities for command words.  If we have
     no state, then make one just for that purpose. */

  if (!state)
    {
      if (hint)
	free (hint);

      path = get_string_value ("PATH");
      path_index = 0;

      hint = savestring (hint_text);
      hint_len = strlen (hint);

      mapping_over = 0;
      val = (char *)NULL;

      /* Initialize the variables for each type of command word. */
      local_index = 0;

      if (varlist)
	free (varlist);

      varlist = all_visible_functions ();
    }

  /* mapping_over says what we are currently hacking.  Note that every case
     in this list must fall through when there are no more possibilities. */

  switch (mapping_over)
    {
    case 0:			/* Aliases come first. */
#if defined (ALIAS)
      while (aliases && aliases[local_index])
	{
	  register char *alias;

	  alias = aliases[local_index++]->name;

	  if (strncmp (alias, hint, hint_len) == 0)
	    return (savestring (alias));
	}
#endif /* ALIAS */
      local_index = 0;
      mapping_over++;

    case 1:			/* Then shell reserved words. */
      {
	extern STRING_INT_ALIST word_token_alist[];

	while (word_token_alist[local_index].word)
	  {
	    register char *reserved_word;

	    reserved_word = word_token_alist[local_index++].word;

	    if (strncmp (reserved_word, hint, hint_len) == 0)
	      return (savestring (reserved_word));
	  }
	local_index = 0;
	mapping_over++;
      }

    case 2:			/* Then function names. */
      while (varlist && varlist[local_index])
	{
	  register char *varname;

	  varname = varlist[local_index++]->name;

	  if (strncmp (varname, hint, hint_len) == 0)
	    return (savestring (varname));
	}
      local_index = 0;
      mapping_over++;

    case 3:			/* Then shell builtins. */
      for (; local_index < num_shell_builtins; local_index++)
	{
	  if (!shell_builtins[local_index].function ||
	      !shell_builtins[local_index].enabled)
	    continue;

	  if (strncmp
	      (shell_builtins[local_index].name, hint, hint_len) == 0)
	    {
	      int i = local_index++;

	      return (savestring (shell_builtins[i].name));
	    }
	}
      local_index = 0;
      mapping_over++;
    }

  /* Repeatedly call filename_completion_function while we have
     members of PATH left.  Question:  should we stat each file?
     Answer: we call executable_file () on each file. */
 outer:

  istate = (val != (char *)NULL);

  if (!istate)
    {
      char *current_path;

      /* Get the next directory from the path.  If there is none, then we
	 are all done. */
      if (!path ||
	  !path[path_index] ||
	  !(current_path = extract_colon_unit (path, &path_index)))
	return ((char *)NULL);

      if (!*current_path)
	{
	  free (current_path);
	  current_path = savestring (".");
	}

      if (*current_path == '~')
	{
	  extern char *tilde_expand ();
	  char *t;

	  t = tilde_expand (current_path);
	  free (current_path);
	  current_path = t;
	}

      if (filename_hint)
	free (filename_hint);

      filename_hint =
	(char *)xmalloc (2 + strlen (current_path)
			 + strlen (hint));
      sprintf (filename_hint, "%s/%s", current_path, hint);

      free (current_path);
    }

 inner:
  val = filename_completion_function (filename_hint, istate);
  istate = 1;

  if (!val)
    {
      goto outer;
    }
  else
    {
      char *rindex (), *temp = rindex (val, '/');
      temp++;
      if ((strncmp (hint, temp, hint_len) == 0) && executable_file (val))
	{
	  temp = (savestring (temp));
	  free (val);
	  val = "";	/* So it won't be NULL. */
	  return (temp);
	}
      else
	{
	  free (val);
	  goto inner;
	}
    }
}

/* Okay, now we write the entry_function for variable completion. */
static char *
variable_completion_function (text, state)
     int state;
     char *text;
{
  register SHELL_VAR *var = (SHELL_VAR *)NULL;
  static SHELL_VAR **varlist = (SHELL_VAR **)NULL;
  static int varlist_index;
  static char *varname = (char *)NULL;
  static int namelen;
  static int first_char, first_char_loc;

  extern SHELL_VAR **all_visible_variables ();

  if (!state)
    {
      if (varname)
	free (varname);

      first_char_loc = 0;
      first_char = text[0];

      if (first_char == '$')
	first_char_loc++;

      varname = savestring (&text[first_char_loc]);

      namelen = strlen (varname);
      if (varlist)
	free (varlist);
      varlist = all_visible_variables ();
      varlist_index = 0;
    }

  while (varlist && varlist[varlist_index])
    {
      var = varlist[varlist_index];

      /* Compare.  You can't do better than Zayre.  No text is also
	 a match.  */
      if (!*varname || (strncmp (varname, var->name, namelen) == 0))
	break;
      varlist_index++;
    }

  if (!varlist || !varlist[varlist_index])
    {
      return ((char *)NULL);
    }
  else
    {
      char *value = (char *)xmalloc (2 + strlen (var->name));

      if (first_char_loc)
	*value = first_char;

      strcpy (&value[first_char_loc], var->name);

      varlist_index++;
      return (value);
    }
}

/* How about a completion function for hostnames? */
static char *
hostname_completion_function (text, state)
     int state;
     char *text;
{
  static char **list = (char **)NULL;
  static int list_index = 0;
  static int first_char, first_char_loc;

  /* If we don't have any state, make some. */
  if (!state)
    {
      if (list)
	free (list);

      list = (char **)NULL;

      first_char_loc = 0;
      first_char = *text;

      if (first_char == '@')
	first_char_loc++;

      list = hostnames_matching (&text[first_char_loc]);
      list_index = 0;
    }

  if (list && list[list_index])
    {
      char *t = (char *)xmalloc (2 + strlen (list[list_index]));

      *t = first_char;
      strcpy (t + first_char_loc, list[list_index]);
      list_index++;
      return (t);
    }
  else
    return ((char *)NULL);
}

/* History and alias expand the line.  But maybe do more?  This
   is a test to see what users like.  Do expand_string on the string. */
static void
shell_expand_line (ignore)
     int ignore;
{
  char *pre_process_line (), *new_line;

  new_line = pre_process_line (rl_line_buffer, 0, 0);

#if defined (ALIAS)
  if (new_line)
    {
      char *alias_expand (), *alias_line;

      alias_line = alias_expand (new_line);
      free (new_line);
      new_line = alias_line;
    }
#endif /* ALIAS */

  if (new_line)
    {
      int old_point = rl_point;
      int at_end = rl_point == rl_end;

      /* If the line was history and alias expanded, then make that
	 be one thing to undo. */

      if (strcmp (new_line, rl_line_buffer) != 0)
	{
	  rl_point = rl_end;

	  rl_add_undo (UNDO_BEGIN, 0, 0, 0);
	  rl_kill_text (0, rl_point);
	  rl_point = rl_end = 0;
	  rl_insert_text (new_line);
	  rl_add_undo (UNDO_END, 0, 0, 0);
	}

      free (new_line);

      /* If there is variable expansion to perform, do that as a separate
	 operation to be undone. */
      {
	extern char *string_list ();
	extern WORD_LIST *expand_string ();
	WORD_LIST *expanded_string;
	char *string_list ();

	expanded_string = expand_string (rl_line_buffer, 0);
	if (!expanded_string)
	  new_line = savestring ("");
	else
	  {
	    new_line = string_list (expanded_string);
	    dispose_words (expanded_string);
	  }

      if (strcmp (new_line, rl_line_buffer) != 0)
	{
	  rl_add_undo (UNDO_BEGIN, 0, 0 ,0);
	  rl_kill_text (0, rl_end);
	  rl_point = rl_end = 0;
	  rl_insert_text (new_line);
	  rl_add_undo (UNDO_END, 0, 0, 0);
	}

      free (new_line);

      /* Place rl_point where we think it should go. */
      if (at_end)
	rl_point = rl_end;
      else if (old_point < rl_end)
	{
	  rl_point = old_point;
	  if (!whitespace (rl_line_buffer[rl_point]))
	    rl_forward_word (1);
	}
      }
    }
  else
    {
      /* There was an error in expansion.  Let the preprocessor print
	 the error here.  Note that we know that pre_process_line ()
	 will return NULL, since it just did. */
      fprintf (rl_outstream, "\n\r");
      pre_process_line (rl_line_buffer, 1, 0);
      rl_forced_update_display ();
    }
}

/* Filename completion ignore.  Emulates the "fignore" facility of
   tcsh.  If FIGNORE is set, then don't match files with the
   given suffixes.  If only one of the possibilities has an acceptable
   suffix, delete the others, else just return and let the completer
   signal an error.  It is called by the completer when real
   completions are done on filenames by the completer's internal
   function, not for completion lists (M-?) and not on "other"
   completion types, such as hostnames or commands.
 
   It is passed a NULL-terminated array of (char *)'s that must be
   free()'d if they are deleted.  The first element (names[0]) is the
   least-common-denominator string of the matching patterns (i.e.
   u<TAB> produces names[0] = "und", names[1] = "under.c", names[2] =
   "undun.c", name[3] = NULL).  */

struct ign {
  char *val;
  int len;
};

static struct ign *ignores;	/* Store the ignore strings here */
static int num_ignores;		/* How many are there? */
static char *last_fignore;	/* Last value of fignore - cached for speed */
extern char *extract_colon_unit (), *get_string_value ();

static void
setup_ignore_patterns ()
{
  int numitems, maxitems, ptr;
  char *colon_bit;
  struct ign *p;
  
  char *this_fignore = get_string_value ("FIGNORE");

  /* If nothing has changed then just exit now. */
  if (this_fignore &&
      last_fignore && 
      strcmp (this_fignore, last_fignore) == 0 ||
      (!this_fignore && !last_fignore))
    {
      return;
    }

  /* Oops.  FIGNORE has changed.  Re-parse it. */
  num_ignores = 0;

  if (ignores)
    {
      for (p = ignores; p->val; p++) free(p->val);
      free (ignores);
      ignores = (struct ign*)NULL;
    }

  if (last_fignore)
    free (last_fignore);

  last_fignore = savestring (this_fignore);
  
  if (!this_fignore || !*this_fignore)
    return;

  numitems = maxitems = ptr = 0;

  while (colon_bit = extract_colon_unit (this_fignore, &ptr))
    {
      if (numitems + 1 > maxitems)
	ignores = (struct ign *)
	  xrealloc (ignores, (maxitems += 10) * sizeof (struct ign));

      ignores[numitems].val = colon_bit;
      ignores[numitems].len = strlen (colon_bit);
      numitems++;
    }
  ignores[numitems].val = NULL;
  num_ignores = numitems;
}

static int
name_is_acceptable (name)
     char *name;
{
  struct ign *p;
  int nlen = strlen (name);

  for (p = ignores; p->val; p++) 
    {
      if (nlen > p->len && p->len > 0 && 
	  strcmp (p->val, &name[nlen - p->len]) == 0)
	return (0);
    }

  return (1);
}

static void
filename_completion_ignore (names)
     char **names;
{
  char **p;
  int idx;

  setup_ignore_patterns ();

  if (!num_ignores)
    return;
  
  for (p = names + 1, idx = -1; *p; p++)
    {
      if (name_is_acceptable (*p))
	{
	  if (idx == -1)	/* First match found. */
	    idx = p - names;
	  else
	    return;		/* Too many matches. */
	}
    }
  
  /* If none are acceptable then let the completer handle it. */
  if (idx == -1)
    return;

  /* Delete all non-matching elements. */
  free (names[0]); 
  for (p = names + 1; *p; p++)
    {
      if (idx == (p - names))
	names[0] = *p;
      else 
	free (*p);

      *p = NULL;
    }
}

/* Handle symbolic link references while hacking completion. */
static void
bash_symbolic_link_hook (dirname)
     char **dirname;
{
  extern int follow_symbolic_links;
  char *make_absolute (), *temp_dirname;

  if (follow_symbolic_links && (strcmp (*dirname, ".") != 0))
    {
      temp_dirname = make_absolute (*dirname, get_working_directory (""));

      if (temp_dirname)
	{
	  free (*dirname);
	  *dirname = temp_dirname;
	}
    }
}

#if defined (DYNAMIC_HISTORY_COMPLETION)
static char **history_completion_array = (char **)NULL;
static int harry_size = 0;
static int harry_len = 0;

static void
build_history_completion_array ()
{
  register int i;

  /* First, clear out the current dynamic history completion list. */
  if (harry_size)
    {
      for (i = 0; history_completion_array[i]; i++)
	free (history_completion_array[i]);

      free (history_completion_array);

      history_completion_array = (char **)NULL;
      harry_size = 0;
      harry_len = 0;
    }

  /* Next, grovel each line of history, making each shell-sized token
     a separate entry in the history_completion_array. */
  {
    HIST_ENTRY **hlist;

    hlist = history_list ();

    if (hlist)
      {
	extern int qsort_string_compare ();
	register int j;

	for (i = 0; hlist[i]; i++)
	  {
	    extern char **history_tokenize ();
	    char **tokens;

	    /* Separate each token, and place into an array. */
	    tokens = history_tokenize (hlist[i]->line);

	    for (j = 0; tokens && tokens[j]; j++)
	      {
		if (harry_len + 2 > harry_size)
		  history_completion_array = (char **) xrealloc
		    (history_completion_array,
		     (harry_size += 10) * sizeof (char *));

		history_completion_array[harry_len++] = tokens[j];
		history_completion_array[harry_len] = (char *)NULL;
	      }
	    free (tokens);
	  }

	/* Sort the complete list of tokens. */
	qsort (history_completion_array, harry_len, sizeof (char *),
	       qsort_string_compare);

	/* Instead of removing the duplicate entries here, we let the
	   code in the completer handle it. */
      }
  }
}

static char *
history_completion_generator (hint_text, state)
     char *hint_text;
     int state;
{
  static int local_index = 0;
  static char *text = (char *)NULL;
  static int len = 0;

  /* If this is the first call to the generator, then initialize the
     list of strings to complete over. */
  if (!state)
    {
      local_index = 0;
      build_history_completion_array ();
      text = hint_text;
      len = strlen (text);
    }

  while (history_completion_array && history_completion_array[local_index])
    {
      if (strncmp (text, history_completion_array[local_index++], len) == 0)
	return (savestring (history_completion_array[local_index - 1]));
    }
  return ((char *)NULL);
}

static void
dynamic_complete_history (count, key)
     int count, key;
{
  extern Function *rl_last_func;
  Function *orig_func;
  Function *orig_attempt_func;

  orig_func = rl_completion_entry_function;
  orig_attempt_func = rl_attempted_completion_function;
  rl_completion_entry_function = (Function *)history_completion_generator;
  rl_attempted_completion_function = (Function *)0x0;

  if (rl_last_func == (Function *)dynamic_complete_history)
    rl_complete_internal ('?');
  else
    rl_complete_internal (TAB);

  rl_completion_entry_function = orig_func;
  rl_attempted_completion_function = orig_attempt_func;
}

#endif /* DYNAMIC_HISTORY_COMPLETION */

#if defined (SPECIFIC_COMPLETION_FUNCTIONS)
static void
bash_complete_username (ignore, ignore2)
     int ignore, ignore2;
{
  bash_complete_username_internal (TAB);
}

static void
bash_possible_username_completions (ignore, ignore2)
     int ignore, ignore2;
{
  bash_complete_username_internal ('?');
}

static void
bash_complete_username_internal (what_to_do)
     int what_to_do;
{
  bash_specific_completion
    (what_to_do, (Function *)username_completion_function);
}

static void
bash_complete_filename (ignore, ignore2)
     int ignore, ignore2;
{
  bash_complete_filename_internal (TAB);
}

static void
bash_possible_filename_completions (ignore, ignore2)
     int ignore, ignore2;
{
  bash_complete_filename_internal ('?');
}

static void
bash_complete_filename_internal (what_to_do)
     int what_to_do;
{
  Function  *orig_func;
  Function *orig_attempt_func;
  char *orig_rl_completer_word_break_characters;
  extern char *rl_completer_word_break_characters;

  orig_func = rl_completion_entry_function;
  orig_attempt_func = rl_attempted_completion_function;
  orig_rl_completer_word_break_characters = rl_completer_word_break_characters;
  rl_completion_entry_function = (Function *)filename_completion_function;
  rl_attempted_completion_function = (Function *)0x0;
  rl_completer_word_break_characters = " \t\n\"\'";

  rl_complete_internal (what_to_do);

  rl_completion_entry_function = orig_func;
  rl_attempted_completion_function = orig_attempt_func;
  rl_completer_word_break_characters = orig_rl_completer_word_break_characters;
}

static void
bash_complete_hostname (ignore, ignore2)
     int ignore, ignore2;
{
  bash_complete_hostname_internal (TAB);
}

static void
bash_possible_hostname_completions (ignore, ignore2)
     int ignore, ignore2;
{
  bash_complete_hostname_internal ('?');
}

static void
bash_complete_variable (ignore, ignore2)
     int ignore, ignore2;
{
  bash_complete_variable_internal (TAB);
}

static void
bash_possible_variable_completions (ignore, ignore2)
     int ignore, ignore2;
{
  bash_complete_variable_internal ('?');
}


static void
bash_complete_command (ignore, ignore2)
     int ignore, ignore2;
{
  bash_complete_command_internal (TAB);
}

static void
bash_possible_command_completions (ignore, ignore2)
     int ignore, ignore2;
{
  bash_complete_command_internal ('?');
}

static void
bash_complete_hostname_internal (what_to_do)
     int what_to_do;
{
  bash_specific_completion
    (what_to_do, (Function *)hostname_completion_function);
}

static void
bash_complete_variable_internal (what_to_do)
     int what_to_do;
{
  bash_specific_completion
    (what_to_do, (Function *)variable_completion_function);
}

static void
bash_complete_command_internal (what_to_do)
     int what_to_do;
{
  bash_specific_completion
    (what_to_do, (Function *)command_word_completion_function);
}

static void
bash_specific_completion (what_to_do, generator)
     int what_to_do;
     Function *generator;
{
  Function *orig_func;
  Function *orig_attempt_func;

  orig_func = rl_completion_entry_function;
  orig_attempt_func = rl_attempted_completion_function;
  rl_completion_entry_function = generator;
  rl_attempted_completion_function = (Function *)0x0;

  rl_complete_internal (what_to_do);

  rl_completion_entry_function = orig_func;
  rl_attempted_completion_function = orig_attempt_func;
}

#endif	/* SPECIFIC_COMPLETION_FUNCTIONS */

