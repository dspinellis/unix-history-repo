/* Copyright (C) 1987, 1989, 1991 Free Software Foundation, Inc.

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
#if defined (HAVE_VPRINTF)
#include <varargs.h>
#endif /* VPRINTF */

#include "../shell.h"
#include "../unwind_prot.h"
#include "../maxpath.h"
#include "../jobs.h"
#include "../builtins.h"
#include "../input.h"
#include "hashcom.h"

void no_args (), remember_args (), parse_and_execute_cleanup ();
extern int follow_symbolic_links, interactive;

/* Read a numeric arg for this_command_name, the name of the shell builtin
   that wants it.  LIST is the word list that the arg is to come from. */
int
get_numeric_arg (list)
     WORD_LIST *list;
{
  int count = 1;

  if (list)
    {
      if (sscanf (list->word->word, "%d", &count) != 1)
	{
	  builtin_error ("bad non-numeric arg `%s'", list->word->word);
	  throw_to_top_level ();
	}
      no_args (list->next);
    }
  return (count);
}

/* This is a lot like report_error (), but it is for shell builtins
   instead of shell control structures, and it won't ever exit the
   shell. */
#if defined (HAVE_VPRINTF)
/* VARARGS */
builtin_error (va_alist)
     va_dcl
{
  extern char *this_command_name;
  char *format;
  va_list args;

  if (this_command_name && *this_command_name)
    fprintf (stderr, "%s: ", this_command_name);

  va_start (args);
  format = va_arg (args, char *);
  vfprintf (stderr, format, args);
  va_end (args);
  fprintf (stderr, "\n");
}

#else /* Without VARARGS. */
builtin_error (format, arg1, arg2, arg3, arg4, arg5)
     char *format, *arg1, *arg2, *arg3, *arg4, *arg5;
{
  extern char *this_command_name;

  if (this_command_name && *this_command_name)
    fprintf (stderr, "%s: ", this_command_name);

  fprintf (stderr, format, arg1, arg2, arg3, arg4, arg5);
  fprintf (stderr, "\n");
  fflush (stderr);
}
#endif /* HAVE_VPRINTF */

/* Remember LIST in $0 ... $9, and REST_OF_ARGS.  If DESTRUCTIVE is
   non-zero, then discard whatever the existing arguments are, else
   only discard the ones that are to be replaced. */
void
remember_args (list, destructive)
     WORD_LIST *list;
     int destructive;
{
  register int i;
  extern WORD_LIST *copy_word_list ();

  for (i = 1; i < 10; i++)
    {
      if (destructive && dollar_vars[i])
	{
	  free (dollar_vars[i]);
	  dollar_vars[i] = (char *)NULL;
	}

      if (list)
	{
	  if (!destructive && dollar_vars[i])
	    free (dollar_vars[i]);

	  dollar_vars[i] = savestring (list->word->word);
	  list = list->next;
	}
    }

  /* If arguments remain, assign them to REST_OF_ARGS. */
  if (!list)
    {
      dispose_words (rest_of_args);
      rest_of_args = NULL;
    }
  else
    {
      rest_of_args = (WORD_LIST *)copy_word_list (list);
    }
}

/* Return if LIST is NULL else barf and jump to top_level. */
void
no_args (list)
     WORD_LIST *list;
{
  if (list)
    {
      builtin_error ("extra arguments");
      longjmp (top_level, DISCARD);
    }
}

/* Return the octal number parsed from STRING, or -1 to indicate
   that the string contained a bad number. */
int
read_octal (string)
     char *string;
{
  int result = 0;
  int digits = 0;

  while (*string && *string >= '0' && *string < '8')
    {
      digits++;
      result = (result * 8) + *string++ - '0';
    }

  if (!digits || result > 0777 || *string)
    result = -1;

  return (result);
}

/* Temporary static. */
char *dotted_filename = (char *)NULL;

/* Return the full pathname that FILENAME hashes to.  If FILENAME
   is hashed, but data->check_dot is non-zero, check ./FILENAME
   and return that if it is executable. */
char *
find_hashed_filename (filename)
     char *filename;
{
  extern HASH_TABLE *hashed_filenames;
  extern int hashing_disabled;
  register BUCKET_CONTENTS *item;

  if (hashing_disabled)
    return ((char *)NULL);

  item = find_hash_item (filename, hashed_filenames);

  if (item)
    {
      /* If this filename is hashed, but `.' comes before it in the path,
	 then see if `./filename' is an executable. */
      if (pathdata(item)->check_dot)
	{
	  if (dotted_filename)
	    free (dotted_filename);

	  dotted_filename = (char *)xmalloc (3 + strlen (filename));
	  strcpy (dotted_filename, "./");
	  strcat (dotted_filename, filename);

	  if (executable_file (dotted_filename))
	    return (dotted_filename);

	  /* Watch out.  If this file was hashed to "./filename", and
	     "./filename" is not executable, then return NULL. */

	  /* Since we already know "./filename" is not executable, what
	     we're really interested in is whether or not the `path'
	     portion of the hashed filename is equivalent to the current
	     directory, but only if it starts with a `.'.  (This catches
	     ./. and so on.)  same_file () is in execute_cmd.c; it tests
	     general Unix file equivalence -- same device and inode. */
	  {
	    char *path = pathdata (item)->path;

	    if (*path == '.')
	      {
		int same = 0;
		char *rindex (), *tail;

		tail = rindex (path, '/');

		if (tail)
		  {
		    *tail = '\0';
		    same = same_file
		      (".", path, (struct stat *)NULL, (struct stat *)NULL);
		    *tail = '/';
		  }
		if (same)
		  return ((char *)NULL);
	      }
	  }
	}
      return (pathdata (item)->path);
    }
  else
    return ((char *)NULL);
}

/* **************************************************************** */
/*								    */
/*		    Pushing and Popping a Context		    */
/*								    */
/* **************************************************************** */

WORD_LIST **dollar_arg_stack = (WORD_LIST **)NULL;
int dollar_arg_stack_slots = 0;
int dollar_arg_stack_index = 0;

void push_dollar_vars (), pop_dollar_vars ();

void
push_context ()
{
  extern int variable_context;

  push_dollar_vars ();
  variable_context++;
}

void
pop_context ()
{
  extern int variable_context;

  pop_dollar_vars ();
  kill_all_local_variables ();
  variable_context--;
}

/* Save the existing positional parameters on a stack. */
void
push_dollar_vars ()
{
  extern WORD_LIST *list_rest_of_args ();

  if (dollar_arg_stack_index + 2 > dollar_arg_stack_slots)
    {
      if (!dollar_arg_stack)
	{
	  dollar_arg_stack =
	    (WORD_LIST **)xrealloc (dollar_arg_stack,
				    (dollar_arg_stack_slots += 10)
				    * sizeof (WORD_LIST **));
	}
    }
  dollar_arg_stack[dollar_arg_stack_index] = list_rest_of_args ();
  dollar_arg_stack[++dollar_arg_stack_index] = (WORD_LIST *)NULL;
}

/* Restore the positional parameters from our stack. */
void
pop_dollar_vars ()
{
  if (!dollar_arg_stack || !dollar_arg_stack_index)
    return;

  remember_args (dollar_arg_stack[--dollar_arg_stack_index], 1);
  dispose_words (dollar_arg_stack[dollar_arg_stack_index]);
  dollar_arg_stack[dollar_arg_stack_index] = (WORD_LIST *)NULL;
}

/* Function called when one of the builtin commands detects a bad
   option. */
bad_option (s)
     char *s;
{
  builtin_error ("unknown option: %s", s);
}

/* Return a consed string which is the current working directory.
   FOR_WHOM is the name of the caller for error printing.  */
char *the_current_working_directory = (char *)NULL;

char *
get_working_directory (for_whom)
     char *for_whom;
{
  if (!follow_symbolic_links)
    {
      if (the_current_working_directory)
	free (the_current_working_directory);

      the_current_working_directory = (char *)NULL;
    }

  if (!the_current_working_directory)
    {
      char *directory, *getwd ();

      the_current_working_directory = (char *)xmalloc (MAXPATHLEN);
      directory = getwd (the_current_working_directory);
      if (!directory)
	{
	  fprintf (stderr, "%s: %s\n\r",
		   for_whom, the_current_working_directory);
	  free (the_current_working_directory);
	  the_current_working_directory = (char *)NULL;
	  return (char *)NULL;
	}
    }

  return (savestring (the_current_working_directory));
}

#if defined (JOB_CONTROL)
/* Return the job spec found in LIST. */
get_job_spec (list)
     WORD_LIST *list;
{
  register char *word;
  int job = NO_JOB;
  int substring = 0;

  if (!list)
    return (current_job);

  word = list->word->word;

  if (!*word)
    return (current_job);

  if (*word == '%')
    word++;

  if (digit (*word) && (sscanf (word, "%d", &job) == 1))
    return (job - 1);

  switch (*word)
    {
    case 0:
    case '%':
    case '+':
      return (current_job);

    case '-':
      return (previous_job);

    case '?':			/* Substring search requested. */
      substring++;
      word++;
      goto find_string;

    default:
    find_string:
      {
	register int i, wl = strlen (word);
	for (i = 0; i < job_slots; i++)
	  {
	    if (jobs[i])
	      {
		register PROCESS *p = jobs[i]->pipe;
		extern char *strindex ();
		do
		  {
		    if ((substring && strindex (p->command, word)) ||
			(strncmp (p->command, word, wl) == 0))
		      if (job != NO_JOB)
			{
			  builtin_error ("ambigious job spec: %s", word);
			  return (DUP_JOB);
			}
		      else
			job = i;

		    p = p->next;
		  }
		while (p != jobs[i]->pipe);
	      }
	  }
	return (job);
      }
    }
}
#endif /* JOB_CONTROL */

/* The command name of the currently running function. */
extern char *this_command_name;

int parse_and_execute_level = 0;

/* How to force parse_and_execute () to clean up after itself. */
void
parse_and_execute_cleanup ()
{
  run_unwind_frame ("parse_and_execute_top");
}

/* Parse and execute the commands in STRING.  Returns whatever
   execute_command () returns.  This frees STRING. */
int
parse_and_execute (string, from_file)
     char *string;
     char *from_file;
{
  extern int remember_on_history;
  extern int history_expansion_inhibited;
  extern int indirection_level;
  extern int builtin_pipe_in, builtin_pipe_out;
  extern COMMAND *global_command;
  extern char *indirection_level_string ();
  extern int pop_stream (), free ();

  int last_result = EXECUTION_SUCCESS;
  int code, jump_to_top_level = 0;
  char *orig_string = string;

  /* Unwind protect this invocation of parse_and_execute (). */
  begin_unwind_frame ("parse_and_execute_top");
    unwind_protect_int (parse_and_execute_level);
    unwind_protect_jmp_buf (top_level);
    unwind_protect_int (indirection_level);
    unwind_protect_int (interactive);
    unwind_protect_int (remember_on_history);
    unwind_protect_int (history_expansion_inhibited);
    add_unwind_protect (pop_stream, (char *)NULL);
    if (orig_string)
      add_unwind_protect (free, orig_string);
  end_unwind_frame ();

  parse_and_execute_level++;
  push_stream ();
  interactive = 0;
  indirection_level++;

  /* We don't remember text read by the shell this way on
     the history list, and we don't use !$ in shell scripts. */
  remember_on_history = 0;
  history_expansion_inhibited = 1;

  with_input_from_string (string, from_file);
  {
    COMMAND *command;

    while (*(bash_input.location.string))
      {
	if (interrupt_state)
	  {
	    last_result = EXECUTION_FAILURE;
	    break;
	  }

	/* Provide a location for functions which `longjmp (top_level)' to
	   jump to.  This prevents errors in substitution from restarting
	   the reader loop directly, for example. */
	code = setjmp (top_level);

	if (code)
	  {
	    switch (code)
	      {
	      case FORCE_EOF:
	      case EXITPROG:
		run_unwind_frame ("pe_dispose");
		/* Remember to call longjmp (top_level) after the old
		   value for it is restored. */
		jump_to_top_level = 1;
		goto out;

	      case DISCARD:
		run_unwind_frame ("pe_dispose");
		continue;

	      default:
		programming_error ("bad jump to top_level: %d", code);
		break;
	      }
	  }
	  
	if (parse_command () == 0)
	  {
	    if ((command = global_command) != (COMMAND *)NULL)
	      {
		extern struct fd_bitmap *new_fd_bitmap ();
		extern void dispose_fd_bitmap ();
		struct fd_bitmap *bitmap;

		bitmap = new_fd_bitmap (FD_BITMAP_SIZE);
		begin_unwind_frame ("pe_dispose");
		add_unwind_protect (dispose_fd_bitmap, bitmap);

		global_command = (COMMAND *)NULL;

		if (builtin_pipe_in != NO_PIPE)
		  bitmap->bitmap[builtin_pipe_in] = 1;

		if (builtin_pipe_out != NO_PIPE)
		  bitmap->bitmap[builtin_pipe_out] = 1;

		last_result =
		  execute_command_internal
		    (command, 0, NO_PIPE, NO_PIPE, bitmap);

		dispose_command (command);
		run_unwind_frame ("pe_dispose");
	      }
	  }
	else
	  {
	    last_result = EXECUTION_FAILURE;

	    /* Since we are shell compatible, syntax errors in a script
	       abort the execution of the script.  Right? */
	    break;
	  }
      }
  }

 out:

  run_unwind_frame ("parse_and_execute_top");

  if (interrupt_state && parse_and_execute_level == 0)
    throw_to_top_level ();

  if (jump_to_top_level)
    longjmp (top_level, code);

  return (last_result);
}

/* Return the address of the builtin named NAME.
   DISABLED_OKAY means find it even if the builtin is disabled. */
Function *
builtin_address_internal (name, disabled_okay)
     char *name;
     int disabled_okay;
{
  int hi, lo, mid, j;

  hi = num_shell_builtins - 1;
  lo = 0;

  while (lo <= hi)
    {
      mid = (lo + hi) / 2;
      j = strcmp (shell_builtins[mid].name, name);

      if (j == 0)
	{
	  if (shell_builtins[mid].function &&
	      (shell_builtins[mid].enabled || disabled_okay))
	    return (shell_builtins[mid].function);
	  else
	    return ((Function *)NULL);
	}
      if (j > 0)
	hi = mid - 1;
      else
	lo = mid + 1;
    }
  return ((Function *)NULL);
}

/* Perform a binary search and return the address of the builtin function
   whose name is NAME.  If the function couldn't be found, or the builtin
   is disabled or has no function associated with it, return NULL. */
Function *
find_shell_builtin (name)
	char *name;
{
  return (builtin_address_internal (name, 0));
}

/* Return the address of builtin with NAME, irregardless of its state of
   enableness. */
Function *
builtin_address (name)
     char *name;
{
  return (builtin_address_internal (name, 1));
}

static int
shell_builtin_compare (sbp1, sbp2)
     struct builtin *sbp1, *sbp2;
{
  return (strcmp (sbp1->name, sbp2->name));
}

/* Sort the table of shell builtins so that the binary search will work
   in find_shell_builtin. */
initialize_shell_builtins ()
{
  qsort (shell_builtins, num_shell_builtins, sizeof (struct builtin),
    shell_builtin_compare);
}
