/* execute_command.c -- Execute a COMMAND structure. */

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
#include <ctype.h>
#include <sys/types.h>
#include <sys/file.h>
#include "posixstat.h"
#include "filecntl.h"
#include <signal.h>

#if !defined (SIGABRT)
#define SIGABRT SIGIOT
#endif

#include <sys/param.h>
#include <errno.h>

#include "shell.h"
#include "y.tab.h"
#include "flags.h"
#include "hash.h"
#include "jobs.h"

#include "sysdefs.h"
#include <glob/fnmatch.h>

int builtin_pipe_in = NO_PIPE;
int builtin_pipe_out = NO_PIPE;

#if !defined (errno)
extern int errno;
#endif

extern int breaking, continuing, loop_level;
extern int interactive, login_shell;

#if defined (JOB_CONTROL)
extern int job_control;
extern int set_job_control ();
#endif /* JOB_CONTROL */

extern int getdtablesize ();
extern int close ();
extern char *strerror ();
extern char *string_list ();

#if defined (USG)
extern pid_t last_made_pid;
#endif

extern WORD_LIST *expand_words (), *expand_word ();
extern char *make_command_string ();

extern Function *find_shell_builtin (), *builtin_address ();

/* Static functions defined and used in this file. */
#if defined (NOTDEF)
/* Currently unused. */
static void close_all_files ();
#endif /* NOTDEF */
static void close_pipes ();
static void do_piping ();
static int do_redirection_internal (), do_redirections ();
static int expandable_redirection_filename ();
static int execute_shell_script ();
static void execute_disk_command ();
static int execute_builtin_or_function ();
static void execute_subshell_builtin_or_function ();
static void cleanup_redirects (), cleanup_func_redirects ();
static void bind_lastarg ();
static int add_undo_redirect ();
static void add_undo_close_redirect ();
static char *find_user_command_internal ();
static char *find_user_command_in_path ();

/* The value returned by the last synchronous command. */
int last_command_exit_value = 0;

/* The list of redirections to preform which will undo the redirections
   that I made in the shell. */
REDIRECT *redirection_undo_list = (REDIRECT *)NULL;

/* Use this as the function to call when adding unwind protects so we
   don't need to know what free() returns. */
static void
vfree(s)
     char *s;
{
  free (s);
}

#define FD_BITMAP_DEFAULT_SIZE 32
/* Functions to allocate and deallocate the structures used to pass
   information from the shell to its children about file descriptors
   to close. */
struct fd_bitmap *
new_fd_bitmap (size)
     long size;
{
  struct fd_bitmap *ret;

  ret = (struct fd_bitmap *)xmalloc (sizeof (struct fd_bitmap));

  ret->size = size;

  if (size)
    {
      ret->bitmap = (char *)xmalloc (size);
      bzero (ret->bitmap, size);
    }
  else
    ret->bitmap = (char *)NULL;
  return (ret);
}

void
dispose_fd_bitmap (fdbp)
     struct fd_bitmap *fdbp;
{
  if (fdbp->bitmap)
    free (fdbp->bitmap);

  free (fdbp);
}

void
close_fd_bitmap (fdbp)
     struct fd_bitmap *fdbp;
{
  register int i;

  if (fdbp)
    {
      for (i = 0; i < fdbp->size; i++)
	if (fdbp->bitmap[i])
	  {
	    close (i);
	    fdbp->bitmap[i] = 0;
	  }
    }
}

/* Execute the command passed in COMMAND.  COMMAND is exactly what
   read_command () places into GLOBAL_COMMAND.  See "shell.h" for the
   details of the command structure.

   EXECUTION_SUCCESS or EXECUTION_FAILURE are the only possible
   return values.  Executing a command with nothing in it returns
   success. */
execute_command (command)
     COMMAND *command;
{
  struct fd_bitmap *fd_close_bmap;
  int r;

  fd_close_bmap = new_fd_bitmap (FD_BITMAP_DEFAULT_SIZE);

  /* Just do the command, but not asynchronously. */
  r = execute_command_internal (command, 0, NO_PIPE, NO_PIPE, fd_close_bmap);
  dispose_fd_bitmap (fd_close_bmap);

#if defined (PROCESS_SUBSTITUTION)
  unlink_fifo_list ();
#endif

  return r;
}

/* Returns 1 if TYPE is a shell control structure type. */
int
shell_control_structure (type)
     enum command_type type;
{
  switch (type)
    {
    case cm_for:
    case cm_case:
    case cm_while:
    case cm_until:
    case cm_if:
    case cm_group:
      return (1);

    default:
      return (0);
    }
}

/* A function to use to unwind_protect the redirection undo list
   for loops. */
static void
cleanup_redirects (list)
     REDIRECT *list;
{
  do_redirections (list, 1, 0, 0);
  dispose_redirects (list);
}

/* Function to unwind_protect the redirections for functions and builtins. */
static void
cleanup_func_redirects (list)
     REDIRECT *list;
{
  do_redirections (list, 1, 0, 0);
}

#if defined (JOB_CONTROL)
/* A function to restore the signal mask to its proper value when the shell
   is interrupted or errors occur while creating a pipeline. */
static int
restore_signal_mask (set)
     sigset_t set;
{
  return (sigprocmask (SIG_SETMASK, &set, (sigset_t *)NULL));
}
#endif /* JOB_CONTROL */

/* A debugging function that can be called from gdb, for instance. */
open_files ()
{
  register int i;
  int f, fd_table_size;

  fd_table_size = getdtablesize ();

  fprintf (stderr, "pid %d open files:", getpid ());
  for (i = 3; i < fd_table_size; i++)
    {
      if ((f = fcntl (i, F_GETFD, 0)) != -1)
	fprintf (stderr, " %d (%s)", i, f ? "close" : "open");
    }
  fprintf (stderr, "\n");
}

execute_command_internal (command, asynchronous, pipe_in, pipe_out, 
			  fds_to_close)
     COMMAND *command;
     int asynchronous;
     int pipe_in, pipe_out;
     struct fd_bitmap *fds_to_close;
{
  int exec_result = EXECUTION_SUCCESS;
  int invert, ignore_return;
  REDIRECT *my_undo_list;

  if (!command || breaking || continuing)
    return (EXECUTION_SUCCESS);

  run_pending_traps ();

  invert = (command->flags & CMD_INVERT_RETURN) != 0;

  /* If a command was being explicitly run in a subshell, or if it is
     a shell control-structure, and it has a pipe, then we do the command
     in a subshell. */

  if ((command->flags & CMD_WANT_SUBSHELL) ||
      (command->flags & CMD_FORCE_SUBSHELL)  ||
      (shell_control_structure (command->type) &&
       (pipe_out != NO_PIPE || pipe_in != NO_PIPE || asynchronous)))
    {
      pid_t paren_pid;

      /* Fork a subshell, turn off the subshell bit, turn off job
	 control and call execute_command () on the command again. */
      paren_pid = make_child (savestring (make_command_string (command)),
			      asynchronous);
      if (paren_pid == 0)
	{
	  int user_subshell, return_code;

	  user_subshell = (command->flags & CMD_WANT_SUBSHELL) != 0;
	  command->flags &= ~(CMD_FORCE_SUBSHELL | CMD_WANT_SUBSHELL);

	  /* If a command is asynchronous in a subshell (like ( foo ) & or
	     the special case of an asynchronous GROUP command where the
	     the subshell bit is turned on down in case cm_group: below), 
	     turn off `asynchronous', so that two subshells aren't spawned.

	     This seems semantically correct to me.  For example, 
	     ( foo ) & seems to say ``do the command `foo' in a subshell
	     environment, but don't wait for that subshell to finish'',
	     and "{ foo ; bar } &" seems to me to be like functions or
	     builtins in the background, which executed in a subshell
	     environment.  I just don't see the need to fork two subshells. */

	  /* Don't fork again, we are already in a subshell. */
	  asynchronous = 0;

	  /* Subshells are neither login nor interactive. */
	  login_shell = interactive = 0;

#if defined (JOB_CONTROL)
	  /* Delete all traces that there were any jobs running.  This is
	     only for subshells. */
	  without_job_control ();
#endif /* JOB_CONTROL */
	  do_piping (pipe_in, pipe_out);

	  if (fds_to_close)
	    close_fd_bitmap (fds_to_close);

	  /* Do redirections, then dispose of them before recursive call. */
	  if (command->redirects)
	    {
	      if (!(do_redirections (command->redirects, 1, 0, 0) == 0))
		exit (EXECUTION_FAILURE);

	      dispose_redirects (command->redirects);
	      command->redirects = (REDIRECT *)NULL;
	    }

	  return_code = execute_command_internal
	    (command, asynchronous, NO_PIPE, NO_PIPE, fds_to_close);

	  /* If we were explicitly placed in a subshell with (), we need
	     to do the `shell cleanup' things, such as running traps[0]. */
	  if (user_subshell)
	    run_exit_trap ();

	  exit (return_code);
	}
      else
	{
	  close_pipes (pipe_in, pipe_out);

	  /* If we are part of a pipeline, and not the end of the pipeline,
	     then we should simply return and let the last command in the
	     pipe be waited for.  If we are not in a pipeline, or are the
	     last command in the pipeline, then we wait for the subshell 
	     and return its exit status as usual. */
	  if (pipe_out != NO_PIPE)
	    return (EXECUTION_SUCCESS);

	  stop_pipeline (asynchronous, (COMMAND *)NULL);

	  if (!asynchronous)
	    {
	      last_command_exit_value = wait_for (paren_pid);

	      /* If we have to, invert the return value. */
	      if (invert)
		{
		  if (last_command_exit_value == EXECUTION_SUCCESS)
		    return (EXECUTION_FAILURE);
		  else
		    return (EXECUTION_SUCCESS);
		}
	      else
		return (last_command_exit_value);
	    }
	  else
	    {
	      if (interactive)
		describe_pid (paren_pid);

	      run_pending_traps ();

	      return (EXECUTION_SUCCESS);
	    }
	}
    }

  /* Handle WHILE FOR CASE etc. with redirections.  (Also '&' input
     redirection.)  */
  do_redirections (command->redirects, 1, 1, 0);
  my_undo_list = (REDIRECT *)copy_redirects (redirection_undo_list);

  begin_unwind_frame ("loop_redirections");

  if (my_undo_list)
    add_unwind_protect ((Function *)cleanup_redirects, my_undo_list);

  ignore_return = (command->flags & CMD_IGNORE_RETURN) != 0;

  switch (command->type)
    {
    case cm_for:
      if (ignore_return)
	command->value.For->flags |= CMD_IGNORE_RETURN;
      exec_result = execute_for_command (command->value.For);
      break;

    case cm_case:
      if (ignore_return)
	command->value.Case->flags |= CMD_IGNORE_RETURN;
      exec_result = execute_case_command (command->value.Case);
      break;

    case cm_while:
      if (ignore_return)
	command->value.While->flags |= CMD_IGNORE_RETURN;
      exec_result = execute_while_command (command->value.While);
      break;

    case cm_until:
      if (ignore_return)
	command->value.While->flags |= CMD_IGNORE_RETURN;
      exec_result = execute_until_command (command->value.While);
      break;

    case cm_if:
      if (ignore_return)
	command->value.If->flags |= CMD_IGNORE_RETURN;
      exec_result = execute_if_command (command->value.If);
      break;

    case cm_group:

      /* This code can be executed from either of two paths: an explicit
	 '{}' command, or via a function call.  If we are executed via a
	 function call, we have already taken care of the function being
	 executed in the background (down there in execute_simple_command ()),
	 and this command should *not* be marked as asynchronous.  If we
	 are executing a regular '{}' group command, and asynchronous == 1,
	 we must want to execute the whole command in the background, so we
	 need a subshell, and we want the stuff executed in that subshell
	 (this group command) to be executed in the foreground of that
	 subshell (i.e. there will not be *another* subshell forked).

	 What we do is to force a subshell if asynchronous, and then call
	 execute_command_internal again with asynchronous still set to 1,
	 but with the original group command, so the printed command will
	 look right.

	 The code above that handles forking off subshells will note that
	 both subshell and async are on, and turn off async in the child
	 after forking the subshell (but leave async set in the parent, so
	 the normal call to describe_pid is made).  This turning off
	 async is *crucial*; if it is not done, this will fall into an
	 infinite loop of executions through this spot in subshell after
	 subshell until the process limit is exhausted. */

      if (asynchronous)
	{
	  command->flags |= CMD_FORCE_SUBSHELL;
	  exec_result =
	    execute_command_internal (command, 1, pipe_in, pipe_out,
				      fds_to_close);
	}
      else
	{
	  if (ignore_return && command->value.Group->command)
	    command->value.Group->command->flags |= CMD_IGNORE_RETURN;
	  exec_result =
	    execute_command_internal (command->value.Group->command,
				      asynchronous, pipe_in, pipe_out,
				      fds_to_close);
	}
      break;

    case cm_simple:
      {
	pid_t last_pid = last_made_pid;

#if defined (JOB_CONTROL)
	extern int already_making_children;
#endif /* JOB_CONTROL */
	if (ignore_return && command->value.Simple)
	  command->value.Simple->flags |= CMD_IGNORE_RETURN;
	exec_result =
	  execute_simple_command (command->value.Simple, pipe_in, pipe_out,
				  asynchronous, fds_to_close);

	/* The temporary environment should be used for only the simple
	   command immediately following its definition. */
	dispose_used_env_vars ();

#if (defined (Ultrix) && defined (mips)) || !defined (HAVE_ALLOCA)
	/* Reclaim memory allocated with alloca () on machines which
	   may be using the alloca emulation code. */
	(void) alloca (0);
#endif /* (Ultrix && mips) || !HAVE_ALLOCA */

	/* If we forked to do the command, then we must wait_for ()
	   the child. */
#if defined (JOB_CONTROL)
	if (already_making_children && pipe_out == NO_PIPE)
#else
	  if (pipe_out == NO_PIPE)
#endif /* JOB_CONTROL */
	    {
	      if (last_pid != last_made_pid)
		{
		  stop_pipeline (asynchronous, (COMMAND *)NULL);

		  if (asynchronous)
		    {
		      if (interactive)
			describe_pid (last_made_pid);
		    }
		  else
#if !defined (JOB_CONTROL)
		    /* Do not wait for asynchronous processes started from
		       startup files. */
		    if (last_made_pid != last_asynchronous_pid)
#endif
		      /* When executing a shell function that executes other
			 commands, this causes the last simple command in
			 the function to be waited for twice. */
		      exec_result = wait_for (last_made_pid);
		}
	    }
      }
      if (!ignore_return && exit_immediately_on_error && !invert &&
	  (exec_result != EXECUTION_SUCCESS))
	{
	  last_command_exit_value = exec_result;
	  run_pending_traps ();
	  longjmp (top_level, EXITPROG);
	}

      break;

    case cm_connection:
      switch (command->value.Connection->connector)
	{
	  /* Do the first command asynchronously. */
	case '&':
	  {
	    COMMAND *tc = command->value.Connection->first;
	    if (ignore_return && tc)
	      tc->flags |= CMD_IGNORE_RETURN;

#if !defined (JOB_CONTROL)
	    {
	      REDIRECT *tr = 
		make_redirection (0, r_inputa_direction,
				  make_word ("/dev/null"));
	      tr->next = tc->redirects;
	      tc->redirects = tr;
	    }
#endif /* !JOB_CONTROL */
	    exec_result = execute_command_internal (tc, 1, pipe_in, pipe_out,
						    fds_to_close);
	    if (command->value.Connection->second)
	      {
		if (ignore_return && command->value.Connection->second)
		  command->value.Connection->second->flags |= CMD_IGNORE_RETURN;

		exec_result =
		  execute_command_internal (command->value.Connection->second,
					    asynchronous, pipe_in, pipe_out,
					    fds_to_close);
	      }
	  }
	  break;

	case ';':
	  /* Just call execute command on both of them. */
	  if (ignore_return)
	    {
	      if (command->value.Connection->first)
		command->value.Connection->first->flags |= CMD_IGNORE_RETURN;
	      if (command->value.Connection->second)
		command->value.Connection->second->flags |= CMD_IGNORE_RETURN;
	    }
	  execute_command (command->value.Connection->first);
	  exec_result =
	    execute_command_internal (command->value.Connection->second,
				      asynchronous, pipe_in, pipe_out,
				      fds_to_close);
	  break;

	case '|':
	  {
	    int prev, fildes[2], new_bitmap_size, dummyfd;
	    COMMAND *cmd;
	    struct fd_bitmap *fd_bitmap;

#if defined (JOB_CONTROL)
	    sigset_t set, oset;
	    BLOCK_CHILD (set, oset);
#endif /* JOB_CONTROL */

	    prev = pipe_in;
	    cmd = command;

	    while (cmd &&
		   cmd->type == cm_connection &&
		   cmd->value.Connection &&
		   cmd->value.Connection->connector == '|')
	      {
		/* Make a pipeline between the two commands. */
		if (pipe (fildes) < 0)
		  {
		    report_error ("pipe error: %s", strerror (errno));
#if defined (JOB_CONTROL)
		    terminate_current_pipeline ();
		    kill_current_pipeline ();
#endif /* JOB_CONTROL */
		    last_command_exit_value = EXECUTION_FAILURE;
		    /* The unwind-protects installed below will take care
		       of closing all of the open file descriptors. */
		    throw_to_top_level ();
		  }
		else
		  {
		    /* Here is a problem: with the new file close-on-exec
		       code, the read end of the pipe (fildes[0]) stays open
		       in the first process, so that process will never get a
		       SIGPIPE.  There is no way to signal the first process
		       that it should close fildes[0] after forking, so it
		       remains open.  No SIGPIPE is ever sent because there
		       is still a file descriptor open for reading connected
		       to the pipe.  We take care of that here.  This passes
		       around a bitmap of file descriptors that must be
		       closed after making a child process in
		       execute_simple_command. */

		    /* We need fd_bitmap to be at least as big as fildes[0].
		       If fildes[0] is less than fds_to_close->size, then
		       use fds_to_close->size. */

		    if (fildes[0] < fds_to_close->size)
		      new_bitmap_size = fds_to_close->size;
		    else
		      new_bitmap_size = fildes[0] + 8;

		    fd_bitmap = new_fd_bitmap (new_bitmap_size);

		    /* Now copy the old information into the new bitmap. */
		    bcopy (fds_to_close->bitmap, fd_bitmap->bitmap,
			   fds_to_close->size);

		    /* And mark the pipe file descriptors to be closed. */
		    fd_bitmap->bitmap[fildes[0]] = 1;

		    /* In case there are pipe or out-of-processes errors, we
		       want all these file descriptors to be closed when
		       unwind-protects are run, and the storage used for the
		       bitmaps freed up. */
		    begin_unwind_frame ("pipe-file-descriptors");
		    add_unwind_protect (dispose_fd_bitmap, fd_bitmap);
		    add_unwind_protect (close_fd_bitmap, fd_bitmap);
		    if (prev >= 0)
		      add_unwind_protect (close, prev);
		    dummyfd = fildes[1];
		    add_unwind_protect (close, dummyfd);

#if defined (JOB_CONTROL)
		    add_unwind_protect (restore_signal_mask, oset);
#endif /* JOB_CONTROL */

		    if (ignore_return && cmd->value.Connection->first)
		      cmd->value.Connection->first->flags |=
			CMD_IGNORE_RETURN;
		    execute_command_internal
		      (cmd->value.Connection->first, asynchronous, prev,
		       fildes[1], fd_bitmap);

		    if (prev >= 0)
		      close (prev);
		    
		    prev = fildes[0];
		    close (fildes[1]);

		    dispose_fd_bitmap (fd_bitmap);
		    discard_unwind_frame ("pipe-file-descriptors");
		  }
		cmd = cmd->value.Connection->second;
	      }

	    /* Now execute the rightmost command in the pipeline.  */
	    if (ignore_return && cmd)
	      cmd->flags |= CMD_IGNORE_RETURN;
	    exec_result =
	      execute_command_internal
		(cmd, asynchronous, prev, pipe_out, fds_to_close);

	    if (prev >= 0)
	      close (prev);

#if defined (JOB_CONTROL)
	    UNBLOCK_CHILD (oset);
#endif
	  }
	  break;

	case AND_AND:
	  if (asynchronous)
	    {
	      /* If we have something like `a && b &', run the && stuff in a
		 subshell.  Force a subshell and just call
		 execute_command_internal again.  Leave asynchronous on
		 so that we get a report from the parent shell about the
		 background job. */
	      command->flags |= CMD_FORCE_SUBSHELL;
	      exec_result = execute_command_internal (command, 1, pipe_in,
			      pipe_out, fds_to_close);
	      break;
	    }

	  /* Execute the first command.  If the result of that is successful,
	     then execute the second command, otherwise return. */

	  if (command->value.Connection->first)
	    command->value.Connection->first->flags |= CMD_IGNORE_RETURN;

	  exec_result = execute_command (command->value.Connection->first);
	  if (exec_result == EXECUTION_SUCCESS)
	    {
	      if (ignore_return && command->value.Connection->second)
		command->value.Connection->second->flags |=
		  CMD_IGNORE_RETURN;

	      exec_result =
		execute_command (command->value.Connection->second);
	    }
	  break;

	case OR_OR:
	  if (asynchronous)
	    {
	      /* If we have something like `a || b &', run the || stuff in a
		 subshell.  Force a subshell and just call
		 execute_command_internal again.  Leave asynchronous on
		 so that we get a report from the parent shell about the
		 background job. */
	      command->flags |= CMD_FORCE_SUBSHELL;
	      exec_result = execute_command_internal (command, 1, pipe_in,
			      pipe_out, fds_to_close);
	      break;
	    }

	  /* Execute the first command.  If the result of that is successful,
	     then return, otherwise execute the second command. */

	  if (command->value.Connection->first)
	    command->value.Connection->first->flags |= CMD_IGNORE_RETURN;

	  exec_result = execute_command (command->value.Connection->first);
	  if (exec_result != EXECUTION_SUCCESS)
	    {
	      if (ignore_return && command->value.Connection->second)
		command->value.Connection->second->flags |=
		  CMD_IGNORE_RETURN;

	      exec_result =
		execute_command (command->value.Connection->second);
	    }

	  break;

	default:
	  programming_error ("Bad connector `%d'!",
			     command->value.Connection->connector);
	  longjmp (top_level, DISCARD);
	  break;
	}
      break;

    case cm_function_def:
      exec_result = intern_function (command->value.Function_def->name,
				     command->value.Function_def->command);
      break;

    default:
      programming_error
	("execute_command: Bad command type `%d'!", command->type);
    }

  if (my_undo_list)
    {
      do_redirections (my_undo_list, 1, 0, 0);
      dispose_redirects (my_undo_list);
    }

  discard_unwind_frame ("loop_redirections");

  /* Invert the return value if we have to */
  if (invert)
    {
      if (exec_result == EXECUTION_SUCCESS)
	exec_result = EXECUTION_FAILURE;
      else
	exec_result = EXECUTION_SUCCESS;
    }

  last_command_exit_value = exec_result;
  run_pending_traps ();
  return (last_command_exit_value);
}

/* Execute a FOR command.  The syntax is: FOR word_desc IN word_list;
   DO command; DONE */
execute_for_command (for_command)
     FOR_COM *for_command;
{
  /* I just noticed that the Bourne shell leaves word_desc bound to the
     last name in word_list after the FOR statement is done.  This seems
     wrong to me; I thought that the variable binding should be lexically
     scoped, i.e., only would last the duration of the FOR command.  This
     behaviour can be gotten by turning on the lexical_scoping switch. */

  register WORD_LIST *releaser, *list;
  WORD_DESC *temp = for_command->name;
  char *identifier;
  SHELL_VAR *old_value = (SHELL_VAR *)NULL; /* Remember the old value of x. */
  int retval = EXECUTION_SUCCESS;
  extern int dispose_words ();
  extern int dispose_variable ();

  if (!check_identifier (temp))
    return (EXECUTION_FAILURE);

  loop_level++;
  identifier = temp->word;

  list = releaser = expand_words (for_command->map_list, 0);

  begin_unwind_frame ("for");
  add_unwind_protect (dispose_words, releaser);

  if (lexical_scoping)
    {
      old_value = copy_variable (find_variable (identifier));
      if (old_value)
	add_unwind_protect (dispose_variable, old_value);
    }

  while (list)
    {
      QUIT;
      bind_variable (identifier, list->word->word);
      if (for_command->flags & CMD_IGNORE_RETURN)
	for_command->action->flags |= CMD_IGNORE_RETURN;
      execute_command (for_command->action);
      retval = last_command_exit_value;
      QUIT;

      if (breaking)
	{
	  breaking--; 
	  break;
	}

      if (continuing)
	{
	  continuing--;
	  if (continuing)
	    break;
	}

      list = list->next;
    }

  loop_level--;

  if (lexical_scoping)
    {
      if (!old_value)
	makunbound (identifier, shell_variables);
      else
	{
	  SHELL_VAR *new_value;

	  new_value = bind_variable (identifier, value_cell(old_value));
	  new_value->attributes = old_value->attributes;
	}
    }

  run_unwind_frame ("for");
  return (retval);
}

/* Execute a CASE command.  The syntax is: CASE word_desc IN pattern_list ESAC.
   The pattern_list is a linked list of pattern clauses; each clause contains
   some patterns to compare word_desc against, and an associated command to
   execute. */
execute_case_command (case_command)
     CASE_COM *case_command;
{
  extern int dispose_words ();

  register WORD_LIST *list;
  WORD_LIST *wlist;
  PATTERN_LIST *clauses;
  char *word;
  int retval;

  wlist = expand_word (case_command->word, 0);
  clauses = case_command->clauses;
  word = (wlist) ? string_list (wlist) : savestring ("");
  retval = EXECUTION_SUCCESS;

  begin_unwind_frame ("case");
  add_unwind_protect (dispose_words, wlist);
  add_unwind_protect ((Function *)vfree, word);

  while (clauses)
    {
      QUIT;
      list = clauses->patterns;
      while (list)
	{
	  WORD_LIST *es = expand_word (list->word, 0);
	  char *pattern = (es) ? es->word->word : "";

	  if (fnmatch (pattern, word, FNM_NOESCAPE) != FNM_NOMATCH)
	    {
	      dispose_words (es);
	      if (clauses->action && 
		  (case_command->flags & CMD_IGNORE_RETURN))
		clauses->action->flags |= CMD_IGNORE_RETURN;
	      execute_command (clauses->action);
	      retval = last_command_exit_value;
	      goto exit_command;
	    }
	  dispose_words (es);
	  list = list->next;
	  QUIT;
	}
      clauses = clauses->next;
    }
 exit_command:
  run_unwind_frame ("case");
  return (retval);
}

#define CMD_WHILE 0
#define CMD_UNTIL 1

/* The WHILE command.  Syntax: WHILE test DO action; DONE.
   Repeatedly execute action while executing test produces
   EXECUTION_SUCCESS. */
execute_while_command (while_command)
     WHILE_COM *while_command;
{
  return (execute_while_or_until (while_command, CMD_WHILE));
}

/* UNTIL is just like WHILE except that the test result is negated. */
execute_until_command (while_command)
     WHILE_COM *while_command;
{
  return (execute_while_or_until (while_command, CMD_UNTIL));
}

/* The body for both while and until.  The only difference between the
   two is that the test value is treated differently.  TYPE is
   CMD_WHILE or CMD_UNTIL.  The return value for both commands should
   be EXECUTION_SUCCESS if no commands in the body are executed, and
   the status of the last command executed in the body otherwise. */
execute_while_or_until (while_command, type)
     WHILE_COM *while_command;
     int type;
{
  extern int breaking;
  extern int continuing;
  int commands_executed = 0;
  int return_value;

  loop_level++;
  while_command->test->flags |= CMD_IGNORE_RETURN;

  while (1)
    {
      return_value = execute_command (while_command->test);

      if (type == CMD_WHILE && return_value != EXECUTION_SUCCESS)
	break;
      if (type == CMD_UNTIL && return_value == EXECUTION_SUCCESS)
	break;

      QUIT;
      commands_executed = 1;

      if (while_command->flags & CMD_IGNORE_RETURN)
	while_command->action->flags |= CMD_IGNORE_RETURN;
      execute_command (while_command->action);

      QUIT;

      if (breaking)
	{
	  breaking--;
	  break;
	}

      if (continuing)
	{
	  continuing--;
	  if (continuing)
	    break;
	}
    }
  loop_level--;

  if (commands_executed)
    return (last_command_exit_value);
  else
    return (EXECUTION_SUCCESS);
}

/* IF test THEN command [ELSE command].
   IF also allows ELIF in the place of ELSE IF, but
   the parser makes *that* stupidity transparent. */
execute_if_command (if_command)
     IF_COM *if_command;
{
  int return_value;

  if_command->test->flags |= CMD_IGNORE_RETURN;
  return_value = execute_command (if_command->test);

  if (return_value == EXECUTION_SUCCESS)
    {
      QUIT;
      if (if_command->true_case && (if_command->flags & CMD_IGNORE_RETURN))
	  if_command->true_case->flags |= CMD_IGNORE_RETURN;
      return (execute_command (if_command->true_case));
    }
  else
    {
      QUIT;

      if (if_command->false_case &&
	  (if_command->flags & CMD_IGNORE_RETURN))
	{
	  if_command->false_case->flags |= CMD_IGNORE_RETURN;
	}

      return (execute_command (if_command->false_case));
    }
}

/* The name of the command that is currently being executed.
   `test' needs this, for example. */
char *this_command_name;

static void
bind_lastarg (arg)
     char *arg;
{
  SHELL_VAR *var;

  if (!arg)
    arg = "";
  var = bind_variable ("_", arg);
  var->attributes &= ~att_exported;
}

/* For catching RETURN in a function. */
int return_catch_flag = 0;
int return_catch_value;
jmp_buf return_catch;

/* The meaty part of all the executions.  We have to start hacking the
   real execution of commands here.  Fork a process, set things up,
   execute the command. */
execute_simple_command (simple_command, pipe_in, pipe_out, async, fds_to_close)
     SIMPLE_COM *simple_command;
     int pipe_in, pipe_out;
     struct fd_bitmap *fds_to_close;
{
  extern int command_string_index, variable_context, line_number;
  extern char *the_printed_command;
  extern pid_t last_command_subst_pid;
  WORD_LIST *expand_words (), *copy_word_list ();
  WORD_LIST *words, *lastword;
  char *command_line, *lastarg;
  int first_word_quoted, result;
  pid_t old_last_command_subst_pid;

  result = EXECUTION_SUCCESS;

  /* If we're in a function, update the pseudo-line-number information. */
  if (variable_context)
    line_number++;

  /* Remember what this command line looks like at invocation. */
  command_string_index = 0;
  print_simple_command (simple_command);
  command_line = (char *)alloca (1 + strlen (the_printed_command));
  strcpy (command_line, the_printed_command);

  first_word_quoted =
    simple_command->words ? simple_command->words->word->quoted : 0;

  old_last_command_subst_pid = last_command_subst_pid;

  /* If we are re-running this as the result of executing the `command'
     builtin, do not expand the command words a second time. */
  if ((simple_command->flags & CMD_INHIBIT_EXPANSION) == 0)
    words = expand_words (simple_command->words);
  else
    words = copy_word_list (simple_command->words);

  lastarg = (char *)NULL;
  begin_unwind_frame ("simple-command");

  /* It is possible for WORDS not to have anything left in it.
     Perhaps all the words consisted of `$foo', and there was
     no variable `$foo'. */
  if (words)
    {
      extern int dispose_words ();
      extern Function *last_shell_builtin, *this_shell_builtin;
      Function *builtin;
      SHELL_VAR *func;
      char *auto_resume_value;

      if (echo_command_at_execute)
	{
	  extern char *indirection_level_string ();
	  char *line = string_list (words);

	  if (line && *line)
	    fprintf (stderr, "%s%s\n", indirection_level_string (), line);

	  if (line)
	    free (line);
	}

      if (simple_command->flags & CMD_NO_FUNCTIONS)
	func = (SHELL_VAR *)NULL;
      else
	func = find_function (words->word->word);

      add_unwind_protect (dispose_words, words);

      QUIT;

      /* Bind the last word in this command to "$_" after execution. */
      for (lastword = words; lastword->next; lastword = lastword->next);
      lastarg = lastword->word->word;

#if defined (JOB_CONTROL)
      /* Is this command a job control related thing? */
      if (words->word->word[0] == '%')
	{
	  int result;

	  if (async)
	    this_command_name = "bg";
	  else
	    this_command_name = "fg";
	      
	  last_shell_builtin = this_shell_builtin;
	  this_shell_builtin = builtin_address (this_command_name);
	  result = (*this_shell_builtin) (words);
	  goto return_result;
	}

      /* One other possiblilty.  The user may want to resume an existing job.
	 If they do, find out whether this word is a candidate for a running
	 job. */
      if ((auto_resume_value = get_string_value ("auto_resume")) &&
	  !first_word_quoted &&
	  !words->next &&
	  words->word->word[0] &&
	  !simple_command->redirects &&
	  pipe_in == NO_PIPE &&
	  pipe_out == NO_PIPE &&
	  !async)
	{
	  char *word = words->word->word;
	  register int i, wl = strlen (word), exact;

	  exact = strcmp (auto_resume_value, "exact") == 0;
	  for (i = job_slots - 1; i > -1; i--)
	    {
	      if (jobs[i])
		{
		  register PROCESS *p = jobs[i]->pipe;
		  do
		    {
		      if ((JOBSTATE (i) == JSTOPPED) &&
			  (strncmp (p->command, word,
				    exact ? strlen (p->command) : wl) == 0))
			{
			  int started_status;

			  run_unwind_frame ("simple-command");
			  last_shell_builtin = this_shell_builtin;
			  this_shell_builtin = builtin_address ("fg");

			  started_status = start_job (i, 1);

			  if (started_status < 0)
			    return (EXECUTION_FAILURE);
			  else
			    return (started_status);
			}
		      p = p->next;
		    }
		  while (p != jobs[i]->pipe);
		}
	    }
	}
#endif /* JOB_CONTROL */

      /* Remember the name of this command globally. */
      this_command_name = words->word->word;

      QUIT;

      /* Not a running job.  Do normal command processing. */
      maybe_make_export_env ();

      /* This command could be a shell builtin or a user-defined function.
	 If so, and we have pipes, then fork a subshell in here.  Else, just
	 do the command. */

      if (func)
	builtin = (Function *)NULL;
      else
	builtin = find_shell_builtin (this_command_name);

      last_shell_builtin = this_shell_builtin;
      this_shell_builtin = builtin;

      if (builtin || func)
	{
	  put_command_name_into_env (this_command_name);
	  if ((pipe_in != NO_PIPE) || (pipe_out != NO_PIPE) || async)
	    {
	      if (make_child (savestring (command_line), async) == 0)
		{
		  execute_subshell_builtin_or_function
		    (words, simple_command->redirects, builtin, func,
		     pipe_in, pipe_out, async, fds_to_close,
		     simple_command->flags);
		}
	      else
		{
		  close_pipes (pipe_in, pipe_out);
		  goto return_result;
		}
	    }
	  else
	    {
	      result = execute_builtin_or_function
		(words, builtin, func, simple_command->redirects, fds_to_close,
		 simple_command->flags);

	      goto return_result;
	    }
	}

      execute_disk_command (words, simple_command->redirects, command_line,
			    pipe_in, pipe_out, async, fds_to_close);

      goto return_result;
    }
  else if (pipe_in != NO_PIPE || pipe_out != NO_PIPE || async)
    {
      /* We have a null command, but we really want a subshell to take
	 care of it.  Just fork, do piping and redirections, and exit. */
      if (make_child (savestring (""), async) == 0)
	{
	  do_piping (pipe_in, pipe_out);

	  if (do_redirections (simple_command->redirects, 1, 0, 0) == 0)
	    exit (EXECUTION_SUCCESS);
	  else
	    exit (EXECUTION_FAILURE);
	}
      else
	{
	  close_pipes (pipe_in, pipe_out);
	  result = EXECUTION_SUCCESS;
	  goto return_result;
	}
    }
  else
    {
      /* Even if there aren't any command names, pretend to do the
	 redirections that are specified.  The user expects the side
	 effects to take place.  If the redirections fail, then return
	 failure.  Otherwise, if a command substitution took place while
	 expanding the command or a redirection, return the value of that
	 substition.  Otherwise, return EXECUTION_SUCCESS. */

      if (do_redirections (simple_command->redirects, 0, 0, 0) != 0)
	result = EXECUTION_FAILURE;
      else if (old_last_command_subst_pid != last_command_subst_pid)
	result = last_command_exit_value;
      else
	result = EXECUTION_SUCCESS;
    }

 return_result:
  bind_lastarg (lastarg);
  run_unwind_frame ("simple-command");
  return (result);
}

/* Execute a shell builtin or function in a subshell environment.  This
   routine does not return; it only calls exit().  If BUILTIN is non-null,
   it points to a function to call to execute a shell builtin; otherwise
   VAR points at the body of a function to execute.  WORDS is the arguments
   to the command, REDIRECTS specifies redirections to perform before the
   command is executed. */
static void
execute_subshell_builtin_or_function (words, redirects, builtin, var,
				      pipe_in, pipe_out, async, fds_to_close,
				      flags)
     WORD_LIST *words;
     REDIRECT *redirects;
     Function *builtin;
     SHELL_VAR *var;
     int pipe_in, pipe_out, async;
     struct fd_bitmap *fds_to_close;
     int flags;
{
  extern int login_shell, interactive;
#if defined (JOB_CONTROL)
  extern int jobs_builtin ();
#endif /* JOB_CONTROL */

  /* A subshell is neither a login shell nor interactive. */
  login_shell = interactive = 0;

#if defined (JOB_CONTROL)
  /* Eradicate all traces of job control after we fork the subshell, so
     all jobs begun by this subshell are in the same process group as
     the shell itself. */

  /* Allow the output of `jobs' to be piped. */
  if (builtin == jobs_builtin && !async &&
      (pipe_out != NO_PIPE || pipe_in != NO_PIPE))
    kill_current_pipeline ();
  else
    without_job_control ();
#endif /* JOB_CONTROL */

  do_piping (pipe_in, pipe_out);

  if (fds_to_close)
    close_fd_bitmap (fds_to_close);

  if (do_redirections (redirects, 1, 0, 0) != 0)
    exit (EXECUTION_FAILURE);

  if (builtin)
    {
      extern jmp_buf top_level;
      int result;

      /* Save the values of pipe_in and pipe_out for
	 possible later use by parse_and_execute (). */
      builtin_pipe_in = pipe_in;
      builtin_pipe_out = pipe_out;

      /* Give builtins a place to jump back to on failure,
	 so we don't go back up to main(). */
      result = setjmp (top_level);

      if (result)
	exit (result);
      else
	exit ((*builtin) (words->next));
    }
  else
    {
      extern int variable_context, line_number;
      extern void dispose_command ();
      COMMAND *fc, *tc;
      int result, return_val;

      tc = (COMMAND *)function_cell (var);
      fc = (COMMAND *)NULL;

      remember_args (words->next, 1);
      line_number = 0;
#if defined (JOB_CONTROL)
      stop_pipeline (async, (COMMAND *)NULL);
#endif

      begin_unwind_frame ("subshell_function_calling");
      unwind_protect_int (variable_context);
      unwind_protect_int (return_catch_flag);
      unwind_protect_jmp_buf (return_catch);
      add_unwind_protect (dispose_command, fc);

      /* We can do this because function bodies are always guaranteed to
	 be group commands, according to the grammar in parse.y.  If we
	 don't do this now, execute_command_internal will graciously fork
	 another subshell for us, and we'll lose contact with the rest of
	 the pipeline and fail to get any SIGPIPE that might be sent. */

      if (tc->type == cm_group)
	fc = (COMMAND *)copy_command (tc->value.Group->command);
      else
	fc = (COMMAND *)copy_command (tc);

      if (fc && (flags & CMD_IGNORE_RETURN))
	fc->flags |= CMD_IGNORE_RETURN;

      /* result = execute_command (fc); doesn't work.
	 We need to explicitly specify the pipes in and out so that they
	 are closed in all the processes that rely on their being closed.
	 If they are not, it is possible to not get the SIGPIPE that we
	 need to kill all the processes sharing the pipe. */

      variable_context++;
      return_catch_flag++;
      return_val = setjmp (return_catch);

      if (return_val)
	result = return_catch_value;
      else
	result =
	  execute_command_internal (fc, 0, pipe_in, pipe_out, fds_to_close);

      run_unwind_frame ("subshell_function_calling");

      exit (result);
    }
}

/* Execute a builtin or function in the current shell context.  If BUILTIN
   is non-null, it is the builtin command to execute, otherwise VAR points
   to the body of a function.  WORDS are the command's arguments, REDIRECTS
   are the redirections to perform.  FDS_TO_CLOSE is the usual bitmap of
   file descriptors to close.

   If BUILTIN is exec_builtin, the redirections specified in REDIRECTS are
   not undone before this function returns. */
static int
execute_builtin_or_function (words, builtin, var, redirects,
			     fds_to_close, flags)
     WORD_LIST *words;
     Function *builtin;
     SHELL_VAR *var;
     REDIRECT *redirects;
     struct fd_bitmap *fds_to_close;
     int flags;
{
  extern int exec_builtin (), eval_builtin ();
  int result = EXECUTION_FAILURE;
  int redir_result;
  REDIRECT *saved_undo_list;

  redir_result = do_redirections (redirects, 1, 1, 0);

  if (redir_result != 0)
    return (EXECUTION_FAILURE);

  saved_undo_list = redirection_undo_list;

  /* Calling the "exec" builtin changes redirections forever. */
  if (builtin == exec_builtin)
    {
      dispose_redirects (saved_undo_list);
      saved_undo_list = (REDIRECT *)NULL;
    }
  else
    {
      begin_unwind_frame ("saved redirects");
      add_unwind_protect (cleanup_func_redirects, (char *)saved_undo_list);
    }

  redirection_undo_list = (REDIRECT *)NULL;

  if (builtin)
    {
      int old_e_flag = exit_immediately_on_error;

      /* The eval builtin calls parse_and_execute, which does not know about
	 the setting of flags, and always calls the execution functions with
	 flags that will exit the shell on an error if -e is set.  If the
	 eval builtin is being called, and we're supposed to ignore the exit
	 value of the command, we turn the -e flag off ourselves, then
	 restore it when the command completes. */
      if ((builtin == eval_builtin) && (flags & CMD_IGNORE_RETURN))
	{
	  begin_unwind_frame ("eval_builtin");
	  unwind_protect_int (exit_immediately_on_error);
	  exit_immediately_on_error = 0;
	}

      result = ((*builtin) (words->next));

      if ((builtin == eval_builtin) && (flags & CMD_IGNORE_RETURN))
	{
	  exit_immediately_on_error += old_e_flag;
	  discard_unwind_frame ("eval_builtin");
	}
    }
  else
    {
      extern void dispose_command ();
      extern int pop_context ();
      extern int line_number;
      int return_val;
      COMMAND *tc;

      tc = (COMMAND *)copy_command (function_cell (var));
      if (tc && (flags & CMD_IGNORE_RETURN))
	tc->flags |= CMD_IGNORE_RETURN;

      begin_unwind_frame ("function_calling");
      push_context ();
      add_unwind_protect (pop_context, (char *)NULL);
      add_unwind_protect (dispose_command, (char *)tc);
      unwind_protect_int (return_catch_flag);
      unwind_protect_int (line_number);
      unwind_protect_jmp_buf (return_catch);

      /* Note the second argument of "1", meaning that we discard
	 the current value of "$*"!  This is apparently the right thing. */
      remember_args (words->next, 1);

      line_number = 0;
      return_catch_flag++;
      return_val =  setjmp (return_catch);

      if (return_val)
	result = return_catch_value;
      else
	result = 
	  execute_command_internal (tc, 0, NO_PIPE, NO_PIPE, fds_to_close);

      run_unwind_frame ("function_calling");
    }

  redirection_undo_list = saved_undo_list;
  if (builtin != exec_builtin)
    discard_unwind_frame ("saved redirects");
  do_redirections (redirection_undo_list, 1, 0, 0);

  return (result);
}

/* Execute a simple command that is hopefully defined in a disk file
   somewhere.

   1) fork ()
   2) connect pipes
   3) look up the command
   4) do redirections
   5) execve ()
   6) If the execve failed, see if the file has executable mode set.
   If so, and it isn't a directory, then execute its contents as
   a shell script.

   Note that the filename hashing stuff has to take place up here,
   in the parent.  This is probably why the Bourne style shells
   don't handle it, since that would require them to go through
   this gnarly hair, for no good reason.  */
static void
execute_disk_command (words, redirects, command_line, pipe_in, pipe_out,
		      async, fds_to_close)
     WORD_LIST *words;
     REDIRECT *redirects;
     char *command_line;
     int pipe_in, pipe_out, async;
     struct fd_bitmap *fds_to_close;
{
  char **make_word_array (), *find_user_command (),
  *find_hashed_filename ();

  char *hashed_file = (char *)NULL, *command, **args;

  /* Don't waste time trying to find hashed data for a pathname
     that is already completely specified. */

  if (!absolute_program (words->word->word))
    hashed_file = find_hashed_filename (words->word->word);

  if (hashed_file)
    command = savestring (hashed_file);
  else
    {
      /* A command containing a slash is not looked up in PATH. */
      if (absolute_program (words->word->word))
	command = savestring (words->word->word);
      else
	command = find_user_command (words->word->word);

      if (command && !hashing_disabled)
	{
	  extern int dot_found_in_search;
	  /* A command name containing a slash is not saved in the
	     hash table. */
	  if (!absolute_program (words->word->word))
	    remember_filename (words->word->word, command, dot_found_in_search);
	  /* Increase the number of hits to 1. */
	  find_hashed_filename (words->word->word);
	}
    }

  if (command)
    {
      put_command_name_into_env (command);
    }

  /* We have to make the child before we check for the non-existance
     of COMMAND, since we want the error messages to be redirected. */

  if (make_child (savestring (command_line), async) == 0)
    {
      do_piping (pipe_in, pipe_out);

      /* Execve expects the command name to be in args[0].  So we
	 leave it there, in the same format that the user used to
	 type it in. */
      args = make_word_array (words);

      if (do_redirections (redirects, 1, 0, 0) != 0)
	exit (EXECUTION_FAILURE);

      if (!command)
	{
	  report_error ("%s: command not found", args[0]);
	  exit (EXECUTION_FAILURE);
	}

      /* This functionality is now provided by close-on-exec of the
	 file descriptors manipulated by redirection and piping.
	 Some file descriptors still need to be closed in all children
	 because of the way bash does pipes; fds_to_close is a 
	 bitmap of all such file descriptors. */
      if (fds_to_close)
	close_fd_bitmap (fds_to_close);

      signal (SIGCHLD, SIG_DFL);
      exit (shell_execve (command, args, export_env));
    }
  else
    {
      /* Make sure that the pipes are closed in the parent. */
      close_pipes (pipe_in, pipe_out);
      free (command);
    }
}

/* If the operating system on which we're running does not handle
   the #! executable format, then help out.  SAMPLE is the text read
   from the file, SAMPLE_LEN characters.  COMMAND is the name of
   the script; it and ARGS, the arguments given by the user, will
   become arguments to the specified interpreter.  ENV is the environment
   to pass to the interpreter.

   The word immediately following the #! is the interpreter to execute.
   A single argument to the interpreter is allowed. */
static int
execute_shell_script (sample, sample_len, command, args, env)
     unsigned char *sample;
     int sample_len;
     char *command;
     char **args, **env;
{
  extern char *shell_name;
  register int i;
  char *execname, *firstarg;
  int start, size_increment, larry;

  /* Find the name of the interpreter to exec. */
  for (i = 2; whitespace (sample[i]) && i < sample_len; i++)
    ;

  for (start = i;
       !whitespace (sample[i]) && sample[i] != '\n' && i < sample_len;
       i++)
    ;

  execname = (char *)xmalloc (1 + (i - start));
  strncpy (execname, sample + start, i - start);
  execname[i - start] = '\0';
  size_increment = 1;

  /* Now the argument, if any. */
  firstarg = (char *)NULL;
  for (start = i;
       whitespace (sample[i]) && sample[i] != '\n' && i < sample_len;
       i++)
    ;

  /* If there is more text on the line, then it is an argument for the
     interpreter. */
  if (i < sample_len && sample[i] != '\n' && !whitespace (sample[i]))
    {
      for (start = i;
	   !whitespace (sample[i]) && sample[i] != '\n' && i < sample_len;
	   i++)
	;
      firstarg = (char *)xmalloc (1 + (i - start));
      strncpy (firstarg, sample + start, i - start);
      firstarg[i - start] = '\0';

      size_increment = 2;
    }

  larry = array_len (args) + size_increment;

  args = (char **)xrealloc (args, (1 + larry) * sizeof (char *));

  for (i = larry - 1; i; i--)
    args[i] = args[i - size_increment];

  args[0] = execname;
  if (firstarg)
    {
      args[1] = firstarg;
      args[2] = command;
    }
  else
    args[1] = command;

  args[larry] = (char *)NULL;

  return (shell_execve (execname, args, env));
}

/* Call execve (), handling interpreting shell scripts, and handling
   exec failures. */
int
shell_execve (command, args, env)
     char *command;
     char **args, **env;
{
#if defined (isc386) && defined (_POSIX_SOURCE)
  __setostype (0);		/* Turn on USGr3 semantics. */
  execve (command, args, env);
  __setostype (1);		/* Turn the POSIX semantics back on. */
#else
  execve (command, args, env);
#endif /* !(isc386 && _POSIX_SOURCE) */

  /* If we get to this point, then start checking out the file.
     Maybe it is something we can hack ourselves. */
  {
    struct stat finfo;

    if (errno != ENOEXEC)
      {
	if ((stat (command, &finfo) == 0) &&
	    (S_ISDIR (finfo.st_mode)))
	  report_error ("%s: is a directory", args[0]);
	else
	  file_error (command);

	return (EXECUTION_FAILURE);
      }
    else
      {
	/* This file is executable.
	   If it begins with #!, then help out people with losing operating
	   systems.  Otherwise, check to see if it is a binary file by seeing
	   if the first line (or up to 30 characters) are in the ASCII set.
	   Execute the contents as shell commands. */
	extern char *shell_name;
	int larray = array_len (args) + 1;
	int i, should_exec = 0;

	{
	  int fd = open (command, O_RDONLY);
	  if (fd != -1)
	    {
	      unsigned char sample[80];
	      int sample_len = read (fd, &sample[0], 80);

	      /* Is this supposed to be an executable script? */
	      /* If so, the format of the line is "#! interpreter [argument]".
		 A single argument is allowed.  The BSD kernel restricts
		 the length of the entire line to 32 characters (32 bytes
		 being the size of the BSD exec header), but we allow 80
		 characters. */

	      if (sample[0] == '#' && sample[1] == '!')
		{
		  close (fd);
		  return (execute_shell_script (sample, sample_len,
		  				command, args, env));
		}
#if defined (NOTDEF)
#if defined (HAVE_CSH) && ( defined (Bsd) || defined (Ultrix) )
	      /* If this system has Csh, then keep the old
		 BSD semantics. */
	      else if (sample_len > 0 && sample[0] == '#')
		{
		  /* Scripts starting with a # are for Csh. */
		  shell_name = savestring ("/bin/csh");
		  should_exec = 1;
		}
#endif /* HAVE_CSH */
#endif /* NOTDEF */
	      else
		{
		  if (sample_len != -1)
		    if (check_binary_file (sample, sample_len))
		      {
			report_error ("%s: cannot execute binary file",
				      command);
			return (EX_BINARY_FILE);
		      }
		}
	      close (fd);
	    }
	}
#if defined (JOB_CONTROL)
	/* Forget about the way that job control was working. We are
	   in a subshell. */
	without_job_control ();
#endif /* JOB_CONTROL */
#if defined (ALIAS)
	/* Forget about any aliases that we knew of.  We are in a subshell. */
	delete_all_aliases ();
#endif /* ALIAS */
	/* Insert the name of this shell into the argument list. */
	args = (char **)xrealloc (args, (1 + larray) * sizeof (char *));

	for (i = larray - 1; i; i--)
	  args[i] = args[i - 1];

	args[0] = shell_name;
	args[1] = command;
	args[larray] = (char *)NULL;

	if (args[0][0] == '-')
	  args[0]++;

	if (should_exec)
	  {
	    struct stat finfo;

#if defined (isc386) && defined (_POSIX_SOURCE)
	    __setostype (0);	/* Turn on USGr3 semantics. */
	    execve (shell_name, args, env);
	    __setostype (1);	/* Turn the POSIX semantics back on. */
#else
	    execve (shell_name, args, env);
#endif /* isc386 && _POSIX_SOURCE */

	    /* Oh, no!  We couldn't even exec this! */
	    if ((stat (args[0], &finfo) == 0) && (S_ISDIR (finfo.st_mode)))
	      report_error ("%s: is a directory", args[0]);
	    else
	      file_error (args[0]);

	    return (EXECUTION_FAILURE);
	  }
	else
	  {
	    extern jmp_buf subshell_top_level;
	    extern int subshell_argc;
	    extern char **subshell_argv;
	    extern char **subshell_envp;

	    subshell_argc = larray;
	    subshell_argv = args;
	    subshell_envp = env;
	    longjmp (subshell_top_level, 1);
	  }
      }
  }
}

/* Currently unused. */
#if defined (NOTDEF)
static void
close_all_files ()
{
  register int i, fd_table_size;

  fd_table_size = getdtablesize ();

  for (i = 3; i < fd_table_size; i++)
    close (i);
}
#endif /* NOTDEF */

static void
close_pipes (in, out)
     int in, out;
{
  if (in >= 0) close (in);
  if (out >= 0) close (out);
}

/* Redirect input and output to be from and to the specified pipes.
   NO_PIPE and REDIRECT_BOTH are handled correctly. */
static void
do_piping (pipe_in, pipe_out)
     int pipe_in, pipe_out;
{
  if (pipe_in != NO_PIPE)
    {
      dup2 (pipe_in, 0);
      close (pipe_in);
    }
  if (pipe_out != NO_PIPE)
    {
      dup2 (pipe_out, 1);
      close (pipe_out);

      if (pipe_out == REDIRECT_BOTH)
	dup2 (1, 2);
    }
}

/* Defined in flags.c.  Non-zero means don't overwrite existing files. */
extern int noclobber;

#define AMBIGUOUS_REDIRECT -1
#define NOCLOBBER_REDIRECT -2
/* Perform the redirections on LIST.  If FOR_REAL, then actually make
   input and output file descriptors, otherwise just do whatever is
   neccessary for side effecting.  INTERNAL says to remember how to
   undo the redirections later, if non-zero.  If SET_CLEXEC is non-zero,
   file descriptors opened in do_redirection () have their close-on-exec
   flag set. */
static int
do_redirections (list, for_real, internal, set_clexec)
     REDIRECT *list;
     int for_real, internal;
{
  register int error;
  register REDIRECT *temp = list;

  if (internal && redirection_undo_list)
    {
      dispose_redirects (redirection_undo_list);
      redirection_undo_list = (REDIRECT *)NULL;
    }

  while (temp)
    {
      extern char *strerror ();

      error = do_redirection_internal (temp, for_real, internal, set_clexec);

      if (error)
	{
	  char *redirection_expand (), *itos ();
	  char *filename;

	  if (expandable_redirection_filename (temp))
	    {
	      filename = redirection_expand (temp->redirectee.filename);
	      if (!filename)
		filename = savestring ("");
	    }
	  else
	    filename = itos (temp->redirectee.dest);

	  switch (error)
	    {
	    case AMBIGUOUS_REDIRECT:
	      report_error ("%s: Ambiguous redirect", filename);
	      break;

	    case NOCLOBBER_REDIRECT:
	      report_error ("%s: Cannot clobber existing file", filename);
	      break;

	    default:
	      report_error ("%s: %s", filename, strerror (error));
	      break;
	    }

	  free (filename);
	  return (error);
	}

      temp = temp->next;
    }
  return (0);
}

/* Return non-zero if the redirection pointed to by REDIRECT has a
   redirectee.filename that can be expanded. */
static int
expandable_redirection_filename (redirect)
     REDIRECT *redirect;
{
  int result;

  switch (redirect->instruction)
    {
    case r_output_direction:
    case r_appending_to:
    case r_input_direction:
    case r_inputa_direction:
    case r_err_and_out:
    case r_input_output:
    case r_output_force:
    case r_duplicating_input_word:
    case r_duplicating_output_word:
      result = 1;
      break;

    default:
      result = 0;
    }
  return (result);
}

/* Expand the word in WORD returning a string.  If WORD expands to
   multiple words (or no words), then return NULL. */
char *
redirection_expand (word)
     WORD_DESC *word;
{
  char *result;
  WORD_LIST *make_word_list (), *expand_words_no_vars ();
  WORD_LIST *tlist1, *tlist2;

  tlist1 = make_word_list (copy_word (word), (WORD_LIST *)NULL);
  tlist2 = expand_words_no_vars (tlist1);
  dispose_words (tlist1);

  if (!tlist2 || tlist2->next)
    {
      /* We expanded to no words, or to more than a single word.
	 Dispose of the word list and return NULL. */
      if (tlist2)
	dispose_words (tlist2);
      return ((char *)NULL);
    }
  result = string_list (tlist2);
  dispose_words (tlist2);
  return (result);
}

/* Do the specific redirection requested.  Returns errno in case of error.
   If FOR_REAL is zero, then just do whatever is neccessary to produce the
   appropriate side effects.   REMEMBERING, if non-zero, says to remember
   how to undo each redirection.  If SET_CLEXEC is non-zero, then
   we set all file descriptors > 2 that we open to be close-on-exec.  */
static int
do_redirection_internal (redirect, for_real, remembering, set_clexec)
     REDIRECT *redirect;
     int for_real, remembering;
{
  WORD_DESC *redirectee = redirect->redirectee.filename;
  int fd, redirector = redirect->redirector;
  char *redirectee_word;
  enum r_instruction ri = redirect->instruction;
  REDIRECT *new_redirect;

  if (ri == r_duplicating_input_word || ri == r_duplicating_output_word)
    {
      /* We have [N]>&WORD or [N]<&WORD.  Expand WORD, then translate
	 the redirection into a new one and continue. */
      redirectee_word = redirection_expand (redirectee);

      if (redirectee_word[0] == '-' && redirectee_word[1] == '\0')
	{
	  new_redirect = make_redirection (redirector, r_close_this, 0);
	}
      else if (all_digits (redirectee_word))
	{
	  if (ri == r_duplicating_input_word)
	    {
	      new_redirect = make_redirection
		(redirector, r_duplicating_input, atoi (redirectee_word));
	    }
	  else
	    {
	      new_redirect = make_redirection
		(redirector, r_duplicating_output, atoi (redirectee_word));
	    }
	}
      else if (ri == r_duplicating_output_word && redirector == 1)
	{
	  new_redirect = make_redirection
	    (1, r_err_and_out, make_word (redirectee_word));
	}
      else
	{
	  free (redirectee_word);
	  return (AMBIGUOUS_REDIRECT);
	}

      free (redirectee_word);

      /* Set up the variables needed by the rest of the function from the
	 new redirection. */
      if (new_redirect->instruction == r_err_and_out)
	{
	  char *alloca_hack;

	  /* Copy the word without allocating any memory that must be
	     explicitly freed. */
	  redirectee = (WORD_DESC *)alloca (sizeof (WORD_DESC));
	  bcopy (new_redirect->redirectee.filename,
		 redirectee, sizeof (WORD_DESC));

	  alloca_hack = (char *)
	    alloca (1 + strlen (new_redirect->redirectee.filename->word));
	  redirectee->word = alloca_hack;
	  strcpy (redirectee->word, new_redirect->redirectee.filename->word);
	}
      else
	/* It's guaranteed to be an integer, and shouldn't be freed. */
	redirectee = new_redirect->redirectee.filename;

      redirector = new_redirect->redirector;
      ri = new_redirect->instruction;

      /* Overwrite the flags element of the old redirect with the new value. */
      redirect->flags = new_redirect->flags;
      dispose_redirects (new_redirect);
    }

  switch (ri)
    {
    case r_output_direction:
    case r_appending_to:
    case r_input_direction:
    case r_inputa_direction:
    case r_err_and_out:		/* command &>filename */
    case r_input_output:
    case r_output_force:

      if (!(redirectee_word = redirection_expand (redirectee)))
	return (AMBIGUOUS_REDIRECT);

      /* If we are in noclobber mode, you are not allowed to overwrite
	 existing files.  Check first. */
      if (noclobber && (ri == r_output_direction ||
		      ri == r_input_output ||
		      ri == r_err_and_out))
      {
	struct stat buf;
	if ((stat (redirectee_word, &buf) == 0) &&
	    (S_ISREG (buf.st_mode)))
	  return (NOCLOBBER_REDIRECT);
      }

      fd = open (redirectee_word, redirect->flags, 0666);
#if defined (AFS_CREATE_BUG)
      if (fd < 0 && errno == EACCES)
	fd = open (redirectee_word, (redirect->flags & ~O_CREAT), 0666);
#endif /* AFS_CREATE_BUG */
      free (redirectee_word);

      if (fd < 0 )
	return (errno);

      if (for_real)
	{
	  if (remembering)
	    /* Only setup to undo it if the thing to undo is active. */
	    if ((fd != redirector) && (fcntl (redirector, F_GETFD, 0) != -1))
	      add_undo_redirect (redirector);
	    else
	      add_undo_close_redirect (redirector);

	  if ((fd != redirector) && (dup2 (fd, redirector) < 0))
	    return (errno);

	  /*
	   * If we're remembering, then this is the result of a while, for
	   * or until loop with a loop redirection, or a function/builtin
	   * executing in the parent shell with a redirection.  In the
	   * function/builtin case, we want to set all file descriptors > 2
	   * to be close-on-exec to duplicate the effect of the old
	   * for i = 3 to NOFILE close(i) loop.  In the case of the loops,
	   * both sh and ksh leave the file descriptors open across execs.
	   * The Posix standard mentions only the exec builtin.
	   */
	  if (set_clexec && (redirector > 2))
	    SET_CLOSE_ON_EXEC (redirector);
	}
      if (fd != redirector)
	close (fd);		/* Don't close what we just opened! */

      /* If we are hacking both stdout and stderr, do the stderr
	 redirection here. */
      if (ri == r_err_and_out)
	{
	  if (for_real)
	    {
	      if (remembering)
		add_undo_redirect (2);
	      if (dup2 (1, 2) < 0)
		return (errno);
	    }
	}
      break;

    case r_reading_until:
    case r_deblank_reading_until:
      {
	/* REDIRECTEE is a pointer to a WORD_DESC containing the text of
	   the new input.  Place it in a temporary file. */
	int document_index = 0;
	char *document = (char *)NULL;

	/* Expand the text if the word that was specified had no quoting.
	   Note that the text that we expand is treated exactly as if it
	   were surrounded by double-quotes.  */

	if (!redirectee)
	  document = savestring ("");
	else
	  {
	    if (!redirectee->quoted)
	      {
		WORD_LIST *temp_word_list =
		  (WORD_LIST *)expand_string (redirectee->word,
					      Q_HERE_DOCUMENT);

		document = string_list (temp_word_list);
		if (!document)
		  document = savestring ("");
		dispose_words (temp_word_list);
	      }
	    else
	      {
		document = redirectee->word;
	      }
	    document_index = strlen (document);

	    {
	      char filename[40];
	      pid_t pid = getpid ();

	      /* Make the filename for the temp file. */
	      sprintf (filename, "/tmp/t%d-sh", pid);

	      fd = open (filename, O_TRUNC | O_WRONLY | O_CREAT, 0666);
	      if (fd < 0)
		{
		  if (!redirectee->quoted)
		    free (document);
		  return (errno);
		}

	      write (fd, document, document_index);
	      close (fd);

	      if (!redirectee->quoted)
		free (document);

	      /* Make the document really temporary.  Also make it the
		 input. */
	      fd = open (filename, O_RDONLY, 0666);

	      if (unlink (filename) < 0 || fd < 0)
		return (errno);

	      if (for_real)
		{
		  if (remembering)
		    /* Only setup to undo it if the thing to undo is active. */
		    if ((fd != redirector) &&
			(fcntl (redirector, F_GETFD, 0) != -1))
		      add_undo_redirect (redirector);
		    else
		      add_undo_close_redirect (redirector);

		  if (dup2 (fd, redirector) < 0)
		    return (errno);

		  if (set_clexec && (redirector > 2))
		    SET_CLOSE_ON_EXEC (redirector);
		}
	      close (fd);
	    }
	  }
      }
      break;

    case r_duplicating_input:
    case r_duplicating_output:
      if (for_real)
	{
	  if (remembering)
	    /* Only setup to undo it if the thing to undo is active. */
	    if (((int)redirectee != redirector) &&
		(fcntl (redirector, F_GETFD, 0) != -1))
	      add_undo_redirect (redirector);
	    else
	      add_undo_close_redirect (redirector);

	  /* This is correct.  2>&1 means dup2 (1, 2); */
	  if (dup2 ((int)redirectee, redirector) < 0)
	    return (errno);

	  /* First duplicate the close-on-exec state of redirectee.  dup2
	     leaves the flag unset on the new descriptor, which means it
	     stays open.  Only set the close-on-exec bit for file descriptors
	     greater than 2 in any case, since 0-2 should always be open
	     unless closed by something like `exec 2<&-'. */
	  /* if ((already_set || set_unconditionally) && (ok_to_set))
		set_it () */
	  if (((fcntl ((int)redirectee, F_GETFD, 0) == 1) || set_clexec) &&
	       (redirector > 2))
	    SET_CLOSE_ON_EXEC (redirector);
	}
      break;

    case r_close_this:
      if (for_real)
	{
	  if (remembering && (fcntl (redirector, F_GETFD, 0) != -1))
	    add_undo_redirect (redirector);

	  close (redirector);
	}
      break;
    }
  return (0);
}

#define SHELL_FD_BASE	10

/* Remember the file descriptor associated with the slot FD,
   on REDIRECTION_UNDO_LIST.  Note that the list will be reversed
   before it is executed. */
static int
add_undo_redirect (fd)
     int fd;
{
  int new_fd, clexec_flag;
  REDIRECT *new_redirect, *closer;

  new_fd = fcntl (fd, F_DUPFD, SHELL_FD_BASE);

  if (new_fd < 0)
    {
      file_error ("redirection error");
      return (-1);
    }
  else
    {
      clexec_flag = fcntl (fd, F_GETFD, 0);
      closer = make_redirection (new_fd, r_close_this, 0);
      new_redirect = make_redirection (fd, r_duplicating_output, new_fd);
      new_redirect->next = closer;
      closer->next = redirection_undo_list;
      redirection_undo_list = new_redirect;
      /*
       * File descriptors used only for saving others should always be
       * marked close-on-exec.  Unfortunately, we have to preserve the
       * close-on-exec state of the file descriptor we are saving, since
       * fcntl (F_DUPFD) sets the new file descriptor to remain open
       * across execs.  If, however, the file descriptor whose state we
       * are saving is <= 2, we can just set the close-on-exec flag,
       * because file descriptors 0-2 should always be open-on-exec,
       * and the restore above in do_redirection() will take care of it.
       */
      if (clexec_flag || fd < 3)
	SET_CLOSE_ON_EXEC (new_fd);
    }
  return (0);
}

/* Set up to close FD when we are finished with the current command
   and its redirections. */
static void
add_undo_close_redirect (fd)
     int fd;
{
  REDIRECT *closer;

  closer = make_redirection (fd, r_close_this, 0);
  closer->next = redirection_undo_list;
  redirection_undo_list = closer;
}

intern_function (name, function)
     WORD_DESC *name;
     COMMAND *function;
{
  SHELL_VAR *var;

  if (!check_identifier (name))
    return (EXECUTION_FAILURE);

  var = find_function (name->word);
  if (var && readonly_p (var))
    {
      report_error ("%s: readonly function", var->name);
      return (EXECUTION_FAILURE);
    }

  bind_function (name->word, function);
  return (EXECUTION_SUCCESS);
}

/* Make sure that identifier is a valid shell identifier, i.e.
   does not contain a dollar sign, nor is quoted in any way.  Nor
   does it consist of all digits. */
check_identifier (word)
     WORD_DESC *word;
{
  if (word->dollar_present || word->quoted || all_digits (word->word))
    {
      report_error ("`%s' is not a valid identifier", word->word);
      return (0);
    }
  else
    return (1);
}

#define u_mode_bits(x) (((x) & 0000700) >> 6)
#define g_mode_bits(x) (((x) & 0000070) >> 3)
#define o_mode_bits(x) (((x) & 0000007) >> 0)
#define X_BIT(x) (x & 1)

/* Return some flags based on information about this file.
   The EXISTS bit is non-zero if the file is found.
   The EXECABLE bit is non-zero the file is executble.
   Zero is returned if the file is not found. */
int
file_status (name)
     char *name;
{
  struct stat finfo;
  static int user_id = -1;

  /* Determine whether this file exists or not. */
  if (stat (name, &finfo) < 0)
    return (0);

  /* If the file is a directory, then it is not "executable" in the
     sense of the shell. */
  if (S_ISDIR (finfo.st_mode))
    return (FS_EXISTS);

  /* Find out if the file is actually executable.  By definition, the
     only other criteria is that the file has an execute bit set that
     we can use. */
  if (user_id == -1)
    user_id = geteuid ();

  /* Root only requires execute permission for any of owner, group or
     others to be able to exec a file. */
  if (user_id == 0)
    {
      int bits;

      bits = (u_mode_bits (finfo.st_mode) |
	      g_mode_bits (finfo.st_mode) |
	      o_mode_bits (finfo.st_mode));

      if (X_BIT (bits))
	return (FS_EXISTS | FS_EXECABLE);
    }

  /* If we are the owner of the file, the owner execute bit applies. */
  if (user_id == finfo.st_uid && X_BIT (u_mode_bits (finfo.st_mode)))
    return (FS_EXISTS | FS_EXECABLE);

  /* If we are in the owning group, the group permissions apply. */
  if (group_member (finfo.st_gid) && X_BIT (g_mode_bits (finfo.st_mode)))
    return (FS_EXISTS | FS_EXECABLE);

  /* If `others' have execute permission to the file, then so do we,
     since we are also `others'. */
  if (X_BIT (o_mode_bits (finfo.st_mode)))
    return (FS_EXISTS | FS_EXECABLE);
  else
    return (FS_EXISTS);
}

/* Return non-zero if FILE exists and is executable.
   Note that this function is the definition of what an
   executable file is; do not change this unless YOU know
   what an executable file is. */
int
executable_file (file)
     char *file;
{
  if (file_status (file) & FS_EXECABLE)
    return (1);
  else
    return (0);
}

/* DOT_FOUND_IN_SEARCH becomes non-zero when find_user_command ()
   encounters a `.' as the directory pathname while scanning the
   list of possible pathnames; i.e., if `.' comes before the directory
   containing the file of interest. */
int dot_found_in_search = 0;

/* Locate the executable file referenced by NAME, searching along
   the contents of the shell PATH variable.  Return a new string
   which is the full pathname to the file, or NULL if the file
   couldn't be found.  If a file is found that isn't executable,
   and that is the only match, then return that. */
char *
find_user_command (name)
     char *name;
{
  return (find_user_command_internal (name, FS_EXEC_PREFERRED));
}

/* Locate the file referenced by NAME, searching along the contents
   of the shell PATH variable.  Return a new string which is the full
   pathname to the file, or NULL if the file couldn't be found.  This
   returns the first file found. */
char *
find_path_file (name)
     char *name;
{
  return (find_user_command_internal (name, FS_EXISTS));
}

static char *
find_user_command_internal (name, flags)
     char *name;
     int flags;
{
  char *path_list;

  path_list = get_string_value ("PATH");

  if (!path_list)
    return (savestring (name));

  return (find_user_command_in_path (name, path_list, flags));
}

/* Return the next element from PATH_LIST, a colon separated list of
   paths.  PATH_INDEX_POINTER is the address of an index into PATH_LIST;
   the index is modified by this function.
   Return the next element of PATH_LIST or NULL if there are no more. */
static char *
get_next_path_element (path_list, path_index_pointer)
     char *path_list;
     int *path_index_pointer;
{
  extern char *extract_colon_unit ();
  char *path;

  path = extract_colon_unit (path_list, path_index_pointer);

  if (!path)
    return (path);

  if (!*path)
    {
      free (path);
      path = savestring (".");
    }

  return (path);
}

char *
user_command_matches (name, flags, state)
     char *name;
     int flags, state;
{
  register int i;
  char *path_list;
  int  path_index;
  char *path_element;
  char *match;
  static char **match_list = NULL;
  static int match_list_size = 0;
  static int match_index = 0;

  if (!state)
    {
      /* Create the list of matches. */
      if (!match_list)
	{
	  match_list =
	    (char **) xmalloc ((match_list_size = 5) * sizeof(char *));

	  for (i = 0; i < match_list_size; i++)
	    match_list[i] = 0;
	}

      /* Clear out the old match list. */
      for (i = 0; i < match_list_size; i++)
	match_list[i] = NULL;

      /* We haven't found any files yet. */
      match_index = 0;

      path_list = get_string_value ("PATH");
      path_index = 0;

      while (path_list && path_list[path_index])
	{
	  char *find_user_command_in_path ();

	  path_element = get_next_path_element (path_list, &path_index);

	  if (!path_element)
	    break;

	  match = find_user_command_in_path (name, path_element, flags);

	  free (path_element);

	  if (!match)
	    continue;

	  if (match_index + 1 == match_list_size)
	    match_list = (char **)xrealloc
	      (match_list, ((match_list_size += 10) + 1) * sizeof (char *));
	  match_list[match_index++] = match;
	  match_list[match_index] = (char *)NULL;
	}

      /* We haven't returned any strings yet. */
      match_index = 0;
    }

  match = match_list[match_index];

  if (match)
    match_index++;

  return (match);
}

/* Return 1 if PATH1 and PATH2 are the same file.  This is kind of
   expensive.  If non-NULL STP1 and STP2 point to stat structures
   corresponding to PATH1 and PATH2, respectively. */
int
same_file (path1, path2, stp1, stp2)
     char *path1, *path2;
     struct stat *stp1, *stp2;
{
  struct stat st1, st2;

  if (stp1 == NULL)
    {
      if (stat (path1, &st1) != 0)
	return (0);
      stp1 = &st1;
    }

  if (stp2 == NULL)
    {
      if (stat (path2, &st2) != 0)
	return (0);
      stp2 = &st2;
    }

  return ((stp1->st_dev == stp2->st_dev) && (stp1->st_ino == stp2->st_ino));
}

/* This does the dirty work for find_path_file () and find_user_command ().
   NAME is the name of the file to search for.
   PATH_LIST is a colon separated list of directories to search.
   FLAGS contains bit fields which control the files which are eligible.
   Some values are:
      FS_EXEC_ONLY:		The file must be an executable to be found.
      FS_EXEC_PREFERRED:	If we can't find an executable, then the
				the first file matching NAME will do.
      FS_EXISTS:		The first file found will do.
*/
static char *
find_user_command_in_path (name, path_list, flags)
     char *name;
     char *path_list;
     int flags;
{
  char *full_path, *path, *file_to_lose_on;
  int status, path_index, name_len;
  struct stat finfo;

  name_len = strlen (name);

  /* The file name which we would try to execute, except that it isn't
     possible to execute it.  This is the first file that matches the
     name that we are looking for while we are searching $PATH for a
     suitable one to execute.  If we cannot find a suitable executable
     file, then we use this one. */
  file_to_lose_on = (char *)NULL;

  /* We haven't started looking, so we certainly haven't seen
     a `.' as the directory path yet. */
  dot_found_in_search = 0;

  if (absolute_program (name))
    {
      full_path = (char *)xmalloc (1 + name_len);
      strcpy (full_path, name);

      status = file_status (full_path);

      if (!(status & FS_EXISTS))
	return (0);

      if ((flags & FS_EXEC_ONLY) && (status & FS_EXECABLE))
	return (full_path);
      else
	{
	  free (full_path);
	  return ((char *)NULL);
	}
    }

  /* Find out the location of the current working directory. */
  stat (".", &finfo);

  path_index = 0;
  while (path_list && path_list[path_index])
    {
      /* Allow the user to interrupt out of a lengthy path search. */
      QUIT;

      path = get_next_path_element (path_list, &path_index);

      if (!path)
	break;

      if (*path == '~')
	{
	  char *tilde_expand ();
	  char *t = tilde_expand (path);
	  free (path);
	  path = t;
	}

      /* Remember the location of "." in the path, in all its forms
	 (as long as they begin with a `.', e.g. `./.') */
      if ((*path == '.') &&
	  same_file (".", path, &finfo, (struct stat *)NULL))
	dot_found_in_search = 1;

      full_path = (char *)xmalloc (2 + strlen (path) + name_len);
      sprintf (full_path, "%s/%s", path, name);
      free (path);

      status = file_status (full_path);

      if (!(status & FS_EXISTS))
	goto next_file;

      /* The file exists.  If the caller simply wants the first file,
	 here it is. */
      if (flags & FS_EXISTS)
	return (full_path);

       /* If the file is executable, then it satisfies the cases of
	  EXEC_ONLY and EXEC_PREFERRED.  Return this file unconditionally. */
      if (status & FS_EXECABLE)
	{
	  if (file_to_lose_on)
	    free (file_to_lose_on);

	  return (full_path);
	}

      /* The file is not executable, but it does exist.  If we prefer
	 an executable, then remember this one if it is the first one
	 we have found. */
      if (flags & FS_EXEC_PREFERRED)
	{
	  if (!file_to_lose_on)
	    file_to_lose_on = savestring (full_path);
	}

    next_file:
      free (full_path);
    }

  /* We didn't find exactly what the user was looking for.  Return
     the contents of FILE_TO_LOSE_ON which is NULL when the search
     required an executable, or non-NULL if a file was found and the
     search would accept a non-executable as a last resort. */
  return (file_to_lose_on);
}

/* Given a string containing units of information separated by colons,
   return the next one pointed to by INDEX, or NULL if there are no more.
   Advance INDEX to the character after the colon. */
char *
extract_colon_unit (string, index)
     char *string;
     int *index;
{
  int i, start;

  i = *index;

  if (!string || (i >= strlen (string)))
    return ((char *)NULL);

  /* Each call to this routine leaves the index pointing at a colon if
     there is more to the path.  If I is > 0, then increment past the
     `:'.  If I is 0, then the path has a leading colon.  Trailing colons
     are handled OK by the `else' part of the if statement; an empty
     string is returned in that case. */
  if (i && string[i] == ':')
    i++;

  start = i;

  while (string[i] && string[i] != ':') i++;

  *index = i;

  if (i == start)
    {
      if (string[i])
	(*index)++;

      /* Return "" in the case of a trailing `:'. */
      return (savestring (""));
    }
  else
    {
      char *value;

      value = (char *)xmalloc (1 + (i - start));
      strncpy (value, &string[start], (i - start));
      value [i - start] = '\0';

      return (value);
    }
}

/* Return non-zero if the characters from SAMPLE are not all valid
   characters to be found in the first line of a shell script.  We
   check up to the first newline, or SAMPLE_LEN, whichever comes first.
   All of the characters must be printable or whitespace. */

#if !defined (isspace)
#define isspace(c) ((c) == ' ' || (c) == '\t' || (c) == '\n' || (c) == '\f')
#endif

#if !defined (isprint)
#define isprint(c) (isletter(c) || digit(c) || ispunct(c))
#endif

int
check_binary_file (sample, sample_len)
     unsigned char *sample;
     int sample_len;
{
  register int i;

  for (i = 0; i < sample_len; i++)
    {
      if (sample[i] == '\n')
	break;

      if (!isspace (sample[i]) && !isprint (sample[i]))
	return (1);
    }
  return (0);
}
