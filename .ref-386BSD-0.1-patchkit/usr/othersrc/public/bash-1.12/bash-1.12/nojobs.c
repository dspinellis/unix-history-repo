/* The thing that makes children, remembers them, and contains wait loops. */

/* This file works under BSD, System V, minix, and Posix systems. */

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
#include <signal.h>
#include <setjmp.h>
#include <errno.h>

#include "config.h"
#include "general.h"
#include "filecntl.h"
#include "jobs.h"

#if !defined (USG) && !defined (_POSIX_VERSION)
#  include <sgtty.h>
#else
#  if defined (_POSIX_VERSION)
#    include <termios.h>
#  else
#    include <termio.h>
#    include <sys/ttold.h>
#  endif /* !POSIX_VERSION */
#endif /* USG && _POSIX_VERSION */

#if !defined (SIGABRT)
#  define SIGABRT SIGIOT
#endif /* !SIGABRT */

#if defined (USG) || defined (_POSIX_VERSION)
#  define killpg(pg, sig)		kill(-(pg),(sig))
#endif /* USG || _POSIX_VERSION */

#if defined (USG)
#  define siginterrupt(sig, code)
#endif /* USG */

#if defned (_POSIX_VERSION)
#  define WAITPID(pid, statusp, options) waitpid (pid, statusp, options)
#else
#  define WAITPID(pid, statusp, options) wait (statusp)
#endif /* !_POSIX_VERSION */

pid_t last_made_pid = (pid_t)-1;
pid_t last_asynchronous_pid = (pid_t)-1;

extern int errno;

/* Initialize the job control mechanism, and set up the tty stuff. */
initialize_jobs ()
{
  get_tty_state ();
}

/* Setup this shell to handle C-C, etc. */
initialize_job_signals ()
{
  extern int login_shell;
  extern sighandler sigint_sighandler ();

  signal (SIGINT, sigint_sighandler);

  /* If this is a login shell we don't wish to be disturbed by
     stop signals. */
  if (login_shell)
    {
#ifdef SIGTSTP
      signal (SIGTSTP, SIG_IGN);
      signal (SIGTTOU, SIG_IGN);
      signal (SIGTTIN, SIG_IGN);
#endif
    }
}

#if defined (_POSIX_VERSION)
/* Collect the status of all zombie children so that their system
   resources can be deallocated. */
static void
reap_zombie_children ()
{
#if defined (WNOHANG)
  while (waitpid (-1, (int *)NULL, WNOHANG) != -1)
    ;
#endif /* WNOHANG */
}
#endif /* _POSIX_VERSION */

/* Fork, handling errors.  Returns the pid of the newly made child, or 0.
   COMMAND is just for remembering the name of the command; we don't do
   anything else with it.  ASYNC_P says what to do with the tty.  If
   non-zero, then don't give it away. */
pid_t
make_child (command, async_p)
     char *command;
     int async_p;
{
  pid_t pid;
#if defined (_POSIX_VERSION)
  int retry = 1;
#endif /* _POSIX_VERSION */

  /* Discard saved memory. */
  if (command)  
    free (command);

  /* Make new environment array if neccessary. */
  maybe_make_export_env ();

  /* Create the child, handle severe errors. */
#if defined (_POSIX_VERSION)
  retry_fork:
#endif /* _POSIX_VERSION */

  if ((pid = fork ()) < 0)
    {
#if defined (_POSIX_VERSION)
      /* Posix systems with a non-blocking waitpid () system call available
	 get another chance after zombies are reaped. */
      if (errno == EAGAIN && retry)
	{
	  reap_zombie_chilren ();
	  retry = 0;
	  goto retry_fork;
	}
#endif /* _POSIX_VERSION */

      report_error ("fork: %s", strerror (errno));

      throw_to_top_level ();
    }
 
  if (!pid)
    {
      /* Cancel shell traps. */
      restore_original_signals ();

      /* Ignore INT and QUIT in asynchronous children. */
      if (async_p)
	{
	  signal (SIGINT, SIG_IGN);
	  signal (SIGQUIT, SIG_IGN);
	  last_asynchronous_pid = getpid ();
	}
      else
	{
#if defined (SIGTSTP)
	  signal (SIGTSTP, SIG_DFL);
	  signal (SIGTTIN, SIG_DFL);
	  signal (SIGTTOU, SIG_DFL);
#endif
	}
    }
  else
    {
      /* In the parent. */
      last_made_pid = pid;

      if (async_p)
	last_asynchronous_pid = pid;
    }
  return (pid);
}

/* Wait for a single pid (PID) and return its exit status. */
wait_for_single_pid (pid)
     pid_t pid;
{
  pid_t got_pid;
  WAIT status;

  /* Make sure that the process we are waiting for is valid. This
     check is not necessary on Posix systems. */
#if !defined (_POSIX_VERSION)
  if ((kill (pid, 0) < 0) && errno == ESRCH)
    return (127);
#endif /* !_POSIX_VERSION */

  siginterrupt (SIGINT, 1);
  while ((got_pid = WAITPID (pid, &status, 0)) != pid)
    {
      if (got_pid < 0)
	{
	  if (errno != EINTR && errno != ECHILD)
	    {
	      siginterrupt (SIGINT, 0);
	      file_error ("wait");
	    }
	  break;
	}
    }
  siginterrupt (SIGINT, 0);
  QUIT;
}

/* Wait for all of the shell's children to exit. */
wait_for_background_pids ()
{
  /* If we aren't using job control, we let the kernel take care of the
     bookkeeping for us.  wait () will return -1 and set errno to ECHILD 
     when there are no more unwaited-for child processes on both
     4.2 BSD-based and System V-based systems. */

  siginterrupt (SIGINT, 1);
  while (1)
    {
      pid_t got_pid;
      WAIT status;

      /* Wait for ECHILD */
      while ((got_pid = WAITPID (-1, &status, 0)) != -1)
	;
      if (errno != EINTR && errno != ECHILD)
	{
	  siginterrupt (SIGINT, 0);
	  file_error("wait");
	}
      break;
    }
  siginterrupt (SIGINT, 0);
  QUIT;
}

/* Wait for pid (one of our children) to terminate. */
int
wait_for (pid)
     pid_t pid;
{
  extern int interactive;
  int return_val;
  pid_t got_pid;
  WAIT status;

  /* Make sure that the process we are waiting for is valid.  This check
     is not necessary on Posix systems. */
#if !defined (_POSIX_VERSION)
  if ((kill (pid, 0) < 0) && (errno == ESRCH))
    return (0);
#endif /* !_POSIX_VERSION */

  siginterrupt (SIGINT, 1);
  while ((got_pid = WAITPID (pid, &status, 0)) != pid)
    {
      if (got_pid < 0 && errno == ECHILD)
	{
#if !defined (_POSIX_VERSION)
	  status.w_termsig = status.w_retcode = 0;
#else
	  status = 0;
#endif /* _POSIX_VERSION */
	  break;
	}
      else if (got_pid < 0 && errno != EINTR)
	programming_error ("got errno %d while waiting for %d", errno, pid);
    }
  siginterrupt (SIGINT, 0);

  /* Default return value. */
  /* ``a full 8 bits of status is returned'' */
  if (WIFSIGNALED (status))
    return_val = 128 + WTERMSIG (status);
  else
    return_val = WEXITSTATUS (status);
                            
  if (!WIFSTOPPED (status) && WIFSIGNALED (status) &&
      (WTERMSIG (status) != SIGINT))
    {
      extern char *sys_siglist[];
      fprintf (stderr, "%s", sys_siglist[WTERMSIG (status)]);
      if (WIFCORED (status))
	fprintf (stderr, " (core dumped)");
      fprintf (stderr, "\n");
    }

  if (WIFSIGNALED (status) || WIFSTOPPED (status))
    set_tty_state ();
  else
    get_tty_state ();
                            
  return (return_val);
}

/* Give PID SIGNAL.  This determines what job the pid belongs to (if any).
   If PID does belong to a job, and the job is stopped, then CONTinue the
   job after giving it SIGNAL.  Returns -1 on failure.  If GROUP is non-null,
   then kill the process group associated with PID. */
int
kill_pid (pid, signal, group)
     pid_t pid;
     int signal, group;
{
  int result;

  if (group)
    result = killpg (pid, signal);
  else
    result = kill (pid, signal);

  return (result);
}

#if defined (_POSIX_VERSION)
static struct termios shell_tty_info;
#else
#  if defined (USG)
static struct termio shell_tty_info;
#  else
static struct sgttyb shell_tty_info;
#  endif /* USG */
#endif /* _POSIX_VERSION */

static int got_tty_state = 0;

/* Fill the contents of shell_tty_info with the current tty info. */
get_tty_state ()
{
  int tty = open ("/dev/tty", O_RDONLY);
  if (tty != -1)
    {
#if defined (_POSIX_VERSION)
      tcgetattr (tty, &shell_tty_info);
#else
#  if defined (USG)
      ioctl (tty, TCGETA, &shell_tty_info);
#  else
      ioctl (tty, TIOCGETP, &shell_tty_info);
#  endif
#endif
      close (tty);
      got_tty_state = 1;
    }
}

/* Make the current tty use the state in shell_tty_info. */
set_tty_state ()
{
  int tty = open ("/dev/tty", O_RDONLY);
  if (tty != -1)
    {
      if (!got_tty_state)
	{
	  close (tty);
	  return;
	}
#if defined (_POSIX_VERSION)
      tcsetattr (tty, TCSADRAIN, &shell_tty_info);
#else
#  if defined (USG)
      ioctl (tty, TCSETAW, &shell_tty_info);  /* Wait for output, no flush */
#  else
      ioctl (tty, TIOCSETN, &shell_tty_info);
#  endif
#endif
      close (tty);
    }
}

/* Give the terminal to PGRP.  */
give_terminal_to (pgrp)
     pid_t pgrp;
{
}

/* Stop a pipeline. */
stop_pipeline (async, ignore)
     int async;
     char *ignore;
{
}

/* Print descriptive information about the job with leader pid PID. */
describe_pid (pid)
     pid_t pid;
{
  fprintf (stderr, "<%d>\n", (int) pid);
}
