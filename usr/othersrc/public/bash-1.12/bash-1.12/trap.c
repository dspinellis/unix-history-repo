/* trap.c -- Not the trap command, but useful functions
   for manipulating those objects.  The trap command is
   in builtins.c */

/* Copyright (C) 1987, 1991 Free Software Foundation, Inc.

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

#include <sys/types.h>
#include "trap.h"
#include "shell.h"

extern int last_command_exit_value;

/* The list of things to do originally, before we started trapping. */
SigHandler *original_signals[NSIG];

/* For each signal, a slot for a string, which is a command to be
   executed when that signal is recieved.  The slot can also contain
   DEFAULT_SIG, which means do whatever you were going to do before
   you were so rudely interrupted, or IGNORE_SIG, which says ignore
   this signal. */
char *trap_list[NSIG];

/* A translation list so we can be polite to our users. */
char *signal_names[NSIG];

/* A bitmap of signals received for which we have trap handlers. */
int pending_traps[NSIG];

static int signal_names_initialized = 0;

initialize_traps ()
{
  register int i;

  if (!signal_names_initialized)
    {
      for (i = 1; i < NSIG; i++)
        {
	  signal_names[i] = (char *)NULL;
	  pending_traps[i] = 0;
        }

      /* `signal' 0 is what we do on exit. */
      signal_names[0] = "EXIT";

#if defined (SIGHUP)		/* hangup */
      signal_names[SIGHUP] = "SIGHUP";
#endif

#if defined (SIGINT)		/* interrupt */
      signal_names[SIGINT] = "SIGINT";
#endif

#if defined (SIGQUIT)		/* quit */
      signal_names[SIGQUIT] = "SIGQUIT";
#endif

#if defined (SIGILL)		/* illegal instruction (not reset when caught) */
      signal_names[SIGILL] = "SIGILL";
#endif

#if defined (SIGTRAP)		/* trace trap (not reset when caught) */
      signal_names[SIGTRAP] = "SIGTRAP";
#endif

#if defined (SIGABRT)		/*  */
      signal_names[SIGABRT] = "SIGABRT";
#endif

#if defined (SIGIOT)		/* IOT instruction */
      signal_names[SIGIOT] = "SIGIOT";
#endif

#if defined (SIGEMT)		/* EMT instruction */
      signal_names[SIGEMT] = "SIGEMT";
#endif

#if defined (SIGFPE)		/* floating point exception */
      signal_names[SIGFPE] = "SIGFPE";
#endif

#if defined (SIGKILL)		/* kill (cannot be caught or ignored) */
      signal_names[SIGKILL] = "SIGKILL";
#endif

#if defined (SIGBUS)		/* bus error */
      signal_names[SIGBUS] = "SIGBUS";
#endif

#if defined (SIGSEGV)		/* segmentation violation */
      signal_names[SIGSEGV] = "SIGSEGV";
#endif

#if defined (SIGSYS)		/* bad argument to system call */
      signal_names[SIGSYS] = "SIGSYS";
#endif

#if defined (SIGPIPE)		/* write on a pipe with no one to read it */
      signal_names[SIGPIPE] = "SIGPIPE";
#endif

#if defined (SIGALRM)		/* alarm clock */
      signal_names[SIGALRM] = "SIGALRM";
#endif

#if defined (SIGTERM)		/* software termination signal from kill */
      signal_names[SIGTERM] = "SIGTERM";
#endif

#if defined (SIGCLD)		/* Like SIGCHLD.  */
      signal_names[SIGCLD] = "SIGCLD";
#endif

#if defined (SIGPWR)		/* Magic thing for some machines. */
      signal_names[SIGPWR] = "SIGPWR";
#endif

#if defined (SIGPOLL)		/* For keyboard input?  */
      signal_names[SIGPOLL] = "SIGPOLL";
#endif

#if defined (SIGURG)		/* urgent condition on IO channel */
      signal_names[SIGURG] = "SIGURG";
#endif

#if defined (SIGSTOP)		/* sendable stop signal not from tty */
      signal_names[SIGSTOP] = "SIGSTOP";
#endif

#if defined (SIGTSTP)		/* stop signal from tty */
      signal_names[SIGTSTP] = "SIGTSTP";
#endif

#if defined (SIGCONT)		/* continue a stopped process */
      signal_names[SIGCONT] = "SIGCONT";
#endif

#if defined (SIGCHLD)		/* to parent on child stop or exit */
      signal_names[SIGCHLD] = "SIGCHLD";
#endif

#if defined (SIGTTIN)		/* to readers pgrp upon background tty read */
      signal_names[SIGTTIN] = "SIGTTIN";
#endif

#if defined (SIGTTOU)		/* like TTIN for output if (tp->t_local&LTOSTOP) */
      signal_names[SIGTTOU] = "SIGTTOU";
#endif

#if defined (SIGIO)		/* input/output possible signal */
      signal_names[SIGIO] = "SIGIO";
#endif

#if defined (SIGXCPU)		/* exceeded CPU time limit */
      signal_names[SIGXCPU] = "SIGXCPU";
#endif

#if defined (SIGXFSZ)		/* exceeded file size limit */
      signal_names[SIGXFSZ] = "SIGXFSZ";
#endif

#if defined (SIGVTALRM)		/* virtual time alarm */
      signal_names[SIGVTALRM] = "SIGVTALRM";
#endif

#if defined (SIGPROF)		/* profiling time alarm */
      signal_names[SIGPROF] = "SIGPROF";
#endif

#if defined (SIGWINCH)		/* window changed */
      signal_names[SIGWINCH] = "SIGWINCH";
#endif

#if defined (SIGLOST)		/* resource lost (eg, record-lock lost) */
      signal_names[SIGLOST] = "SIGLOST";
#endif

#if defined (SIGUSR1)		/* user defined signal 1 */
      signal_names[SIGUSR1] = "SIGUSR1";
#endif

#if defined (SIGUSR2)		/* user defined signal 2 */
      signal_names[SIGUSR2] = "SIGUSR2";
#endif

#if defined (SIGMSG)	/* HFT input data pending */
      signal_names[SIGMSG] = "SIGMSG";
#endif

#if defined (SIGPWR)	/* power failure imminent (save your data) */
      signal_names[SIGPWR] = "SIGPWR";
#endif

#if defined (SIGDANGER)	/* system crash imminent */
      signal_names[SIGDANGER] = "SIGDANGER";
#endif

#if defined (SIGMIGRATE)	/* migrate process to another CPU */
      signal_names[SIGMIGRATE] = "SIGMIGRATE";
#endif

#if defined (SIGPRE)	/* programming error */
      signal_names[SIGPRE] = "SIGPRE";
#endif

#if defined (SIGGRANT)	/* HFT monitor mode granted */
      signal_names[SIGGRANT] = "SIGGRANT";
#endif

#if defined (SIGRETRACT)	/* HFT monitor mode retracted */
      signal_names[SIGRETRACT] = "SIGRETRACT";
#endif

#if defined (SIGSOUND)	/* HFT sound sequence has completed */
      signal_names[SIGSOUND] = "SIGSOUND";
#endif

      for (i = 0; i < NSIG; i++)
	if (signal_names[i] == (char *)NULL)
	  {
	    signal_names[i] = (char *)xmalloc (10 + strlen ("SIGJUNK"));
	    sprintf (signal_names[i], "SIGJUNK(%d)", i);
	  }
    }


  trap_list[0] = (char *)NULL;

  for (i = 1; i < NSIG; i++)
    {
      trap_list[i] = (char *)DEFAULT_SIG;
      original_signals[i] = (SigHandler *)signal (i, SIG_DFL);
      signal (i, original_signals[i]);
    }
}

/* Return the print name of this signal. */
char *
signal_name (signal)
     int signal;
{
  if (signal > NSIG)
     return ("bad signal number");
  else return (signal_names[signal]);
}

/* Turn a string into a signal number, or a number into
   a signal number.  If STRING is "2", "SIGINT", or "INT",
   then (int)2 is returned.  Return NO_SIG if STRING doesn't
   contain a valid signal descriptor. */
int
decode_signal (string)
     char *string;
{
  int sig;

  if (sscanf (string, "%d", &sig) == 1)
    {
      if (sig < NSIG && sig >= 0)
	return (sig);
      else
	return (NO_SIG);
    }

  for (sig = 0; sig < NSIG; sig++)
    if ((stricmp (string, signal_names[sig]) == 0) ||
	(stricmp (string, &(signal_names[sig])[3]) == 0))
      return (sig);

  return (NO_SIG);
}

/* Non-zero when we catch a trapped signal. */
static int catch_flag = 0;

#if !defined (USG) && !defined (USGr4)
#define HAVE_BSD_SIGNALS
#endif

run_pending_traps ()
{
  register int sig;
  int old_exit_value;

  if (catch_flag == 0)		/* simple optimization */
    return;

  catch_flag = 0;

  /* Preserve $? when running trap. */
  old_exit_value = last_command_exit_value;

  for (sig = 0; sig < NSIG; sig++)
    {
      if (pending_traps[sig])
	{
#if defined (_POSIX_VERSION)
	  sigset_t set, oset;

	  sigemptyset (&set);
	  sigemptyset (&oset);

	  sigaddset (&set, sig);
	  sigprocmask (SIG_BLOCK, &set, &oset);
#else
#  if defined (HAVE_BSD_SIGNALS)
	  int oldmask = sigblock (sigmask (sig));
#  endif
#endif /* POSIX_VERSION */

	  parse_and_execute (savestring (trap_list[sig]), "trap");
	  pending_traps[sig] = 0;

#if defined (_POSIX_VERSION)
	  sigprocmask (SIG_SETMASK, &oset, (sigset_t *)NULL);
#else
#  if defined (HAVE_BSD_SIGNALS)
	  sigsetmask (oldmask);
#  endif
#endif /* POSIX_VERSION */
	}
    }

  last_command_exit_value = old_exit_value;
}

sighandler
trap_handler (sig)
     int sig;
{
  extern int interrupt_immediately;

  if ((sig >= NSIG) ||
      (trap_list[sig] == (char *)DEFAULT_SIG) ||
      (trap_list[sig] == (char *)IGNORE_SIG))
    programming_error ("trap_handler: Bad signal %d", sig);
  else
    {

#if defined (USG) && !defined (HAVE_BSD_SIGNALS)
      signal (sig, trap_handler);
#endif /* USG && !HAVE_BSD_SIGNALS */

      catch_flag = 1;
      pending_traps[sig]++;

      if (interrupt_immediately)
	run_pending_traps ();
    }
#if !defined (VOID_SIGHANDLER)
  return (0);
#endif /* VOID_SIGHANDLER */
}

/* Set SIG to call STRING as a command. */
void
set_signal (sig, string)
     int sig;
     char *string;
{
  void change_signal ();

  /* A signal ignored on entry to the shell cannot be trapped or reset, but
     no error is reported when attempting to do so.  -- Posix.2 */
  if (original_signals[sig] == SIG_IGN)
    return;

#if defined (SIGCHLD)
  /* Don't change the function that catches SIGCHLD, but store the command
     to be executed.  It will be run from jobs.c: flush_child(). */
  if (sig &&
      sig != SIGINT &&
      sig != SIGCHLD)
#else
  if (sig && sig != SIGINT)
#endif /* SIGCHLD */
    signal (sig, SIG_IGN);

  change_signal (sig, savestring (string));

#if defined (SIGCHLD)
  /* Don't change the function that catches SIGCHLD, but store the command
     to be executed.  It will be run from jobs.c: flush_child().  Do the
     same thing for SIGINT; the trap commands are run from
     run_interrupt_trap (), which is called from throw_to_top_level (). */
  if (sig &&
      sig != SIGINT &&
      sig != SIGCHLD)
#else
  if (sig && sig != SIGINT)
#endif /* SIGCHLD */
    signal (sig, trap_handler);
}

/* If SIG has a string assigned to it, get rid of it.  Then give it
   VALUE. */
void
change_signal (sig, value)
     int sig;
     char *value;
{
  if ((((int)trap_list[sig]) > 0) && (trap_list[sig] != (char *)IGNORE_SIG))
    free (trap_list[sig]);
  trap_list[sig] = value;
}

/* Restore the default action for SIG; i.e., the action the shell
   would have taken before you used the trap command. */
void
restore_default_signal (sig)
     int sig;
{
#if defined (SIGCHLD)
  /* Don't allow the signal catchers for SIGINT or SIGCHLD
     to be overridden. */
  if (sig != SIGINT && sig != SIGCHLD)
#else
  if (sig != SIGINT)
#endif /* !SIGCHLD */
    signal (sig, original_signals[sig]);

  change_signal (sig, (char *)DEFAULT_SIG);
}

/* Make this signal be ignored. */
void
ignore_signal (sig)
     int sig;
{
#ifdef SIGCHLD
  /* Don't allow the SIGCHLD signal catcher to be overridden. */
  if (sig != SIGCHLD)
#endif
    signal (sig, SIG_IGN);
  change_signal (sig, (char *)IGNORE_SIG);
}

/* Handle the calling of "trap 0".  The only sticky situation is when
   the command to be executed includes an "exit".  This is why we have
   to provide our own place for top_level to jump to. */
void
run_exit_trap ()
{
  if ((trap_list[0] != (char *)DEFAULT_SIG) &&
      (trap_list[0] != (char *)IGNORE_SIG))
    {
      char *trap_command = savestring (trap_list[0]);
      int code, old_exit_value;

      old_exit_value = last_command_exit_value;
      change_signal (0, (char *)NULL);
      code = setjmp (top_level);

      if (code == 0)
	parse_and_execute (trap_command, "trap");

      last_command_exit_value = old_exit_value;
    }
}
      
/* Reset all trapped signals to their original values.  Signals set to be
   ignored with trap '' SIGNAL should be ignored, so we make sure that they
   are. */
void
restore_original_signals ()
{
  register int i;

  for (i = 0; i < NSIG; i++)
    {
      if (trap_list[i] != (char *)DEFAULT_SIG)
	{
	  if (trap_list[i] == (char *)IGNORE_SIG)
	    signal (i, SIG_IGN);
	  else
	    restore_default_signal (i);
	}
      else
	{
	  /* If it's one of the signals the shell handles specially,
	     make sure it gets set back to the value it had when the
	     shell was started. */
	  if (i == SIGINT || i == SIGQUIT || i == SIGTERM)
	    restore_default_signal (i);
	}
    }
}

/* Run a trap set on SIGINT.  This is called from throw_to_top_level (), and
   declared here to localize the trap functions. */
run_interrupt_trap ()
{
  char *command, *saved_command;
  int old_exit_value;

  if ((trap_list[SIGINT] != (char *) DEFAULT_SIG) &&
      (trap_list[SIGINT] != (char *) IGNORE_SIG))
    {
      command = savestring (trap_list[SIGINT]);

      old_exit_value = last_command_exit_value;
      saved_command = trap_list[SIGINT];
      unwind_protect_string (trap_list[SIGINT]);
      trap_list[SIGINT] = (char *)NULL;
      parse_and_execute (command, "interrupt trap");
      trap_list[SIGINT] = saved_command;
      last_command_exit_value = old_exit_value;
    }
}
