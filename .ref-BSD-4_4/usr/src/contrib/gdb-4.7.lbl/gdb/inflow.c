/* Low level interface to ptrace, for GDB when running under Unix.
   Copyright 1986, 1987, 1989, 1991, 1992 Free Software Foundation, Inc.

This file is part of GDB.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "defs.h"
#include "frame.h"
#include "inferior.h"
#include "command.h"
#include "signals.h"
#include "terminal.h"
#include "target.h"

#ifdef USG
#include <sys/types.h>
#endif

/* Some USG-esque systems (some of which are BSD-esque enough so that USG
   is not defined) want this header, and it won't do any harm.  */
#include <fcntl.h>

#include <sys/param.h>
#include <signal.h>

static void
kill_command PARAMS ((char *, int));

static void
terminal_ours_1 PARAMS ((int));

/* Nonzero if we are debugging an attached outside process
   rather than an inferior.  */

int attach_flag;


/* Record terminal status separately for debugger and inferior.  */

/* Does GDB have a terminal (on stdin)?  */
int gdb_has_a_terminal;
#if !defined(__GO32__)
static TERMINAL sg_inferior;
static TERMINAL sg_ours;
#endif
static int tflags_inferior;
static int tflags_ours;

#if defined(TIOCGETC) && !defined(TIOCGETC_BROKEN)
static struct tchars tc_inferior;
static struct tchars tc_ours;
#endif

#if defined(TIOCGLTC) && !defined(TIOCGLTC_BROKEN)
static struct ltchars ltc_inferior;
static struct ltchars ltc_ours;
#endif

#ifdef TIOCLGET
/* The line discipline flags.  Note that even when gdb_has_a_terminal
   is true, the ioctl's to get and set the line discipline flags may
   fail.  An example is running gdb under "script" using the streams
   based interface under SVR4.  So we keep track of whether or not
   the flags we get are valid by setting the *_valid flag, and don't
   try to reset them unless they are valid. */
static int lmode_inferior;
static int lmode_inferior_valid;
static int lmode_ours;
static int lmode_ours_valid;
#endif

#ifdef TIOCGPGRP
# ifdef SHORT_PGRP
static short pgrp_inferior;
static short pgrp_ours;
# else /* not def SHORT_PGRP */
static int pgrp_inferior;
static int pgrp_ours;
# endif /* not def SHORT_PGRP */
#else /* not def TIOCGPGRP */
static void (*sigint_ours) ();
static void (*sigquit_ours) ();
#endif /* TIOCGPGRP */

/* The name of the tty (from the `tty' command) that we gave to the inferior
   when it was last started.  */

static char *inferior_thisrun_terminal;

/* Nonzero if our terminal settings are in effect.
   Zero if the inferior's settings are in effect.  */

static int terminal_is_ours;

/* Macro for printing errors from ioctl operations */

#define	OOPSY(what)	\
  if (result == -1)	\
    fprintf(stderr, "[%s failed in terminal_inferior: %s]\n", \
	    what, strerror (errno))

static void terminal_ours_1 ();

/* Initialize the terminal settings we record for the inferior,
   before we actually run the inferior.  */

void
terminal_init_inferior ()
{
#if !defined(__GO32__)
  sg_inferior = sg_ours;
  tflags_inferior = tflags_ours;

#if defined(TIOCGETC) && !defined(TIOCGETC_BROKEN)
  tc_inferior = tc_ours;
#endif

#if defined(TIOCGLTC) && !defined(TIOCGLTC_BROKEN)
  ltc_inferior = ltc_ours;
#endif

#ifdef TIOCLGET
  lmode_inferior = lmode_ours;
  lmode_inferior_valid = lmode_ours_valid;
#endif

#ifdef TIOCGPGRP
  pgrp_inferior = inferior_pid;
#endif /* TIOCGPGRP */
#endif
  terminal_is_ours = 1;
}

/* Put the inferior's terminal settings into effect.
   This is preparation for starting or resuming the inferior.  */

void
terminal_inferior ()
{
#if !defined(__GO32__)
  int result;

  if (gdb_has_a_terminal && terminal_is_ours && inferior_thisrun_terminal == 0)
    {
      result = fcntl (0, F_SETFL, tflags_inferior);
      result = fcntl (0, F_SETFL, tflags_inferior);
      OOPSY ("fcntl F_SETFL");
      result = ioctl (0, TIOCSETN, &sg_inferior);
      OOPSY ("ioctl TIOCSETN");

#if defined(TIOCGETC) && !defined(TIOCGETC_BROKEN)
      result = ioctl (0, TIOCSETC, &tc_inferior);
      OOPSY ("ioctl TIOCSETC");
#endif
#if defined(TIOCGLTC) && !defined(TIOCGLTC_BROKEN)
      result = ioctl (0, TIOCSLTC, &ltc_inferior);
      OOPSY ("ioctl TIOCSLTC");
#endif
#ifdef TIOCLSET
      if (lmode_inferior_valid)
	{
	  result = ioctl (0, TIOCLSET, &lmode_inferior);
	  OOPSY ("ioctl TIOCLSET");
	}
#endif

#ifdef TIOCGPGRP
      result = ioctl (0, TIOCSPGRP, &pgrp_inferior);
      /* If we attached to the process, we might or might not be sharing
	 a terminal.  Avoid printing error msg if we are unable to set our
	 terminal's process group to his process group ID.  */
      if (!attach_flag) {
	OOPSY ("ioctl TIOCSPGRP");
      }
#else
      sigint_ours = (void (*) ()) signal (SIGINT, SIG_IGN);
      sigquit_ours = (void (*) ()) signal (SIGQUIT, SIG_IGN);
#endif /* TIOCGPGRP */
    }
#endif
  terminal_is_ours = 0;
}

/* Put some of our terminal settings into effect,
   enough to get proper results from our output,
   but do not change into or out of RAW mode
   so that no input is discarded.

   After doing this, either terminal_ours or terminal_inferior
   should be called to get back to a normal state of affairs.  */

void
terminal_ours_for_output ()
{
  terminal_ours_1 (1);
}

/* Put our terminal settings into effect.
   First record the inferior's terminal settings
   so they can be restored properly later.  */

void
terminal_ours ()
{
  terminal_ours_1 (0);
}

static void
terminal_ours_1 (output_only)
     int output_only;
{
#if !defined(__GO32__)
  int result;
#ifdef TIOCGPGRP
  /* Ignore this signal since it will happen when we try to set the pgrp.  */
  void (*osigttou) ();
#endif /* TIOCGPGRP */

  /* Checking inferior_thisrun_terminal is necessary so that
     if GDB is running in the background, it won't block trying
     to do the ioctl()'s below.  Checking gdb_has_a_terminal
     avoids attempting all the ioctl's when running in batch.  */
  if (inferior_thisrun_terminal != 0 || gdb_has_a_terminal == 0)
    return;

  if (!terminal_is_ours)
    {
      terminal_is_ours = 1;

#ifdef TIOCGPGRP
      osigttou = (void (*) ()) signal (SIGTTOU, SIG_IGN);

      result = ioctl (0, TIOCGPGRP, &pgrp_inferior);
      result = ioctl (0, TIOCSPGRP, &pgrp_ours);

      signal (SIGTTOU, osigttou);
#else
      signal (SIGINT, sigint_ours);
      signal (SIGQUIT, sigquit_ours);
#endif /* TIOCGPGRP */

      tflags_inferior = fcntl (0, F_GETFL, 0);
      result = ioctl (0, TIOCGETP, &sg_inferior);

#if defined(TIOCGETC) && !defined(TIOCGETC_BROKEN)
      result = ioctl (0, TIOCGETC, &tc_inferior);
#endif
#if defined(TIOCGLTC) && !defined(TIOCGLTC_BROKEN)
      result = ioctl (0, TIOCGLTC, &ltc_inferior);
#endif
#ifdef TIOCLGET
      result = ioctl (0, TIOCLGET, &lmode_inferior);
      lmode_inferior_valid = (result == 0);
#endif
    }

#ifdef HAVE_TERMIO
  sg_ours.c_lflag |= ICANON;
  if (output_only && !(sg_inferior.c_lflag & ICANON))
    sg_ours.c_lflag &= ~ICANON;
#else /* not HAVE_TERMIO */
  sg_ours.sg_flags &= ~RAW & ~CBREAK;
  if (output_only)
    sg_ours.sg_flags |= (RAW | CBREAK) & sg_inferior.sg_flags;
#endif /* not HAVE_TERMIO */

  result = fcntl (0, F_SETFL, tflags_ours);
  result = fcntl (0, F_SETFL, tflags_ours);
  result = ioctl (0, TIOCSETN, &sg_ours);

#if defined(TIOCGETC) && !defined(TIOCGETC_BROKEN)
  result = ioctl (0, TIOCSETC, &tc_ours);
#endif
#if defined(TIOCGLTC) && !defined(TIOCGLTC_BROKEN)
  result = ioctl (0, TIOCSLTC, &ltc_ours);
#endif
#ifdef TIOCLGET
  if (lmode_ours_valid)
    {
      result = ioctl (0, TIOCLSET, &lmode_ours);
    }
#endif

#ifdef HAVE_TERMIO
  sg_ours.c_lflag |= ICANON;
#else /* not HAVE_TERMIO */
  sg_ours.sg_flags &= ~RAW & ~CBREAK;
#endif /* not HAVE_TERMIO */

  result = result;	/* lint */
#endif
}

/* ARGSUSED */
void
term_info (arg, from_tty)
     char *arg;
     int from_tty;
{
  target_terminal_info (arg, from_tty);
}

/* ARGSUSED */
void
child_terminal_info (args, from_tty)
     char *args;
     int from_tty;
{
  register int i;

  if (!gdb_has_a_terminal) {
    printf_filtered ("This GDB does not control a terminal.\n");
    return;
  }
#if !defined(__GO32__)
#ifdef TIOCGPGRP
  printf_filtered ("Inferior's terminal status (currently saved by GDB):\n");

  printf_filtered ("owner pgrp = %d, fcntl flags = 0x%x.\n",
	  pgrp_inferior, tflags_inferior);
#endif /* TIOCGPGRP */

#ifdef HAVE_TERMIO

  printf_filtered ("c_iflag = 0x%x, c_oflag = 0x%x,\n",
	  sg_inferior.c_iflag, sg_inferior.c_oflag);
  printf_filtered ("c_cflag = 0x%x, c_lflag = 0x%x, c_line = 0x%x.\n",
	  sg_inferior.c_cflag, sg_inferior.c_lflag, sg_inferior.c_line);
  printf_filtered ("c_cc: ");
  for (i = 0; (i < NCC); i += 1)
    printf_filtered ("0x%x ", sg_inferior.c_cc[i]);
  printf_filtered ("\n");

#else /* not HAVE_TERMIO */

  printf_filtered ("sgttyb.sg_flags = 0x%x.\n", sg_inferior.sg_flags);

#endif /* not HAVE_TERMIO */

#if defined(TIOCGETC) && !defined(TIOCGETC_BROKEN)
  printf_filtered ("tchars: ");
  for (i = 0; i < (int)sizeof (struct tchars); i++)
    printf_filtered ("0x%x ", ((unsigned char *)&tc_inferior)[i]);
  printf_filtered ("\n");
#endif

#if defined(TIOCGLTC) && !defined(TIOCGLTC_BROKEN)
  printf_filtered ("ltchars: ");
  for (i = 0; i < (int)sizeof (struct ltchars); i++)
    printf_filtered ("0x%x ", ((unsigned char *)&ltc_inferior)[i]);
  printf_filtered ("\n");
#endif
  
#ifdef TIOCLGET
  printf_filtered ("lmode:  0x%x\n", lmode_inferior);
#endif
#else
  printf_filtered("This is a DOS machine; there is no terminal state\n");
#endif

}

/* NEW_TTY_PREFORK is called before forking a new child process,
   so we can record the state of ttys in the child to be formed.
   TTYNAME is null if we are to share the terminal with gdb;
   or points to a string containing the name of the desired tty.

   NEW_TTY is called in new child processes under Unix, which will
   become debugger target processes.  This actually switches to
   the terminal specified in the NEW_TTY_PREFORK call.  */

void
new_tty_prefork (ttyname)
     char *ttyname;
{
  /* Save the name for later, for determining whether we and the child
     are sharing a tty.  */
  inferior_thisrun_terminal = ttyname;
}

void
new_tty ()
{
  register int tty;
  void (*osigttou) ();

  if (inferior_thisrun_terminal == 0)
    return;
#if !defined(__GO32__)
#ifdef TIOCNOTTY
  /* Disconnect the child process from our controlling terminal.  On some
     systems (SVR4 for example), this may cause a SIGTTOU, so temporarily
     ignore SIGTTOU. */
  tty = open("/dev/tty", O_RDWR);
  if (tty > 0)
    {
      osigttou = (void (*)()) signal(SIGTTOU, SIG_IGN);
      ioctl(tty, TIOCNOTTY, 0);
      close(tty);
      signal(SIGTTOU, osigttou);
    }
#endif

  /* Now open the specified new terminal.  */

#ifdef USE_O_NOCTTY
  tty = open(inferior_thisrun_terminal, O_RDWR | O_NOCTTY);
#else
  tty = open(inferior_thisrun_terminal, O_RDWR);
#endif
  if (tty == -1)
    {
      print_sys_errmsg (inferior_thisrun_terminal, errno);
      _exit(1);
    }

  /* Avoid use of dup2; doesn't exist on all systems.  */
  if (tty != 0)
    { close (0); dup (tty); }
  if (tty != 1)
    { close (1); dup (tty); }
  if (tty != 2)
    { close (2); dup (tty); }
  if (tty > 2)
    close(tty);
#endif /* !go32*/o
}

/* Kill the inferior process.  Make us have no inferior.  */

/* ARGSUSED */
static void
kill_command (arg, from_tty)
     char *arg;
     int from_tty;
{
  if (inferior_pid == 0)
    error ("The program is not being run.");
  if (!query ("Kill the inferior process? "))
    error ("Not confirmed.");
  target_kill ();

  /* Killing off the inferior can leave us with a core file.  If so,
     print the state we are left in.  */
  if (target_has_stack) {
    printf_filtered ("In %s,\n", current_target->to_longname);
    if (selected_frame == NULL)
      fputs_filtered ("No selected stack frame.\n", stdout);
    else
      print_stack_frame (selected_frame, selected_frame_level, 1);
  }
}

/* The inferior process has died.  Long live the inferior!  */

void
generic_mourn_inferior ()
{
  inferior_pid = 0;
  attach_flag = 0;
  mark_breakpoints_out ();
  registers_changed ();

#ifdef CLEAR_DEFERRED_STORES
  /* Delete any pending stores to the inferior... */
  CLEAR_DEFERRED_STORES;
#endif

  reopen_exec_file ();
  if (target_has_stack) {
    set_current_frame ( create_new_frame (read_register (FP_REGNUM),
					  read_pc ()));
    select_frame (get_current_frame (), 0);
  } else {
    set_current_frame (0);
    select_frame ((FRAME) 0, -1);
  }
  /* It is confusing to the user for ignore counts to stick around
     from previous runs of the inferior.  So clear them.  */
  breakpoint_clear_ignore_counts ();
}


#if 0 
/* This function is just for testing, and on some systems (Sony NewsOS
   3.2) <sys/user.h> also includes <sys/time.h> which leads to errors
   (since on this system at least sys/time.h is not protected against
   multiple inclusion).  */
/* ARGSUSED */
static void
try_writing_regs_command (arg, from_tty)
     char *arg;
     int from_tty;
{
  register int i;
  register int value;

  if (inferior_pid == 0)
    error ("There is no inferior process now.");

  /* A Sun 3/50 or 3/60 (at least) running SunOS 4.0.3 will have a
     kernel panic if we try to write past the end of the user area.
     Presumably Sun will fix this bug (it has been reported), but it
     is tacky to crash the system, so at least on SunOS4 we need to
     stop writing when we hit the end of the user area.  */
  for (i = 0; i < sizeof (struct user); i += 2)
    {
      QUIT;
      errno = 0;
      value = call_ptrace (3, inferior_pid, (PTRACE_ARG3_TYPE) i, 0);
      call_ptrace (6, inferior_pid, (PTRACE_ARG3_TYPE) i, value);
      if (errno == 0)
	{
	  printf (" Succeeded with address 0x%x; value 0x%x (%d).\n",
		  i, value, value);
	}
      else if ((i & 0377) == 0)
	printf (" Failed at 0x%x.\n", i);
    }
}
#endif

void
_initialize_inflow ()
{
  int result;

  add_info ("terminal", term_info,
	   "Print inferior's saved terminal status.");

#if 0
  add_com ("try-writing-regs", class_obscure, try_writing_regs_command,
	   "Try writing all locations in inferior's system block.\n\
Report which ones can be written.");
#endif

  add_com ("kill", class_run, kill_command,
	   "Kill execution of program being debugged.");

  inferior_pid = 0;

  /* Get all the current tty settings (including whether we have a tty at
     all!).  */

#if !defined(__GO32__)
  tflags_ours = fcntl (0, F_GETFL, 0);

  result = ioctl (0, TIOCGETP, &sg_ours);
  if (result == 0) {
    gdb_has_a_terminal = 1;
    /* Get the rest of the tty settings, then... */
#if defined(TIOCGETC) && !defined(TIOCGETC_BROKEN)
    ioctl (0, TIOCGETC, &tc_ours);
#endif
#if defined(TIOCGLTC) && !defined(TIOCGLTC_BROKEN)
    ioctl (0, TIOCGLTC, &ltc_ours);
#endif
#ifdef TIOCLGET
    result = ioctl (0, TIOCLGET, &lmode_ours);
    lmode_ours_valid = (result == 0);
#endif
#ifdef TIOCGPGRP
    ioctl (0, TIOCGPGRP, &pgrp_ours);
#endif /* TIOCGPGRP */
  } else {
    gdb_has_a_terminal = 0;
  }
#else
  gdb_has_a_terminal = 0;
#endif


  terminal_is_ours = 1;
}
