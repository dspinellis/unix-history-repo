/* siglist.c -- signal list for those machines that don't have one. */

/* Copyright (C) 1989 Free Software Foundation, Inc.

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

#if !defined (NSIG)
#  if defined (_NSIG)
#    define NSIG _NSIG
#  else
#    define NSIG 64
#  endif /* !_NSIG */
#endif /* !NSIG */

char *sys_siglist[NSIG];

extern char *xmalloc (), *malloc ();

initialize_siglist ()
{
  register int i;

  for (i = 0; i < NSIG; i++)
    sys_siglist[i] = (char *)0x0;

  sys_siglist[0] = "Bogus signal";

#if defined (SIGHUP)
  sys_siglist[SIGHUP] = "Hangup";
#endif

#if defined (SIGINT)
  sys_siglist[SIGINT] = "Interrupt";
#endif

#if defined (SIGQUIT)
  sys_siglist[SIGQUIT] = "Quit";
#endif

#if defined (SIGILL)
  sys_siglist[SIGILL] = "Illegal instruction";
#endif

#if defined (SIGTRAP)
  sys_siglist[SIGTRAP] = "BPT trace/trap";
#endif

#if defined (SIGIOT) && !defined (SIGABRT)
#define SIGABRT SIGIOT
#endif

#if defined (SIGABRT)
  sys_siglist[SIGABRT] = "ABORT instruction";
#endif

#if defined (SIGEMT)
  sys_siglist[SIGEMT] = "EMT instruction";
#endif

#if defined (SIGFPE)
  sys_siglist[SIGFPE] = "Floating point exception";
#endif

#if defined (SIGKILL)
  sys_siglist[SIGKILL] = "Killed";
#endif

#if defined (SIGBUS)
  sys_siglist[SIGBUS] = "Bus error";
#endif

#if defined (SIGSEGV)
  sys_siglist[SIGSEGV] = "Segmentation fault";
#endif

#if defined (SIGSYS)
  sys_siglist[SIGSYS] = "Bad system call";
#endif

#if defined (SIGPIPE)
  sys_siglist[SIGPIPE] = "Broken pipe";
#endif

#if defined (SIGALRM)
  sys_siglist[SIGALRM] = "Alarm clock";
#endif

#if defined (SIGTERM)
  sys_siglist[SIGTERM] = "Terminated";
#endif

#if defined (SIGURG)
  sys_siglist[SIGURG] = "Urgent IO condition";
#endif

#if defined (SIGSTOP)
  sys_siglist[SIGSTOP] = "Stopped (signal)";
#endif

#if defined (SIGTSTP)
  sys_siglist[SIGTSTP] = "Stopped";
#endif

#if defined (SIGCONT)
  sys_siglist[SIGCONT] = "Continue";
#endif

#if !defined (SIGCHLD) && defined (SIGCLD)
#define SIGCHLD SIGCLD
#endif

#if defined (SIGCHLD)
  sys_siglist[SIGCHLD] = "Child death or stop";
#endif

#if defined (SIGTTIN)
  sys_siglist[SIGTTIN] = "Stopped (tty input)";
#endif

#if defined (SIGTTOU)
  sys_siglist[SIGTTOU] = "Stopped (tty output)";
#endif

#if defined (SIGIO)
  sys_siglist[SIGIO] = "I/O ready";
#endif

#if defined (SIGXCPU)
  sys_siglist[SIGXCPU] = "CPU limit";
#endif

#if defined (SIGXFSZ)
  sys_siglist[SIGXFSZ] = "File limit";
#endif

#if defined (SIGVTALRM)
  sys_siglist[SIGVTALRM] = "Alarm (virtual)";
#endif

#if defined (SIGPROF)
  sys_siglist[SIGPROF] = "Alarm (profile)";
#endif

#if defined (SIGWINCH)
  sys_siglist[SIGWINCH] = "Window changed";
#endif

#if defined (SIGLOST)
  sys_siglist[SIGLOST] = "Record lock";
#endif

#if defined (SIGUSR1)
  sys_siglist[SIGUSR1] = "User signal 1";
#endif

#if defined (SIGUSR2)
  sys_siglist[SIGUSR2] = "User signal 2";
#endif

#if defined (SIGMSG)
  sys_siglist[SIGMSG] = "HFT input data pending";
#endif 

#if defined (SIGPWR)
  sys_siglist[SIGPWR] = "power failure imminent";
#endif 

#if defined (SIGDANGER)
  sys_siglist[SIGDANGER] = "system crash imminent";
#endif 

#if defined (SIGMIGRATE)
  sys_siglist[SIGMIGRATE] = "migrate process to another CPU";
#endif 

#if defined (SIGPRE)
  sys_siglist[SIGPRE] = "programming error";
#endif 

#if defined (SIGGRANT)
  sys_siglist[SIGGRANT] = "HFT monitor mode granted";
#endif 

#if defined (SIGRETRACT)
  sys_siglist[SIGRETRACT] = "HFT monitor mode retracted";
#endif 

#if defined (SIGSOUND)
  sys_siglist[SIGSOUND] = "HFT sound sequence has completed";
#endif 

  for (i = 0; i < NSIG; i++)
    {
      if (!sys_siglist[i])
	{
	  sys_siglist[i] =
	    (char *)xmalloc (10 + strlen ("Unknown Signal #"));

	  sprintf (sys_siglist[i], "Unknown Signal #%d", i);
	}
    }
}
