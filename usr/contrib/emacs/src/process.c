/* Asynchronous subprocess control for GNU Emacs.
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


#include <signal.h>

#include "config.h"

#ifdef subprocesses
/* The entire file is within this conditional */

#include <stdio.h>
#include <errno.h>
#include <setjmp.h>
#include <sys/types.h>		/* some typedefs are used in sys/file.h */
#include <sys/file.h>
#include <sys/stat.h>
#ifdef BSD
#include <sys/ioctl.h>
#endif /* BSD */
#ifdef USG
#include <termio.h>
#include <fcntl.h>
#endif /* USG */

#ifdef HAVE_TIMEVAL
#if defined (USG) && !defined (UNIPLUS)
#include <time.h>
#else
#include <sys/time.h>
#endif
#endif /* HAVE_TIMEVAL */

#if defined (HPUX) && defined (HAVE_PTYS)
#include <sys/ptyio.h>
#endif

#undef NULL
#include "lisp.h"
#include "window.h"
#include "buffer.h"
#include "process.h"
#include "termhooks.h"

/* Define SIGCHLD as an alias for SIGCLD.  There are many conditionals
   testing SIGCHLD.  */

#if !defined (SIGCHLD) && defined (SIGCLD)
#define SIGCHLD SIGCLD
#endif /* SIGCLD */

/* Define the structure that the wait system call stores.
   On many systems, there is a structure defined for this.
   But on vanilla-ish USG systems there is not.  */

#if !defined (BSD) && !defined (UNIPLUS) && !defined (STRIDE)
#define WAITTYPE int
#define WIFSTOPPED(w) ((w&0377) == 0177)
#define WIFSIGNALED(w) ((w&0377) != 0177 && (w&~0377) == 0)
#define WIFEXITED(w) ((w&0377) == 0)
#define WRETCODE(w) (w >> 8)
#define WSTOPSIG(w) (w >> 8)
#define WCOREDUMP(w) ((w&0200) != 0)
#define WTERMSIG(w) (w & 0377)
#else
#ifdef BSD4_1
#include <wait.h>
#else
#include <sys/wait.h>
#endif /* not BSD 4.1 */
#define WAITTYPE union wait
#define WRETCODE(w) w.w_retcode
#define WSTOPSIG(w) w.w_stopsig
#define WCOREDUMP(w) w.w_coredump
#define WTERMSIG(w) w.w_termsig
#endif

extern errno;
extern sys_nerr;
extern char *sys_errlist[];

#ifndef BSD4_1
extern char *sys_siglist[];
#else
char *sys_siglist[] =
  {
    "bum signal!!",
    "hangup",
    "interrupt",
    "quit",
    "illegal instruction",
    "trace trap",
    "iot instruction",
    "emt instruction",
    "floating point exception",
    "kill",
    "bus error",
    "segmentation violation",
    "bad argument to system call",
    "write on a pipe with no one to read it",
    "alarm clock",
    "software termination signal from kill",
    "status signal",
    "sendable stop signal not from tty",
    "stop signal from tty",
    "continue a stopped process",
    "child status has changed",
    "background read attempted from control tty",
    "background write attempted from control tty",
    "input record available at control tty",
    "exceeded CPU time limit",
    "exceeded file size limit"
    };
#endif

#ifdef vipc

#include "vipc.h"
extern int comm_server;
extern int net_listen_address;
#endif vipc


#ifdef SKTPAIR
#include <sys/socket.h>
#endif /* SKTPAIR */

int  child_changed;		/* Flag when a child process has ceased
				   to be */

/* Mask of bits indicating the descriptors that we wait for input on */

int input_wait_mask;

int delete_exited_processes;

#define MAXDESC 32

/* Indexed by descriptor, gives the process (if any) for that descriptor */
Lisp_Object chan_process[MAXDESC];

/* Alist of elements (NAME . PROCESS) */
Lisp_Object Vprocess_alist;

Lisp_Object Qprocessp;

Lisp_Object get_process ();

/* Buffered-ahead input char from process, indexed by channel.
   -1 means empty (no char is buffered).
   Used on sys V where the only way to tell if there is any
   output from the process is to read at least one char.
   Always -1 on systems that support FIONREAD.  */

int proc_buffered_char[MAXDESC];

#ifdef HAVE_PTYS

/* Open an available pty, putting descriptor in *ptyv,
  and return the file name of the pty.  Return 0 if none available.  */

char ptyname[24];

char *
pty (ptyv)
     int *ptyv;
{
  struct stat stb;
  register c, i;

  for (c = FIRST_PTY_LETTER; c <= 'z'; c++)
    for (i = 0; i < 16; i++)
      {
#ifdef HPUX
	sprintf (ptyname, "/dev/ptym/pty%c%x", c, i);
#else
	sprintf (ptyname, "/dev/pty%c%x", c, i);
#endif /* not HPUX */
	if (stat (ptyname, &stb) < 0)
	  return 0;

	*ptyv = open (ptyname, O_RDWR | O_NDELAY, 0);
	if (*ptyv >= 0)
	  {
	    /* check to make certain that both sides are available
	       this avoids a nasty yet stupid bug in rlogins */
	    int x;
#ifdef HPUX
            sprintf (ptyname, "/dev/pty/tty%c%x", c, i);
#else
            sprintf (ptyname, "/dev/tty%c%x", c, i);
#endif /* not HPUX */
#ifndef UNIPLUS
	    x = open (ptyname, O_RDWR | O_NDELAY, 0);
	    if (x < 0)
	      {
		close (*ptyv);
		continue;
	      }
	    close(x);
#endif /* not UNIPLUS */
	    /*
		* If the following statement is included,
		* then a 0 length record is EOT, but no other
		* control characters can be sent down the pty
		* (e.g., ^S/^Q, ^O, etc.).  If it is not
		* included, then sending ^D down the pty-pipe
		* makes a pretty good EOF.
		*/
/*	      ioctl( *ptyv, TIOCREMOTE, &on );	/* for EOT */
/* this is said to be unecessary, and to be harmful in 4.3.  */
/*	    ioctl (*ptyv, FIONBIO, &on);  */
	    return ptyname;
	  }
      }
  return 0;
}

#endif /* HAVE_PTYS */

Lisp_Object
make_process (name)
     Lisp_Object name;
{
  Lisp_Object val, tem, name1;
  register struct Lisp_Process *p;
  char suffix[10];
  register int i;

  val = Fmake_vector (make_number ((sizeof (struct Lisp_Process)
				    - sizeof (int) - sizeof (struct Lisp_Vector *))
				   / sizeof (Lisp_Object)),
		      Qnil);
  XSETTYPE (val, Lisp_Process);

  p = XPROCESS (val);
  XFASTINT (p->infd) = 0;
  XFASTINT (p->outfd) = 0;
  XFASTINT (p->pid) = 0;
  XFASTINT (p->flags) = 0;
  XFASTINT (p->reason) = 0;
  p->mark = Fmake_marker ();

  /* If name is already in use, modify it until it is unused.  */

  name1 = name;
  for (i = 1; ; i++)
    {
      tem = Fget_process (name1);
      if (NULL (tem)) break;
      sprintf (suffix, "<%d>", i);
      name1 = concat2 (name, build_string (suffix));
    }
  name = name1;
  p->name = name;
  Vprocess_alist = Fcons (Fcons (name, val), Vprocess_alist);
  return val;
}

remove_process (proc)
     Lisp_Object proc;
{
  Lisp_Object pair;

  pair = Frassq (proc, Vprocess_alist);
  Vprocess_alist = Fdelq (pair, Vprocess_alist);
  Fset_marker (XPROCESS (proc)->mark, Qnil, Qnil);

  deactivate_process (proc);
}

DEFUN ("processp", Fprocessp, Sprocessp, 1, 1, 0,
  "Return t if OBJECT is a process.")
  (obj)
     Lisp_Object obj;
{
  return XTYPE (obj) == Lisp_Process ? Qt : Qnil;
}

DEFUN ("get-process", Fget_process, Sget_process, 1, 1, 0,
  "Return the process named NAME, or nil if there is none.")
  (name)
     Lisp_Object name;
{
  if (XTYPE (name) == Lisp_Process)
    return name;
  CHECK_STRING (name, 0);
  return Fcdr (Fassoc (name, Vprocess_alist));
}

DEFUN ("get-buffer-process", Fget_buffer_process, Sget_buffer_process, 1, 1, 0,
  "Return the (or, a) process associated with BUFFER.\n\
BUFFER may be a buffer or the name of one.")
  (name)
     Lisp_Object name;
{
  Lisp_Object buf, tail, proc;

  if (NULL (name)) return Qnil;
  buf = Fget_buffer (name);
  if (NULL (buf)) return Qnil;

  for (tail = Vprocess_alist; !NULL (tail); tail = Fcdr (tail))
    {
      proc = Fcdr (Fcar (tail));
      if (XTYPE (proc) == Lisp_Process && EQ (XPROCESS (proc)->buffer, buf))
	return proc;
    }
  return Qnil;
}

/* This is how commands for the user decode process arguments */

Lisp_Object
get_process (name)
     Lisp_Object name;
{
  Lisp_Object proc;
  if (NULL (name))
    proc = Fget_buffer_process (Fcurrent_buffer ());
  else
    {
      proc = Fget_process (name);
      if (NULL (proc))
	proc = Fget_buffer_process (Fget_buffer (name));
    }

  if (!NULL (proc))
    return proc;

  if (NULL (name))
    error ("Current buffer has no process");
  else
    error ("Process %s does not exist", XSTRING (name)->data);
  /* NOTREACHED */
}

DEFUN ("delete-process", Fdelete_process, Sdelete_process, 1, 1, 0,
  "Delete PROCESS: kill it and forget about it immediately.\n\
PROCESS may be a process or the name of one, or a buffer name.")
  (proc)
     Lisp_Object proc;
{
  proc = get_process (proc);
  if (XFASTINT (XPROCESS (proc)->infd))
    Fkill_process (proc, Qnil);
  remove_process (proc);
  return Qnil;
}

DEFUN ("process-status", Fprocess_status, Sprocess_status, 1, 1, 0,
  "Return the status of PROCESS: a symbol, one of these:\n\
run  -- for a process that is running.\n\
stop -- for a process stopped but continuable.\n\
exit -- for a process that has exited.\n\
signal -- for a process that has got a fatal signal.\n\
command -- for a command channel opened to Emacs by another process.\n\
external -- for an i/o channel opened to Emacs by another process.\n\
nil -- if arg is a process name and no such process exists.")
  (proc)
     Lisp_Object proc;
{
  register struct Lisp_Process *p;
  proc = Fget_process (proc);
  if (NULL (proc))
    return proc;
  p = XPROCESS (proc);

  switch (XFASTINT (p->flags) & PROC_STATUS)
    {
    case RUNNING:
      if (!NULL (p->childp))
	return intern ("run");
      else if (!NULL (p->command_channel_p))
	return intern ("command");
      return intern ("external");

    case EXITED:
      return intern ("exit");

    case SIGNALED:
      return intern ("signal");

    case STOPPED:
      return intern ("stop");
    }

  /* NOTREACHED */
}

DEFUN ("process-id", Fprocess_id, Sprocess_id, 1, 1, 0,
  "Return the process id of PROCESS.\n\
This is the pid of the Unix process which PROCESS uses or talks to.")
  (proc)
     Lisp_Object proc;
{
  CHECK_PROCESS (proc, 0);
  return XPROCESS (proc)->pid;
}

DEFUN ("process-name", Fprocess_name, Sprocess_name, 1, 1, 0,
  "Return the name of PROCESS, as a string.\n\
This is the name of the program invoked in PROCESS,\n\
possibly modified to make it unique among process names.")
  (proc)
     Lisp_Object proc;
{
  CHECK_PROCESS (proc, 0);
  return XPROCESS (proc)->name;
}

DEFUN ("process-command", Fprocess_command, Sprocess_command, 1, 1, 0,
  "Return the command that was executed to start PROCESS.\n\
This is a list of strings, the first string being the program executed\n\
and the rest of the strings being the arguments given to it.\n\
For a non-child channel, this is nil.")
  (proc)
     Lisp_Object proc;
{
  CHECK_PROCESS (proc, 0);
  return XPROCESS (proc)->command;
}

DEFUN ("set-process-buffer", Fset_process_buffer, Sset_process_buffer,
  2, 2, 0,
  "Set buffer associated with PROCESS to BUFFER (a buffer, or nil).")
  (proc, buffer)
     Lisp_Object proc, buffer;
{
  CHECK_PROCESS (proc, 0);
  if (!NULL (buffer))
    CHECK_BUFFER (buffer, 1);
  XPROCESS (proc)->buffer = buffer;
  return buffer;
}

DEFUN ("process-buffer", Fprocess_buffer, Sprocess_buffer,
  1, 1, 0,
  "Return the buffer PROCESS is associated with.\n\
Output from PROCESS is inserted in this buffer\n\
unless PROCESS has a filter.")
  (proc)
     Lisp_Object proc;
{
  CHECK_PROCESS (proc, 0);
  return XPROCESS (proc)->buffer;
}

DEFUN ("process-mark", Fprocess_mark, Sprocess_mark,
  1, 1, 0,
  "Return the marker for the end of the last output from PROCESS.")
  (proc)
     Lisp_Object proc;
{
  CHECK_PROCESS (proc, 0);
  return XPROCESS (proc)->mark;
}

DEFUN ("set-process-filter", Fset_process_filter, Sset_process_filter,
  2, 2, 0,
  "Give PROCESS the filter function FILTER; nil means no filter.\n\
When a process has a filter, each time it does output\n\
the entire string of output is passed to the filter.\n\
The filter gets two arguments: the process and the string of output.\n\
If the process has a filter, its buffer is not used for output.")
  (proc, filter)
     Lisp_Object proc, filter;
{
  CHECK_PROCESS (proc, 0);
  XPROCESS (proc)->filter = filter;
  return filter;
}

DEFUN ("process-filter", Fprocess_filter, Sprocess_filter,
  1, 1, 0,
  "Returns the filter function of PROCESS; nil if none.\n\
See set-process-filter for more info on filter functions.")
  (proc)
     Lisp_Object proc;
{
  CHECK_PROCESS (proc, 0);
  return XPROCESS (proc)->filter;
}

DEFUN ("set-process-sentinel", Fset_process_sentinel, Sset_process_sentinel,
  2, 2, 0,
  "Give PROCESS the sentinel SENTINEL; nil for none.\n\
The sentinel is called as a function when the process changes state.\n\
It gets two arguments: the process, and a string describing the change.")
  (proc, sentinel)
     Lisp_Object proc, sentinel;
{
  CHECK_PROCESS (proc, 0);
  XPROCESS (proc)->sentinel = sentinel;
  return sentinel;
}

DEFUN ("process-sentinel", Fprocess_sentinel, Sprocess_sentinel,
  1, 1, 0,
  "Return the sentinel of PROCESS; nil if none.\n\
See set-process-sentinel for more info on sentinels.")
  (proc)
     Lisp_Object proc;
{
  CHECK_PROCESS (proc, 0);
  return XPROCESS (proc)->sentinel;
}

DEFUN ("process-kill-without-query", Fprocess_kill_without_query,
  Sprocess_kill_without_query, 1, 1, 0,
  "Say no query needed if this process is running when Emacs is exited.")
  (proc)
     Lisp_Object proc;
{
  CHECK_PROCESS (proc, 0);
  XPROCESS (proc)->kill_without_query = Qt;
  return Qt;
}

Lisp_Object
list_processes_1 ()
{
  Lisp_Object tail, proc, minspace, tem, tem1;
  register struct buffer *old = bf_cur;
  register struct Lisp_Process *p;
  register int state;
  char tembuf[10];

  XFASTINT (minspace) = 1;

  SetBfp (XBUFFER (Vstandard_output));
  Fbuffer_flush_undo (Vstandard_output);

  bf_cur->truncate_lines = Qt;

  write_string ("\
Proc         Status   Buffer         Command\n\
----         ------   ------         -------\n", -1);

  for (tail = Vprocess_alist; !NULL (tail); tail = Fcdr (tail))
    {
      proc = Fcdr (Fcar (tail));
      p = XPROCESS (proc);
      if (NULL (p->childp))
	continue;

      Finsert (1, &p->name);
      Findent_to (make_number (13), minspace);

      state = XFASTINT (p->flags) & PROC_STATUS;
      switch (state)
	{
	case RUNNING:
	  write_string ("Run", -1);
	  break;

	case STOPPED:
	  write_string ("Stop", -1);
	  break;

	case EXITED:
	  write_string ("Exit", -1);
	  if (XFASTINT (p->reason))
	    {
	      sprintf (tembuf, " %d", XFASTINT (p->reason));
	      write_string (tembuf, -1);
	    }
	  remove_process (proc);
	  break;

	case SIGNALED:
	  if (XFASTINT (p->reason) < NSIG)
	    write_string (sys_siglist [XFASTINT (p->reason)], -1);
	  else
	    write_string ("Signal", -1);
	  remove_process (proc);
	}

      Findent_to (make_number (22), minspace);
      if (NULL (p->buffer))
	InsStr ("(none)");
      else if (NULL (XBUFFER (p->buffer)->name))
	InsStr ("(Killed)");
      else
	Finsert (1, &XBUFFER (p->buffer)->name);

      Findent_to (make_number (37), minspace);

      tem = p->command;
      while (1)
	{
	  tem1 = Fcar (tem);
	  Finsert (1, &tem1);
	  tem = Fcdr (tem);
	  if (NULL (tem))
	    break;
	  InsStr (" ");
	}

      InsStr ("\n");
    }

  SetBfp (old);
  return Qnil;
}

DEFUN ("list-processes", Flist_processes, Slist_processes, 0, 0, "",
  "Display a list of all processes.\n\
\(Any processes listed as Exited or Signaled are actually eliminated\n\
after the listing is made.)")
  ()
{
  internal_with_output_to_temp_buffer ("*Process List*",
				       list_processes_1, Qnil);
  return Qnil;
}

DEFUN ("start-process", Fstart_process, Sstart_process, 3, MANY, 0,
  "Start a program in a subprocess.  Return the process object for it.\n\
First arg is name for process.  It is modified if nec to make it unique.\n\
Second arg is buffer to associate with the process (or buffer name).\n\
 Process output goes at end of that buffer, unless you specify\n\
 an output stream or filter function to handle the output.\n\
Third arg is program file name.  It is searched for as in the shell.\n\
Remaining arguments are strings to give program as arguments.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  Lisp_Object buffer, name, program, proc, tem;
  register unsigned char **new_argv;
  register int i;

  name = args[0];
  CHECK_STRING (name, 0);

  buffer = args[1];
  program = args[2];

  CHECK_STRING (program, 2);

  new_argv = (unsigned char **) alloca ((nargs - 1) * sizeof (char *));

  for (i = 3; i < nargs; i++)
    {
      tem = args[i];
      CHECK_STRING (tem, i);
      new_argv[i - 2] = XSTRING (tem)->data;
    }
  new_argv[i - 2] = 0;
  new_argv[0] = XSTRING (program)->data;

  /* If program file name is not absolute, search our path for it */
  if (new_argv[0][0] != '/')
    {
      tem = Qnil;
      openp (Vexec_path, program, "", &tem, 1);
      if (NULL (tem))
	report_file_error ("Searching for program", Fcons (program, Qnil));
      new_argv[0] = XSTRING (tem)->data;
    }

  if (!NULL (buffer))
    buffer = Fget_buffer_create (buffer);
  proc = make_process (name);

  XPROCESS (proc)->childp = Qt;
  XPROCESS (proc)->command_channel_p = Qnil;
  XPROCESS (proc)->buffer = buffer;
  XPROCESS (proc)->sentinel = Qnil;
  XPROCESS (proc)->filter = Qnil;
  XPROCESS (proc)->command = Flist (nargs - 2, args + 2);

  create_process (proc, new_argv);

  return proc;
}

create_process_1 (signo)
     int signo;
{
#ifdef USG
  /* USG systems forget handlers when they are used;
     must reestablish each time */
  signal (signo, create_process_1);
#endif /* USG */
}

create_process (process, new_argv)
     Lisp_Object process;
     char **new_argv;
{
  int pid, inchannel, outchannel, forkin, forkout;
  int sv[2];
  int (*sigchld)();

#ifdef HAVE_PTYS
  char	*ptyname;

  ptyname = pty (&inchannel);
  outchannel = inchannel;
  if (ptyname)
    {
      forkout = forkin = open (ptyname, O_RDWR, 0);
      if (forkin < 0)
	report_file_error ("Opening pty", Qnil);
    }
  else
#endif /* HAVE_PTYS */
#ifdef SKTPAIR
    {
      if (socketpair (AF_UNIX, SOCK_STREAM, 0, sv) < 0)
	report_file_error ("Opening socketpair", Qnil);
      outchannel = inchannel = sv[0];
      forkout = forkin = sv[1];
    }
#else /* not SKTPAIR */
    {
      pipe (sv);
      inchannel = sv[0];
      forkout = sv[1];
      pipe (sv);
      outchannel = sv[1];
      forkin = sv[0];
    }
#endif /* not SKTPAIR */

#ifdef FIOCLEX
  ioctl (inchannel, FIOCLEX, 0);
  ioctl (outchannel, FIOCLEX, 0);
#endif

/* Stride people say it's a mystery why this is needed
   as well as the O_NDELAY, but that it fails without this.  */
#ifdef STRIDE
  {
    int one = 1;
    ioctl (inchannel, FIONBIO, &one);
  }
#endif

#ifdef O_NDELAY
  fcntl (inchannel, F_SETFL, O_NDELAY);
#endif

  chan_process[inchannel] = process;
  XFASTINT (XPROCESS (process)->infd) = inchannel;
  XFASTINT (XPROCESS (process)->outfd) = outchannel;
  XFASTINT (XPROCESS (process)->flags) = RUNNING;

  input_wait_mask |= ChannelMask (inchannel);

  /* Delay interrupts until we have a chance to store
     the new fork's pid in its process structure */
#ifdef SIGCHLD
#ifdef BSD4_1
  sighold (SIGCHLD);
#else /* not BSD4_1 */
#if defined (BSD) || defined (UNIPLUS)
  sigsetmask (1 << (SIGCHLD - 1));
#else /* ordinary USG */
  sigchld = signal (SIGCHLD, SIG_DFL);
#endif /* ordinary USG */
#endif /* not BSD4_1 */
#endif /* SIGCHLD */

  pid = vfork ();
  if (pid == 0)
    {
      int xforkin = forkin;
      int xforkout = forkout;
#ifdef HAVE_PTYS
#ifdef TIOCNOTTY
      /* In 4.3BSD, the TIOCSPGRP bug has been fixed, and now you
	 can do TIOCSPGRP only to the process's controlling tty.
	 We must make the pty terminal the controlling tty of the child.  */
      if (ptyname)
	{
	  /* I wonder: would just ioctl (0, TIOCNOTTY, 0) work here? 
	     I can't test it since I don't have 4.3.  */
	  int j = open ("/dev/tty", O_RDWR, 0);
	  ioctl (j, TIOCNOTTY, 0);
          close (j);

#ifndef UNIPLUS
	  /* I wonder if close (open (ptyname, ...)) would work?  */
	  close (xforkin);
          xforkout = xforkin = open (ptyname, O_RDWR, 0);

          if (xforkin < 0)
	    abort ();
#endif /* not UNIPLUS */
        }
#endif /* TIOCNOTTY */
#endif /* HAVE_PTYS */
      child_setup (xforkin, xforkout, xforkout, new_argv);
    }

  /* If the subfork execv fails, and it exits,
     this close hangs.  I don't know why.
     So have an interrupt jar it loose.  */
  signal (SIGALRM, create_process_1);
  alarm (1);
  close (forkin);
  alarm (0);
  if (forkin != forkout)
    close (forkout);

  if (pid < 0)
    {
      remove_process (process);
      report_file_error ("Doing vfork", Qnil);
    }

  XFASTINT (XPROCESS (process)->pid) = pid;

#ifdef SIGCHLD
#ifdef BSD4_1
  sigrelse (SIGCHLD);
#else /* not BSD4_1 */
#if defined (BSD) || defined (UNIPLUS)
  sigsetmask (0);
#else /* ordinary USG */
  signal (SIGCHLD, sigchld);
#endif /* ordinary USG */
#endif /* not BSD4_1 */
#endif /* SIGCHLD */
}

deactivate_process (proc)
     Lisp_Object proc;
{
  register int inchannel, outchannel;
  register struct Lisp_Process *p = XPROCESS (proc);

  inchannel = XFASTINT (p->infd);
  outchannel = XFASTINT (p->outfd);

  if (inchannel)
    {
      /* Beware SIGCHLD hereabouts. */
      flush_pending_output (inchannel);
      close (inchannel);
      if (outchannel  &&  outchannel != inchannel)
 	close (outchannel);

      XFASTINT (p->infd) = 0;
      XFASTINT (p->outfd) = 0;
      chan_process[inchannel] = Qnil;
      input_wait_mask &= ~ChannelMask (inchannel);
    }
}

DEFUN ("accept-process-output", Faccept_process_output, Saccept_process_output,
  0, 1, 0,
  "Allow any pending output from subprocesses to be read by Emacs.\n\
It is read into the processs' buffers or given to their filter functions.\n\
Non-nil arg PROCESS means do not return until some output has been received\n\
from PROCESS.")
  (proc)
     Lisp_Object proc;
{
  if (NULL (proc))
    wait_reading_process_input (-1, 0, 0);
  else
    {
      proc = get_process (proc);
      wait_reading_process_input (0, 10 + XFASTINT (XPROCESS (proc)->infd),
				  0);
    }
  return Qnil;
}

/* Read and dispose of subprocess output
 while waiting for timeout to elapse and/or keyboard input to be available.

 time_limit is the timeout in seconds, or zero for no limit.
 -1 means gobble data available immediately but don't wait for any.

 read_kbd is 1 to return when input is available.
 Negative means caller will actually read the input.
 10 + I means wait until input received from channel I.

 do_display means redisplay should be done to show
 subprocess output that arrives.  */

wait_reading_process_input (time_limit, read_kbd, do_display)
     int time_limit, read_kbd, do_display;
{
  register int channel, nfds, m;
  int Available = 0;
  int Exception;
  Lisp_Object proc;
#ifdef HAVE_TIMEVAL
  struct timeval timeout, end_time, garbage;
#else
  long timeout, end_time, temp;
#endif /* not HAVE_TIMEVAL */
  int Atemp;
  int wait_channel = 0;
  extern kbd_count;

  if (read_kbd > 10)
    {
      wait_channel = read_kbd - 10;
      read_kbd = 0;
    }

  /* Since we may need to wait several times,
     compute the absolute time to return at.  */
  if (time_limit)
    {
#ifdef HAVE_TIMEVAL
      gettimeofday (&end_time, &garbage);
      end_time.tv_sec += time_limit;
#else /* not HAVE_TIMEVAL */
      time (&end_time);
      end_time += time_limit;
#endif /* not HAVE_TIMEVAL */
    }

  while (1)
    {
      /* If calling from keyboard input, do not quit
	 since we want to return C-g as an input character.
	 Otherwise, do pending quit if requested.  */
      if (read_kbd >= 0)
	QUIT;

      /* If status of something has changed, and no input is available,
	 notify the user of the change right away */
      if (child_changed && do_display)
	{
	  Atemp = input_wait_mask;
#ifdef HAVE_TIMEVAL
	  timeout.tv_sec=0; timeout.tv_usec=0;
#else /* not HAVE_TIMEVAL */
	  timeout = 0;
#endif /* not HAVE_TIMEVAL */
	  if (select (MAXDESC, &Atemp, 0, 0, &timeout) <= 0)
	    change_msgs();
	}

      if (fix_screen_hook)
	(*fix_screen_hook) ();

      /* Compute time from now till when time limit is up */
      /* Exit if already run out */
      if (time_limit == -1)
	{
	  /* -1 specified for timeout means
	     gobble output available now
	     but don't wait at all. */
	  time_limit = -2;
#ifdef HAVE_TIMEVAL
	  timeout.tv_sec = 0;
	  timeout.tv_usec = 0;
#else
	  timeout = 0;
#endif /* not HAVE_TIMEVAL */
	}
      else if (time_limit)
	{
#ifdef HAVE_TIMEVAL
	  gettimeofday (&timeout, &garbage);
	  timeout.tv_sec = end_time.tv_sec - timeout.tv_sec;
	  timeout.tv_usec = end_time.tv_usec - timeout.tv_usec;
	  if (timeout.tv_usec < 0)
	    timeout.tv_usec += 1000000,
	    timeout.tv_sec--;
	  if (timeout.tv_sec < 0)
	    break;
#else /* not HAVE_TIMEVAL */
          time (&temp);
	  timeout = end_time - temp;
	  if (timeout < 0)
	    break;
#endif /* not HAVE_TIMEVAL */
	}
      else
	{
#ifdef HAVE_TIMEVAL
	  /* If no real timeout, loop sleeping with a big timeout
	     so that input interrupt can wake us up by zeroing it  */
	  timeout.tv_sec = 100;
	  timeout.tv_usec = 0;
#else /* not HAVE_TIMEVAL */
          timeout = 100000;	/* 100000 recognized by the select emulator */
#endif /* not HAVE_TIMEVAL */
	}

      /* Cause C-g and alarm signals to take immediate action,
	 and cause input available signals to zero out timeout */
      if (read_kbd < 0)
	set_waiting_for_input (&timeout);

      /* Wait till there is something to do */

      Available = Exception = input_wait_mask;
      if (!read_kbd)
	Available &= ~1;

      if (read_kbd && kbd_count)
	nfds = 0;
      else
	nfds = select (MAXDESC, &Available, 0, &Exception, &timeout);

      if (fix_screen_hook)
	(*fix_screen_hook) ();

      /* Make C-g and alarm signals set flags again */
      clear_waiting_for_input ();

      if (time_limit && nfds == 0)	/* timeout elapsed */
	break;
      if (nfds < 0)
	{
	  if (errno == EINTR)
	    Available = 0;
	  else if (errno == EBADF)
	    abort ();
	  else
	    error("select error: %s", sys_errlist[errno]);
	}

      /* Check for keyboard input */
      /* If there is any, return immediately
	 to give it higher priority than subprocesses */

      if (read_kbd && (kbd_count || !NULL (Vquit_flag)))
	break;

      if (read_kbd && (Available & ChannelMask (0)))
	break;

#ifdef vipc
      /* Check for connection from other process */

      if (Available & ChannelMask (comm_server))
	{
	  Available &= ~(ChannelMask (comm_server));
	  create_commchan ();
	}
#endif vipc

      /* Check for data from a process or a command channel */

      for (channel = 3; Available && channel < MAXDESC; channel++)
	{
	  m = ChannelMask (channel);
	  if (m & Available)
	    {
	      Available &= ~m;
	      /* If waiting for this channel,
		 arrange to return as soon as no more input
		 to be processed.  No more waiting.  */
	      if (wait_channel == channel)
		{
		  wait_channel = 0;
		  time_limit = -1;
		}
	      proc = chan_process[channel];
	      if (NULL (proc))
		continue;

#ifdef vipc
	      /* It's a command channel */
	      if (!NULL (XPROCESS (proc)->command_channel_p))
		{
		  ProcessCommChan (channel, proc);
		  if (NULL (XPROCESS (proc)->command_channel_p))
		    {
		      /* It has ceased to be a command channel! */
		      int bytes_available;
		      if (ioctl (channel, FIONREAD, &bytes_available) < 0)
			bytes_available = 0;
		      if (bytes_available)
			Available |= m;
		    }
		  continue;
		}
#endif vipc

	      /* Read data from the process, starting with our
		 buffered-ahead character if we have one.  */

	      if (read_process_output (proc, channel) > 0)
		{
		  if (do_display)
		    DoDsp (1);
		}
	      else
		{
		  /* Preserve status of processes already terminated.  */
		  child_changed++;
		  deactivate_process (proc);

/*
 * With pty:s, when the parent process of a pty exits we are notified,
 * just as we would be with any of our other children.  After the process
 * exits, select() will indicate that we can read the channel.  When we
 * do this, read() returns 0.  Upon receiving this, we close the channel.
 *
 * For external channels, when the peer closes the connection, select()
 * will indicate that we can read the channel.  When we do this, read()
 * returns -1 with errno = ECONNRESET.  Since we never get notified of
 * this via wait3(), we must explictly mark the process as having exited.
 */
		  if ((XFASTINT (XPROCESS (proc)->flags) & PROC_STATUS)
		      == RUNNING)
		    {
		      XFASTINT (XPROCESS (proc)->flags) = EXITED | CHANGED;
		      XFASTINT (XPROCESS (proc)->reason) = 0;
		    }
		}
	    }
	} /* end for */
    } /* end while */
}

/* Read pending output from the process channel,
   starting with our buffered-ahead character if we have one.
   Yield number of characters read.  */

read_process_output (proc, channel)
     Lisp_Object proc;
     register int channel;
{
  register int count;
  register int total = 0;
  char buf[1024];

  while (1)
    {
      if (proc_buffered_char[channel] < 0)
	count = read (channel, buf, sizeof buf);
      else
	{
	  buf[0] = proc_buffered_char[channel];
	  proc_buffered_char[channel] = -1;
	  count = read (channel, buf + 1, sizeof buf - 1) + 1;
	}

      if (count <= 0)
	break;

      total += count;
      handle_process_output (proc, buf, count);
    }
  return total;
}

/*
 * Output has been received from a process on "chan" and should be stuffed in
 * the correct buffer.
 */
handle_process_output (proc, chars, nchars)
     Lisp_Object proc;
     char *chars;
     int nchars;
{
  Lisp_Object outstream;
  register struct buffer *old = bf_cur;
  register struct Lisp_Process *p = XPROCESS (proc);
  register int opoint;

  outstream = p->filter;
  if (!NULL (outstream))
    {
      call2 (outstream, proc, make_string (chars, nchars));
      return 1;
    }

  /* If no filter, write into buffer if it isn't dead.  */
  if (!NULL (p->buffer) && !NULL (XBUFFER (p->buffer)->name))
    {
      Fset_buffer (p->buffer);
      opoint = point;

      /* Insert new output into buffer
	 at the current end-of-output marker,
	 thus preserving logical ordering of input and output.  */
      if (XMARKER (p->mark)->buffer)
	SetPoint (marker_position (p->mark));
      else
	SetPoint (NumCharacters + 1);
      if (point <= opoint)
	opoint += nchars;

      InsCStr (chars, nchars);
      Fset_marker (p->mark, make_number (point), p->buffer);
      RedoModes++;

      SetPoint (opoint);
      SetBfp (old);
    }
  else return 0;

  /* Old feature was, delete early chars in chunks if
    buffer gets bigger that ProcessBufferSize.
    This feature is flushed */

  return 1;
}

/* Sending data to subprocess */

jmp_buf send_process_frame;

send_process_trap ()
{
#ifdef BSD4_1
  sigrelse (SIGPIPE);
  sigrelse (SIGALRM);
#endif /* BSD4_1 */
  longjmp (send_process_frame, 1);
}

send_process_1 (proc, buf, len)
     Lisp_Object proc;
     char *buf;
     int len;
{
  /* Don't use register vars; longjmp can lose them.  */
  int rv;
  unsigned char *procname = XSTRING (XPROCESS (proc)->name)->data;

  if ((XFASTINT (XPROCESS (proc)->flags) & PROC_STATUS) != RUNNING)
    error ("Process %s not running", procname);

  signal (SIGPIPE, send_process_trap);

  if (!setjmp (send_process_frame))
    while (len > 0)
      {
	rv = write (XFASTINT (XPROCESS (proc)->outfd), buf, len);
	if (rv < 0)
	  break;
	buf += rv;
	len -= rv;
      }
  else
    {
      signal (SIGPIPE, SIG_DFL);
      XFASTINT (XPROCESS (proc)->flags) =  EXITED | CHANGED;
      deactivate_process (proc);
      error ("SIGPIPE raised on process %s; closed it", procname);
    }

  signal (SIGPIPE, SIG_DFL);

  if (rv < 0)
    report_file_error ("writing to process", Fcons (proc, Qnil));
}

/*** Is it really safe for this to get an error ?  */

send_process (proc, buf, count)
     Lisp_Object proc;
     char *buf;
     int count;
{
#ifdef vipc
  struct { int checkword, type, datalen; } header;

  if (!NULL (XPROCESS (proc)->command_channel_p))
    {
      checkword = UNIQUE_FROB;
      type = VIPC_MESG;
      datalen = count;
      send_process_1 (proc, &header, sizeof header);
    }
#endif vipc
  send_process_1 (proc, buf, count);
}

DEFUN ("send-region", Fsend_region, Ssend_region, 3, 3, 0,
  "Send current contents of region as input to PROCESS.\n\
PROCESS may be a process name.\n\
Called from program, takes three arguments, PROCESS, START and END.")
  (process, start, end)
     Lisp_Object process, start, end;
{
  Lisp_Object proc;
  proc = get_process (process);
  validate_region (&start, &end);

  if (XINT (start) < bf_s1 && XINT (end) >= bf_s1)
    GapTo (start);

  send_process (proc, &CharAt (XINT (start)), XINT (end) - XINT (start));

  return Qnil;
}

DEFUN ("send-string", Fsend_string, Ssend_string, 2, 2, 0,
  "Send PROCESS the contents of STRING as input.\n\
PROCESS may be a process name.")
  (process, string)
     Lisp_Object process, string;
{
  Lisp_Object proc;
  CHECK_STRING (string, 1);
  proc = get_process (process);
  send_process (proc, XSTRING (string)->data, XSTRING (string)->size);
  return Qnil;
}

/* send a signal number SIGNO to PROCESS.
   CURRENT_GROUP means send to the process group that currently owns
   the terminal being used to communicate with PROCESS.
   This is used for various commands in shell mode.
   If NOMSG is zero, insert signal-announcements into process's buffers
   right away.  */

sig_process (process, signo, current_group, nomsg)
     Lisp_Object process;
     int signo;
     Lisp_Object current_group;
     int nomsg;
{
  Lisp_Object proc;
  register struct Lisp_Process *p;
  int gid;

  proc = get_process (process);
  p = XPROCESS (proc);

  if (NULL (p->childp))
    error ("Process %s is not a subprocess",
	   XSTRING (p->name)->data);
  if (!XFASTINT (p->infd))
    error ("Process %s is not active",
	   XSTRING (p->name)->data);

#ifdef TIOCGPGRP		/* Not sure about this! (fnf) */
  /* If we are using pgrps, get a pgrp number and make it negative.  */
  if (!NULL (current_group))
    {
      ioctl (XFASTINT (p->infd), TIOCGPGRP, &gid);
      gid = - gid;
    }
  else
    gid = - XFASTINT (p->pid);
#else /* not using pgrps */
  /* Can't select pgrps on this system, so we know that
     the child itself heads the pgrp.  */
  gid = - XFASTINT (p->pid);
#endif /* not using pgrps */

  switch (signo)
    {
#ifdef SIGCONT
    case SIGCONT:
      XFASTINT (p->flags) = RUNNING | CHANGED;
      child_changed++;
      break;
#endif
    case SIGINT:
    case SIGQUIT:
    case SIGKILL:
      flush_pending_output (XFASTINT (p->infd));
      break;
    }
  /* gid may be a pid, or minus a pgrp's number */
#ifdef BSD
  /* On bsd, [man says] kill does not accept a negative number to kill a pgrp.
     Must do that differently.  */
  killpg (-gid, signo);
#else /* Not BSD.  */
  kill (gid, signo);
#endif /* Not BSD.  */

  /* Put notices in buffers now, since it is safe now.
     Because of this, we know that a process we have just killed
     will never need to use its buffer again.  */
  if (!nomsg)
    change_msgs ();
}

DEFUN ("interrupt-process", Finterrupt_process, Sinterrupt_process, 0, 2, 0,
  "Interrupt process PROCESS.  May be process or name of one.\n\
Nil or no arg means current buffer's process.\n\
Second arg CURRENT-GROUP non-nil means send signal to\n\
the current process-group of the process's controlling terminal\n\
rather than to the process's own process group.\n\
If the process is a shell, this means interrupt current subjob\n\
rather than the shell.")
  (process, current_group)
     Lisp_Object process, current_group;
{
  sig_process (process, SIGINT, current_group, 0);
  return process;
}

DEFUN ("kill-process", Fkill_process, Skill_process, 0, 2, 0,
  "Kill process PROCESS.  May be process or name of one.\n\
See function interrupt-process for more details on usage.")
  (process, current_group)
     Lisp_Object process, current_group;
{
  sig_process (process, SIGKILL, current_group, 0);
  return process;
}

DEFUN ("quit-process", Fquit_process, Squit_process, 0, 2, 0,
  "Send QUIT signal to process PROCESS.  May be process or name of one.\n\
See function interrupt-process for more details on usage.")
  (process, current_group)
     Lisp_Object process, current_group;
{
  sig_process (process, SIGQUIT, current_group, 0);
  return process;
}

DEFUN ("stop-process", Fstop_process, Sstop_process, 0, 2, 0,
  "Stop process PROCESS.  May be process or name of one.\n\
See function interrupt-process for more details on usage.")
  (process, current_group)
     Lisp_Object process, current_group;
{
#ifndef SIGTSTP
  error ("no SIGTSTP support");
#else
  sig_process (process, SIGTSTP, current_group, 0);
#endif
  return process;
}

DEFUN ("continue-process", Fcontinue_process, Scontinue_process, 0, 2, 0,
  "Continue process PROCESS.  May be process or name of one.\n\
See function interrupt-process for more details on usage.")
  (process, current_group)
     Lisp_Object process, current_group;
{
#ifdef SIGCONT
    sig_process (process, SIGCONT, current_group, 0);
#else
    error ("no SIGCONT support");
#endif
  return process;
}

DEFUN ("process-send-eof", Fprocess_send_eof, Sprocess_send_eof, 0, 1, 0,
  "Make PROCESS see end-of-file in its input.\n\
Eof comes after any text already sent to it.\n\
Nil or no arg means current buffer's process.")
  (process)
     Lisp_Object process;
{
  Lisp_Object proc;

  proc = get_process (process);
  send_process (proc, "\004", 1);
  return process;
}

/* Kill all processes associated with `buffer'.
 If `buffer' is nil, kill all processes  */

kill_buffer_processes (buffer)
     Lisp_Object buffer;
{
  Lisp_Object tail, proc;

  for (tail = Vprocess_alist; XGCTYPE (tail) == Lisp_Cons;
       tail = XCONS (tail)->cdr)
    {
      proc = XCONS (XCONS (tail)->car)->cdr;
      if (XGCTYPE (proc) == Lisp_Process
	  && (NULL (buffer) || EQ (XPROCESS (proc)->buffer, buffer)))
	if (XFASTINT (XPROCESS (proc)->infd))
	  sig_process (proc, SIGKILL, Qnil, 1);
    }
}

count_active_processes ()
{
  register Lisp_Object tail, proc;
  register int count = 0;

  for (tail = Vprocess_alist; !NULL (tail); tail = Fcdr (tail))
    {
      proc = Fcdr (Fcar (tail));

      if ((1 << (XFASTINT (XPROCESS (proc)->flags) & PROC_STATUS)
	   & ((1 << RUNNING) | (1 << STOPPED)))
	  && NULL (XPROCESS (proc)->kill_without_query))
	count++;
    }

  return count;
}

/* On receipt of a signal that a child status has changed,
 loop asking about children with changed statuses until
 the system says there are no more.
   All we do is change the flags components;
 we do not run sentinels or print notifications.
 That is saved for the next time keyboard input is done,
 in order to avoid timing errors.  */

/** WARNING: this can be called during garbage collection.
 Therefore, it must not be fooled by the presence of mark bits in
 Lisp objects.  */

/** USG WARNING:  Although it is not obvious from the documentation
 in signal(2), on a USG system the SIGCLD handler MUST NOT call
 signal() before executing at least one wait(), otherwise the handler
 will be called again, resulting in an infinite loop.  The relevant
 portion of the documentation reads "SIGCLD signals will be queued
 and the signal-catching function will be continually reentered until
 the queue is empty".  Invoking signal() causes the kernel to reexamine
 the SIGCLD queue.   Fred Fish, UniSoft Systems Inc. */

child_sig (signo)
     int signo;
{
  register int pid;
  WAITTYPE w;
  Lisp_Object tail, proc;
  register struct Lisp_Process *p;
  
#ifdef BSD4_1
  extern int synch_process_pid;
  extern int sigheld;
  sigheld |= sigbit (SIGCHLD);
#endif

loop: 

#ifdef WNOHANG
#ifndef WUNTRACED
#define WUNTRACED 0
#endif /* no WUNTRACED */
  pid = wait3 (&w, WNOHANG | WUNTRACED, 0);
  if (pid <= 0)
    {
      if (errno == EINTR)
	{
	  errno = 0;
	  goto loop;
	}
  /* USG systems forget handlers when they are used;
     must reestablish each time */
#ifdef USG
      signal (signo, child_sig);   /* WARNING - must come after wait3() */
#endif
#ifdef  BSD4_1
      sigheld &= ~sigbit (SIGCHLD);
      sigrelse (SIGCHLD);
#endif
      return;
    }
#else
  pid = wait (&w);
#endif /* no WNOHANG */

#ifdef BSD4_1
  if (synch_process_pid == pid)
    synch_process_pid = 0;         /* Zero it to show process has died. */
#endif

  for (tail = Vprocess_alist; XSYMBOL (tail) != XSYMBOL (Qnil); tail = XCONS (tail)->cdr)
    {
      proc = XCONS (XCONS (tail)->car)->cdr;
      p = XPROCESS (proc);
      if (!NULL (p->childp) && XFASTINT (p->pid) == pid)
	break;
    }

  if (XSYMBOL (tail) == XSYMBOL (Qnil))
#ifdef USG
    goto ignore;
#else
    goto loop;		/* We don't know who this is */
#endif

  child_changed++;
  if (WIFSTOPPED (w))
    {
      XFASTINT (p->flags) = STOPPED | CHANGED;
      XFASTINT (p->reason) = WSTOPSIG (w);
    }
  else if (WIFEXITED (w))
    {
      XFASTINT (p->flags) = EXITED | CHANGED;
      if (WCOREDUMP (w))
	XFASTINT (p->flags) |= COREDUMPED;
      XFASTINT (p->reason) = WRETCODE (w);
    }
  else if (WIFSIGNALED (w))
    {
      XFASTINT (p->flags) = SIGNALED | CHANGED;
      if (WCOREDUMP (w))
	XFASTINT (p->flags) |= COREDUMPED;
      XFASTINT (p->reason) = WTERMSIG (w);
    }
#ifndef USG
  goto loop;
#else
 ignore:
  signal (signo, child_sig);
#endif /* not USG */
}

/* Find all process marked as "changed"
  and notify the user in a suitable fashion
  (either run the sentinel or output a message).
  This is done while Emacs is waiting for keyboard input */

change_msgs()
{
  Lisp_Object tail, proc, buffer;
  register struct Lisp_Process *p;
  register struct buffer *old = bf_cur;
  char line[50];
  int opoint;

  child_changed = 0;

  for (tail = Vprocess_alist; !NULL (tail); tail = Fcdr (tail))
    {
      proc = Fcdr (Fcar (tail));
      p = XPROCESS (proc);

      if (!(XFASTINT (p->flags) & CHANGED))
	continue;

      /* If process is still active, read any output that remains.  */
      if (XFASTINT (p->infd))
	read_process_output (proc, XFASTINT (p->infd));

      XFASTINT (p->flags) &= ~CHANGED;

      line[0] = 0;
      buffer = p->buffer;

      if ((XFASTINT (p->flags) & PROC_STATUS) == SIGNALED
	  || (XFASTINT (p->flags) & PROC_STATUS) == STOPPED)
	{
	  sprintf (line, "%s%s\n",
		   XFASTINT (p->reason) < NSIG
		     ? sys_siglist[XFASTINT (p->reason)] : "unknown",
		   XFASTINT (p->flags) & COREDUMPED ? " (core dumped)" : "");
	  if (line[0] >= 'A' && line[0] <= 'Z')
	    line[0] += 040;

	  if ((XFASTINT (p->flags) & PROC_STATUS) == SIGNALED)
	    if (delete_exited_processes)
	      remove_process (proc);
	    else
	      deactivate_process (proc);
	}
      else if ((XFASTINT (p->flags) & PROC_STATUS) == EXITED)
	{
	  if (XFASTINT (p->reason))
	    sprintf (line, "exited abnormally with code %d\n",
		     XFASTINT (p->reason));
	  else
	    sprintf (line, "finished\n");

	  if (delete_exited_processes)
	    remove_process (proc);
	  else
	    deactivate_process (proc);
	}

      if (!NULL (p->sentinel))
	exec_sentinel (proc, build_string (line));
      else if (line[0] && !NULL (buffer))
	{
	  /* Avoid error if buffer is deleted
	     (probably that's why the process is dead, too) */
	  if (NULL (XBUFFER (buffer)->name))
	    continue;
	  Fset_buffer (buffer);
	  opoint = point;
	  SetPoint (NumCharacters + 1);
	  if (point == opoint)
	    opoint = -1;
	  InsStr ("\nProcess ");
	  Finsert (1, &p->name);
	  InsStr (" ");
	  InsStr (line);
	  if (opoint > 0)
	    SetPoint (opoint);
	}
    } /* end for */

  SetBfp (old);

  RedoModes++;  /* in case buffers use %s in mode-line-format */
  DoDsp (1);
}

exec_sentinel (proc, reason)
     Lisp_Object proc, reason;
{
  Lisp_Object sentinel;
  register struct Lisp_Process *p = XPROCESS (proc);

  sentinel = p->sentinel;
  if (NULL (sentinel))
    return;

  p->sentinel = Qnil;
  call2 (sentinel, proc, reason);
  p->sentinel = sentinel;
}

init_process ()
{
  register int i;

#ifdef SIGCHLD
  signal (SIGCHLD, child_sig);
#endif

  input_wait_mask = ChannelMask(0);
  Vprocess_alist = Qnil;
  for (i = 0; i < MAXDESC; i++)
    {
      chan_process[i] = Qnil;
      proc_buffered_char[i] = -1;
    }
}

syms_of_process ()
{
  Qprocessp = intern ("processp");
  staticpro (&Qprocessp);

  staticpro (&Vprocess_alist);

  DefBoolVar ("delete-exited-processes", &delete_exited_processes,
    "*Non-nil means delete processes immediately when they exit.\n\
nil means don't delete them until list-processes is done.");

  delete_exited_processes = 1;

  defsubr (&Sprocessp);
  defsubr (&Sget_process);
  defsubr (&Sget_buffer_process);
  defsubr (&Sdelete_process);
  defsubr (&Sprocess_status);
  defsubr (&Sprocess_id);
  defsubr (&Sprocess_name);
  defsubr (&Sprocess_command);
  defsubr (&Sset_process_buffer);
  defsubr (&Sprocess_buffer);
  defsubr (&Sprocess_mark);
  defsubr (&Sset_process_filter);
  defsubr (&Sprocess_filter);
  defsubr (&Sset_process_sentinel);
  defsubr (&Sprocess_sentinel);
  defsubr (&Sprocess_kill_without_query);
  defsubr (&Slist_processes);
  defsubr (&Sstart_process);
  defsubr (&Saccept_process_output);
  defsubr (&Ssend_region);
  defsubr (&Ssend_string);
  defsubr (&Sinterrupt_process);
  defsubr (&Skill_process);
  defsubr (&Squit_process);
  defsubr (&Sstop_process);
  defsubr (&Scontinue_process);
  defsubr (&Sprocess_send_eof);
}

#endif subprocesses
