/* Interfaces to system-dependent kernel and library entries.
   Copyright (C) 1985, 1986, 1987, 1988, 1990 Free Software Foundation, Inc.

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


#include <signal.h>
#include <setjmp.h>

#include "config.h"
#include "lisp.h"
#undef NULL

#define min(x,y) ((x) > (y) ? (y) : (x))

/* In this file, open, read and write refer to the system calls,
   not our sugared interfaces  sys_open, sys_read and sys_write.
   Contrariwise, for systems where we use the system calls directly,
   define sys_read, etc. here as aliases for them.  */
#ifndef read
#define sys_read read
#define sys_write write
#endif /* `read' is not a macro */

#undef read
#undef write

#ifndef close
#define sys_close close
#else 
#undef close
#endif

#ifndef open
#define sys_open open
#else /* `open' is a macro */
#undef open
#endif /* `open' is a macro */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

extern int errno;
#ifndef VMS
#ifndef BSD4_4
extern char *sys_errlist[];
#endif
#endif

char __progname[] = "emacs";

#ifdef VMS
#include <rms.h>
#include <ttdef.h>
#include <tt2def.h>
#include <iodef.h>
#include <ssdef.h>
#include <descrip.h>
#include <ctype.h>
#include <file.h>
#ifndef RAB$C_BID
#include <rab.h>
#endif
#define	MAXIOSIZE ( 32 * PAGESIZE )	/* Don't I/O more than 32 blocks at a time */
#endif /* VMS */

#ifndef BSD4_1
#ifdef BSD /* this is done this way to avoid defined(BSD) || defined (USG)
	      because the vms compiler doesn't grok `defined' */
#include <fcntl.h>
#endif
#ifdef USG
#include <fcntl.h>
#endif
#endif /* not 4.1 bsd */

#ifdef BSD
#include <sys/ioctl.h>
#ifdef BSD4_1
#include <wait.h>
#else /* not 4.1 */
#include <sys/wait.h>
#endif /* not 4.1 */
#endif /* BSD */

#ifdef STRIDE
#include <sys/ioctl.h>
#endif 

#ifdef mips
#include <sys/ioctl.h>
#endif 

#ifdef AIX
/* Get files for keyboard remapping */
#define HFNKEYS 2
#include <sys/hft.h>
#include <sys/devinfo.h>
#endif

/* Get rid of LLITOUT in 4.1, since it is said to stimulate kernel bugs.  */
#ifdef BSD4_1
#undef LLITOUT
#define LLITOUT 0
#endif /* 4.1 */

#ifdef HAVE_TERMIO
#ifdef IBMR2AIX
#include <termio.h>
#include <termios.h>
#ifdef TIOCGETP
#undef TIOCGETP
#endif
#define TIOCGETP TCGETS
#undef TIOCSETN
#define TIOCSETN TCSETSW
#undef TIOCSETP
#define TIOCSETP TCSETSF
#undef TCSETAW
#define TCSETAW TCSETS
#else
#include <termio.h>
#undef TIOCGETP
#define TIOCGETP TCGETA
#undef TIOCSETN
#define TIOCSETN TCSETAW
#undef TIOCSETP
#define TIOCSETP TCSETAF
#endif

#ifndef IBMR2AIX
#define TERMINAL struct termio
#else
#define TERMINAL struct termios
#endif

#define OSPEED(str) (str.c_cflag & CBAUD)
#define SETOSPEED(str,new) (str.c_cflag = (str.c_cflag & ~CBAUD) | (new))
#define TABS_OK(str) ((str.c_oflag & TABDLY) != TAB3)
#endif /* HAVE_TERMIO */

#ifdef XENIX
#undef TIOCGETC  /* Avoid confusing some conditionals that test this.  */
#endif

#ifdef BROKEN_TIOCGETC
#undef TIOCGETC
#endif

#ifndef HAVE_TERMIO
#ifndef VMS
#include <sgtty.h>
#define TERMINAL struct sgttyb
#define OSPEED(str) str.sg_ospeed
#define SETOSPEED(str,new) (str.sg_ospeed = (new))
#define TABS_OK(str) ((str.sg_flags & XTABS) != XTABS)
#undef TCSETAW
#define TCSETAW TIOCSETN
#endif /* not VMS */
#endif /* not HAVE_TERMIO */

#ifdef USG
#include <sys/utsname.h>
#include <memory.h>
#include <string.h>
#ifdef TIOCGWINSZ
#ifndef IRIS_4D
#ifndef mips
#ifndef aix386
#ifndef NO_SIOCTL_H
/* Some USG systems with TIOCGWINSZ need this file; some don't have it.
   We don't know how to distinguish them.
   If this #include gets an error, just delete it.  */
#include <sys/sioctl.h>
#endif
#ifdef NEED_PTEM_H
#include <sys/stream.h>
#include <sys/ptem.h>
#endif
#endif /* not aix386 */
#endif /* not mips */
#endif /* not IRIS_4D */
#endif /* TIOCGWINSZ */
#ifdef HAVE_TIMEVAL
#ifdef HPUX
#include <time.h>
#else
#include <sys/time.h>
#endif
#endif /* HAVE_TIMEVAL */
#endif /* USG */

#ifdef VMS
#include "window.h"
#endif
  
#ifdef NEED_BSDTTY
#include <sys/bsdtty.h>
#endif 

#if defined (HPUX) && defined (HAVE_PTYS)
#include <sys/ptyio.h>
#endif
  
#ifdef AIX
#include <sys/pty.h>
#include <unistd.h>
#endif /* AIX */

#ifdef SYSV_PTYS
#include <sys/tty.h>
#include <sys/pty.h>
#endif

#ifdef BROKEN_FIONREAD
#undef FIONREAD
#endif

extern int quit_char;

#include "termhooks.h"
#include "termchar.h"
#include "termopts.h"
#include "dispextern.h"

#ifdef NONSYSTEM_DIR_LIBRARY
#include "ndir.h"
#endif /* NONSYSTEM_DIR_LIBRARY */

#ifndef sigmask
#define sigmask(no) (1L << ((no) - 1))
#endif

/* Define SIGCHLD as an alias for SIGCLD.  There are many conditionals
   testing SIGCHLD.  */

#ifndef VMS
#ifdef SIGCLD
#ifndef SIGCHLD
#define SIGCHLD SIGCLD
#endif /* not SIGCHLD */
#endif /* SIGCLD */
#endif /* not VMS */

static int baud_convert[] =
#ifdef BAUD_CONVERT
  BAUD_CONVERT;
#else
  {
    0, 50, 75, 110, 135, 150, 200, 300, 600, 1200,
    1800, 2400, 4800, 9600, 19200, 38400
  };
#endif

extern short ospeed;

#ifdef VMS
static struct iosb
{
  short status;
  short offset;
  short termlen;
  short term;
} input_iosb;

int kbd_input_ast ();

int waiting_for_ast;
int stop_input;
int input_ef = 0;
int timer_ef = 0;
int process_ef = 0;
int input_eflist;
int timer_eflist;

static int input_chan;
static $DESCRIPTOR (input_dsc, "TT");
static int terminator_mask[2] = { 0, 0 };

static struct sensemode {
  short status;
  unsigned char xmit_baud;
  unsigned char rcv_baud;
  unsigned char crfill;
  unsigned char lffill;
  unsigned char parity;
  unsigned char unused;
  char class;
  char type;
  short scr_wid;
  unsigned long tt_char : 24, scr_len : 8;
  unsigned long tt2_char;
} sensemode_iosb;
#define TERMINAL struct sensemode
#define OSPEED(str) (str.xmit_baud)
#define TABS_OK(str) ((str.tt_char & TT$M_MECHTAB) != 0)
#endif /* VMS */

discard_tty_input ()
{
  TERMINAL buf;

  if (noninteractive)
    return;

#ifdef VMS
  end_kbd_input ();
  SYS$QIOW (0, input_chan, IO$_READVBLK|IO$M_PURGE, input_iosb, 0, 0,
	    &buf, 0, 0, terminator_mask, 0, 0);
  queue_kbd_input ();
#else /* not VMS */
  ioctl (0, TIOCGETP, &buf);
  ioctl (0, TIOCSETP, &buf);
#endif /* not VMS */
}

#ifdef SIGTSTP

stuff_char (c)
     char c;
{
/* Should perhaps error if in batch mode */
#ifdef TIOCSTI
  ioctl (0, TIOCSTI, &c);
#else /* no TIOCSTI */
  error ("Cannot stuff terminal input characters in this version of Unix.");
#endif /* no TIOCSTI */
}

#endif /* SIGTSTP */

init_baud_rate ()
{
  TERMINAL sg;

  if (noninteractive)
    ospeed = 0;
  else
    {
#ifdef VMS
      SYS$QIOW (0, input_chan, IO$_SENSEMODE, &sg, 0, 0,
		&sg.class, 12, 0, 0, 0, 0 );
#else
      SETOSPEED (sg, B9600);
      ioctl (0, TIOCGETP, &sg);
#endif /* not VMS */
      ospeed = OSPEED (sg);
    }
  
  baud_rate = (ospeed < sizeof baud_convert / sizeof baud_convert[0]
	       ? baud_convert[ospeed] : 9600);
  if (baud_rate == 0)
    baud_rate = 1200;
}

/*ARGSUSED*/
set_exclusive_use (fd)
     int fd;
{
#ifdef FIOCLEX
  ioctl (fd, FIOCLEX, 0);
#endif
  /* Ok to do nothing if this feature does not exist */
}

#ifndef subprocesses

wait_without_blocking ()
{
#ifdef BSD
  wait3 (0, WNOHANG | WUNTRACED, 0);
#else
  croak ("wait_without_blocking");
#endif
}

#endif /* not subprocesses */

int wait_debugging;   /* Set nonzero to make following function work under dbx
		         (at least for bsd).  */

/* Wait for subprocess with process id `pid' to terminate and
   make sure it will get eliminated (not remain forever as a zombie) */

wait_for_termination (pid)
     int pid;
{
  while (1)
    {
#ifdef subprocesses
#if defined(BSD) || (defined(HPUX) && !defined(HPUX_5))
      /* Note that kill returns -1 even if the process is just a zombie now.
	 But inevitably a SIGCHLD interrupt should be generated
	 and child_sig will do wait3 and make the process go away. */
      /* There is some indication that there is a bug involved with
	 termination of subprocesses, perhaps involving a kernel bug too,
	 but no idea what it is.  Just as a hunch we signal SIGCHLD to see
	 if that causes the problem to go away or get worse.  */
#ifdef BSD4_1
      extern int synch_process_pid;
      sighold (SIGCHLD);
      if (synch_process_pid == 0)
	{
          sigrelse (SIGCHLD);
	  break;
	}
      if (wait_debugging)
	sleep (1);
      else
	sigpause (SIGCHLD);
#else /* not BSD4_1 */
      sigsetmask (1 << (SIGCHLD - 1));
      if (0 > kill (pid, 0))
        {
	  sigsetmask (0);
	  kill (getpid (), SIGCHLD);
	  break;
	}
      if (wait_debugging)
	sleep (1);
      else
	sigpause (0);
#endif /* not BSD4_1 */
#else /* not BSD, and not HPUX version >= 6 */
#ifdef UNIPLUS
      if (0 > kill (pid, 0))
	break;
      wait (0);
#else /* neither BSD nor UNIPLUS: random sysV */
      if (0 > kill (pid, 0))
	break;
      pause ();
#endif /* not UNIPLUS */
#endif /* not BSD, and not HPUX version >= 6 */
#else /* not subprocesses */
#ifndef BSD4_1
      if (0 > kill (pid, 0))
	break;
      wait (0);
#else /* BSD4_1 */
      int status;
      status = wait (0);
      if (status == pid || status == -1)
	break;
#endif /* BSD4_1 */
#endif /* not subprocesses */
    }
}

#ifdef subprocesses

/*
 *	flush any pending output
 *      (may flush input as well; it does not matter the way we use it)
 */
 
flush_pending_output (channel)
     int channel;
{
#ifdef TCFLSH
  ioctl (channel, TCFLSH, 1);
#else
#ifdef TIOCFLUSH
  int zero = 0;
  /* 3rd arg should be ignored
     but some 4.2 kernels actually want the address of an int
     and nonzero means something different.  */
  ioctl (channel, TIOCFLUSH, &zero);
#endif
#endif
}

/*  Set up the terminal at the other end of a pseudo-terminal that
    we will be controlling an inferior through.
    It should not echo or do line-editing, since that is done
    in Emacs.  No padding needed for insertion into an Emacs buffer.  */

child_setup_tty (out)
     int out;
{
  TERMINAL s;

  ioctl (out, TIOCGETP, &s);
#ifdef HAVE_TERMIO
  s.c_oflag |= OPOST;		/* Enable output postprocessing */
  s.c_oflag &= ~ONLCR;		/* Disable map of NL to CR-NL on output */
  s.c_oflag &= ~(NLDLY|CRDLY|TABDLY|BSDLY|VTDLY|FFDLY);	/* No output delays */
  s.c_lflag &= ~ECHO;		/* Disable echo */
  s.c_lflag |= ISIG;		/* Enable signals */
  s.c_iflag &= ~IUCLC;		/* Disable map of upper case to lower on input */
  s.c_oflag &= ~OLCUC;		/* Disable map of lower case to upper on output */
/* said to be unnecesary
  s.c_cc[VMIN] = 1;		/* minimum number of characters to accept
  s.c_cc[VTIME] = 0;		/* wait forever for at least 1 character
*/
  s.c_lflag |= ICANON;		/* Enable erase/kill and eof processing */
  s.c_cc[VEOF] = 04;		/* insure that EOF is Control-D */
  s.c_cc[VERASE] = 0377;	/* disable erase processing */
  s.c_cc[VKILL] = 0377;		/* disable kill processing */
#ifdef HPUX
  s.c_cflag = (s.c_cflag & ~CBAUD) | B9600; /* baud rate sanity */
#endif HPUX

#ifdef AIX
/* AIX enhanced edit loses NULs, so disable it */
#ifndef IBMR2AIX
  s.c_line = 0;
  s.c_iflag &= ~ASCEDIT;
#endif
  /* Also, PTY overloads NUL and BREAK.
     don't ignore break, but don't signal either, so it looks like NUL.  */
  s.c_iflag &= ~IGNBRK;
  s.c_iflag &= ~BRKINT;
/* QUIT and INTR work better as signals, so disable character forms */
  s.c_cc[VQUIT] = 0377;
  s.c_cc[VINTR] = 0377;
  s.c_cc[VEOL] = 0377;
  s.c_lflag &= ~ISIG;
  s.c_cflag = (s.c_cflag & ~CBAUD) | B9600; /* baud rate sanity */
#endif /* AIX */

#else /* not HAVE_TERMIO */
  s.sg_flags &= ~(ECHO | CRMOD | ANYP | ALLDELAY | RAW | LCASE | CBREAK | TANDEM);
#endif /* not HAVE_TERMIO */

  ioctl (out, TIOCSETN, &s);

#ifdef BSD4_1
  if (interrupt_input)
    reset_sigio ();
#endif /* BSD4_1 */
#ifdef RTU
  {
    int zero = 0;
    ioctl (out, FIOASYNC, &zero);
  }
#endif /* RTU */
}

#endif /* subprocesses */

/*ARGSUSED*/
setpgrp_of_tty (pid)
     int pid;
{
#ifdef IBMR2AIX
  tcsetpgrp ( 0, pid);
#else
#ifdef TIOCSPGRP
  ioctl (0, TIOCSPGRP, &pid);
#else
  /* Just ignore this for now and hope for the best */
#endif
#endif
}

/* Record a signal code and the handler for it.  */
struct save_signal
{
  int code;
  int (*handler) ();
};

/* Suspend the Emacs process; give terminal to its superior.  */

sys_suspend ()
{
#ifdef VMS
  unsigned long parent_id;

  parent_id = getppid ();
  if (parent_id && parent_id != 0xffffffff)
    {
      int oldsig = signal (SIGINT, SIG_IGN);
      int status = LIB$ATTACH (&parent_id) & 1;
      signal (SIGINT, oldsig);
      return status;
    }
  return -1;
#else
#ifdef SIGTSTP
#ifdef BSD
  killpg (getpgrp (0), SIGTSTP);
#else
  kill (-getpgrp (0), SIGTSTP);
#endif

#else
#ifdef USG_JOBCTRL /* If you don't know what this is don't mess with it */
  ptrace (0, 0, 0, 0);		/* set for ptrace - caught by csh */
  kill (getpid (), SIGQUIT);

#else

/* On a system where suspending is not implemented,
   instead fork a subshell and let it talk directly to the terminal
   while we wait.  */
  int pid = fork ();
  struct save_signal saved_handlers[5];

  saved_handlers[0].code = SIGINT;
  saved_handlers[1].code = SIGQUIT;
  saved_handlers[2].code = SIGTERM;
#ifdef SIGIO
  saved_handlers[3].code = SIGIO;
  saved_handlers[4].code = 0;
#else
  saved_handlers[3].code = 0;
#endif

  if (pid == -1)
    error ("Can't spawn subshell");
  if (pid == 0)
    {
      char *sh;

      sh = (char *) egetenv ("SHELL");
      if (sh == 0)
	sh = "sh";
      /* Use our buffer's default directory for the subshell.  */
      {
	Lisp_Object dir;
	unsigned char *str;
	int len;

	/* mentioning current_buffer->buffer would mean including buffer.h,
	   which somehow wedges the hp compiler.  So instead... */

	dir = intern ("default-directory");
	/* Can't use NULL */
	if (XFASTINT (Fboundp (dir)) == XFASTINT (Qnil))
	  goto xyzzy;
	dir = Fsymbol_value (dir);
	if (XTYPE (dir) != Lisp_String)
	  goto xyzzy;

	str = (unsigned char *) alloca (XSTRING (dir)->size + 2);
	len = XSTRING (dir)->size;
	bcopy (XSTRING (dir)->data, str, len);
	if (str[len - 1] != '/') str[len++] = '/';
	str[len] = 0;
	chdir (str);
      }
    xyzzy:
#ifdef subprocesses
      close_process_descs ();	/* Close Emacs's pipes/ptys */
#endif
      execlp (sh, sh, 0);
      write (1, "Can't execute subshell", 22);
      _exit (1);
    }
  save_signal_handlers (&saved_handlers);
  wait_for_termination (pid);
  restore_signal_handlers (&saved_handlers);
#endif /* no USG_JOBCTRL */
#endif /* no SIGTSTP */
#endif /* not VMS */
}

save_signal_handlers (saved_handlers)
     struct save_signal *saved_handlers;
{
  while (saved_handlers->code)
    {
      saved_handlers->handler
	= (int (*) ()) signal (saved_handlers->code, SIG_IGN);
      saved_handlers++;
    }
}

restore_signal_handlers (saved_handlers)
     struct save_signal *saved_handlers;
{
  while (saved_handlers->code)
    {
      signal (saved_handlers->code, saved_handlers->handler);
      saved_handlers++;
    }
}

#ifdef F_SETFL

int old_fcntl_flags;

init_sigio ()
{
#ifdef FASYNC
  old_fcntl_flags = fcntl (0, F_GETFL, 0) & ~FASYNC;
#endif
  request_sigio ();
}

reset_sigio ()
{
  unrequest_sigio ();
}

#ifdef FASYNC		/* F_SETFL does not imply existance of FASYNC */

request_sigio ()
{
#ifdef SIGWINCH
  int omask = sigblock (0);
  sigsetmask (omask & ~sigmask (SIGWINCH));
#endif
  fcntl (0, F_SETFL, old_fcntl_flags | FASYNC);

  interrupts_deferred = 0;
}

unrequest_sigio ()
{
#ifdef SIGWINCH
  sigblock (sigmask (SIGWINCH));
#endif
  fcntl (0, F_SETFL, old_fcntl_flags);
  interrupts_deferred = 1;
}

#else /* no FASYNC */
#ifdef STRIDE		/* Stride doesn't have FASYNC - use FIOASYNC */

request_sigio ()
{
  int on = 1;
  ioctl (0, FIOASYNC, &on);
  interrupts_deferred = 0;
}

unrequest_sigio ()
{
  int off = 0;

  ioctl (0, FIOASYNC, &off);
  interrupts_deferred = 1;
}

#else /* not FASYNC, not STRIDE */
 
request_sigio ()
{
  croak ("request_sigio");
}
 
unrequest_sigio ()
{
  croak ("unrequest_sigio");
}
 
#endif /* STRIDE */
#endif /* FASYNC */
#endif /* F_SETFL */

TERMINAL old_gtty;		/* The initial tty mode bits */

int term_initted;		/* 1 if outer tty status has been recorded */

#ifdef F_SETOWN
int old_fcntl_owner;
#endif /* F_SETOWN */

#ifdef TIOCGLTC
struct ltchars old_ltchars;
#endif /* TIOCGLTC */

#ifdef TIOCGETC
struct tchars old_tchars;
int old_lmode;

int lmode;			/* Current lmode value. */
				/* Needed as global for 4.1 */
#endif /* TIOCGETC */

/* This may also be defined in stdio,
   but if so, this does no harm,
   and using the same name avoids wasting the other one's space.  */

#ifdef USG
unsigned char _sobuf[BUFSIZ+8];
#else
char _sobuf[BUFSIZ];
#endif
 
#ifdef TIOCGLTC
static struct ltchars new_ltchars = {-1,-1,-1,-1,-1,-1};
#endif
#ifdef TIOCGETC
  static struct tchars new_tchars = {-1,-1,-1,-1,-1,-1};
#endif 

init_sys_modes ()
{
  TERMINAL tty;
#ifdef TIOCGETC
  struct tchars tchars;
#endif
#ifdef VMS
#if 0
  static int oob_chars[2] = {0, 1 << 7}; /* catch C-g's */
  extern int (*interrupt_signal) ();
#endif
#endif

  if (noninteractive)
    return;

#ifdef VMS
  if (!input_ef)
    LIB$GET_EF (&input_ef);
  SYS$CLREF (input_ef);
  waiting_for_ast = 0;
  if (!timer_ef)
    LIB$GET_EF (&timer_ef);
  SYS$CLREF (timer_ef);
  if (!process_ef)
    {
      LIB$GET_EF (&process_ef);
      SYS$CLREF (process_ef);
    }
  if (input_ef / 32 != process_ef / 32)
    croak ("Input and process event flags in different clusters.");
  if (input_ef / 32 != timer_ef / 32)
    croak ("Input and process event flags in different clusters.");
  input_eflist = ((unsigned) 1 << (input_ef % 32)) |
    ((unsigned) 1 << (process_ef % 32));
  timer_eflist = ((unsigned) 1 << (input_ef % 32)) |
    ((unsigned) 1 << (timer_ef % 32));
  SYS$QIOW (0, input_chan, IO$_SENSEMODE, &old_gtty, 0, 0,
	    &old_gtty.class, 12, 0, 0, 0, 0);
#ifndef VMS4_4
  sys_access_reinit ();
#endif
#else /* not VMS */
  ioctl (0, TIOCGETP, &old_gtty);
#endif /* not VMS */
  if (!read_socket_hook)
    {
      tty = old_gtty;

#ifdef HAVE_TERMIO
      tty.c_iflag |= (IGNBRK);	/* Ignore break condition */
      tty.c_iflag &= ~ICRNL;	/* Disable map of CR to NL on input */
#ifdef ISTRIP
      tty.c_iflag &= ~ISTRIP;	/* don't strip 8th bit on input */
#endif
      tty.c_lflag &= ~ECHO;	/* Disable echo */
      tty.c_lflag &= ~ICANON;	/* Disable erase/kill processing */
      tty.c_lflag |= ISIG;	/* Enable signals */
      if (flow_control)
	{
	  tty.c_iflag |= IXON;	/* Enable start/stop output control */
#ifdef IXANY
	  tty.c_iflag &= ~IXANY;
#endif /* IXANY */
	}
      else
	tty.c_iflag &= ~IXON;	/* Disable start/stop output control */
      tty.c_oflag &= ~ONLCR;	/* Disable map of NL to CR-NL on output */
      tty.c_oflag &= ~TAB3;	/* Disable tab expansion */
#ifdef CS8
      tty.c_cflag |= CS8;	/* allow 8th bit on input */
      tty.c_cflag &= ~PARENB;	/* Don't check parity */
#endif
      tty.c_cc[VINTR] = quit_char;	/* ^G gives SIGINT */
      /* Set up C-g (usually) for both SIGQUIT and SIGINT.
	 We don't know which we will get, but we handle both alike
	 so which one it really gives us does not matter.  */
      tty.c_cc[VQUIT] = quit_char;
      tty.c_cc[VMIN] = 1;	/* Input should wait for at least 1 char */
      tty.c_cc[VTIME] = 0;	/* no matter how long that takes.  */
#ifdef VSWTCH
      tty.c_cc[VSWTCH] = CDEL;	/* Turn off shell layering use of C-z */
#endif /* VSWTCH */
#ifdef mips  /* The following code looks like the right thing in general,
		but it is said to cause a crash on USG V.4.
		Let's play safe by turning it on only for the MIPS.  */
#ifdef VSUSP
      tty.c_cc[VSUSP] = CDEL;	/* Turn off mips handling of C-z.  */
#endif /* VSUSP */
#ifdef V_DSUSP
      tty.c_cc[V_DSUSP] = CDEL;	/* Turn off mips handling of C-y.  */
#endif /* V_DSUSP */
#endif /* mips */

#ifdef AIX
#ifndef IBMR2AIX
      /* AIX enhanced edit loses NULs, so disable it */
      tty.c_line = 0;
      tty.c_iflag &= ~ASCEDIT;
#else
      tty.c_cc[VSTRT] = 255;
      tty.c_cc[VSTOP] = 255;
      tty.c_cc[VSUSP] = 255;
      tty.c_cc[VDSUSP] = 255;
#endif /* IBMR2AIX */
      /* Also, PTY overloads NUL and BREAK.
	 don't ignore break, but don't signal either, so it looks like NUL.
	 This really serves a purpose only if running in an XTERM window
	 or via TELNET or the like, but does no harm elsewhere.  */
      tty.c_iflag &= ~IGNBRK;
      tty.c_iflag &= ~BRKINT;
#endif /* AIX */

#else /* if not HAVE_TERMIO */
#ifdef VMS
      tty.tt_char |= TT$M_NOECHO | TT$M_EIGHTBIT;
      if (flow_control)
	tty.tt_char |= TT$M_TTSYNC;
      else
	tty.tt_char &= ~TT$M_TTSYNC;
      tty.tt2_char |= TT2$M_PASTHRU | TT2$M_XON;
#else /* not VMS (BSD, that is) */
      tty.sg_flags &= ~(ECHO | CRMOD | XTABS);
      tty.sg_flags |= ANYP;
      tty.sg_flags |= interrupt_input ? RAW : CBREAK;
#endif /* not VMS (BSD, that is) */
#endif /* not HAVE_TERMIO */

#ifdef VMS
      SYS$QIOW (0, input_chan, IO$_SETMODE, &input_iosb, 0, 0,
		&tty.class, 12, 0, 0, 0, 0);
#else
      ioctl (0, TIOCSETN, &tty);
#endif /* not VMS */

      /* This code added to insure that, if flow-control is not to be used,
	 we have an unlocked screen at the start. */
#ifdef TCXONC
      if (!flow_control) ioctl (0, TCXONC, 1);
#endif
#ifdef TIOCSTART
      if (!flow_control) ioctl (0, TIOCSTART, 0);
#endif

#ifdef AIX
      hft_init ();
#ifdef IBMR2AIX
  {
    /* IBM's HFT device usually thinks a ^J should be LF/CR.  We need it
       to be only LF.  This is the way that is done. */
    struct termio tty;

    if (ioctl (1, HFTGETID, &tty) != -1)
      write (1, "\033[20l", 5);
  }
#endif
#endif

#ifdef F_SETFL
#ifdef F_GETOWN		/* F_SETFL does not imply existance of F_GETOWN */
      if (interrupt_input)
	{
	  old_fcntl_owner = fcntl (0, F_GETOWN, 0);
	  fcntl (0, F_SETOWN, getpid ());
	  init_sigio ();
	}
#endif /* F_GETOWN */
#endif /* F_SETFL */

      /* If going to use CBREAK mode, we must request C-g to interrupt
	   and turn off start and stop chars, etc.
	   If not going to use CBREAK mode, do this anyway
	   so as to turn off local flow control for user coming over
	   network on 4.2; in this case, only t_stopc and t_startc really matter.  */
#ifdef TIOCGLTC
      ioctl (0, TIOCGLTC, &old_ltchars);
#endif /* TIOCGLTC */
#ifndef HAVE_TERMIO
#ifdef TIOCGETC
      ioctl (0, TIOCGETC, &old_tchars);
      ioctl (0, TIOCLGET, &old_lmode);

      /* Note: if not using CBREAK mode, it makes no difference how we set this */
      tchars = new_tchars;
      tchars.t_intrc = quit_char;
      if (flow_control)
	{
	  tchars.t_startc = '\021';
	  tchars.t_stopc = '\023';
	}
/* LPASS8 is new in 4.3, and makes cbreak mode provide all 8 bits.  */
#ifndef LPASS8
#define LPASS8 0
#endif

#ifdef BSD4_1
#define LNOFLSH 0100000
#endif

      lmode = LDECCTQ | LLITOUT | LPASS8 | LNOFLSH | old_lmode;

      ioctl (0, TIOCSETC, &tchars);
      ioctl (0, TIOCLSET, &lmode);
#endif /* TIOCGETC */
#endif /* not HAVE_TERMIO */
#ifdef TIOCGLTC
      ioctl (0, TIOCSLTC, &new_ltchars);
#endif /* TIOCGLTC */

#ifdef BSD4_1
      if (interrupt_input)
	init_sigio ();
#endif
#ifdef VMS
/*  Appears to do nothing when in PASTHRU mode.
      SYS$QIOW (0, input_chan, IO$_SETMODE|IO$M_OUTBAND, 0, 0, 0,
		interrupt_signal, oob_chars, 0, 0, 0, 0);
*/
      queue_kbd_input (0);
#endif /* VMS */
    }
#ifdef VMS  /* VMS sometimes has this symbol but lacks setvbuf.  */
#undef _IOFBF
#endif
#ifdef _IOFBF
  /* This symbol is defined on recent USG systems.
     Someone says without this call USG won't really buffer the file
     even with a call to setbuf(). */
  setvbuf (stdout, _sobuf, _IOFBF, sizeof _sobuf);
#else
  setbuf (stdout, _sobuf);
#endif
  set_terminal_modes ();
  if (term_initted && no_redraw_on_reenter)
    {
      if (display_completed)
	direct_output_forward_char (0);
    }
  else
    screen_garbaged = 1;
  term_initted = 1;
}

/* Return nonzero if safe to use tabs in output.
   At the time this is called, init_sys_modes has not been done yet.  */
   
tabs_safe_p ()
{
  TERMINAL tty;
  if (noninteractive)
    return 1;
#ifdef VMS
  SYS$QIOW (0, input_chan, IO$_SENSEMODE, &tty, 0, 0,
	    &tty.class, 12, 0, 0, 0, 0);
#else
  ioctl (0, TIOCGETP, &tty);
#endif /* not VMS */
  return (TABS_OK(tty));
}

/* Get terminal size from system.
   Store number of lines into *heightp and width into *widthp.
   If zero or a negative number is stored, the value is not valid.  */

get_screen_size (widthp, heightp)
     int *widthp, *heightp;
{
/* Define the 4.3 names in terms of the Sun names
   if the latter exist and the former do not.  */
#ifdef TIOCGSIZE
#ifndef TIOCGWINSZ
#define TIOCGWINSZ TIOCGSIZE
#define winsize ttysize
#define ws_row ts_lines
#define ws_col ts_cols
#endif
#endif /* Sun */

/* Do it using the 4.3 names if possible.  */
#ifdef TIOCGWINSZ
  struct winsize size;
  *widthp = 0;
  *heightp = 0;
  if (ioctl (0, TIOCGWINSZ, &size) < 0)
    return;
  *widthp = size.ws_col;
  *heightp = size.ws_row;
#else /* not TIOCGWNSIZ */
#ifdef VMS
  TERMINAL tty;
  SYS$QIOW (0, input_chan, IO$_SENSEMODE, &tty, 0, 0,
	    &tty.class, 12, 0, 0, 0, 0);
  *widthp = tty.scr_wid;
  *heightp = tty.scr_len;
#else /* system doesn't know size */
  *widthp = 0;
  *heightp = 0;
#endif /* system does not know size */
#endif /* not TIOCGWINSZ */
}

reset_sys_modes ()
{
  if (noninteractive)
    {
      fflush (stdout);
      return;
    }
  if (!term_initted)
    return;
  if (read_socket_hook)
    return;
  move_cursor (screen_height - 1, 0);
  clear_end_of_line (screen_width);
  /* clear_end_of_line may move the cursor */
  move_cursor (screen_height - 1, 0);
  /* Output raw CR so kernel can track the cursor hpos.  */
  cmputc ('\r');
#ifdef IBMR2AIX
  {
    /* HFT devices normally use ^J as a LF/CR.  We forced it to 
       do the LF only.  Now, we need to reset it. */
    struct termio tty;

    if (ioctl (1, HFTGETID, &tty) != -1)
      write (1, "\033[20h", 5);
  }
#endif

  reset_terminal_modes ();
  fflush (stdout);
#ifdef BSD
#ifndef BSD4_1
  /* Avoid possible loss of output when changing terminal modes.  */
  fsync (fileno (stdout));
#endif
#endif
#ifdef TIOCGLTC
  ioctl (0, TIOCSLTC, &old_ltchars);
#endif /* TIOCGLTC */
#ifndef HAVE_TERMIO
#ifdef TIOCGETC
  ioctl (0, TIOCSETC, &old_tchars);
  ioctl (0, TIOCLSET, &old_lmode);
#endif /* TIOCGETC */
#endif /* not HAVE_TERMIO */
#ifdef F_SETFL
#ifdef F_SETOWN		/* F_SETFL does not imply existance of F_SETOWN */
  if (interrupt_input)
    {
      reset_sigio ();
      fcntl (0, F_SETOWN, old_fcntl_owner);
    }
#endif /* F_SETOWN */
#endif /* F_SETFL */
#ifdef BSD4_1
  if (interrupt_input)
    reset_sigio ();
#endif /* BSD4_1 */
#ifdef VMS
  end_kbd_input ();
  SYS$QIOW (0, input_chan, IO$_SETMODE, &input_iosb, 0, 0,
	    &old_gtty.class, 12, 0, 0, 0, 0);
#else /* not VMS */
  while (ioctl (0, TCSETAW, &old_gtty) < 0 && errno == EINTR);
#endif /* not VMS */

#ifdef AIX
  hft_reset ();
#endif
}

#ifdef HAVE_PTYS

/* Set up the proper status flags for use of a pty.  */

setup_pty (fd)
     int fd;
{
  /* I'm told that TOICREMOTE does not mean control chars
     "can't be sent" but rather that they don't have
     input-editing or signaling effects.
     That should be good, because we have other ways
     to do those things in Emacs.
     However, telnet mode seems not to work on 4.2.
     So TIOCREMOTE is turned off now. */

  /* Under hp-ux, if TIOCREMOTE is turned on, some calls
     will hang.  In particular, the "timeout" feature (which
     causes a read to return if there is no data available)
     does this.  Also it is known that telnet mode will hang
     in such a way that Emacs must be stopped (perhaps this
     is the same problem).
     
     If TIOCREMOTE is turned off, then there is a bug in
     hp-ux which sometimes loses data.  Apparently the
     code which blocks the master process when the internal
     buffer fills up does not work.  Other than this,
     though, everything else seems to work fine.
     
     Since the latter lossage is more benign, we may as well
     lose that way.  -- cph */
#ifdef FIONBIO
#ifdef SYSV_PTYS
  {
    int on = 1;
    ioctl (fd, FIONBIO, &on);
  }
#endif
#endif
#ifdef IBMRTAIX
  /* On AIX, the parent gets SIGHUP when a pty attached child dies.  So, we */
  /* ignore SIGHUP once we've started a child on a pty.  Note that this may */
  /* cause EMACS not to die when it should, i.e., when its own controlling  */
  /* tty goes away.  I've complained to the AIX developers, and they may    */
  /* change this behavior, but I'm not going to hold my breath.             */
  signal (SIGHUP, SIG_IGN);
#endif
}
#endif /* HAVE_PTYS */

#ifdef VMS

/* Assigning an input channel is done at the start of Emacs execution.
   This is called each time Emacs is resumed, also, but does nothing
   because input_chain is no longer zero.  */

init_vms_input()
{
  int status;
  
  if (input_chan == 0)
    {
      status = SYS$ASSIGN (&input_dsc, &input_chan, 0, 0);
      if (! (status & 1))
	LIB$STOP (status);
    }
}

/* Deassigning the input channel is done before exiting.  */

stop_vms_input ()
{
  return SYS$DASSGN (input_chan);
}

short input_buffer;

/* Request reading one character into the keyboard buffer.
   This is done as soon as the buffer becomes empty.  */

queue_kbd_input ()
{
  int status;
  waiting_for_ast = 0;
  stop_input = 0;
  status = SYS$QIO (0, input_chan, IO$_READVBLK,
		    &input_iosb, kbd_input_ast, 1,
		    &input_buffer, 1, 0, terminator_mask, 0, 0);
}

int input_count;

/* Ast routine that is called when keyboard input comes in
   in accord with the SYS$QIO above.  */

kbd_input_ast ()
{
  register int c = -1;
  int old_errno = errno;

  if (waiting_for_ast)
    SYS$SETEF (input_ef);
  waiting_for_ast = 0;
  input_count++;
#ifdef ASTDEBUG
  if (input_count == 25)
    exit (1);
  printf ("Ast # %d,", input_count);
  printf (" iosb = %x, %x, %x, %x",
	  input_iosb.offset, input_iosb.status, input_iosb.termlen,
	  input_iosb.term);
#endif
  if (input_iosb.offset)
    {
      c = input_buffer;
#ifdef ASTDEBUG
      printf (", char = 0%o", c);
#endif
    }
#ifdef ASTDEBUG
  printf ("\n");
  fflush (stdout);
  sleep (1);
#endif
  if (! stop_input)
    queue_kbd_input ();
  if (c >= 0)
    kbd_buffer_store_char (c);

  errno = old_errno;
}

/* Wait until there is something in kbd_buffer.  */

wait_for_kbd_input ()
{
  extern int have_process_input, process_exited;

  /* If already something, avoid doing system calls.  */
  if (detect_input_pending ())
    {
      return;
    }
  /* Clear a flag, and tell ast routine above to set it.  */
  SYS$CLREF (input_ef);
  waiting_for_ast = 1;
  /* Check for timing error: ast happened while we were doing that.  */
  if (!detect_input_pending ())
    {
      /* No timing error: wait for flag to be set.  */
      set_waiting_for_input (0);
      SYS$WFLOR (input_ef, input_eflist);
      clear_waiting_for_input (0);
      if (!detect_input_pending ())
	/* Check for subprocess input availability */
	{
	  int dsp = have_process_input || process_exited;

	  sys$clref (process_ef);
	  if (have_process_input)
	    process_command_input ();
	  if (process_exited)
	    process_exit ();
	  if (dsp)
	    {
	      update_mode_lines++;
	      redisplay_preserve_echo_area ();
	    }
	}
    }
  waiting_for_ast = 0;
}

/* Get rid of any pending QIO, when we are about to suspend
   or when we want to throw away pending input.
   We wait for a positive sign that the AST routine has run
   and therefore there is no I/O request queued when we return.
   SYS$SETAST is used to avoid a timing error.  */

end_kbd_input()
{
#ifdef ASTDEBUG
  printf ("At end_kbd_input.\n");
  fflush (stdout);
  sleep (1);
#endif
  if (LIB$AST_IN_PROG ())  /* Don't wait if suspending from kbd_buffer_store_char! */
    {
      SYS$CANCEL (input_chan);
      return;
    }

  SYS$SETAST (0);
  /* Clear a flag, and tell ast routine above to set it.  */
  SYS$CLREF (input_ef);
  waiting_for_ast = 1;
  stop_input = 1;
  SYS$CANCEL (input_chan);
  SYS$SETAST (1);
  SYS$WAITFR (input_ef);
  waiting_for_ast = 0;
}

/* Wait for either input available or time interval expiry.  */

input_wait_timeout (timeval)
     int timeval;		/* Time to wait, in seconds */
{
  int time [2];
  
  LIB$EMUL (&timeval, &-10000000, &0, time); 	  /* Convert to VMS format */

  /* If already something, avoid doing system calls.  */
  if (detect_input_pending ())
    {
      return;
    }
  /* Clear a flag, and tell ast routine above to set it.  */
  SYS$CLREF (input_ef);
  waiting_for_ast = 1;
  /* Check for timing error: ast happened while we were doing that.  */
  if (!detect_input_pending ())
    {
      /* No timing error: wait for flag to be set.  */
      SYS$CANTIM (1, 0);
      if (SYS$SETIMR (timer_ef, time, 0, 1) & 1) /* Set timer */
	SYS$WFLOR (timer_ef, timer_eflist);	  /* Wait for timer expiry or input */
    }
  waiting_for_ast = 0;
}

/* The standard `sleep' routine works some other way
   and it stops working if you have ever quit out of it.
   This one continues to work.  */

sys_sleep (timeval)
     int timeval;
{
  int time [2];
  
  LIB$EMUL (&timeval, &-10000000, &0, time); 	  /* Convert to VMS format */

  SYS$CANTIM (1, 0);
  if (SYS$SETIMR (timer_ef, time, 0, 1) & 1) /* Set timer */
    SYS$WAITFR (timer_ef);	  /* Wait for timer expiry only */
}

init_sigio ()
{
  request_sigio ();
}

reset_sigio ()
{
  unrequest_sigio ();
}

request_sigio ()
{
  croak ("request sigio");
}

unrequest_sigio ()
{
  croak ("unrequest sigio");
}

#endif /* VMS */

/* Note that VMS compiler won't accept defined (CANNOT_DUMP).  */
#ifndef CANNOT_DUMP
#define NEED_STARTS
#endif

#ifndef SYSTEM_MALLOC
#ifndef NEED_STARTS
#define NEED_STARTS
#endif
#endif

#ifdef NEED_STARTS
/* Some systems that cannot dump also cannot implement these.  */

/*
 *	Return the address of the start of the text segment prior to
 *	doing an unexec().  After unexec() the return value is undefined.
 *	See crt0.c for further explanation and _start().
 *
 */

#ifndef CANNOT_UNEXEC
char *
start_of_text ()
{
#ifdef TEXT_START
  return ((char *) TEXT_START);
#else
#ifdef GOULD
  extern csrt();
  return ((char *) csrt);
#else /* not GOULD */
  extern int _start ();
  return ((char *) _start);
#endif /* GOULD */
#endif /* TEXT_START */
}
#endif /* not CANNOT_UNEXEC */

/*
 *	Return the address of the start of the data segment prior to
 *	doing an unexec().  After unexec() the return value is undefined.
 *	See crt0.c for further information and definition of data_start.
 *
 *	Apparently, on BSD systems this is etext at startup.  On
 *	USG systems (swapping) this is highly mmu dependent and
 *	is also dependent on whether or not the program is running
 *	with shared text.  Generally there is a (possibly large)
 *	gap between end of text and start of data with shared text.
 *
 *	On Uniplus+ systems with shared text, data starts at a
 *	fixed address.  Each port (from a given oem) is generally
 *	different, and the specific value of the start of data can
 *	be obtained via the UniPlus+ specific "uvar(2)" system call,
 *	however the method outlined in crt0.c seems to be more portable.
 *
 *	Probably what will have to happen when a USG unexec is available,
 *	at least on UniPlus, is temacs will have to be made unshared so
 *	that text and data are contiguous.  Then once loadup is complete,
 *	unexec will produce a shared executable where the data can be
 *	at the normal shared text boundry and the startofdata variable
 *	will be patched by unexec to the correct value.
 *
 */
 
char *
start_of_data ()
{
#ifdef DATA_START
  return ((char *) DATA_START);
#else
  extern int data_start;
  return ((char *) &data_start);
#endif
}
#endif /* NEED_STARTS (not CANNOT_DUMP or not SYSTEM_MALLOC) */

#ifndef CANNOT_DUMP
/* Some systems that cannot dump also cannot implement these.  */

/*
 *	Return the address of the end of the text segment prior to
 *	doing an unexec().  After unexec() the return value is undefined.
 */
 
char *
end_of_text ()
{
#ifdef TEXT_END
  return ((char *) TEXT_END);
#else
  extern int etext;
  return ((char *) &etext);
#endif
}
 
/*
 *	Return the address of the end of the data segment prior to
 *	doing an unexec().  After unexec() the return value is undefined.
 */

char *
end_of_data ()
{
#ifdef DATA_END
  return ((char *) DATA_END);
#else
  extern int edata;
  return ((char *) &edata);
#endif
}

#endif /* not CANNOT_DUMP */

/* Get_system_name returns as its value
 a string for the Lisp function system-name to return. */

#ifdef BSD4_1
#include <whoami.h>
#endif

#ifdef USG
/* Can't have this within the function since `static' is #defined to nothing */
static struct utsname get_system_name_name;
#endif

char *
get_system_name ()
{
#ifdef USG
  uname (&get_system_name_name);
  return (get_system_name_name.nodename);
#else /* Not USG */
#ifdef BSD4_1
  return sysname;
#else /* not USG, not 4.1 */
  static char system_name_saved[32];
#ifdef VMS
  char *sp;
  if ((sp = egetenv("SYS$NODE")) == 0)
    sp = "vax-vms";
  else
    {
      char *end;

      if ((end = index (sp, ':')) != 0)
	*end = '\0';
    }
  strcpy (system_name_saved, sp);
#else /* not VMS */
  gethostname (system_name_saved, sizeof (system_name_saved));
#endif /* not VMS */
  return system_name_saved;
#endif /* not USG, not 4.1 */
#endif /* not USG */
}

#ifndef HAVE_SELECT

/* Emulate as much as select as is possible under 4.1 and needed by Gnu Emacs
 * Only checks read descriptors.
 */
/* How long to wait between checking fds in select */
#define SELECT_PAUSE 1
int select_alarmed;

/* For longjmp'ing back to read_input_waiting.  */

jmp_buf read_alarm_throw;

/* Nonzero if the alarm signal should throw back to read_input_waiting.
   The read_socket_hook function sets this to 1 while it is waiting.  */

int read_alarm_should_throw;

select_alarm ()
{
  select_alarmed = 1;
#ifdef BSD4_1
  sigrelse (SIGALRM);
#else /* not BSD4_1 */
  signal (SIGALRM, SIG_IGN);
#endif /* not BSD4_1 */
  if (read_alarm_should_throw)
    longjmp (read_alarm_throw, 1);
}

/* Only rfds are checked.  */
int
select (nfds, rfds, wfds, efds, timeout)
     int nfds;
     int *rfds, *wfds, *efds, *timeout;
{
  int ravail = 0, orfds = 0, old_alarm;
  int timeoutval = timeout ? *timeout : 100000;
  int *local_timeout = &timeoutval;
  extern int kbd_count;
  extern int proc_buffered_char[];
#ifndef subprocesses
  int process_tick = 0, update_tick = 0;
#else
  extern int process_tick, update_tick;
#endif
  int (*old_trap) ();
  char buf;

  if (rfds)
    {
      orfds = *rfds;
      *rfds = 0;
    }
  if (wfds)
    *wfds = 0;
  if (efds)
    *efds = 0;

  /* If we are looking only for the terminal, with no timeout,
     just read it and wait -- that's more efficient.  */
  if (orfds == 1 && (!timeout || *timeout == 100000)
      && process_tick == update_tick)
    {
      if (!kbd_count)
	read_input_waiting ();
      *rfds = 1;
      return 1;
    }

  /* Once a second, till the timer expires, check all the flagged read
   * descriptors to see if any input is available.  If there is some then
   * set the corresponding bit in the return copy of rfds.
   */ 
  while (1)
    {
      register int to_check, bit, fd;

      if (rfds)
	{
	  for (to_check = nfds, bit = 1, fd = 0; --to_check >= 0; bit <<= 1, fd++)
	    {
	      if (orfds & bit)
		{
		  int avail = 0, status = 0;

		  if (bit == 1)
		    avail = detect_input_pending(); /* Special keyboard handler */
		  else
		    {
#ifdef FIONREAD
		      status = ioctl (fd, FIONREAD, &avail);
#else /* no FIONREAD */
		      /* Hoping it will return -1 if nothing available
			 or 0 if all 0 chars requested are read.  */
		      if (proc_buffered_char[fd] >= 0)
			avail = 1;
		      else
			{
			  avail = read (fd, &buf, 1);
			  if (avail > 0)
			    proc_buffered_char[fd] = buf;
			}
#endif /* no FIONREAD */
		    }
		  if (status >= 0 && avail > 0)
		    {
		      (*rfds) |= bit;
		      ravail++;
		    }
		}
	    }
	}
      if (*local_timeout == 0 || ravail != 0 || process_tick != update_tick)
	break;
      old_alarm = alarm (0);
      old_trap = (int (*)()) signal (SIGALRM, select_alarm);
      select_alarmed = 0;
      alarm (SELECT_PAUSE);
      /* Wait for a SIGALRM (or maybe a SIGTINT) */
      while (select_alarmed == 0 && *local_timeout != 0
	     && process_tick == update_tick)
	{
	  /* If we are interested in terminal input,
	     wait by reading the terminal.
	     That makes instant wakeup for terminal input at least.  */
	  if (orfds & 1)
	    {
	      read_input_waiting ();
	      if (kbd_count)
		select_alarmed = 1;
	    }
	  else
	    pause();
	}
      (*local_timeout) -= SELECT_PAUSE;
      /* Reset the old alarm if there was one */
      alarm (0);
      signal (SIGALRM, old_trap);
      if (old_alarm != 0)
	{
	  /* Reset or forge an interrupt for the original handler. */
	  old_alarm -= SELECT_PAUSE;
	  if (old_alarm <= 0)
	    kill (getpid (), SIGALRM); /* Fake an alarm with the orig' handler */
	  else
	    alarm (old_alarm);
	}
      if (*local_timeout == 0)  /* Stop on timer being cleared */
	break;
    }
  return ravail;
}

/* Read keyboard input into the standard buffer,
   waiting for at least one character.  */

/* Make all keyboard buffers much bigger when using X windows.  */
#ifdef HAVE_X_WINDOWS
#define BUFFER_SIZE_FACTOR 16
#else
#define BUFFER_SIZE_FACTOR 1
#endif

read_input_waiting ()
{
  extern int kbd_count;
  extern unsigned char kbd_buffer[];
  extern unsigned char *kbd_ptr;
  int val;

  if (read_socket_hook)
    {
      read_alarm_should_throw = 0;
      if (! setjmp (read_alarm_throw))
	val = (*read_socket_hook) (0, kbd_buffer, 256 * BUFFER_SIZE_FACTOR);
      else
	val = -1;
    }
  else
    val = read (fileno (stdin), kbd_buffer, 1);

  if (val > 0)
    {
      kbd_ptr = kbd_buffer;
      kbd_count = val;
    }
}

#endif /* not HAVE_SELECT */

#ifdef BSD4_1
/* VARARGS */
setpriority ()
{
  return 0;
}

/*
 * Partially emulate 4.2 open call.
 * open is defined as this in 4.1.
 *
 * - added by Michael Bloom @ Citicorp/TTI
 *
 */

int
sys_open (path, oflag, mode)
     char *path;
     int oflag, mode;
{
  if (oflag & O_CREAT) 
    return creat (path, mode);
  else
    return open (path, oflag);
}

init_sigio ()
{
  if (noninteractive)
    return;
  lmode = LINTRUP | lmode;
  ioctl (0, TIOCLSET, &lmode);
}

reset_sigio ()
{
  if (noninteractive)
    return;
  lmode = ~LINTRUP & lmode;
  ioctl (0, TIOCLSET, &lmode);
}

request_sigio ()
{
  sigrelse (SIGTINT);

  interrupts_deferred = 0;
}

unrequest_sigio ()
{
  sighold (SIGTINT);

  interrupts_deferred = 1;
}

/* still inside #ifdef BSD4_1 */
#ifdef subprocesses

int sigheld; /* Mask of held signals */

sigholdx (signum)
     int signum;
{
  sigheld |= sigbit (signum);
  sighold (signum);
}

sigisheld (signum)
     int signum;
{
  sigheld |= sigbit (signum);
}

sigunhold (signum)
     int signum;
{
  sigheld &= ~sigbit (signum);
  sigrelse (signum);
}

sigfree ()    /* Free all held signals */
{
  int i;
  for (i = 0; i < NSIG; i++)
    if (sigheld & sigbit (i))
      sigrelse (i);
  sigheld = 0;
}

sigbit (i)
{
  return 1 << (i - 1);
}
#endif /* subprocesses */
#endif /* BSD4_1 */

#ifndef BSTRING

void
bzero (b, length)
     register char *b;
     register int length;
{
#ifdef VMS
  short zero = 0;
  long max_str = 65535;

  while (length > max_str) {
    (void) LIB$MOVC5 (&zero, &zero, &zero, &max_str, b);
    length -= max_str;
    b += max_str;
  }
  (void) LIB$MOVC5 (&zero, &zero, &zero, &length, b);
#else
  while (length-- > 0)
    *b++ = 0;
#endif /* not VMS */
}

/* Saying `void' requires a declaration, above, where bcopy is used
   and that declaration causes pain for systems where bcopy is a macro.  */
bcopy (b1, b2, length)
     register char *b1;
     register char *b2;
     register int length;
{
#ifdef VMS
  long max_str = 65535;

  while (length > max_str) {
    (void) LIB$MOVC3 (&max_str, b1, b2);
    length -= max_str;
    b1 += max_str;
    b2 += max_str;
  }
  (void) LIB$MOVC3 (&length, b1, b2);
#else
  while (length-- > 0)
    *b2++ = *b1++;
#endif /* not VMS */
}

int
bcmp (b1, b2, length)	/* This could be a macro! */
     register char *b1;
     register char *b2;
     register int length;
{
#ifdef VMS
  struct dsc$descriptor_s src1 = {length, DSC$K_DTYPE_T, DSC$K_CLASS_S, b1};
  struct dsc$descriptor_s src2 = {length, DSC$K_DTYPE_T, DSC$K_CLASS_S, b2};

  return STR$COMPARE (&src1, &src2);
#else
  while (length-- > 0)
    if (*b1++ != *b2++)
      return 1;

  return 0;
#endif /* not VMS */
}
#endif /* not BSTRING */

#ifdef BSD4_1
long random ()
{
  return (rand ());
}

srandom (arg)
     int arg;
{
  srand (arg);
}
#endif BSD4_1

#ifdef HPUX
#ifdef X11
#define HAVE_RANDOM
#endif
#endif

#ifdef USG
#ifndef HAVE_RANDOM
/*
 *	The BSD random(3) returns numbers in the range of
 *	0 to 2e31 - 1.  The USG rand(3C) returns numbers in the
 *	range of 0 to 2e15 - 1.  This is probably not significant
 *	in this usage.
 */
  
long
random ()
{
  /* Arrange to return a range centered on zero.  */
  return rand () - (1 << 14);
}

srandom (arg)
     int arg;
{
  srand (arg);
}

#endif /* HAVE_RANDOM */
#endif /* USG */


#ifdef VMS

#ifdef getenv
/* If any place else asks for the TERM variable,
   allow it to be overridden with the EMACS_TERM variable
   before attempting to translate the logical name TERM.  As a last
   resort, ask for VAX C's special idea of the TERM variable.  */
#undef getenv
char *
sys_getenv (name)
     char *name;
{
  register char *val;
  static char buf[256];
  static struct dsc$descriptor_s equiv
    = {sizeof(buf), DSC$K_DTYPE_T, DSC$K_CLASS_S, buf};
  static struct dsc$descriptor_s d_name
    = {0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0};
  short eqlen;

  if (!strcmp (name, "TERM"))
    {
      val = (char *) getenv ("EMACS_TERM");
      if (val)
	return val;
    }

  d_name.dsc$w_length = strlen (name);
  d_name.dsc$a_pointer = name;
  if (lib$sys_trnlog (&d_name, &eqlen, &equiv) == 1)
    {
      char *str = (char *) xmalloc (eqlen + 1);
      bcopy (buf, str, eqlen);
      str[eqlen] = '\0';
      /* This is a storage leak, but a pain to fix.  With luck,
	 no one will ever notice.  */
      return str;
    }
  return (char *) getenv (name);
}
#endif /* getenv */

#ifdef abort
/* Since VMS doesn't believe in core dumps, the only way to debug this beast is
   to force a call on the debugger from within the image. */
#undef abort
sys_abort ()
{
  reset_sys_modes ();
  LIB$SIGNAL (SS$_DEBUG);
}
#endif /* abort */
#endif /* VMS */

#ifdef VMS
#ifdef LINK_CRTL_SHARE
#ifdef SHAREABLE_LIB_BUG
/* Variables declared noshare and initialized in shareable libraries
   cannot be shared.  The VMS linker incorrectly forces you to use a private
   version which is uninitialized... If not for this "feature", we
   could use the C library definition of sys_nerr and sys_errlist. */
int sys_nerr = 35;
char *sys_errlist[] =
  {
    "error 0",
    "not owner",
    "no such file or directory",
    "no such process",
    "interrupted system call",
    "i/o error",
    "no such device or address",
    "argument list too long",
    "exec format error",
    "bad file number",
    "no child process",
    "no more processes",
    "not enough memory",
    "permission denied",
    "bad address",
    "block device required",
    "mount devices busy",
    "file exists",
    "cross-device link",
    "no such device",
    "not a directory",
    "is a directory",
    "invalid argument",
    "file table overflow",
    "too many open files",
    "not a typewriter",
    "text file busy",
    "file too big",
    "no space left on device",
    "illegal seek",
    "read-only file system",
    "too many links",
    "broken pipe",
    "math argument",
    "result too large",
    "I/O stream empty",
    "vax/vms specific error code nontranslatable error"
  };
#endif /* SHAREABLE_LIB_BUG */
#endif /* LINK_CRTL_SHARE */
#endif /* VMS */

#ifdef INTERRUPTABLE_OPEN

int
/* VARARGS 2 */
sys_open (path, oflag, mode)
     char *path;
     int oflag, mode;
{
  register int rtnval;
  
  while ((rtnval = open (path, oflag, mode)) == -1
	 && (errno == EINTR));
  return (rtnval);
}

#endif /* INTERRUPTABLE_OPEN */

#ifdef INTERRUPTABLE_CLOSE

sys_close (fd)
     int fd;
{
  register int rtnval;

  while ((rtnval = close(fd)) == -1
	 && (errno == EINTR));
  return rtnval;
}

#endif /* INTERRUPTABLE_CLOSE */

#ifdef INTERRUPTABLE_IO

int
sys_read (fildes, buf, nbyte)
     int fildes;
     char *buf;
     unsigned int nbyte;
{
  register int rtnval;
  
  while ((rtnval = read (fildes, buf, nbyte)) == -1
	 && (errno == EINTR));
  return (rtnval);
}

int
sys_write (fildes, buf, nbyte)
     int fildes;
     char *buf;
     unsigned int nbyte;
{
  register int rtnval;

  while ((rtnval = write (fildes, buf, nbyte)) == -1
	 && (errno == EINTR));
  return (rtnval);
}

#endif /* INTERRUPTABLE_IO */

#ifdef USG
/*
 *	All of the following are for USG.
 *
 *	On USG systems the system calls are interruptable by signals
 *	that the user program has elected to catch.  Thus the system call
 *	must be retried in these cases.  To handle this without massive
 *	changes in the source code, we remap the standard system call names
 *	to names for our own functions in sysdep.c that do the system call
 *	with retries.  Actually, for portability reasons, it is good
 *	programming practice, as this example shows, to limit all actual
 *	system calls to a single occurance in the source.  Sure, this
 *	adds an extra level of function call overhead but it is almost
 *	always negligible.   Fred Fish, Unisoft Systems Inc.
 */

char *sys_siglist[NSIG + 1] =
{
#ifdef AIX
/* AIX has changed the signals a bit */
  "bogus signal",			/* 0 */
  "hangup",				/* 1  SIGHUP */
  "interrupt",				/* 2  SIGINT */
  "quit",				/* 3  SIGQUIT */
  "illegal instruction",		/* 4  SIGILL */
  "trace trap",				/* 5  SIGTRAP */
  "IOT instruction",			/* 6  SIGIOT */
  "crash likely",			/* 7  SIGDANGER */
  "floating point exception",		/* 8  SIGFPE */
  "kill",				/* 9  SIGKILL */
  "bus error",				/* 10 SIGBUS */
  "segmentation violation",		/* 11 SIGSEGV */
  "bad argument to system call",	/* 12 SIGSYS */
  "write on a pipe with no one to read it", /* 13 SIGPIPE */
  "alarm clock",			/* 14 SIGALRM */
  "software termination signum",	/* 15 SIGTERM */
  "user defined signal 1",		/* 16 SIGUSR1 */
  "user defined signal 2",		/* 17 SIGUSR2 */
  "death of a child",			/* 18 SIGCLD */
  "power-fail restart",			/* 19 SIGPWR */
  "bogus signal",			/* 20 */
  "bogus signal",			/* 21 */
  "bogus signal",			/* 22 */
  "bogus signal",			/* 23 */
  "bogus signal",			/* 24 */
  "LAN I/O interrupt",			/* 25 SIGAIO */
  "PTY I/O interrupt",			/* 26 SIGPTY */
  "I/O intervention required",		/* 27 SIGIOINT */
  "HFT grant",				/* 28 SIGGRANT */
  "HFT retract",			/* 29 SIGRETRACT */
  "HFT sound done",			/* 30 SIGSOUND */
  "HFT input ready",			/* 31 SIGMSG */
#else /* not AIX */
  "bogus signal",			/* 0 */
  "hangup",				/* 1  SIGHUP */
  "interrupt",				/* 2  SIGINT */
  "quit",				/* 3  SIGQUIT */
  "illegal instruction",		/* 4  SIGILL */
  "trace trap",				/* 5  SIGTRAP */
  "IOT instruction",			/* 6  SIGIOT */
  "EMT instruction",			/* 7  SIGEMT */
  "floating point exception",		/* 8  SIGFPE */
  "kill",				/* 9  SIGKILL */
  "bus error",				/* 10 SIGBUS */
  "segmentation violation",		/* 11 SIGSEGV */
  "bad argument to system call",	/* 12 SIGSYS */
  "write on a pipe with no one to read it", /* 13 SIGPIPE */
  "alarm clock",			/* 14 SIGALRM */
  "software termination signum",	/* 15 SIGTERM */
  "user defined signal 1",		/* 16 SIGUSR1 */
  "user defined signal 2",		/* 17 SIGUSR2 */
  "death of a child",			/* 18 SIGCLD */
  "power-fail restart",			/* 19 SIGPWR */
#endif /* not AIX */
  0
  };

/*
 *	Warning, this function may not duplicate 4.2 action properly
 *	under error conditions.
 */

#ifndef MAXPATHLEN
/* In 4.1, param.h fails to define this.  */
#define MAXPATHLEN 1024
#endif

#ifndef HAVE_GETWD

char *
getwd (pathname)
     char *pathname;
{
  char *npath, *spath;
  extern char *getcwd ();

  spath = npath = getcwd ((char *) 0, MAXPATHLEN);
  /* On Altos 3068, getcwd can return @hostname/dir, so discard
     up to first slash.  Should be harmless on other systems.  */
  while (*npath && *npath != '/')
    npath++;
  strcpy (pathname, npath);
  free (spath);			/* getcwd uses malloc */
  return pathname;
}

#endif HAVE_GETWD

#ifndef HAVE_RENAME

/*
 *	Emulate rename using unlink/link.  Note that this is
 *	only partially correct.  Also, doesn't enforce restriction
 *	that files be of same type (regular->regular, dir->dir, etc).
 */

rename (from, to)
     char *from;
     char *to;
{
  if (access (from, 0) == 0)
    {
      unlink (to);
      if (link (from, to) == 0)
	if (unlink (from) == 0)
	  return (0);
    }
  return (-1);
}
#endif /* not HAVE_RENAME */

/* VARARGS */
setpriority ()
{
  return (0);
}

#ifndef HAVE_VFORK

/*
 *	Substitute fork(2) for vfork(2) on USG flavors.
 */

vfork ()
{
  return (fork ());
}

#endif /* not HAVE_VFORK */

#ifdef MISSING_UTIMES

/* HPUX (among others) sets HAVE_TIMEVAL but does not implement utimes.  */

utimes ()
{
}
#endif

#ifdef IRIS_UTIME

/* The IRIS (3.5) has timevals, but uses sys V utime, and doesn't have the
   utimbuf structure defined anywhere but in the man page. */

struct utimbuf
 {
   long actime;
   long modtime;
 };

utimes (name, tvp)
     char *name;
     struct timeval tvp[];
{
  struct utimbuf utb;
  utb.actime  = tvp[0].tv_sec;
  utb.modtime = tvp[1].tv_sec;
  utime (name, &utb);
}
#endif /* IRIS_UTIME */


#if 0
#ifdef HPUX

/* HPUX curses library references perror, but as far as we know
   it won't be called.  Anyway this definition will do for now.  */

perror ()
{
}

#endif /* HPUX */
#endif /* 0 */

#ifndef HAVE_DUP2

/*
 *	Emulate BSD dup2(2).  First close newd if it already exists.
 *	Then, attempt to dup oldd.  If not successful, call dup2 recursively
 *	until we are, then close the unsuccessful ones.
 */

dup2 (oldd, newd)
     int oldd;
     int newd;
{
  register int fd;
  
  sys_close (newd);

#ifdef F_DUPFD
  fd = fcntl (oldd, F_DUPFD, newd);
  if (fd != newd)
    error ("cant dup2(%i,%i) : %s", oldd, newd, sys_errlist[errno]);
#else
  while ((fd = dup (oldd)) != newd)
    {
      dup2 (oldd, newd);
      sys_close (fd);
    }
#endif
}

#endif /* not HAVE_DUP2 */

/*
 *	Gettimeofday.  Simulate as much as possible.  Only accurate
 *	to nearest second.  Emacs doesn't use tzp so ignore it for now.
 *	Only needed when subprocesses are defined.
 */

#ifdef subprocesses
#ifndef HAVE_GETTIMEOFDAY
#ifdef HAVE_TIMEVAL
 
/* ARGSUSED */
gettimeofday (tp, tzp)
     struct timeval *tp;
     struct timezone *tzp;
{
  extern long time ();

  tp->tv_sec = time ((long *)0);    
  tp->tv_usec = 0;
}
 
#endif
#endif
#endif /* subprocess && !HAVE_GETTIMEOFDAY && HAVE_TIMEVAL */
  
/*
 *	This function will go away as soon as all the stubs fixed. (fnf)
 */

croak (badfunc)
     char *badfunc;
{
  printf ("%s not yet implemented\r\n", badfunc);
  reset_sys_modes ();
  exit (1);
}

#endif /* USG */

/* Directory routines for systems that don't have them. */

#ifdef SYSV_SYSTEM_DIR

#include <dirent.h>

#ifndef HAVE_CLOSEDIR
int
closedir (dirp)
     register DIR *dirp;              /* stream from opendir() */
{
  sys_close (dirp->dd_fd);
  free ((char *) dirp->dd_buf);       /* directory block defined in <dirent.h> */
  free ((char *) dirp);
}
#endif /* not HAVE_CLOSEDIR */

#endif /* SYSV_SYSTEM_DIR */

#ifdef NONSYSTEM_DIR_LIBRARY

DIR *
opendir (filename)
     char *filename;	/* name of directory */
{
  register DIR *dirp;		/* -> malloc'ed storage */
  register int fd;		/* file descriptor for read */
  struct stat sbuf;		/* result of fstat() */

  fd = sys_open (filename, 0);
  if (fd < 0)
    return 0;

  if (fstat (fd, &sbuf) < 0
      || (sbuf.st_mode & S_IFMT) != S_IFDIR
      || (dirp = (DIR *) malloc (sizeof (DIR))) == 0)
    {
      sys_close (fd);
      return 0;		/* bad luck today */
    }

  dirp->dd_fd = fd;
  dirp->dd_loc = dirp->dd_size = 0;	/* refill needed */

  return dirp;
}

void
closedir (dirp)
     register DIR *dirp;		/* stream from opendir() */
{
  sys_close (dirp->dd_fd);
  free ((char *) dirp);
}


#ifndef VMS
#define DIRSIZ	14
struct olddir
  {
    ino_t od_ino; 		/* inode */
    char od_name[DIRSIZ];	/* filename */
  };
#endif /* not VMS */

struct direct dir_static;	/* simulated directory contents */

/* ARGUSED */
struct direct *
readdir (dirp)
     register DIR *dirp;	/* stream from opendir() */
{
#ifndef VMS
  register struct olddir *dp;	/* -> directory data */
#else /* VMS */
  register struct dir$_name *dp; /* -> directory data */
  register struct dir$_version *dv; /* -> version data */
#endif /* VMS */

  for (; ;)
    {
      if (dirp->dd_loc >= dirp->dd_size)
	dirp->dd_loc = dirp->dd_size = 0;

      if (dirp->dd_size == 0 	/* refill buffer */
	  && (dirp->dd_size = sys_read (dirp->dd_fd, dirp->dd_buf, DIRBLKSIZ)) <= 0)
	return 0;

#ifndef VMS
      dp = (struct olddir *) &dirp->dd_buf[dirp->dd_loc];
      dirp->dd_loc += sizeof (struct olddir);

      if (dp->od_ino != 0)	/* not deleted entry */
	{
	  dir_static.d_ino = dp->od_ino;
	  strncpy (dir_static.d_name, dp->od_name, DIRSIZ);
	  dir_static.d_name[DIRSIZ] = '\0';
	  dir_static.d_namlen = strlen (dir_static.d_name);
	  dir_static.d_reclen = sizeof (struct direct)
	    - MAXNAMLEN + 3
	      + dir_static.d_namlen - dir_static.d_namlen % 4;
	  return &dir_static;	/* -> simulated structure */
	}
#else /* VMS */
      dp = (struct dir$_name *) dirp->dd_buf;
      if (dirp->dd_loc == 0)
	dirp->dd_loc = (dp->dir$b_namecount&1) ? dp->dir$b_namecount + 1
	  : dp->dir$b_namecount;
      dv = (struct dir$_version *)&dp->dir$t_name[dirp->dd_loc];
      dir_static.d_ino = dv->dir$w_fid_num;
      dir_static.d_namlen = dp->dir$b_namecount;
      dir_static.d_reclen = sizeof (struct direct)
	- MAXNAMLEN + 3
	  + dir_static.d_namlen - dir_static.d_namlen % 4;
      strncpy (dir_static.d_name, dp->dir$t_name, dp->dir$b_namecount);
      dir_static.d_name[dir_static.d_namlen] = '\0';
      dirp->dd_loc = dirp->dd_size; /* only one record at a time */
      return &dir_static;
#endif /* VMS */
    }
}

#ifdef VMS
/* readdirver is just like readdir except it returns all versions of a file
   as separate entries.  */

/* ARGUSED */
struct direct *
readdirver (dirp)
     register DIR *dirp;	/* stream from opendir() */
{
  register struct dir$_name *dp; /* -> directory data */
  register struct dir$_version *dv; /* -> version data */

  if (dirp->dd_loc >= dirp->dd_size - sizeof (struct dir$_name))
    dirp->dd_loc = dirp->dd_size = 0;

  if (dirp->dd_size == 0 	/* refill buffer */
      && (dirp->dd_size = sys_read (dirp->dd_fd, dirp->dd_buf, DIRBLKSIZ)) <= 0)
    return 0;

  dp = (struct dir$_name *) dirp->dd_buf;
  if (dirp->dd_loc == 0)
    dirp->dd_loc = (dp->dir$b_namecount & 1) ? dp->dir$b_namecount + 1
		   : dp->dir$b_namecount;
  dv = (struct dir$_version *) &dp->dir$t_name[dirp->dd_loc];
  strncpy (dir_static.d_name, dp->dir$t_name, dp->dir$b_namecount);
  sprintf (&dir_static.d_name[dp->dir$b_namecount], ";%d", dv->dir$w_version);
  dir_static.d_namlen = strlen (dir_static.d_name);
  dir_static.d_ino = dv->dir$w_fid_num;
  dir_static.d_reclen = sizeof (struct direct) - MAXNAMLEN + 3
			+ dir_static.d_namlen - dir_static.d_namlen % 4;
  dirp->dd_loc = ((char *) (++dv) - dp->dir$t_name);
  return &dir_static;
}

#endif /* VMS */

#endif /* NONSYSTEM_DIR_LIBRARY */

/* Functions for VMS */
#ifdef VMS
#include <pwd.h>
#include <acldef.h>
#include <chpdef.h>
#include <jpidef.h>

/* Return as a string the VMS error string pertaining to STATUS.
   Reuses the same static buffer each time it is called.  */

char *
vmserrstr (status)
     int status;		/* VMS status code */
{
  int bufadr[2];
  short len;
  static char buf[257];

  bufadr[0] = sizeof buf - 1;
  bufadr[1] = buf;
  if (! (SYS$GETMSG (status, &len, bufadr, 0x1, 0) & 1))
    return "untranslatable VMS error status";
  buf[len] = '\0';
  return buf;
}

#ifdef access
#undef access
  
/* The following is necessary because 'access' emulation by VMS C (2.0) does
 * not work correctly.  (It also doesn't work well in version 2.3.)
 */

#ifdef VMS4_4

#define DESCRIPTOR(name,string) struct dsc$descriptor_s name = \
	{ strlen(string), DSC$K_DTYPE_T, DSC$K_CLASS_S, string }

typedef union {
    struct {
	unsigned short s_buflen;
	unsigned short s_code;
	char *s_bufadr;
	unsigned short *s_retlenadr;
    } s;
    int end;
} item;
#define buflen s.s_buflen
#define code s.s_code
#define bufadr s.s_bufadr
#define retlenadr s.s_retlenadr

#define R_OK 4	/* test for read permission */
#define W_OK 2	/* test for write permission */
#define X_OK 1	/* test for execute (search) permission */
#define F_OK 0	/* test for presence of file */

int
sys_access (path, mode)
     char *path;
     int mode;
{
  static char *user = NULL;
  char dir_fn[512];

  /* translate possible directory spec into .DIR file name, so brain-dead
   * access() can treat the directory like a file.  */
  if (directory_file_name (path, dir_fn))
    path = dir_fn;
  
  if (mode == F_OK)
    return access (path, mode);
  if (user == NULL && (user = getenv ("USER")) == NULL)
    return -1;
  {
	int stat;
	int flags;
	int acces;
	int dummy;
	item itemlst[3];
	DESCRIPTOR(path_desc, path);
	DESCRIPTOR(user_desc, user);
 
	flags = 0;
	acces = 0;
	if ((mode & X_OK) && ((stat = access(path, mode)) < 0 || mode == X_OK))
	    return stat;
	if (mode & R_OK)
	    acces |= CHP$M_READ;
	if (mode & W_OK)
	    acces |= CHP$M_WRITE;
	itemlst[0].buflen = sizeof (int);
	itemlst[0].code = CHP$_FLAGS;
	itemlst[0].bufadr = &flags;
	itemlst[0].retlenadr = &dummy;
	itemlst[1].buflen = sizeof (int);
	itemlst[1].code = CHP$_ACCESS;
	itemlst[1].bufadr = &acces;
	itemlst[1].retlenadr = &dummy;
	itemlst[2].end = CHP$_END;
	stat = SYS$CHECK_ACCESS(&ACL$C_FILE, &path_desc, &user_desc, itemlst);
	return stat == SS$_NORMAL ? 0 : -1;
    }
}

#else /* not VMS4_4 */

#include <prvdef.h>
#define	ACE$M_WRITE	2
#define	ACE$C_KEYID	1

static unsigned short memid, grpid;
static unsigned int uic;

/* Called from init_sys_modes, so it happens not very often
   but at least each time Emacs is loaded.  */
sys_access_reinit ()
{
  uic = 0;
}

int
sys_access (filename, type)
     char * filename;
     int type;
{
  struct FAB fab;
  struct XABPRO xab;
  int status, prvmask[2], size, i, typecode, acl_controlled;
  unsigned int *aclptr, *aclend, aclbuf[60];

  /* Get UIC and GRP values for protection checking.  */
  if (uic == 0)
    {
      status = LIB$GETJPI (&JPI$_UIC, 0, 0, &uic, 0, 0);
      if (! (status & 1))
	return -1;
      memid = uic & 0xFFFF;
      grpid = uic >> 16;
    }

  if (type != 2)		/* not checking write access */
    return access (filename, type);

  /* Check write protection. */
    
#define	CHECKPRIV(bit)    (prvmask[bit / 32] & (1 << (bit % 32)))
#define	WRITEABLE(field)  (! ((xab.xab$w_pro >> field) & XAB$M_NOWRITE))

  /* Find privilege bits */
  status = sys$setprv (0, 0, 0, prvmask);
  if (! (status & 1))
    error ("Unable to find privileges: %s", vmserrstr (status));
  if (CHECKPRIV (PRV$V_BYPASS))
    return 0;			/* BYPASS enabled */
  fab = cc$rms_fab;
  fab.fab$b_fac = FAB$M_GET;
  fab.fab$l_fna = filename;
  fab.fab$b_fns = strlen (filename);
  fab.fab$l_xab = &xab;
  xab = cc$rms_xabpro;
  xab.xab$l_aclbuf = aclbuf;
  xab.xab$w_aclsiz = sizeof (aclbuf);
  status = sys$open (&fab, 0, 0);
  if (! (status & 1))
    return -1;
  sys$close (&fab, 0, 0);
  /* Check system access */
  if (CHECKPRIV (PRV$V_SYSPRV) && WRITEABLE (XAB$V_SYS))
    return 0;
  /* Check ACL entries, if any */
  acl_controlled = 0;
  if (xab.xab$w_acllen > 0)
    {
      aclptr = aclbuf;
      aclend = &aclbuf[xab.xab$w_acllen / 4];
      while (*aclptr && aclptr < aclend)
	{
	  size = (*aclptr & 0xff) / 4;
	  typecode = (*aclptr >> 8) & 0xff;
	  if (typecode == ACE$C_KEYID)
	    for (i = size - 1; i > 1; i--)
	      if (aclptr[i] == uic)
		{
		  acl_controlled = 1;
		  if (aclptr[1] & ACE$M_WRITE)
		    return 0;	/* Write access through ACL */
		}
	  aclptr = &aclptr[size];
	}
      if (acl_controlled)	/* ACL specified, prohibits write access */
	return -1;
    }
  /* No ACL entries specified, check normal protection */
  if (WRITEABLE (XAB$V_WLD))	/* World writeable */
    return 0;
  if (WRITEABLE (XAB$V_GRP) &&
      (unsigned short) (xab.xab$l_uic >> 16) == grpid)
    return 0;			/* Group writeable */
  if (WRITEABLE (XAB$V_OWN) &&
      (xab.xab$l_uic & 0xFFFF) == memid)
    return 0;			/* Owner writeable */

  return -1;	/* Not writeable */
}
#endif /* not VMS4_4 */
#endif /* access */
  
static char vtbuf[NAM$C_MAXRSS+1];

/* translate a vms file spec to a unix path */
char *
sys_translate_vms (vfile)
     char * vfile;
{
  char * p;
  char * targ;

  if (!vfile)
    return 0;

  targ = vtbuf;

  /* leading device or logical name is a root directory */
  if (p = strchr (vfile, ':'))
    {
      *targ++ = '/';
      while (vfile < p)
	*targ++ = *vfile++;
      vfile++;
      *targ++ = '/';
    }
  p = vfile;
  if (*p == '[' || *p == '<')
    {
      while (*++vfile != *p + 2)
	switch (*vfile)
	  {
	  case '.':
	    if (vfile[-1] == *p)
	      *targ++ = '.';
	    *targ++ = '/';
	    break;

	  case '-':
	    *targ++ = '.';
	    *targ++ = '.';
	    break;
	    
	  default:
	    *targ++ = *vfile;
	    break;
	  }
      vfile++;
      *targ++ = '/';
    }
  while (*vfile)
    *targ++ = *vfile++;

  return vtbuf;
}

static char utbuf[NAM$C_MAXRSS+1];

/* translate a unix path to a VMS file spec */
char *
sys_translate_unix (ufile)
     char * ufile;
{
  int slash_seen = 0;
  char *p;
  char * targ;

  if (!ufile)
    return 0;

  targ = utbuf;

  if (*ufile == '/')
    {
      ufile++;
    }

  while (*ufile)
    {
      switch (*ufile)
	{
	case '/':
	  if (slash_seen)
	    if (index (&ufile[1], '/'))
	      *targ++ = '.';
	    else
	      *targ++ = ']';
	  else
	    {
	      *targ++ = ':';
	      if (index (&ufile[1], '/'))
		*targ++ = '[';
	      slash_seen = 1;
	    }
	  break;

	case '.':
	  if (strncmp (ufile, "./", 2) == 0)
	    {
	      if (!slash_seen)
		{
		  *targ++ = '[';
		  slash_seen = 1;
		}
	      ufile++;		/* skip the dot */
	      if (index (&ufile[1], '/'))
		*targ++ = '.';
	      else
		*targ++ = ']';
	    }
	  else if (strncmp (ufile, "../", 3) == 0)
	    {
	      if (!slash_seen)
		{
		  *targ++ = '[';
		  slash_seen = 1;
		}
	      *targ++ = '-';
	      ufile += 2;	/* skip the dots */
	      if (index (&ufile[1], '/'))
		*targ++ = '.';
	      else
		*targ++ = ']';
	    }
	  else
	    *targ++ = *ufile;
	  break;

	default:
	  *targ++ = *ufile;
	  break;
	}
      ufile++;
    }
  *targ = '\0';
  
  return utbuf;
}

char *
getwd (pathname)
     char *pathname;
{
  char *ptr;
  strcpy (pathname, egetenv ("PATH"));

  ptr = pathname;
  while (*ptr)
    {
      if ('a' <= *ptr && *ptr <= 'z')
	*ptr -= 040;
      ptr++;
    }
  return pathname;
}

getppid ()
{
  long item_code = JPI$_OWNER;
  unsigned long parent_id;
  int status;

  if (((status = LIB$GETJPI (&item_code, 0, 0, &parent_id)) & 1) == 0)
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return -1;
    }
  return parent_id;
}

#undef getuid
unsigned
sys_getuid ()
{
  return (getgid () << 16) | getuid ();
}

int
sys_read (fildes, buf, nbyte)
     int fildes;
     char *buf;
     unsigned int nbyte;
{
  return read (fildes, buf, (nbyte < MAXIOSIZE ? nbyte : MAXIOSIZE));
}

#if 0
int
sys_write (fildes, buf, nbyte)
     int fildes;
     char *buf;
     unsigned int nbyte;
{
  register int nwrote, rtnval = 0;

  while (nbyte > MAXIOSIZE && (nwrote = write (fildes, buf, MAXIOSIZE)) > 0) {
    nbyte -= nwrote;
    buf += nwrote;
    rtnval += nwrote;
  }
  if (nwrote < 0)
    return rtnval ? rtnval : -1;
  if ((nwrote = write (fildes, buf, nbyte)) < 0)
    return rtnval ? rtnval : -1;
  return (rtnval + nwrote);
}
#endif /* 0 */

/*
 *	VAX/VMS VAX C RTL really loses. It insists that records
 *      end with a newline (carriage return) character, and if they
 *	don't it adds one (nice of it isn't it!)
 *
 *	Thus we do this stupidity below.
 */

int
sys_write (fildes, buf, nbytes)
     int fildes;
     char *buf;
     unsigned int nbytes;
{
  register char *p;
  register char *e;
  int retval, sum;
  p = buf;
  sum = 0;
  while (nbytes > 0)
    {
      e =  p + min (MAXIOSIZE, nbytes) - 1;
      while (*e != '\n' && e > p) e--;
      if (p == e)		/* Ok.. so here we add a newline... sigh. */
	e = p + min (MAXIOSIZE, nbytes) - 1;
      retval = write (fildes, p, e - p + 1);
      if (retval != e - p + 1) return -1;
      p = e + 1;
      sum = sum + retval;
      nbytes -= retval;
    }
  return sum;
}

/* Create file NEW copying its attributes from file OLD.  If
   OLD is 0 or does not exist, create based on the value of
   vms_stmlf_recfm. */

int
creat_copy_attrs (old, new)
     char *old, *new;
{
  struct FAB fab = cc$rms_fab;
  struct XABPRO xabpro;
  char aclbuf[256];	/* Choice of size is arbitrary.  See below. */
  extern int vms_stmlf_recfm;

  if (old)
    {
      fab.fab$b_fac = FAB$M_GET;
      fab.fab$l_fna = old;
      fab.fab$b_fns = strlen (old);
      fab.fab$l_xab = &xabpro;
      xabpro = cc$rms_xabpro;
      xabpro.xab$l_aclbuf = aclbuf;
      xabpro.xab$w_aclsiz = sizeof aclbuf;
      /* Call $OPEN to fill in the fab & xabpro fields. */
      if (sys$open (&fab, 0, 0) & 1)
	{
	  sys$close (&fab, 0, 0);
	  fab.fab$l_alq = 0;	/* zero the allocation quantity */
	  if (xabpro.xab$w_acllen > 0)
	    {
	      if (xabpro.xab$w_acllen > sizeof aclbuf)
		/* If the acl buffer was too short, redo open with longer one.
		   Wouldn't need to do this if there were some system imposed
		   limit on the size of an ACL, but I can't find any such. */
		{
		  xabpro.xab$l_aclbuf = alloca (xabpro.xab$w_acllen);
		  xabpro.xab$w_aclsiz = xabpro.xab$w_acllen;
		  if (sys$open (&fab, 0, 0) & 1)
		    sys$close (&fab, 0, 0);
		  else
		    old = 0;
		}
	    }
	  else
	    xabpro.xab$l_aclbuf = 0;
	}
      else
	old = 0;
    }
  fab.fab$l_fna = new;
  fab.fab$b_fns = strlen (new);
  if (!old)
    {
      fab.fab$l_xab = 0;
      fab.fab$b_rfm = vms_stmlf_recfm ? FAB$C_STMLF : FAB$C_VAR;
      fab.fab$b_rat = FAB$M_CR;
    }
  /* Create the new file with either default attrs or attrs copied
     from old file. */
  if (!(SYS$CREATE (&fab, 0, 0) & 1))
    return -1;
  sys$close (&fab, 0, 0);
  /* As this is a "replacement" for creat, return a file descriptor
     opened for writing. */
  return open (new, O_WRONLY);
}

#ifdef creat
#undef creat
#include <varargs.h>

sys_creat (va_alist)
     va_dcl
{
  va_list list_incrementor;
  char *name;
  int mode;
  int rfd;			/* related file descriptor */
  int fd;			/* Our new file descriptor */
  int count;
  struct stat st_buf;
  char rfm[12];
  char rat[15];
  char mrs[13];
  char fsz[13];
  extern int vms_stmlf_recfm;

  va_count (count);
  va_start (list_incrementor);
  name = va_arg (list_incrementor, char *);
  mode = va_arg (list_incrementor, int);
  if (count > 2)
    rfd = va_arg (list_incrementor, int);
  va_end (list_incrementor);
  if (count > 2)
    {
      /* Use information from the related file descriptor to set record
	 format of the newly created file. */
      fstat (rfd, &st_buf);
      switch (st_buf.st_fab_rfm)
	{
	case FAB$C_FIX:
	  strcpy (rfm, "rfm = fix");
	  sprintf (mrs, "mrs = %d", st_buf.st_fab_mrs);
	  strcpy (rat, "rat = ");
	  if (st_buf.st_fab_rat & FAB$M_CR)
	    strcat (rat, "cr");
	  else if (st_buf.st_fab_rat & FAB$M_FTN)
	    strcat (rat, "ftn");
	  else if (st_buf.st_fab_rat & FAB$M_PRN)
	    strcat (rat, "prn");
	  if (st_buf.st_fab_rat & FAB$M_BLK)
	    if (st_buf.st_fab_rat & (FAB$M_CR|FAB$M_FTN|FAB$M_PRN))
	      strcat (rat, ", blk");
	    else
	      strcat (rat, "blk");
	  return creat (name, 0, rfm, rat, mrs);

	case FAB$C_VFC:
	  strcpy (rfm, "rfm = vfc");
	  sprintf (fsz, "fsz = %d", st_buf.st_fab_fsz);
	  strcpy (rat, "rat = ");
	  if (st_buf.st_fab_rat & FAB$M_CR)
	    strcat (rat, "cr");
	  else if (st_buf.st_fab_rat & FAB$M_FTN)
	    strcat (rat, "ftn");
	  else if (st_buf.st_fab_rat & FAB$M_PRN)
	    strcat (rat, "prn");
	  if (st_buf.st_fab_rat & FAB$M_BLK)
	    if (st_buf.st_fab_rat & (FAB$M_CR|FAB$M_FTN|FAB$M_PRN))
	      strcat (rat, ", blk");
	    else
	      strcat (rat, "blk");
	  return creat (name, 0, rfm, rat, fsz);

	case FAB$C_STM:
	  strcpy (rfm, "rfm = stm");
	  break;

	case FAB$C_STMCR:
	  strcpy (rfm, "rfm = stmcr");
	  break;

	case FAB$C_STMLF:
	  strcpy (rfm, "rfm = stmlf");
	  break;

	case FAB$C_UDF:
	  strcpy (rfm, "rfm = udf");
	  break;

	case FAB$C_VAR:
	  strcpy (rfm, "rfm = var");
	  break;
	}
      strcpy (rat, "rat = ");
      if (st_buf.st_fab_rat & FAB$M_CR)
	strcat (rat, "cr");
      else if (st_buf.st_fab_rat & FAB$M_FTN)
	strcat (rat, "ftn");
      else if (st_buf.st_fab_rat & FAB$M_PRN)
	strcat (rat, "prn");
      if (st_buf.st_fab_rat & FAB$M_BLK)
	if (st_buf.st_fab_rat & (FAB$M_CR|FAB$M_FTN|FAB$M_PRN))
	  strcat (rat, ", blk");
	else
	  strcat (rat, "blk");
    }
  else
    {
      strcpy (rfm, vms_stmlf_recfm ? "rfm = stmlf" : "rfm=var");
      strcpy (rat, "rat=cr");
    }
  /* Until the VAX C RTL fixes the many bugs with modes, always use
     mode 0 to get the user's default protection. */
  fd = creat (name, 0, rfm, rat);
  if (fd < 0 && errno == EEXIST)
    {
      if (unlink (name) < 0)
	report_file_error ("delete", build_string (name));
      fd = creat (name, 0, rfm, rat);
    }
  return fd;
}
#endif /* creat */

/* fwrite to stdout is S L O W.  Speed it up by using fputc...*/
sys_fwrite (ptr, size, num, fp)
     register char * ptr;
     FILE * fp;
{
  register int tot = num * size;

  while (tot--)
    fputc (*ptr++, fp);
}

/*
 * The VMS C library routine creat() actually creates a new version of an
 * existing file rather than truncating the old version.  There are times
 * when this is not the desired behavior, for instance, when writing an
 * auto save file (you only want one version), or when you don't have
 * write permission in the directory containing the file (but the file
 * itself is writable).  Hence this routine, which is equivalent to 
 * "close (creat (fn, 0));" on Unix if fn already exists.
 */
int
vms_truncate (fn)
     char *fn;
{
  struct FAB xfab = cc$rms_fab;
  struct RAB xrab = cc$rms_rab;
  int status;

  xfab.fab$l_fop = FAB$M_TEF;	/* free allocated but unused blocks on close */
  xfab.fab$b_fac = FAB$M_TRN | FAB$M_GET; /* allow truncate and get access */
  xfab.fab$b_shr = FAB$M_NIL;	/* allow no sharing - file must be locked */
  xfab.fab$l_fna = fn;
  xfab.fab$b_fns = strlen (fn);
  xfab.fab$l_dna = ";0";	/* default to latest version of the file */
  xfab.fab$b_dns = 2;
  xrab.rab$l_fab = &xfab;

  /* This gibberish opens the file, positions to the first record, and
     deletes all records from there until the end of file. */
  if ((sys$open (&xfab) & 01) == 01)
    {
      if ((sys$connect (&xrab) & 01) == 01 &&
	  (sys$find (&xrab) & 01) == 01 &&
	  (sys$truncate (&xrab) & 01) == 01)
	status = 0;
      else
	status = -1;
    }
  else
    status = -1;
  sys$close (&xfab);
  return status;
}

/* Define this symbol to actually read SYSUAF.DAT.  This requires either
   SYSPRV or a readable SYSUAF.DAT. */

#ifdef READ_SYSUAF
/*
 * getuaf.c
 *
 * Routine to read the VMS User Authorization File and return
 * a specific user's record.
 */

static struct UAF retuaf;

struct UAF *
get_uaf_name(uname)
     char * uname;
{
  register status;
  struct FAB uaf_fab;
  struct RAB uaf_rab;
  
  uaf_fab = cc$rms_fab;
  uaf_rab = cc$rms_rab;
  /* initialize fab fields */
  uaf_fab.fab$l_fna = "SYS$SYSTEM:SYSUAF.DAT";
  uaf_fab.fab$b_fns = 21;
  uaf_fab.fab$b_fac = FAB$M_GET;
  uaf_fab.fab$b_org = FAB$C_IDX;
  uaf_fab.fab$b_shr = FAB$M_GET|FAB$M_PUT|FAB$M_UPD|FAB$M_DEL;
  /* initialize rab fields */
  uaf_rab.rab$l_fab = &uaf_fab;
  /* open the User Authorization File */
  status = sys$open(&uaf_fab);
  if (!(status&1))
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return 0;
    }
  status = sys$connect(&uaf_rab);
  if (!(status&1))
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return 0;
    }
  /* read the requested record - index is in uname */
  uaf_rab.rab$l_kbf = uname;
  uaf_rab.rab$b_ksz = strlen (uname);
  uaf_rab.rab$b_rac = RAB$C_KEY;
  uaf_rab.rab$l_ubf = (char *)&retuaf;
  uaf_rab.rab$w_usz = sizeof retuaf;
  status = sys$get(&uaf_rab);
  if (!(status&1))
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return 0;
    }
  /* close the User Authorization File */
  status = sys$disconnect(&uaf_rab);
  if (!(status&1))
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return 0;
    }
  status = sys$close(&uaf_fab);
  if (!(status&1))
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return 0;
    }
  return &retuaf;
}

struct UAF *
get_uaf_uic(uic)
     unsigned long uic;
{
  register status;
  struct FAB uaf_fab;
  struct RAB uaf_rab;
  
  uaf_fab = cc$rms_fab;
  uaf_rab = cc$rms_rab;
  /* initialize fab fields */
  uaf_fab.fab$l_fna = "SYS$SYSTEM:SYSUAF.DAT";
  uaf_fab.fab$b_fns = 21;
  uaf_fab.fab$b_fac = FAB$M_GET;
  uaf_fab.fab$b_org = FAB$C_IDX;
  uaf_fab.fab$b_shr = FAB$M_GET|FAB$M_PUT|FAB$M_UPD|FAB$M_DEL;
  /* initialize rab fields */
  uaf_rab.rab$l_fab = &uaf_fab;
  /* open the User Authorization File */
  status = sys$open(&uaf_fab);
  if (!(status&1))
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return 0;
    }
  status = sys$connect(&uaf_rab);
  if (!(status&1))
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return 0;
    }
  /* read the requested record - index is in uic */
  uaf_rab.rab$b_krf = 1;	/* 1st alternate key */
  uaf_rab.rab$l_kbf = (char *) &uic;
  uaf_rab.rab$b_ksz = sizeof uic;
  uaf_rab.rab$b_rac = RAB$C_KEY;
  uaf_rab.rab$l_ubf = (char *)&retuaf;
  uaf_rab.rab$w_usz = sizeof retuaf;
  status = sys$get(&uaf_rab);
  if (!(status&1))
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return 0;
    }
  /* close the User Authorization File */
  status = sys$disconnect(&uaf_rab);
  if (!(status&1))
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return 0;
    }
  status = sys$close(&uaf_fab);
  if (!(status&1))
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return 0;
    }
  return &retuaf;
}

static struct passwd retpw;

struct passwd *
cnv_uaf_pw (up)
     struct UAF * up;
{
  char * ptr;

  /* copy these out first because if the username is 32 chars, the next
     section will overwrite the first byte of the UIC */
  retpw.pw_uid = up->uaf$w_mem;
  retpw.pw_gid = up->uaf$w_grp;

  /* I suppose this is not the best sytle, to possibly overwrite one
     byte beyond the end of the field, but what the heck... */
  ptr = &up->uaf$t_username[UAF$S_USERNAME];
  while (ptr[-1] == ' ')
    ptr--;
  *ptr = '\0';
  strcpy (retpw.pw_name, up->uaf$t_username);

  /* the rest of these are counted ascii strings */
  strncpy (retpw.pw_gecos, &up->uaf$t_owner[1], up->uaf$t_owner[0]);
  retpw.pw_gecos[up->uaf$t_owner[0]] = '\0';
  strncpy (retpw.pw_dir, &up->uaf$t_defdev[1], up->uaf$t_defdev[0]);
  retpw.pw_dir[up->uaf$t_defdev[0]] = '\0';
  strncat (retpw.pw_dir, &up->uaf$t_defdir[1], up->uaf$t_defdir[0]);
  retpw.pw_dir[up->uaf$t_defdev[0] + up->uaf$t_defdir[0]] = '\0';
  strncpy (retpw.pw_shell, &up->uaf$t_defcli[1], up->uaf$t_defcli[0]);
  retpw.pw_shell[up->uaf$t_defcli[0]] = '\0';

  return &retpw;
}
#else /* not READ_SYSUAF */
static struct passwd retpw;
#endif /* not READ_SYSUAF */

struct passwd *
getpwnam (name)
     char * name;
{
#ifdef READ_SYSUAF
  struct UAF *up;
#else
  char * user;
  char * dir;
  char * full;
#endif /* READ_SYSUAF */
  char *ptr = name;

  while (*ptr)
    {
      if ('a' <= *ptr && *ptr <= 'z')
	*ptr -= 040;
      ptr++;
    }
#ifdef READ_SYSUAF
  if (!(up = get_uaf_name (name)))
    return 0;
  return cnv_uaf_pw (up);
#else
  if (strcmp (name, getenv ("USER")) == 0)
    {
      retpw.pw_uid = getuid ();
      retpw.pw_gid = getgid ();
      strcpy (retpw.pw_name, name);
      if (full = egetenv ("FULLNAME"))
	strcpy (retpw.pw_gecos, full);
      else
	*retpw.pw_gecos = '\0';
      strcpy (retpw.pw_dir, egetenv ("HOME"));
      *retpw.pw_shell = '\0';
      return &retpw;
    }
  else
    return 0;
#endif /* not READ_SYSUAF */
}

struct passwd *
getpwuid (uid)
     unsigned long uid;
{
#ifdef READ_SYSUAF
  struct UAF * up;

  if (!(up = get_uaf_uic (uid)))
    return 0;
  return cnv_uaf_pw (up);
#else
  if (uid == sys_getuid ())
    return getpwnam (egetenv ("USER"));
  else
    return 0;
#endif /* not READ_SYSUAF */
}

/* return total address space available to the current process.  This is
   the sum of the current p0 size, p1 size and free page table entries
   available. */
vlimit ()
{
  int item_code;
  unsigned long free_pages;
  unsigned long frep0va;
  unsigned long frep1va;
  register status;

  item_code = JPI$_FREPTECNT;
  if (((status = LIB$GETJPI (&item_code, 0, 0, &free_pages)) & 1) == 0)
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return -1;
    }
  free_pages *= 512;

  item_code = JPI$_FREP0VA;
  if (((status = LIB$GETJPI (&item_code, 0, 0, &frep0va)) & 1) == 0)
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return -1;
    }
  item_code = JPI$_FREP1VA;
  if (((status = LIB$GETJPI (&item_code, 0, 0, &frep1va)) & 1) == 0)
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return -1;
    }

  return free_pages + frep0va + (0x7fffffff - frep1va);
}

define_logical_name (varname, string)
     char *varname;
     char *string;
{
  struct dsc$descriptor_s strdsc =
    {strlen (string), DSC$K_DTYPE_T, DSC$K_CLASS_S, string};
  struct dsc$descriptor_s envdsc =
    {strlen (varname), DSC$K_DTYPE_T, DSC$K_CLASS_S, varname};
  struct dsc$descriptor_s lnmdsc =
    {7, DSC$K_DTYPE_T, DSC$K_CLASS_S, "LNM$JOB"};

  return LIB$SET_LOGICAL (&envdsc, &strdsc, &lnmdsc, 0, 0);
}

delete_logical_name (varname)
     char *varname;
{
  struct dsc$descriptor_s envdsc =
    {strlen (varname), DSC$K_DTYPE_T, DSC$K_CLASS_S, varname};
  struct dsc$descriptor_s lnmdsc =
    {7, DSC$K_DTYPE_T, DSC$K_CLASS_S, "LNM$JOB"};

  return LIB$DELETE_LOGICAL (&envdsc, &lnmdsc);
}

ulimit()
{}

setpriority()
{}

setpgrp()
{}

execvp()
{
  error ("execvp system call not implemented");
}

int
rename (from, to)
     char *from, *to;
{
  int status;
  struct FAB from_fab = cc$rms_fab, to_fab = cc$rms_fab;
  struct NAM from_nam = cc$rms_nam, to_nam = cc$rms_nam;
  char from_esn[NAM$C_MAXRSS];
  char to_esn[NAM$C_MAXRSS];

  from_fab.fab$l_fna = from;
  from_fab.fab$b_fns = strlen (from);
  from_fab.fab$l_nam = &from_nam;
  from_fab.fab$l_fop = FAB$M_NAM;

  from_nam.nam$l_esa = from_esn;
  from_nam.nam$b_ess = sizeof from_esn;

  to_fab.fab$l_fna = to;
  to_fab.fab$b_fns = strlen (to);
  to_fab.fab$l_nam = &to_nam;
  to_fab.fab$l_fop = FAB$M_NAM;

  to_nam.nam$l_esa = to_esn;
  to_nam.nam$b_ess = sizeof to_esn;

  status = SYS$RENAME (&from_fab, 0, 0, &to_fab);

  if (status & 1)
    return 0;
  else
    {
      if (status == RMS$_DEV)
	errno = EXDEV;
      else
	errno = EVMSERR;
      vaxc$errno = status;
      return -1;
    }
}

link (file, new)
     char * file, * new;
{
  register status;
  struct FAB fab;
  struct NAM nam;
  unsigned short fid[3];
  char esa[NAM$C_MAXRSS];

  fab = cc$rms_fab;
  fab.fab$l_fop = FAB$M_OFP;
  fab.fab$l_fna = file;
  fab.fab$b_fns = strlen (file);
  fab.fab$l_nam = &nam;

  nam = cc$rms_nam;
  nam.nam$l_esa = esa;
  nam.nam$b_ess = NAM$C_MAXRSS;

  status = SYS$PARSE (&fab);
  if ((status & 1) == 0)
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return -1;
    }
  status = SYS$SEARCH (&fab);
  if ((status & 1) == 0)
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return -1;
    }

  fid[0] = nam.nam$w_fid[0];
  fid[1] = nam.nam$w_fid[1];
  fid[2] = nam.nam$w_fid[2];

  fab.fab$l_fna = new;
  fab.fab$b_fns = strlen (new);

  status = SYS$PARSE (&fab);
  if ((status & 1) == 0)
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return -1;
    }

  nam.nam$w_fid[0] = fid[0];
  nam.nam$w_fid[1] = fid[1];
  nam.nam$w_fid[2] = fid[2];

  nam.nam$l_esa = nam.nam$l_name;
  nam.nam$b_esl = nam.nam$b_name + nam.nam$b_type + nam.nam$b_ver;

  status = SYS$ENTER (&fab);
  if ((status & 1) == 0)
    {
      errno = EVMSERR;
      vaxc$errno = status;
      return -1;
    }

  return 0;
}

croak (badfunc)
     char *badfunc;
{
  printf ("%s not yet implemented\r\n", badfunc);
  reset_sys_modes ();
  exit (1);
}

long
random ()
{
  /* Arrange to return a range centered on zero.  */
  return rand () - (1 << 30);
}

srandom (seed)
{
  srand (seed);
}
#endif /* VMS */

#ifdef AIX

/* Called from init_sys_modes.  */
hft_init ()
{
  /* If we're not on an HFT we shouldn't do any of this.  We determine
     if we are on an HFT by trying to get an HFT error code.  If this
     call fails, we're not on an HFT. */ 
#ifdef IBMR2AIX
  if (ioctl (0, HFQERROR, 0) < 0)
    return;
#else /* not IBMR2AIX */
  if (ioctl (0, HFQEIO, 0) < 0)
    return;
#endif /* not IBMR2AIX */

  /* On AIX the default hft keyboard mapping uses backspace rather than delete
     as the rubout key's ASCII code.  Here this is changed.  The bug is that
     there's no way to determine the old mapping, so in reset_sys_modes
     we need to assume that the normal map had been present.  Of course, this
     code also doesn't help if on a terminal emulator which doesn't understand
     HFT VTD's. */
  {
    struct hfbuf buf;
    struct hfkeymap keymap;

    buf.hf_bufp = (char *)&keymap;
    buf.hf_buflen = sizeof (keymap);
    keymap.hf_nkeys = 2;
    keymap.hfkey[0].hf_kpos = 15;
    keymap.hfkey[0].hf_kstate = HFMAPCHAR | HFSHFNONE;
#ifdef IBMR2AIX
    keymap.hfkey[0].hf_keyidh = '<';
#else /* not IBMR2AIX */
    keymap.hfkey[0].hf_page = '<';
#endif /* not IBMR2AIX */
    keymap.hfkey[0].hf_char = 127;
    keymap.hfkey[1].hf_kpos = 15;
    keymap.hfkey[1].hf_kstate = HFMAPCHAR | HFSHFSHFT;
#ifdef IBMR2AIX
    keymap.hfkey[1].hf_keyidh = '<';
#else /* not IBMR2AIX */
    keymap.hfkey[1].hf_page = '<';
#endif /* not IBMR2AIX */
    keymap.hfkey[1].hf_char = 127;
    hftctl (0, HFSKBD, &buf);
  }
  /* The HFT system on AIX doesn't optimize for scrolling, so it's really ugly
     at times. */
  line_ins_del_ok = char_ins_del_ok = 0;
}

/* Reset the rubout key to backspace. */

hft_reset ()
{
  struct hfbuf buf;
  struct hfkeymap keymap;

#ifdef IBMR2AIX
  if (ioctl (0, HFQERROR, 0) < 0)
    return;
#else /* not IBMR2AIX */
  if (ioctl (0, HFQEIO, 0) < 0)
    return;
#endif /* not IBMR2AIX */

  buf.hf_bufp = (char *)&keymap;
  buf.hf_buflen = sizeof(keymap);
  keymap.hf_nkeys = 2;
  keymap.hfkey[0].hf_kpos = 15;
  keymap.hfkey[0].hf_kstate = HFMAPCHAR | HFSHFNONE;
#ifdef IBMR2AIX
  keymap.hfkey[0].hf_keyidh = '<';
#else /* not IBMR2AIX */
  keymap.hfkey[0].hf_page = '<';
#endif /* not IBMR2AIX */
  keymap.hfkey[0].hf_char = 8;
  keymap.hfkey[1].hf_kpos = 15;
  keymap.hfkey[1].hf_kstate = HFMAPCHAR | HFSHFSHFT;
#ifdef IBMR2AIX
  keymap.hfkey[1].hf_keyidh = '<';
#else /* not IBMR2AIX */
  keymap.hfkey[1].hf_page = '<';
#endif /* not IBMR2AIX */
  keymap.hfkey[1].hf_char = 8;
  hftctl (0, HFSKBD, &buf);
}

#endif /* AIX */
