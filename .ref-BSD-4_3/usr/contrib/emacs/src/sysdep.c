/* Interfaces to system-dependent kernel and library entries.
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
#include "lisp.h"
#undef NULL

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

#ifndef open
#define sys_open open
#endif /* `open' is not a macro.  */

#undef open

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

#if defined (USG) || (defined (BSD) && !defined (BSD4_1))
#include <fcntl.h>
#endif

#ifdef BSD
#include <sys/ioctl.h>
#ifdef BSD4_1
#include <wait.h>
#else /* not 4.1 */
#include <sys/wait.h>
#endif /* not 4.1 */
#include <sgtty.h>
#define TERMINAL struct sgttyb
#define OSPEED(str) str.sg_ospeed
#define TABS_OK(str) ((str.sg_flags & XTABS) != XTABS)
#endif

/* Get rid of LLITOUT in 4.1, since it is said to stimulate kernel bugs.  */
#ifdef BSD4_1
#undef LLITOUT
#define LLITOUT 0
#endif /* 4.1 */

#ifdef USG
#include <termio.h>
#include <sys/utsname.h>
#include <memory.h>
#include <string.h>
#ifdef HAVE_TIMEVAL
#ifdef HPUX
#include <time.h>
#else
#include <sys/time.h>
#endif
#endif /* HAVE_TIMEVAL */
#include <errno.h>
#define TIOCGETP TCGETA
#define TIOCSETN TCSETA
#define TIOCSETP TCSETAF
#define TERMINAL struct termio
#define OSPEED(str) (str.c_cflag & CBAUD)
#define TABS_OK(str) ((str.c_oflag & TABDLY) != TAB3)
#endif /* USG */

#include "termhooks.h"
#include "termchar.h"
#include "termopts.h"
#include "dispextern.h"

#ifdef NONSYSTEM_DIR_LIBRARY
#include "ndir.h"
#endif /* NONSYSTEM_DIR_LIBRARY */

/* Define SIGCHLD as an alias for SIGCLD.  There are many conditionals
   testing SIGCHLD.  */

#if !defined (SIGCHLD) && defined (SIGCLD)
#define SIGCHLD SIGCLD
#endif /* SIGCLD and not SIGCHLD */

static int baud_convert[] =
  {
    0, 50, 75, 110, 135, 150, 200, 300, 600, 1200,
    1800, 2400, 4800, 9600, 19200, 38400
  };

extern short ospeed;

discard_tty_input ()
{
  TERMINAL buf;

  if (noninteractive)
    return;

  ioctl (0, TIOCGETP, &buf);
  ioctl (0, TIOCSETP, &buf);
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
      ioctl (0, TIOCGETP, &sg);
      ospeed = OSPEED (sg);
    }
  baud_rate = ospeed == 0 ? 1200
    : ospeed < sizeof baud_convert / sizeof baud_convert[0]
      ? baud_convert[ospeed] : 9600;
}

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
#ifndef USG
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
  int status;
  while (1)
    {
#ifdef subprocesses
#ifdef BSD
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
#else /* not BSD */
#ifdef UNIPLUS
      if (0 > kill (pid, 0))
	break;
      wait (0);
#else /* neither BSD nor UNIPLUS: random sysV */
      if (0 > kill (pid, 0))
	break;
      pause ();
#endif /* not UNIPLUS */
#endif /* not BSD */
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
 
/*
 *	Insert description of what this command is really supposed to
 *	to (I.E. what state is the child process line to be placed into,
 *	and why).  I have tried to interpret this as much as possible from
 *	the BSD setup and map to an appropriate USG control, but don't
 *	guarantee the results.  fnf@unisoft
 */

child_setup_tty (out)
     int out;
{
  TERMINAL s;

  ioctl (out, TIOCGETP, &s);
#ifdef USG
  s.c_oflag |= OPOST;		/* Enable output postprocessing */
  s.c_oflag &= ~ONLCR;		/* Disable map of NL to CR-NL on output */
  s.c_oflag &= ~(NLDLY|CRDLY|TABDLY|BSDLY|VTDLY|FFDLY);	/* No output delays */
  s.c_lflag &= ~ECHO;		/* Disable echo */
  s.c_lflag &= ~ICANON;		/* Disable erase/kill processing */
  s.c_lflag |= ISIG;		/* Enable signals */
  s.c_iflag &= ~IUCLC;		/* Disable map of upper case to lower on input */
  s.c_oflag &= ~OLCUC;		/* Disable map of lower case to upper on output */
  s.c_cc[VMIN] = 1;		/* minimum number of characters to accept */
  s.c_cc[VTIME] = 0;		/* wait forever for at least 1 character */
#else /* not USG */
  s.sg_flags &= ~(ECHO | CRMOD | ANYP | ALLDELAY | RAW | LCASE | CBREAK | TANDEM);
#endif /* not USG */
  ioctl (out, TIOCSETN, &s);

#ifdef BSD4_1
  if (interrupt_input)
    reset_sigio ();
#endif /* BSD4_1 */
}

setpgrp_of_tty (pid)
     int pid;
{
#ifdef TIOCSPGRP
  ioctl (0, TIOCSPGRP, &pid);
#else
  /* Just ignore this for now and hope for the best */
#endif
}

#ifdef F_SETFL

init_sigio ()
{
  request_sigio ();
}

reset_sigio ()
{
  unrequest_sigio ();
}

#ifdef FASYNC		/* F_SETFL does not imply existance of FASYNC */
int old_fcntl_flags;

request_sigio ()
{
  old_fcntl_flags = fcntl (0, F_GETFL, 0);
  fcntl (0, F_SETFL, old_fcntl_flags | FASYNC);
}

unrequest_sigio ()
{
  fcntl (0, F_SETFL, old_fcntl_flags);
}

#else /* no FASYNC */
 
request_sigio ()
{
  croak ("request_sigio");
}
 
unrequest_sigio ()
{
  croak ("unrequest_sigio");
}
 
#endif /* FASYNC */
#endif /* F_SETFL */

TERMINAL old_gtty;		/* The initial tty mode bits */

int term_initted;		/* 1 if outer tty status has been recorded */

#ifdef F_SETOWN
int old_fcntl_owner;
#endif /* F_SETOWN */

#ifdef TIOCGLTC
struct tchars old_tchars;
struct ltchars old_ltchars;
int old_lmode;

int lmode;			/* Current lmode value. */
				/* Needed as global for 4.1 */
#endif /* TIOCGLTC */

/* This may also be defined in stdio,
   but if so, this does no harm,
   and using the same name avoids wasting the other one's space.  */

#ifdef USG
unsigned char _sobuf[BUFSIZ+8];
#else
char _sobuf[BUFSIZ];
#endif
 
init_sys_modes ()
{
  TERMINAL sg;
#ifdef TIOCGLTC
  struct tchars tchars;
  static struct tchars new_tchars = {-1,-1,-1,-1,-1,-1};
  static struct ltchars new_ltchars = {-1,-1,-1,-1,-1,-1};
#endif

  if (noninteractive)
    return;

  ioctl (0, TIOCGETP, &old_gtty);
  if (!read_socket_hook)
    {
      sg = old_gtty;

#ifdef USG
      sg.c_iflag |= (IGNBRK);	/* Ignore break condition */
      sg.c_iflag &= ~ICRNL;	/* Disable map of CR to NL on input */
#ifdef ISTRIP
      sg.c_iflag &= ~ISTRIP;	/* don't strip 8th bit on input */
#endif
      sg.c_lflag &= ~ECHO;	/* Disable echo */
      sg.c_lflag &= ~ICANON;	/* Disable erase/kill processing */
      sg.c_lflag |= ISIG;	/* Enable signals */
      if (flow_control)
	{
	  sg.c_iflag |= IXON;	/* Enable start/stop output control */
#ifdef IXANY
	  sg.c_iflag &= ~IXANY;
#endif /* IXANY */
	}
      else
	sg.c_iflag &= ~IXON;	/* Disable start/stop output control */
      sg.c_oflag &= ~ONLCR;	/* Disable map of NL to CR-NL on output */
      sg.c_oflag &= ~TAB3;	/* Disable tab expansion */
#ifdef CS8
      sg.c_cflag |= CS8;	/* allow 8th bit on input */
      sg.c_cflag &= ~PARENB;	/* Don't check parity */
#endif
      sg.c_cc[VINTR] = '\007';	/* ^G gives SIGINT */
#ifdef HPUX
      /* Can't use CDEL as that makes Meta-DEL do SIGQUIT.
	 Instead set up C-g for both; we handle both alike
	 so which one it really gives us does not matter.  */
      sg.c_cc[VQUIT] = '\007';
#else /* not HPUX */
      sg.c_cc[VQUIT] = CDEL;	/* Turn off SIGQUIT */
#endif /* not HPUX */
      sg.c_cc[VMIN] = 1;	/* Input should wait for at least 1 char */
      sg.c_cc[VTIME] = 0;	/* no matter how long that takes.  */
#ifdef VSWTCH
      sg.c_cc[VSWTCH] = CDEL;	/* Turn off shell layering use of C-z */
#endif /* VSWTCH */
#else /* if not USG */
      sg.sg_flags &= ~(ECHO | CRMOD | XTABS);
      sg.sg_flags |= ANYP;
      sg.sg_flags |= interrupt_input ? RAW : CBREAK;
#endif /* not USG (BSD, that is) */

      ioctl (0, TIOCSETN, &sg);

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
      ioctl (0, TIOCGETC, &old_tchars);
      ioctl (0, TIOCGLTC, &old_ltchars);
      ioctl (0, TIOCLGET, &old_lmode);

      /* Note: if not using CBREAK mode, it makes no difference how we set this */
      tchars = new_tchars;
      tchars.t_intrc = 07;
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
      ioctl (0, TIOCSLTC, &new_ltchars);
      ioctl (0, TIOCLSET, &lmode);
#endif TIOCGLTC
#ifdef BSD4_1
      if (interrupt_input)
	init_sigio ();
#endif
    }
  screen_garbaged = 1;
  setbuf (stdout, _sobuf);
  term_initted = 1;
  set_terminal_modes ();
}

/* Return nonzero if safe to use tabs in output.
   At the time this is called, init_sys_modes has not been done yet.  */
   
tabs_safe_p ()
{
  TERMINAL sg;
  if (noninteractive)
    return 1;
  ioctl (0, TIOCGETP, &sg);
  return (TABS_OK(sg));
}

/* Get terminal size from system.
   Store number of lines into *heightp and width into *widthp.
   If zero or a negative number is stored, the value is not valid.  */

get_screen_size (widthp, heightp)
     int *widthp, *heightp;
{
/* Define the 4.3 names in terms of the Sun names
   if the latter exist and the former do not.  */
#if !defined (TIOCGWINSZ) && defined (TIOCGSIZE)
#define TIOCGWINSZ TIOCGSIZE
#define winsize ttysize
#define ws_row ts_lines
#define ws_col ts_cols
#endif /* Sun */

#ifdef TIOCGWINSZ
  struct winsize size;
  *widthp = 0;
  *heightp = 0;
  if (ioctl (0, TIOCGWINSZ, &size) < 0)
    return;
  *widthp = size.ws_col;
  *heightp = size.ws_row;
#else /* system doesn't know size */
  *widthp = 0;
  *heightp = 0;
#endif /* system does not know size */
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
  topos (screen_height - 1, 0);
  clear_end_of_line (screen_width);
  /* clear_end_of_line may move the cursor */
  topos (screen_height - 1, 0);
  reset_terminal_modes ();
  fflush (stdout);
  if (read_socket_hook)
    return;
#ifdef TIOCGLTC
  ioctl (0, TIOCSETC, &old_tchars);
  ioctl (0, TIOCSLTC, &old_ltchars);
  ioctl (0, TIOCLSET, &old_lmode);
#endif /* TIOCGLTC */
#ifdef F_SETFL
#ifdef F_SETOWN		/* F_SETFL does not imply existance of F_SETOWN */
  if (interrupt_input)
    {
#ifdef FASYNC
      old_fcntl_flags &= ~FASYNC;
#endif /* FASYNC */
      reset_sigio ();
      reset_sigio ();
      fcntl (0, F_SETOWN, old_fcntl_owner);
    }
#endif /* F_SETOWN */
#endif /* F_SETFL */
#ifdef BSD4_1
  if (interrupt_input)
    reset_sigio ();
#endif /* BSD4_1 */
  ioctl (0, TIOCSETN, &old_gtty);
}

/*
 *	flush any pending output
 */
 
flush_pending_output (channel)
     int channel;
{
#ifdef USG
  ioctl (channel, TCFLSH, 1);
#else
  ioctl (channel, TIOCFLUSH, 0);
#endif
}
 
/*
 *	Return the address of the start of the text segment prior to
 *	doing an unexec().  After unexec() the return value is undefined.
 *	See crt0.c for further explanation and _start().
 *
 */

char *
start_of_text ()
{
#ifdef TEXT_START
  return ((char *) TEXT_START);
#else
  extern int _start ();
  return ((char *) _start);
#endif
}

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

#ifdef NOTDEF

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

#endif NOTDEF


/* Get_system_name returns as its value
 a string for the Lisp function system-name to return. */

#ifdef USG
struct utsname get_system_name_name;
#endif
#ifdef BSD4_1
#include <whoami.h>
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
#else /* BSD, not 4.1 */
  static char system_name_saved[32];
  (void) gethostname (system_name_saved, sizeof (system_name_saved));
  return (system_name_saved);
#endif /* BSD, not 4.1 */
#endif /* not USG */
}

#ifndef HAVE_SELECT

/* Emulate as much as select as is possible under 4.1 and needed by Gnu Emacs
 * Only checks read descriptors.
 */
/* How long to wait between checking fds in select */
#define SELECT_PAUSE 1
int select_alarmed;

select_alarm ()
{
  select_alarmed = 1;
#ifdef BSD4_1
  sigrelse (SIGALRM);
#else /* not BSD4_1 */
  signal (SIGALRM, SIG_IGN);
#endif /* not BSD4_1 */
}

/* Only rfds are checked and timeout must point somewhere */
int
select (nfds, rfds, wfds, efds, timeout)
     int nfds;
     int *rfds, *wfds, *efds, *timeout;
{
  int ravail = 0, orfds = 0, old_alarm, val;
  extern int kbd_count;
  extern int proc_buffered_char[];
  extern int child_changed;
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
  if (orfds == 1 && *timeout == 100000 && !child_changed)
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
      if (*timeout == 0 || ravail != 0 || child_changed)
	break;
      old_alarm = alarm (0);
      old_trap = signal (SIGALRM, select_alarm);
      select_alarmed = 0;
      alarm (SELECT_PAUSE);
      /* Wait for a SIGALRM (or maybe a SIGTINT) */
      while (select_alarmed == 0 && *timeout != 0 && child_changed == 0)
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
      (*timeout) -= SELECT_PAUSE;
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
      if (*timeout == 0)  /* Stop on timer being cleared */
	break;
    }
  return ravail;
}

/* Read keyboard input into the standard buffer,
   waiting for at least one character.  */

read_input_waiting ()
{
  extern int kbd_count;
  extern unsigned char kbd_buffer[];
  extern unsigned char *kbd_ptr;
  int val = read (fileno(stdin), kbd_buffer, 1);
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
  lmode =  LINTRUP | lmode;
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
}

unrequest_sigio ()
{
  sighold (SIGTINT);
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
  while (length-- > 0)
    *b++ = 0;
}

void 
bcopy (b1, b2, length)
     register char *b1;
     register char *b2;
     register int length;
{
  while (length-- > 0)
    *b2++ = *b1++;
}

int
bcmp (b1, b2, length)	/* This could be a macro! */
     register char *b1;
     register char *b2;
     register int length;
{
  while (length-- > 0)
    if (*b1++ != *b2++)
      return 1;

  return 0;
}
#endif /* not BSTRING */

#if defined (BSD4_1) || defined (USG)

/*
 *	The BSD random(3) returns numbers in the range of
 *	0 to 2e31 - 1.  The USG rand(3C) returns numbers in the
 *	range of 0 to 2e15 - 1.  This is probably not significant
 *	in this usage.
 */
  
long
random ()
{
  return (rand ());
}

srandom (arg)
     int arg;
{
  srand (arg);
}

#endif /* bsd4.1 or any USG */

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
  0
  };

int
sys_read (fildes, buf, nbyte)
     int fildes;
     char *buf;
     unsigned int nbyte;
{
  register int rtnval;
  
  while ((rtnval = read (fildes, buf, nbyte)) == -1 && errno == EINTR);
  return (rtnval);
}

int
/* VARARGS 2 */
sys_open (path, oflag, mode)
     char *path;
     int oflag, mode;
{
  register int rtnval;
  
  while ((rtnval = open (path, oflag, mode)) == -1 && errno == EINTR);
  return (rtnval);
}

int
sys_write (fildes, buf, nbyte)
     int fildes;
     char *buf;
     unsigned int nbyte;
{
  register int rtnval;

  while ((rtnval = write (fildes, buf, nbyte)) == -1 && errno == EINTR);
  return (rtnval);
}

/*
 *	Warning, this function may not duplicate 4.2 action properly
 *	under error conditions.
 */

#ifndef MAXPATHLEN
/* In 4.1, param.h fails to define this.  */
#define MAXPATHLEN 1024
#endif

char *
getwd (pathname)
     char *pathname;
{
  extern char *getcwd ();

  return (getcwd (pathname, MAXPATHLEN));
}

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

/* VARARGS */
setpriority ()
{
  return (0);
}

#ifndef HPUX

/*
 *	Substitute fork(2) for vfork(2) on USG flavors.
 */

vfork ()
{
  return (fork ());
}

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
  
  close (newd);
  while ((fd = dup (oldd)) != newd) {
    dup2 (oldd, newd);
    close (fd);
  }
}

/*
 *	Gettimeofday.  Simulate as much as possible.  Only accurate
 *	to nearest second.  Emacs doesn't use tzp so ignore it for now.
 *	Only needed when subprocesses are defined.
 */

#if defined(subprocesses) && !defined(STRIDE) && defined(HAVE_TIMEVAL)
 
/* ARGSUSED */
gettimeofday (tp, tzp)
     struct timeval *tp;
     struct timezone *tzp;
{
  extern long time ();

  tp->tv_sec = time ((long *)0);    
  tp->tv_usec = 0;
}
 
#endif /* subprocess && ~STRIDE && HAVE_TIMEVAL */
#endif /* not HPUX */ 
  
/*
 *	This function will go away as soon as all the stubs fixed.  (fnf)
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
      close (fd);
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
  close (dirp->dd_fd);
  free ((char *) dirp);
}


#define DIRSIZ	14
struct olddir
  {
    ino_t od_ino; 		/* inode */
    char od_name[DIRSIZ];	/* filename */
  };

struct direct dir_static;	/* simulated directory contents */

struct direct *
readdir (dirp)
     register DIR *dirp;	/* stream from opendir() */
{
  register struct olddir *dp;	/* -> directory data */

  for ( ; ; )
    {
      if (dirp->dd_loc >= dirp->dd_size)
	dirp->dd_loc = dirp->dd_size = 0;

      if (dirp->dd_size == 0 	/* refill buffer */
	  && (dirp->dd_size = sys_read (dirp->dd_fd, dirp->dd_buf, DIRBLKSIZ)) <= 0)
	return 0;

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
    }
}

#endif /* NONSYSTEM_DIR_LIBRARY */
