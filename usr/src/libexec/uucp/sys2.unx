/* sys2.unx
   The system dependent communication routines for UNIX.

   Copyright (C) 1991, 1992 Ian Lance Taylor

   This file is part of the Taylor UUCP package.

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   The author of the program may be contacted at ian@airs.com or
   c/o AIRS, P.O. Box 520, Waltham, MA 02254.

   $Log: sys2.unx,v $
   Revision 1.72  1992/04/03  17:43:39  ian
   Petri Helenius: don't clobber undefined bits in termio or termios

   Revision 1.71  1992/04/01  21:52:04  ian
   T. William Wells: handle a system without <poll.h> or <stropts.h>

   Revision 1.70  1992/03/30  15:29:58  ian
   Added HAVE_SVR4_LOCKFILES

   Revision 1.69  1992/03/29  22:25:27  ian
   Always block and unblock read and write descriptors together

   Revision 1.68  1992/03/28  04:12:17  ian
   Gerben Wierda: minor cleanups

   Revision 1.67  1992/03/28  03:06:04  ian
   Don't use TIOCEXCL locking

   Revision 1.66  1992/03/28  02:47:30  ian
   Rework HAVE_UNBLOCKED_WRITES to work even if writes are unblocked

   Revision 1.65  1992/03/17  15:35:28  ian
   Log signals when they happen, even if we continue looping

   Revision 1.64  1992/03/17  05:01:33  ian
   Don't block when opening the write descriptor

   Revision 1.63  1992/03/17  01:03:03  ian
   Miscellaneous cleanup

   Revision 1.62  1992/03/16  01:23:08  ian
   Make blocking writes optional

   Revision 1.61  1992/03/15  07:15:42  ian
   T. William Wells: don't use unblocked writes

   Revision 1.60  1992/03/15  04:51:17  ian
   Keep an array of signals we've received rather than a single variable

   Revision 1.59  1992/03/15  01:54:46  ian
   All execs are now done in isspawn, all waits are done in iswait

   Revision 1.58  1992/03/12  21:50:50  ian
   Moved local header includes above sleep routine determination

   Revision 1.57  1992/03/12  19:54:43  ian
   Debugging based on types rather than number

   Revision 1.56  1992/03/11  02:09:57  ian
   Franc,ois Pinard: retry fork several times before giving up

   Revision 1.55  1992/03/09  22:11:46  ian
   Franc,ois Pinard: sleep for a second after closing a serial port

   Revision 1.54  1992/03/09  22:07:36  ian
   Wait for terminal output to drain at various points

   Revision 1.53  1992/03/08  16:56:51  ian
   Ted Lindgreen: if CRTSCTS is defined, don't turn on IXOFF

   Revision 1.52  1992/03/08  04:56:21  ian
   Peter da Silva: added ``lockname'' command for ports

   Revision 1.51  1992/03/08  01:56:01  ian
   Include <sys/ioctl.h> if we have it

   Revision 1.50  1992/03/08  01:37:45  ian
   Suppurt TIOCEXCL locking

   Revision 1.49  1992/03/07  16:25:21  ian
   Preserve unknown bits in c_cflag

   Revision 1.48  1992/03/04  23:43:39  ian
   Petri Helenius: didn't remove lock file if open failed

   Revision 1.47  1992/03/04  01:40:51  ian
   Thomas Fischer: tweaked a bit for the NeXT

   Revision 1.46  1992/03/03  21:01:20  ian
   Use strict timeout in fsserial_read, eliminate all race conditions

   Revision 1.45  1992/03/03  04:25:00  ian
   T. William Wells: don't arbitrarily extend read timeout

   Revision 1.44  1992/03/02  04:53:07  ian
   Marc Unangst: added HAVE_SCO_LOCKFILES configuration parameter

   Revision 1.43  1992/02/28  05:06:15  ian
   T. William Wells: fsysdep_catch must be a macro

   Revision 1.42  1992/02/27  05:40:54  ian
   T. William Wells: detach from controlling terminal, handle signals safely

   Revision 1.41  1992/02/24  21:22:47  ian
   The poll function takes milliseconds, not microseconds (my error)

   Revision 1.40  1992/02/24  21:18:17  ian
   Roberto Biancardi: use poll for sleeping if we haven't got anything else

   Revision 1.39  1992/02/24  20:07:43  ian
   John Theus: some systems don't have <fcntl.h>

   Revision 1.38  1992/02/23  03:26:51  ian
   Overhaul to use automatic configure shell script

   Revision 1.37  1992/02/17  22:08:50  ian
   Bob Denny: log chat script messages as LOG_NORMAL, not LOG_ERROR

   Revision 1.36  1992/02/08  20:02:36  ian
   Added HAVE_SETRET configuration option for systems without setjmp

   Revision 1.35  1992/02/08  03:54:18  ian
   Include <string.h> only in <uucp.h>, added 1992 copyright

   Revision 1.34  1992/01/16  16:32:44  ian
   Mike Park: ioctl is sometimes declared varadic, so we can't declare it

   Revision 1.33  1992/01/15  21:06:11  ian
   Mike Park: some systems can't include <sys/time.h> and <time.h> together

   Revision 1.32  1992/01/15  20:40:04  ian
   Mike Park: some systems don't have <limits.h>

   Revision 1.31  1992/01/15  19:40:35  ian
   Mike Park: handle HAVE_UNION_WAIT correctly and completely

   Revision 1.30  1992/01/14  04:18:47  ian
   Chip Salzenberg: added HAVE_USLEEP configuration parameter

   Revision 1.29  1992/01/13  06:11:39  ian
   David Nugent: can't declare open or fcntl

   Revision 1.28  1991/12/31  04:16:19  ian
   Chip Salzenberg: don't turn on IXON and IXOFF at the start

   Revision 1.27  1991/12/29  04:04:18  ian
   Added a bunch of extern definitions

   Revision 1.26  1991/12/29  00:55:23  ian
   Monty Solomon: added HAVE_UNION_WAIT

   Revision 1.25  1991/12/22  22:14:19  ian
   Monty Solomon: added HAVE_UNISTD_H configuration parameter

   Revision 1.24  1991/12/20  02:23:10  ian
   Don't change port settings if we don't have to

   Revision 1.23  1991/12/19  04:25:57  ian
   Terry Gardner: configuration parameter to not use both NONBLOCK and NDELAY

   Revision 1.22  1991/12/17  23:14:08  ian
   T. William Wells: allow dialer complete and abort to be chat scripts

   Revision 1.21  1991/12/17  22:31:15  ian
   Start in RAW mode, to avoid dropping characters when we switch to it

   Revision 1.20  1991/12/17  05:24:01  ian
   David Nugent: flush pending input in fsserial_open

   Revision 1.19  1991/12/15  04:28:23  ian
   Don't turn on ISTRIP initially

   Revision 1.18  1991/12/10  19:45:05  ian
   Added ulog_device to record device name for log file

   Revision 1.17  1991/12/01  02:23:12  ian
   Niels Baggesen: don't multiply include <unistd.h>

   Revision 1.16  1991/11/26  01:50:30  ian
   Set fread_blocking and fwrite_blocking correctly for TCP routines

   Revision 1.15  1991/11/26  01:45:42  ian
   Marty Shannon: configuration option to not include <sys/wait.h>

   Revision 1.14  1991/11/22  06:05:57  ian
   Gregory Gulik: fix wait status macro definitions

   Revision 1.13  1991/11/21  20:58:18  ian
   Brian Campbell: for HAVE_SIGSETJMP use sigjmp_buf, not jmp_buf

   Revision 1.12  1991/11/15  23:13:53  ian
   Fixed termio(s) version of fsserial_set

   Revision 1.11  1991/11/13  20:38:00  ian
   Added TCP port type for connections over TCP

   Revision 1.10  1991/11/12  19:07:27  ian
   Be careful to only call fsetterminfo on a terminal

   Revision 1.9  1991/11/11  23:47:24  ian
   Added chat-program to run a program to do a chat script

   Revision 1.8  1991/11/11  00:39:45  ian
   Open port in seven bit mode, added fport_set to change to eight bit

   Revision 1.7  1991/11/08  22:52:34  ian
   Brian Campbell: only include <sys/time.h> and <sys/ioctl.h> when needed

   Revision 1.6  1991/11/08  22:11:45  ian
   Brian Campbell: allow sigsetjmp as configuration option

   Revision 1.5  1991/11/07  22:52:11  ian
   Chip Salzenberg: force stdin and stdout to stay open in case of spawning

   Revision 1.4  1991/11/07  21:43:59  ian
   Chip Salzenberg: set terminal modes directly, don't or them in

   Revision 1.3  1991/11/07  19:42:16  ian
   Chip Salzenberg: declare inline functions consistently

   Revision 1.2  1991/11/07  19:32:28  ian
   Chip Salzenberg: allow LOCKDIR, and check that locking process exists

   Revision 1.1  1991/09/10  19:45:50  ian
   Initial revision

   */

#include "uucp.h"

#if USE_RCS_ID
char sys2_unx_rcsid[] = "$Id: sys2.unx,v 1.72 1992/04/03 17:43:39 ian Rel $";
#endif

#include <errno.h>

#if HAVE_LIMITS_H
#include <limits.h>
#endif

#if USE_STDIO && HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "system.h"
#include "sysdep.h"
#include "port.h"

/* Pick a timing routine to use.  I somewhat arbitrarily picked usleep
   above nap above napms above poll above select.  */

#if HAVE_USLEEP || HAVE_NAP || HAVE_NAPMS || HAVE_POLL
#define USE_SELECT_TIMER 0
#else
#define USE_SELECT_TIMER HAVE_SELECT
#endif

#if HAVE_USLEEP || HAVE_NAP || HAVE_NAPMS
#undef HAVE_POLL
#define HAVE_POLL 0
#endif

#if HAVE_USLEEP || HAVE_NAP
#undef HAVE_NAPMS
#define HAVE_NAPMS 0
#endif

#if HAVE_USLEEP
#undef HAVE_NAP
#define HAVE_NAP 0
#endif

#if HAVE_FCNTL_H
#include <fcntl.h>
#else
#if HAVE_SYS_FILE_H
#include <sys/file.h>
#endif
#endif

#ifndef O_RDONLY
#define O_RDONLY 0
#define O_WRONLY 1
#define O_RDWR 2
#endif

#if HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif

#if USE_SELECT_TIMER || HAVE_BSD_TTY
#include <sys/time.h>
#endif

#if HAVE_POLL
#if HAVE_STROPTS_H
#include <stropts.h>
#endif
#if HAVE_POLL_H
#include <poll.h>
#endif
#if ! HAVE_STROPTS_H && ! HAVE_POLL_H
/* We need a definition for struct pollfd, although it doesn't matter
   what it contains.  It's used in usysdep_pause.  */
struct pollfd
{
  int idummy;
};
#endif /* ! HAVE_STROPTS_H && ! HAVE_POLL_H */
#endif /* HAVE_POLL */

#if HAVE_TIME_H
#if HAVE_SYS_TIME_AND_TIME_H || (! USE_SELECT_TIMER && ! HAVE_BSD_TTY)
#include <time.h>
#endif
#endif

/* Get definitions for both O_NONBLOCK and O_NDELAY.  */

#ifndef O_NDELAY
#ifdef FNDELAY
#define O_NDELAY FNDELAY
#else /* ! defined (FNDELAY) */
#define O_NDELAY 0
#endif /* ! defined (FNDELAY) */
#endif /* ! defined (O_NDELAY) */

#ifndef O_NONBLOCK
#ifdef FNBLOCK
#define O_NONBLOCK FNBLOCK
#else /* ! defined (FNBLOCK) */
#define O_NONBLOCK 0
#endif /* ! defined (FNBLOCK) */
#endif /* ! defined (O_NONBLOCK) */

#if O_NDELAY == 0 && O_NONBLOCK == 0
 #error No way to do nonblocking I/O
#endif

/* If we can define them both together, do so.  This is because some
   ancient drivers on some systems appear to look for one but not the
   other.  Otherwise just use O_NONBLOCK.  */
#if COMBINED_UNBLOCK
#define FILE_UNBLOCKED (O_NDELAY | O_NONBLOCK)
#else
#define FILE_UNBLOCKED O_NONBLOCK
#endif

/* Get definitions for both EAGAIN and EWOULDBLOCK.  */

#ifndef EAGAIN
#ifndef EWOULDBLOCK
#define EAGAIN (-1)
#define EWOULDBLOCK (-1)
#else /* defined (EWOULDBLOCK) */
#define EAGAIN EWOULDBLOCK
#endif /* defined (EWOULDBLOCK) */
#else /* defined (EAGAIN) */
#ifndef EWOULDBLOCK
#define EWOULDBLOCK EAGAIN
#endif /* ! defined (EWOULDBLOCK) */
#endif /* defined (EAGAIN) */

/* Make sure we have a definition for MAX_INPUT.  */

#ifndef MAX_INPUT
#define MAX_INPUT (256)
#endif

/* Make sure we have definitions for major and minor.  */

#ifndef major
#define major(i) (((i) >> 8) & 0xff)
#endif
#ifndef minor
#define minor(i) ((i) & 0xff)
#endif

/* If we have the TIOCSINUSE ioctl call, we use it to lock a terminal.
   Otherwise, if we have the TIOCEXCL ioctl call, we have to open the
   terminal before we know that it is unlocked.  */
#ifdef TIOCSINUSE
#define HAVE_TIOCSINUSE 1
#else
#ifdef TIOCEXCL
#define HAVE_TIOCEXCL 1
#endif
#endif

/* Determine bits to clear for the various terminal control fields for
   HAVE_SYSV_TERMIO and HAVE_POSIX_TERMIOS.  */

#if HAVE_SYSV_TERMIO
#define ICLEAR_IFLAG (IGNBRK | BRKINT | IGNPAR | PARMRK | INPCK \
		      | ISTRIP | INLCR | IGNCR | ICRNL | IUCLC \
		      | IXON | IXANY | IXOFF)
#define ICLEAR_OFLAG (OPOST | OLCUC | ONLCR | OCRNL | ONOCR | ONLRET \
		      | OFILL | OFDEL | NLDLY | CRDLY | TABDLY | BSDLY \
		      | VTDLY | FFDLY)
#define ICLEAR_CFLAG (CBAUD | CLOCAL | CSIZE | PARENB | PARODD)
#define ISET_CFLAG (CS8 | CREAD | HUPCL)
#define ICLEAR_LFLAG (ISIG | ICANON | XCASE | ECHO | ECHOE | ECHOK \
		      | ECHONL | NOFLSH)
#endif
#if HAVE_POSIX_TERMIOS
#define ICLEAR_IFLAG (BRKINT | ICRNL | IGNBRK | IGNCR | IGNPAR \
		      | INLCR | INPCK | ISTRIP | IXOFF | IXON \
		      | PARMRK)
#define ICLEAR_OFLAG (OPOST)
#define ICLEAR_CFLAG (CLOCAL | CSIZE | PARENB | PARODD)
#define ISET_CFLAG (CS8 | CREAD | HUPCL)
#define ICLEAR_LFLAG (ECHO | ECHOE | ECHOK | ECHONL | ICANON | IEXTEN \
		      | ISIG | NOFLSH | TOSTOP)
#endif

/* External functions.  */
extern char *strlwr ();
extern int close (), pipe (), dup2 (), read (), write ();
extern int fclose ();
extern void _exit ();
#if USE_SELECT_TIMER || HAVE_BSD_TTY
extern int select ();
#endif
#if HAVE_NAP
extern int nap ();
#endif
#if HAVE_NAPMS
extern int napms ();
#endif
#if HAVE_POLL
extern int poll ();
#endif

/* Local functions.  */

static SIGtype usalarm P((int isig));
static boolean fsserial_lockfile P((boolean flok, const struct sport *,
				    const char *zdevice));
static boolean fsserial_lock P((const struct sport *qport,
				struct ssysdep_serial_port *q,
				boolean fin, const char *zdevice));
static boolean fsserial_open P((const char *z, long ibaud, boolean fwait,
				struct ssysdep_serial_port *q));
__inline__ static boolean fsblock P((struct ssysdep_serial_port *q,
				     boolean fblock));
static boolean fsserial_close P((struct ssysdep_serial_port *q));
static boolean fsserial_reset P((struct ssysdep_serial_port *q));
static boolean fsserial_read P((struct ssysdep_serial_port *q,
				char *zbuf, int *pclen, int cmin,
				int ctimeout, boolean freport,
				boolean fpty));
static boolean fsserial_write P((struct ssysdep_serial_port *q,
				 const char *zwrite, int cwrite));
static boolean fsserial_io P((struct ssysdep_serial_port *q,
			      const char *zwrite, int *pcwrite,
			      char *zread, int *pcread));
static boolean fsserial_break P((struct ssysdep_serial_port *q));
static boolean fsserial_set P((struct ssysdep_serial_port *q,
			       enum tportsetting tset));
static boolean fsrun_chat P((int oread, int owrite, const char *zprog));

/* This code handles SIGALRM.  See the discussion above fsserial_read.
   Normally we ignore SIGALRM, but the handler will temporarily be set
   to this function, which should set fSalarm and then either longjmp
   or schedule another SIGALRM.  fSalarm is never referred to outside
   of this file, but we don't make it static to try to fool compilers
   which don't understand volatile.  */

volatile sig_atomic_t fSalarm;

static SIGtype
usalarm (isig)
     int isig;
{
#if ! HAVE_SIGACTION && ! HAVE_SIGVEC && ! HAVE_SIGSET
  (void) signal (isig, usalarm);
#endif

  fSalarm = TRUE;

#if HAVE_RESTARTABLE_SYSCALLS
  longjmp (sSjmp_buf, 1);
#else
  alarm (1);
#endif
}

/* We need a simple routine to block SIGINT, SIGQUIT, SIGTERM and
   SIGPIPE and another to restore the original state.  When these
   functions are called (in fsysdep_modem_close) SIGHUP is being
   ignored.  The routines are isblocksigs, which returns a value of
   type HELD_SIG_MASK and usunblocksigs which takes a single argument
   of type HELD_SIG_MASK.  */

#if HAVE_SIGPROCMASK

/* Use the POSIX sigprocmask call.  */

extern int sigprocmask ();

#define HELD_SIG_MASK sigset_t

static sigset_t isblocksigs P((void));

static sigset_t
isblocksigs ()
{
  sigset_t sblock, sold;

  sigemptyset (&sblock);
  sigaddset (&sblock, SIGINT);
  sigaddset (&sblock, SIGQUIT);
  sigaddset (&sblock, SIGTERM);
  sigaddset (&sblock, SIGPIPE);
  sigprocmask (SIG_BLOCK, &sblock, &sold);
  return sold;
}

#define usunblocksigs(s) \
  ((void) sigprocmask (SIG_SETMASK, &(s), (sigset_t *) NULL))

#else /* ! HAVE_SIGPROCMASK */
#if HAVE_SIGBLOCK

/* Use the BSD sigblock and sigsetmask calls.  */

extern int sigblock (), sigsetmask ();

#define HELD_SIG_MASK int

#ifndef sigmask
#define sigmask(i) (1 << ((i) - 1))
#endif

#define isblocksigs() \
  sigblock (sigmask (SIGINT) | sigmask (SIGQUIT) \
	    | sigmask (SIGTERM) | sigmask (SIGPIPE))

#define usunblocksigs(i) ((void) sigsetmask (i))

#else /* ! HAVE_SIGBLOCK */

#if HAVE_SIGHOLD

/* Use the SVR3 sighold and sigrelse calls.  */

extern int sighold (), sigrelse ();

#define HELD_SIG_MASK int

static int isblocksigs P((void));

static int
isblocksigs ()
{
  sighold (SIGINT);
  sighold (SIGQUIT);
  sighold (SIGTERM);
  sighold (SIGPIPE);
  return 0;
}

static void usunblocksigs P((int));

/*ARGSUSED*/
static void
usunblocksigs (i)
     int i;
{
  sigrelse (SIGINT);
  sigrelse (SIGQUIT);
  sigrelse (SIGTERM);
  sigrelse (SIGPIPE);
}

#else /* ! HAVE_SIGHOLD */

/* We have no way to block signals.  This system will suffer from a
   race condition in fsysdep_modem_close.  */

#define HELD_SIG_MASK int

#define isblocksigs() 0

#define usunblocksigs(i)

#endif /* ! HAVE_SIGHOLD */
#endif /* ! HAVE_SIGBLOCK */
#endif /* ! HAVE_SIGPROCMASK */

/* Pause for half a second.  This doesn't really belong in this file,
   but all the timing routines are here.  */

void
usysdep_pause ()
{
#if HAVE_NAPMS
  napms (500);
#endif /* HAVE_NAPMS */
#if HAVE_NAP
  nap (500L);
#endif /* HAVE_NAP */
#if HAVE_USLEEP
  usleep (500 * (long) 1000);
#endif /* HAVE_USLEEP */
#if HAVE_POLL
  struct pollfd sdummy;

  /* We need to pass an unused pollfd structure because poll checks
     the address before checking the number of elements.  */
  poll (&sdummy, 0, 500);
#endif /* HAVE_POLL */
#if USE_SELECT_TIMER
  struct timeval s;

  s.tv_sec = 0;
  s.tv_usec = 500 * (long) 1000;
  select (0, (int *) NULL, (int *) NULL, (int *) NULL, &s);
#endif /* USE_SELECT_TIMER */
#if ! HAVE_NAPMS && ! HAVE_NAP && ! HAVE_USLEEP
#if ! USE_SELECT_TIMER && ! HAVE_POLL
  sleep (1);
#endif /* ! USE_SELECT_TIMER && ! HAVE_POLL */
#endif /* ! HAVE_NAPMS && ! HAVE_NAP && ! HAVE_USLEEP */
}

/* This routine is used for both locking and unlocking.  It is the
   only routine which knows how to translate a device name into the
   name of a lock file.  If it can't figure out a name, it does
   nothing and returns TRUE.  */

static boolean
fsserial_lockfile (flok, qport, zdevice)
     boolean flok;
     const struct sport *qport;
     const char *zdevice;
{
  const char *z;

  z = qport->zlockname;
  if (z == NULL)
    {
      char *zalc;

      z = zdevice;
      if (z == NULL)
	{
	  z = qport->zname;
	  if (z == NULL)
	    return TRUE;
	}

#if ! HAVE_SVR4_LOCKFILES
      if (strncmp (z, "/dev/", sizeof "/dev/" - 1) == 0)
	z += sizeof "/dev/" - 1;
      zalc = (char *) alloca (strlen (z) + sizeof "LCK..");
      sprintf (zalc, "LCK..%s", z);
#if HAVE_SCO_LOCKFILES
      strlwr (zalc + sizeof "LCK.." - 1);
#endif
#else /* HAVE_SVR4_LOCKFILES */
      {
	struct stat s;

	if (*z != '/')
	  {
	    zalc = (char *) alloca (sizeof "/dev/" + strlen (z));
	    sprintf (zalc, "/dev/%s", z);
	    z = zalc;
	  }
	if (stat (z, &s) != 0)
	  {
	    ulog (LOG_ERROR, "stat (%s): %s", z, strerror (errno));
	    return FALSE;
	  }
	zalc = (char *) alloca (sizeof "LK.123.123.123");
	sprintf (zalc, "LK.%03d.%03d.%03d", major (s.st_dev),
		 major (s.st_rdev), minor (s.st_rdev));
      }
#endif /* HAVE_SVR4_LOCKFILES */

      z = zalc;
    }

  if (flok)
    return fsdo_lock (z, FALSE);
  else
    return fsdo_unlock (z, FALSE);
}

/* If we can mark a modem line in use, then when we lock a port we
   must open it and mark it in use.  We can't wait until the actual
   open because we can't fail out if it is locked then.  */

static boolean
fsserial_lock (qport, q, fin, zdevice)
     const struct sport *qport;
     struct ssysdep_serial_port *q;
     boolean fin;
     const char *zdevice;
{
  if (! fsserial_lockfile (TRUE, qport, zdevice))
    return FALSE;

#if HAVE_TIOCSINUSE || HAVE_TIOCEXCL
  /* Open the line and, if possible, mark it in use.  */
  {
    const char *z;
    int oread, iflag;

    z = zdevice;
    if (z == NULL)
      {
	z = qport->zname;
	if (z == NULL)
	  return TRUE;
      }

    if (fin)
      iflag = 0;
    else
      iflag = FILE_UNBLOCKED;

    if (*z != '/')
      {
	char *zcopy;

	zcopy = (char *) alloca (sizeof "/dev/" + strlen (z));
	sprintf (zcopy, "/dev/%s", z);
	z = zcopy;
      }

    oread = open (z, O_RDWR | iflag);
    if (oread < 0)
      {
	if (errno != EBUSY)
	  ulog (LOG_ERROR, "open (%s): %s", z, strerror (errno));
	(void) fsserial_lockfile (FALSE, qport, zdevice);
	return FALSE;
      }

#if HAVE_TIOCSINUSE
    /* If we can't mark it in use, return FALSE to indicate that the
       lock failed.  */
    if (ioctl (oread, TIOCSINUSE, 0) < 0)
      {
	if (errno != EALREADY)
	  ulog (LOG_ERROR, "ioctl (TIOCSINUSE): %s", strerror (errno));
	(void) close (oread);
	(void) fsserial_lockfile (FALSE, qport, zdevice);
	return FALSE;
      }
#endif

#ifdef TIOCSCTTY
    /* On BSD 4.4, make it our controlling terminal.  */
    (void) ioctl (oread, TIOCSCTTY, 0);
#endif

    q->oread = q->owrite = oread;
  }
#endif /* HAVE_TIOCSINUSE || HAVE_TIOCEXCL */

  return TRUE;
}

/* We don't need to lock a stdin port.  */

/*ARGSUSED*/
boolean
fsysdep_stdin_lock (qport, fin)
     struct sport *qport;
     boolean fin;
{
  return TRUE;
}

/* Lock a modem port.  */

boolean
fsysdep_modem_lock (qport, fin)
     struct sport *qport;
     boolean fin;
{
  return fsserial_lock (qport, &qport->u.smodem.s.s, fin,
			qport->u.smodem.zdevice);
}

/* Lock a direct port.  */

boolean
fsysdep_direct_lock (qport, fin)
     struct sport *qport;
     boolean fin;
{
  return fsserial_lock (qport, &qport->u.sdirect.s.s, fin,
			qport->u.sdirect.zdevice);
}

/* Open a serial line.  This sets the terminal settings.  We begin in
   seven bit mode and let the protocol change if necessary.  */

static struct sbaud_table
{
#if HAVE_POSIX_TERMIOS
  speed_t icode;
#else
  int icode;
#endif
  long ibaud;
} asSbaud_table[] =
{
  { B50, 50 },
  { B75, 75 },
  { B110, 110 },
  { B134, 134 },
  { B150, 150 },
  { B200, 200 },
  { B300, 300 },
  { B600, 600 },
  { B1200, 1200 },
  { B1800, 1800 },
  { B2400, 2400 },
  { B4800, 4800 },
  { B9600, 9600 },
#ifdef B19200
  { B19200, 19200 },
#else /* ! defined (B19200) */
#ifdef EXTA
  { EXTA, 19200 },
#endif /* EXTA */
#endif /* ! defined (B19200) */
#ifdef B38400
  { B38400, 38400 }
#else /* ! defined (B38400) */
#ifdef EXTB
  { EXTB, 38400 }
#endif /* EXTB */
#endif /* ! defined (B38400) */
};

#define CBAUD_TABLE (sizeof asSbaud_table / sizeof asSbaud_table[0])

#if HAVE_SYSV_TERMIO || HAVE_POSIX_TERMIOS
/* Hold the MIN value for the terminal to avoid setting it
   unnecessarily.  */
static int cSmin;
#endif /* HAVE_SYSV_TERMIO || HAVE_POSIX_TERMIOS */

static boolean
fsserial_open (z, ibaud, fwait, q)
     const char *z;
     long ibaud;
     boolean fwait;
     struct ssysdep_serial_port *q;
{
#if HAVE_POSIX_TERMIOS
  speed_t ib;
#else
  int ib;
#endif

  if (z == NULL)
    {
      const char *zport;
      boolean fdummy;

      zport = zsysdep_port_name (&fdummy);
      if (zport != NULL)
	ulog_device (zport);
    }
  else
    {
      if (strncmp (z, "/dev/", sizeof "/dev/" - 1) == 0)
	ulog_device (z + sizeof "/dev/" - 1);
      else
	ulog_device (z);
    }

  ib = B0;

  if (ibaud != 0)
    {
      int i;

      for (i = 0; i < CBAUD_TABLE; i++)
	if (asSbaud_table[i].ibaud == ibaud)
	  break;
      if (i >= CBAUD_TABLE)
	{
	  ulog (LOG_ERROR, "Unsupported baud rate %ld", ibaud);
	  if (q->oread != -1)
	    {
	      (void) close (q->oread);
	      if (q->oread != q->owrite)
		(void) close (q->owrite);
	    }
	  return FALSE;
	}
      ib = asSbaud_table[i].icode;
    }

  /* The port may have already been opened by the locking routine.  */
  if (q->oread == -1)
    {
      int oread, owrite;

      if (z == NULL)
	{
	  oread = 0;
	  owrite = 1;
	}
      else
	{
	  int iflag;

	  if (fwait)
	    iflag = 0;
	  else
	    iflag = FILE_UNBLOCKED;

	  if (*z != '/')
	    {
	      char *zcopy;

	      zcopy = (char *) alloca (sizeof "/dev/" + strlen (z));
	      sprintf (zcopy, "/dev/%s", z);
	      z = zcopy;
	    }

	  oread = open (z, O_RDWR | iflag);
	  if (oread < 0)
	    {
	      ulog (LOG_ERROR, "open (%s): %s", z, strerror (errno));
	      return FALSE;
	    }

#ifdef TIOCSCTTY
	  /* On BSD 4.4, make it our controlling terminal.  */
	  (void) ioctl (oread, TIOCSCTTY, 0);
#endif

	  owrite = oread;
	}

      q->oread = oread;
      q->owrite = owrite;
    }

  /* Make sure the ports are blocking.  */

  if (fcntl (q->oread, F_SETFL, 0) < 0
      || (q->oread != q->owrite
	  && fcntl (q->owrite, F_SETFL, 0) < 0))
    {
      ulog (LOG_ERROR, "fcntl: %s", strerror (errno));
      (void) close (q->oread);
      if (q->oread != q->owrite)
	(void) close (q->owrite);
      return FALSE;
    }

  q->fread_blocking = TRUE;

  if (! fgetterminfo (q->oread, &q->sorig))
    {
      q->fterminal = FALSE;
      return TRUE;
    }

  q->fterminal = TRUE;

  q->snew = q->sorig;

#if HAVE_BSD_TTY

  q->snew.sg_flags = RAW | ANYP;
  if (ibaud == 0)
    ib = q->snew.sg_ospeed;
  else
    {
      q->snew.sg_ispeed = ib;
      q->snew.sg_ospeed = ib;
    }

#ifdef TIOCHPCL
  /* When the file is closed, hang up the line.  This is a safety
     measure in case the program crashes.  */
  (void) ioctl (q->oread, TIOCHPCL, 0);
#endif

#ifdef TIOCFLUSH
  {
    int iparam;

    /* Flush pending input.  */
#ifdef FREAD
    iparam = FREAD;
#else
    iparam = 0;
#endif
    (void) ioctl (q->oread, TIOCFLUSH, &iparam);
  }
#endif /* TIOCFLUSH */

#endif /* HAVE_BSD_TTY */

#if HAVE_SYSV_TERMIO

  if (ibaud == 0)
    ib = q->snew.c_cflag & CBAUD;

  q->snew.c_iflag &=~ ICLEAR_IFLAG;
  q->snew.c_oflag &=~ ICLEAR_OFLAG;
  q->snew.c_cflag &=~ ICLEAR_CFLAG;
  q->snew.c_cflag |= (ib | ISET_CFLAG);
  q->snew.c_lflag &=~ ICLEAR_LFLAG;
  cSmin = 6;
  q->snew.c_cc[VMIN] = cSmin;
  q->snew.c_cc[VTIME] = 0;

#ifdef TCFLSH
  /* Flush pending input.  */
  (void) ioctl (q->oread, TCFLSH, 0);
#endif

#endif /* HAVE_SYSV_TERMIO */

#if HAVE_POSIX_TERMIOS

  if (ibaud == 0)
    ib = cfgetospeed (&q->snew);

  q->snew.c_iflag &=~ ICLEAR_IFLAG;
  q->snew.c_oflag &=~ ICLEAR_OFLAG;
  q->snew.c_cflag &=~ ICLEAR_CFLAG;
  q->snew.c_cflag |= ISET_CFLAG;
  q->snew.c_lflag &=~ ICLEAR_LFLAG;
  cSmin = 6;
  q->snew.c_cc[VMIN] = cSmin;
  q->snew.c_cc[VTIME] = 0;

  (void) cfsetospeed (&q->snew, ib);
  (void) cfsetispeed (&q->snew, ib);

  /* Flush pending input.  */
  (void) tcflush (q->oread, TCIFLUSH);

#endif /* HAVE_POSIX_TERMIOS */

  if (! fsetterminfo (q->oread, &q->snew))
    {
      ulog (LOG_ERROR, "Can't set terminal settings: %s", strerror (errno));
      (void) close (q->oread);
      if (q->oread != q->owrite)
	(void) close (q->owrite);
      return FALSE;
    }

  if (ibaud != 0)
    q->ibaud = ibaud;
  else
    {
      int i;

      q->ibaud = 1200;
      for (i = 0; i < CBAUD_TABLE; i++)
	{
	  if (asSbaud_table[i].icode == ib)
	    {
	      q->ibaud = asSbaud_table[i].ibaud;
	      break;
	    }
	}

      DEBUG_MESSAGE1 (DEBUG_PORT,
		      "fsserial_open: Baud rate is %ld", q->ibaud);
    }

  return TRUE;
}

/* Open a stdin port.  */

boolean
fsysdep_stdin_open (qport, ibaud, fwait)
     struct sport *qport;
     long ibaud;
     boolean fwait;
{
  return fsserial_open ((const char *) NULL, ibaud, fwait,
			&qport->u.sstdin.s.s);
}

/* Open a modem port.  */

boolean
fsysdep_modem_open (qport, ibaud, fwait)
     struct sport *qport;
     long ibaud;
     boolean fwait;
{
  const char *z;
  boolean fret;

  z = qport->u.smodem.zdevice;
  if (z == NULL)
    {
      z = qport->zname;
      if (z == NULL)
	{
	  ulog (LOG_ERROR, "Port has no name or device");
	  return FALSE;
	}
    }
  if (ibaud == 0)
    ibaud = qport->u.smodem.ibaud;
  fret = fsserial_open (z, ibaud, fwait, &qport->u.smodem.s.s);
  if (! fret)
    (void) fsserial_lockfile (FALSE, qport, qport->u.smodem.zdevice);
  return fret;
}

/* Open a direct port.  */

boolean
fsysdep_direct_open (qport, ibaud, fwait)
     struct sport *qport;
     long ibaud;
     boolean fwait;
{
  const char *z;
  boolean fret;

  z = qport->u.sdirect.zdevice;
  if (z == NULL)
    {
      z = qport->zname;
      if (z == NULL)
	{
	  ulog (LOG_ERROR, "Port has no name or device");
	  return FALSE;
	}
    }
  if (ibaud == 0)
    ibaud = qport->u.sdirect.ibaud;
  fret = fsserial_open (z, ibaud, fwait, &qport->u.sdirect.s.s);
  if (! fret)
    (void) fsserial_lockfile (FALSE, qport, qport->u.sdirect.zdevice);
  return fret;
}

/* Change the blocking status of the port.  We keep track of the
   current blocking status to avoid calling fcntl unnecessarily; fcntl
   turns out to be surprisingly expensive, at least on Ultrix.  We
   used to keep track of the blocking status of the read port and the
   write port independently.  Unfortunately, this is nonportable,
   because on BSD, and probably some other systems, unblocking a
   terminal applies to all descriptors to that terminal.  Now, if
   oread != owrite, we set both.  */

__inline__
static boolean
fsblock (qs, fblock)
     struct ssysdep_serial_port *qs;
     boolean fblock;
{
  if (fblock ? ! qs->fread_blocking : qs->fread_blocking)
    {
      int iset;

      if (fblock)
	iset = 0;
      else
	iset = FILE_UNBLOCKED;
      if (fcntl (qs->oread, F_SETFL, iset) < 0
	  || (qs->oread != qs->owrite
	      && fcntl (qs->owrite, F_SETFL, iset) < 0))
	{
	  ulog (LOG_ERROR, "fcntl: %s", strerror (errno));
	  return FALSE;
	}
      qs->fread_blocking = fblock;
    }
  return TRUE;
}

/* Close a serial port.  */

static boolean
fsserial_close (q)
     struct ssysdep_serial_port *q;
{
  if (q->oread >= 0)
    {
      /* Use a 30 second timeout to avoid hanging while draining
	 output.  */
      if (q->fterminal)
	{
	  fSalarm = FALSE;

	  if (fsysdep_catch ())
	    {
	      usysdep_start_catch ();
	      usset_signal (SIGALRM, usalarm, TRUE, (boolean *) NULL);
	      (void) alarm (30);

	      (void) fsetterminfodrain (q->oread, &q->sorig);
	    }

	  usset_signal (SIGALRM, SIG_IGN, TRUE, (boolean *) NULL);
	  (void) alarm (0);
	  usysdep_end_catch ();

	  /* If we timed out, use the non draining call.  Hopefully
	     this can't hang.  */
	  if (fSalarm)
	    (void) fsetterminfo (q->oread, &q->sorig);
	}

      (void) close (q->oread);
      if (q->oread != q->owrite)
	(void) close (q->owrite);
      q->oread = q->owrite = -1;

      /* Sleep for a second to give the terminal a chance to settle,
	 in case we are about to call out again.  */
      sleep (1);
    }

  return TRUE;
}

/* Close a stdin port.  */

/*ARGSUSED*/
boolean
fsysdep_stdin_close (qport, fsuccess)
     struct sport *qport;
     boolean fsuccess;
{
  return fsserial_close (&qport->u.sstdin.s.s);
}

/* Close a modem port.  */

boolean
fsysdep_modem_close (qport, fsuccess)
     struct sport *qport;
     boolean fsuccess;
{
  boolean fret;
  struct sdialer *qdial;
  struct sdialer sdial;

  fret = TRUE;
  qdial = NULL;

  /* We're no longer interested in carrier.  */
  (void) fsysdep_modem_no_carrier (qport);

  if (qport->u.smodem.zdialer != NULL)
    {
      char *zcopy;

      zcopy = (char *) alloca (strlen (qport->u.smodem.zdialer) + 1);
      strcpy (zcopy, qport->u.smodem.zdialer);
      zcopy[strcspn (zcopy, " \t")] = '\0';
      if (! fread_dialer_info (zcopy, &sdial))
	fret = FALSE;
      else
	qdial = &sdial;
    }
  else
    qdial = qport->u.smodem.qdialer;

  if (qdial != NULL)
    {
      boolean fsighup_ignored;
      HELD_SIG_MASK smask;
      int i;
      sig_atomic_t afhold[INDEXSIG_COUNT];
      const struct schat_info *qchat;

      /* The port I/O routines check whether any signal has been
	 received, and abort if one has.  While we are closing down
	 the modem, we don't care if we received a signal in the past,
	 but we do care if we receive a new signal (otherwise it would
	 be difficult to kill a uucico which was closing down a
	 modem).  We never care if we get SIGHUP at this point.  So we
	 turn off SIGHUP, remember what signals we've already seen,
	 and clear our notion of what signals we've seen.  We have to
	 block the signals while we remember and clear the array,
	 since we might otherwise miss a signal which occurred between
	 the copy and the clear (old systems can't block signals; they
	 will just have to suffer the race).  */

      usset_signal (SIGHUP, SIG_IGN, FALSE, &fsighup_ignored);
      smask = isblocksigs ();
      for (i = 0; i < INDEXSIG_COUNT; i++)
	{
	  afhold[i] = afSignal[i];
	  afSignal[i] = FALSE;
	}
      usunblocksigs (smask);

      if (fsuccess)
	qchat = &qdial->scomplete;
      else
	qchat = &qdial->sabort;
      if (! fchat (qchat, (const struct ssysteminfo *) NULL,
		   (const struct sdialer *) NULL, (const char *) NULL,
		   FALSE, qport->zname, qport->u.smodem.s.s.ibaud))
	fret = FALSE;

      /* Restore the old signal array and the SIGHUP handler.  It is
	 not necessary to block signals here, since all we are doing
	 is exactly what the signal handler itself would do if the
	 signal occurred.  */
      for (i = 0; i < INDEXSIG_COUNT; i++)
	if (afhold[i])
	  afSignal[i] = TRUE;
      if (! fsighup_ignored)
	usset_signal (SIGHUP, ussignal, TRUE, (boolean *) NULL);
    }

  if (! fsserial_close (&qport->u.smodem.s.s))
    fret = FALSE;

  if (! fsserial_lockfile (FALSE, qport, qport->u.smodem.zdevice))
    fret = FALSE;

  return fret;
}

/* Close a direct port.  */

/*ARGSUSED*/
boolean
fsysdep_direct_close (qport, fsuccess)
     struct sport *qport;
     boolean fsuccess;
{
  boolean fret;

  fret = fsserial_close (&qport->u.sdirect.s.s);

  if (! fsserial_lockfile (FALSE, qport, qport->u.sdirect.zdevice))
    fret = FALSE;

  return fret;
}

/* Reset a serial port by hanging up.  */

#if ! HAVE_POSIX_TERMIOS

static boolean
fsserial_reset (q)
     struct ssysdep_serial_port *q;
{
  sterminal sbaud;

  if (! q->fterminal)
    return TRUE;

  sbaud = q->snew;

#if HAVE_BSD_TTY
  sbaud.sg_ispeed = B0;
  sbaud.sg_ospeed = B0;
#else /* ! HAVE_BSD_TTY */
  sbaud.c_cflag = (sbaud.c_cflag &~ CBAUD) | B0;
#endif /* ! HAVE_BSD_TTY */

  if (! fsetterminfodrain (q->oread, &sbaud))
    ulog (LOG_FATAL, "Can't hangup terminal: %s", strerror (errno));

  /* Give the terminal a chance to settle.  */
  sleep (1);

  if (! fsetterminfo (q->oread, &q->snew))
    ulog (LOG_FATAL, "Can't reopen terminal: %s", strerror (errno));
  
  return TRUE;
}

#else /* HAVE_POSIX_TERMIOS */

static boolean
fsserial_reset (q)
     struct ssysdep_serial_port *q;
{
  sterminal sbaud;
  speed_t iin, iout;

  if (! q->fterminal)
    return TRUE;

  iin = cfgetispeed (&q->snew);
  iout = cfgetospeed (&q->snew);

  sbaud = q->snew;

  if (cfsetospeed (&sbaud, B0) != 0
      || ! fsetterminfodrain (q->oread, &sbaud))
    ulog (LOG_FATAL, "Can't hangup terminal: %s", strerror (errno));

  /* Give the terminal a chance to settle.  */
  sleep (1);

  if (cfsetispeed (&q->snew, iin) != 0
      || cfsetospeed (&q->snew, iout) != 0
      || ! fsetterminfo (q->oread, &q->snew))
    ulog (LOG_FATAL, "Can't reopen terminal: %s", strerror (errno));

  return TRUE;
}

#endif /* HAVE_POSIX_TERMIOS */

/* Reset a stdin port.  */

boolean
fsysdep_stdin_reset (qport)
     struct sport *qport;
{
  return fsserial_reset (&qport->u.sstdin.s.s);
}

/* Reset a modem port.  */

boolean
fsysdep_modem_reset (qport)
     struct sport *qport;
{
  return fsserial_reset (&qport->u.smodem.s.s);
}

/* Reset a direct port.  */

boolean
fsysdep_direct_reset (qport)
     struct sport *qport;
{
  return fsserial_reset (&qport->u.sdirect.s.s);
}

/* Begin dialing out on a modem port.  This opens the dialer device if
   there is one.  */

boolean
fsysdep_modem_begin_dial (qport, qdial)
     struct sport *qport;
     struct sdialer *qdial;
{
#ifdef TIOCMODEM
  /* If we can tell the modem to obey modem control, do so.  */
  {
    int iperm;

    iperm = 0;
    (void) ioctl (qport->u.smodem.s.s.oread, TIOCMODEM, &iperm);
  }
#endif /* TIOCMODEM */

#ifdef TIOCCDTR
  /* If we supposed to toggle DTR, do so.  */

  if (qdial->fdtr_toggle)
    {
      (void) ioctl (qport->u.smodem.s.s.oread, TIOCCDTR, 0);
      (void) ioctl (qport->u.smodem.s.s.oread, TIOCSDTR, 0);

      if (qdial->fdtr_toggle_wait)
	sleep (1);
    }
#endif /* TIOCCDTR */

  if (! fsysdep_modem_no_carrier (qport))
    return FALSE;

  /* Open the dial device if there is one.  */
  if (qport->u.smodem.zdial_device != NULL)
    {
      const char *z;
      int oread;

      qport->u.smodem.s.s.oholdread = qport->u.smodem.s.s.oread;
      qport->u.smodem.s.s.oholdwrite = qport->u.smodem.s.s.owrite;

      z = qport->u.smodem.zdial_device;
      if (*z != '/')
	{
	  char *zcopy;

	  zcopy = (char *) alloca (sizeof "/dev/" + strlen (z));
	  sprintf (zcopy, "/dev/%s", z);
	  z = zcopy;
	}

      oread = open (z, O_RDWR);
      if (oread < 0)
	{
	  ulog (LOG_ERROR, "open (%s): %s", z, strerror (errno));
	  return FALSE;
	}

      qport->u.smodem.s.s.oread = qport->u.smodem.s.s.owrite = oread;
    }

  return TRUE;
}

/* Tell the port to not require carrier.  I don't know how to do this
   on a BSD system that doesn't support TIOCNCAR, if there are any
   such systems.  */

boolean
fsysdep_modem_no_carrier (qport)
     struct sport *qport;
{
  if (! qport->u.smodem.s.s.fterminal)
    return TRUE;

#ifdef TIOCNCAR
  /* Tell the modem to ignore carrier.  */ 
  if (ioctl (qport->u.smodem.s.s.oread, TIOCNCAR, 0) < 0)
    {
      ulog (LOG_ERROR, "ioctl (TIOCNCAR): %s", strerror (errno));
      return FALSE;
    }
#endif /* TIOCNCAR */

#if HAVE_SYSV_TERMIO || HAVE_POSIX_TERMIOS
  /* Put the modem into local mode (ignore carrier) to start the chat
     script.  */
  qport->u.smodem.s.s.snew.c_cflag |= CLOCAL;
  if (! fsetterminfo (qport->u.smodem.s.s.oread,
		      &qport->u.smodem.s.s.snew))
    {
      ulog (LOG_ERROR, "Can't set CLOCAL: %s", strerror (errno));
      return FALSE;
    }
#endif /* HAVE_SYSV_TERMIO || HAVE_POSIX_TERMIOS */

  return TRUE;
}

/* Tell the port to require carrier.  If the port does not support
   carrier, we do nothing.  We do not need to worry whether the
   dialer supports carrier, since this will only be called when
   explicitly requested by a dialer chat script.  */

boolean
fsysdep_modem_need_carrier (qport)
     struct sport *qport;
{
  if (! qport->u.smodem.s.s.fterminal)
    return TRUE;

  if (qport->u.smodem.fcarrier)
    {
#ifdef TIOCCAR
      /* Tell the modem to pay attention to carrier.  */
      if (ioctl (qport->u.smodem.s.s.oread, TIOCCAR, 0) < 0)
	{
	  ulog (LOG_ERROR, "ioctl (TIOCCAR): %s", strerror (errno));
	  return FALSE;
	}
#endif /* TIOCCAR */

#if HAVE_SYSV_TERMIO || HAVE_POSIX_TERMIOS
      /* Put the modem into nonlocal mode.  */
      qport->u.smodem.s.s.snew.c_cflag &=~ CLOCAL;
      if (! fsetterminfo (qport->u.smodem.s.s.oread,
			  &qport->u.smodem.s.s.snew))
	{
	  ulog (LOG_ERROR, "Can't clear CLOCAL: %s", strerror (errno));
	  return FALSE;
	}
#endif /* HAVE_SYSV_TERMIO || HAVE_POSIX_TERMIOS */
    }

  return TRUE;
}

/* Finish dialing out on a modem by closing any dialer device and waiting
   for carrier.  */

boolean
fsysdep_modem_end_dial (qport, qdial)
     struct sport *qport;
     struct sdialer *qdial;
{
  if (qport->u.smodem.zdial_device != NULL)
    {
      (void) close (qport->u.smodem.s.s.oread);
      qport->u.smodem.s.s.oread = qport->u.smodem.s.s.oholdread;
      qport->u.smodem.s.s.owrite = qport->u.smodem.s.s.oholdwrite;
    }

  if (qport->u.smodem.fcarrier && qdial->fcarrier)
    {
      /* Tell the port that we need carrier.  */

      if (! fsysdep_modem_need_carrier (qport))
	return FALSE;

#ifdef TIOCWONLINE

      /* We know how to wait for carrier, so do so.  */

      /* If we already got a signal, just quit now.  */
      if (FGOT_QUIT_SIGNAL ())
	return FALSE;

      /* This bit of code handles signals just like fsserial_read
	 does.  See that function for a longer explanation.  */

      /* Use fsysdep_catch to handle a longjmp from the signal
	 handler.  */

      fSalarm = FALSE;

      if (fsysdep_catch ())
	{
	  /* Start catching SIGALRM; normally we ignore it.  */
	  usysdep_start_catch ();
	  usset_signal (SIGALRM, usalarm, TRUE, (boolean *) NULL);
	  (void) alarm (qdial->ccarrier_wait);

	  /* We really don't care if we get an error, since that will
	     probably just mean that TIOCWONLINE isn't supported in
	     which case there's nothing we can do anyhow.  If we get
	     SIGINT we want to keep waiting for carrier, because
	     SIGINT just means don't start any new sessions.  We don't
	     handle SIGINT correctly if we do a longjmp in the signal
	     handler; too bad.  */
	  while (ioctl (qport->u.smodem.s.s.oread, TIOCWONLINE, 0) < 0
		 && errno == EINTR)
	    {
	      /* Log the signal.  */
	      ulog (LOG_ERROR, (const char *) NULL);
	      if (FGOT_QUIT_SIGNAL () || fSalarm)
		break;
	    }
	}

      /* Turn off the pending SIGALRM and ignore SIGALARM again.  */
      usset_signal (SIGALRM, SIG_IGN, TRUE, (boolean *) NULL);
      (void) alarm (0);
      usysdep_end_catch ();

      /* If we got a random signal, just return FALSE.  */
      if (FGOT_QUIT_SIGNAL ())
	return FALSE;

      /* If we timed out, give an error.  */
      if (fSalarm)
	{
	  ulog (LOG_ERROR, "Timed out waiting for carrier");
	  return FALSE;
	}

#endif /* TIOCWONLINE */
    }

  return TRUE; 
}

/* Read data from a serial port, with a timeout.

   This function should return when we have read cmin characters or
   the timeout has occurred.  We have to work a bit to get UNIX to do
   this efficiently.  The simple implementation schedules a SIGALRM
   signal and then calls read; if there is a single character
   available, the call to read will return immediately, so there must
   be a loop which terminates when the SIGALRM is delivered or the
   correct number of characters has been read.  This can be very
   inefficient with a fast CPU or a low baud rate (or both!), since
   each call to read may return only one or two characters.

   Under POSIX or System V, we can specify a minimum number of
   characters to read, so there is no serious trouble.

   Under BSD, we figure out how many characters we have left to read,
   how long it will take for them to arrive at the current baud rate,
   and sleep that long.

   Doing this with a timeout and avoiding all possible race conditions
   get very hairy, though.  Basically, we're going to schedule a
   SIGALRM for when the timeout expires.  I don't really want to do a
   longjmp in the SIGALRM handler, though, because that may lose data.
   Therefore, I have the signal handler set a variable.  However, this
   means that there will be a span of time between the time the code
   checks the variable and the time it calls the read system call; if
   the SIGALRM occurs during that time, the read might hang forever.
   To avoid this, the SIGALRM handler not only sets a global variable,
   it also schedules another SIGALRM for one second in the future
   (POSIX specifies that a signal handler is permitted to safely call
   alarm).  To avoid getting a continual sequence of SIGALRM
   interrupts, we change the signal handler to ignore SIGALRM when
   we're about to exit the function.  This means that every time we
   execute fsserial_read we make at least five system calls.  It's the
   best I've been able to come up with, though.

   When fsserial_read finishes, there will be no SIGALRM scheduled and
   SIGALRM will be ignored.  */

static boolean
fsserial_read (q, zbuf, pclen, cmin, ctimeout, freport, fpty)
     struct ssysdep_serial_port *q;
     char *zbuf;
     int *pclen;
     int cmin;
     int ctimeout;
     boolean freport;
     boolean fpty;
{
  CATCH_PROTECT int cwant;
  boolean fret;

  cwant = *pclen;
  *pclen = 0;

  /* Guard against a bad timeout.  We return TRUE when a timeout
     expires.  It is possible to get a negative timeout here because
     the calling code does not check user supplied timeouts for
     plausibility.  */
  if (ctimeout <= 0)
    return TRUE;

  /* We want to do a blocking read.  */
  if (! fsblock (q, TRUE))
    return FALSE;

  fSalarm = FALSE;

  /* We're going to set up an alarm signal to last for the entire
     read.  If the read system call cannot be interrupted, the signal
     handler will do a longjmp causing fsysdep_catch (a macro) to
     return FALSE.  We handle that here.  If read can be interrupted,
     fsysdep_catch will be defined to TRUE.  */

  if (fsysdep_catch ())
    {
      /* Prepare to catch SIGALRM and schedule the signal.  */
      usysdep_start_catch ();
      usset_signal (SIGALRM, usalarm, TRUE, (boolean *) NULL);
      alarm (ctimeout);
    }
  else
    {
      /* We caught a signal.  We don't actually have to do anything,
	 as all the appropriate checks are made at the start of the
	 following loop.  */
    }

  fret = FALSE;

  while (TRUE)
    {
      int cgot;

#if HAVE_SYSV_TERMIO || HAVE_POSIX_TERMIOS
      /* If we can tell the terminal not to return until we have a
	 certain number of characters, do so.  */
      if (q->fterminal)
	{
	  int csetmin;

	  /* I'm not that confident about setting MIN to values larger
	     than 127, although up to 255 would probably work.  */
	  if (cmin < 127)
	    csetmin = cmin;
	  else
	    csetmin = 127;

	  if (csetmin != cSmin)
	    {
	      q->snew.c_cc[VMIN] = csetmin;
	      if (! fsetterminfo (q->oread, &q->snew))
		{
		  int ierr;

		  /* We turn off the signal before reporting the error
		     to minimize any problems with interrupted system
		     calls.  */
		  ierr = errno;
		  usset_signal (SIGALRM, SIG_IGN, TRUE, (boolean *) NULL);
		  alarm (0);
		  usysdep_end_catch ();
		  ulog (LOG_ERROR, "Can't set MIN for terminal: %s",
			strerror (ierr));
		  return FALSE;
		}
	      cSmin = csetmin;
	    }
	}
#endif /* HAVE_SYSV_TERMIO || HAVE_POSIX_TERMIOS */

      /* If we've received a signal, get out now.  */
      if (FGOT_QUIT_SIGNAL ())
	break;

      /* If we've already gotten a SIGALRM, get out with whatever
	 we've accumulated.  */

      if (fSalarm)
	{
	  fret = TRUE;
	  break;
	}

      /* Right here is the race condition which we avoid by having the
	 SIGALRM handler schedule another SIGALRM.  */

      cgot = read (q->oread, zbuf, cwant);

      /* If the read returned an error, check for signals.  */
      if (cgot < 0)
	{
	  if (errno == EINTR)
	    {
	      /* Log the signal.  */
	      ulog (LOG_ERROR, (const char *) NULL);
	    }
	  if (fSalarm)
	    {
	      fret = TRUE;
	      break;
	    }
	  if (FGOT_QUIT_SIGNAL ())
	    break;
	}

      /* If read returned an error, get out.  We just ignore EINTR
	 here, since it must be from some signal we don't care about.
	 If the read returned 0 then the line must have been hung up
	 (normally we would have received SIGHUP, but we can't count
	 on that).  We turn off the signals before calling ulog to
	 reduce problems with interrupted system calls.  */
      if (cgot <= 0)
	{
	  if (cgot < 0 && errno == EINTR)
	    cgot = 0;
	  else
	    {
	      int ierr;

	      ierr = errno;

	      usset_signal (SIGALRM, SIG_IGN, TRUE, (boolean *) NULL);
	      alarm (0);
	      usysdep_end_catch ();

	      if (freport)
		{
		  if (cgot == 0)
		    ulog (LOG_ERROR, "Line disconnected");
		  else
		    ulog (LOG_ERROR, "read: %s", strerror (ierr));
		}

	      return FALSE;
	    }
	}

      cwant -= cgot;
      cmin -= cgot;
      zbuf += cgot;
      *pclen += cgot;

      /* If we have enough data, get out now.  */

      if (cmin <= 0)
	{
	  fret = TRUE;
	  break;
	}

#if HAVE_BSD_TTY
      /* We still want more data, so sleep long enough for the rest of
	 it to arrive.  We don't this for System V or POSIX because
	 setting MIN is good enough (we can't sleep longer than it
	 takes to get MAX_INPUT characters anyhow).

	 The baud rate is approximately 10 times the number of
	 characters which will arrive in one second, so the number of
	 milliseconds to sleep ==
	 characters * (milliseconds / character) ==
	 characters * (1000 * (seconds / character)) ==
	 characters * (1000 * (1 / (baud / 10))) ==
	 characters * (10000 / baud)

	 We arbitrarily reduce the sleep amount by 10 milliseconds to
	 attempt to account for the amount of time it takes to set up
	 the sleep.  This is how long it takes to get half a character
	 at 19200 baud.  We then don't bother to sleep for less than
	 10 milliseconds.  We don't sleep if the read was interrupted.

	 We use select to sleep.  It would be easy to use poll as
	 well, but it's unlikely that any system with BSD ttys would
	 have poll but not select.  Using select avoids hassles with
	 the pending SIGALRM; if it hits the select will be
	 interrupted, and otherwise the select will not affect it.  */

#if ! HAVE_SELECT
 #error This code requires select; feel free to extend it
#endif

      if (q->fterminal && ! fpty && cmin > 1 && cgot > 0)
	{
	  int csleepchars;
	  int isleep;

	  /* We don't try to read all the way up to MAX_INPUT,
	     since that might drop a character.  */

	  if (cmin <= MAX_INPUT - 10)
	    csleepchars = cmin;
	  else
	    csleepchars = MAX_INPUT - 10;

	  isleep = (int) (((long) csleepchars * 10000L) / q->ibaud);
	  isleep -= 10;

	  if (isleep > 10)
	    {
	      struct timeval s;

	      s.tv_sec = isleep / 1000;
	      s.tv_usec = (isleep % 1000) * 1000;

	      /* Some versions of select take a pointer to an int,
		 while some take a pointer to an fd_set.  I just cast
		 the arguments to a generic pointer, and assume that
		 any machine which distinguishes int * from fd_set *
		 (I would be amazed if there are any such machines)
		 have an appropriate prototype somewhere or other.  */
	      (void) select (0, (pointer) NULL, (pointer) NULL,
			     (pointer) NULL, &s);

	      /* Here either the select finished sleeping or we got a
		 SIGALRM.  If the latter occurred, fSalarm was set to
		 TRUE; it will be checked at the top of the loop.  */
	    }
	}
#endif /* HAVE_BSD_TTY */
    }

  /* Turn off the pending SIGALRM and return.  */

  usset_signal (SIGALRM, SIG_IGN, TRUE, (boolean *) NULL);
  alarm (0);
  usysdep_end_catch ();

  return fret;
}

/* Read from a stdin port.  */

boolean
fsysdep_stdin_read (qport, zbuf, pclen, cmin, ctimeout, freport)
     struct sport *qport;
     char *zbuf;
     int *pclen;
     int cmin;
     int ctimeout;
     boolean freport;
{
  return fsserial_read (&qport->u.sstdin.s.s, zbuf, pclen, cmin, ctimeout,
			freport, qport->u.sstdin.s.fpty);
}

/* Read from a modem port.  */

boolean
fsysdep_modem_read (qport, zbuf, pclen, cmin, ctimeout, freport)
     struct sport *qport;
     char *zbuf;
     int *pclen;
     int cmin;
     int ctimeout;
     boolean freport;
{
  return fsserial_read (&qport->u.smodem.s.s, zbuf, pclen, cmin, ctimeout,
			freport, FALSE);
}

/* Read from a direct port.  */

boolean
fsysdep_direct_read (qport, zbuf, pclen, cmin, ctimeout, freport)
     struct sport *qport;
     char *zbuf;
     int *pclen;
     int cmin;
     int ctimeout;
     boolean freport;
{
  return fsserial_read (&qport->u.sdirect.s.s, zbuf, pclen, cmin, ctimeout,
			freport, FALSE);
}

/* Write data to a serial port.  */

static boolean
fsserial_write (q, zwrite, cwrite)
     struct ssysdep_serial_port *q;
     const char *zwrite;
     int cwrite;
{
  int czero;

  /* We want blocking writes here.  */
  if (! fsblock (q, TRUE))
    return FALSE;

  czero = 0;

  while (cwrite > 0)
    {
      int cdid;

      /* If we've received a signal, don't continue.  */
      if (FGOT_QUIT_SIGNAL ())
	return FALSE;

      /* Loop until we don't get an interrupt.  */
      while ((cdid = write (q->owrite, zwrite, cwrite)) < 0
	     && errno == EINTR)
	{
	  /* Log the signal.  */
	  ulog (LOG_ERROR, (const char *) NULL);
	  if (FGOT_QUIT_SIGNAL ())
	    return FALSE;
	}

      if (cdid < 0)
	{
	  if (errno != EWOULDBLOCK && errno != EAGAIN)
	    {
	      ulog (LOG_ERROR, "write: %s", strerror (errno));
	      return FALSE;
	    }
	  cdid = 0;
	}

      if (cdid == 0)
	{
	  /* On some systems write will return 0 if carrier is lost.
	     If we fail to write anything ten times in a row, we
	     assume that this has happened.  This is hacked in like
	     this because there seems to be no reliable way to tell
	     exactly why the write returned 0.  */
	  ++czero;
	  if (czero >= 10)
	    {
	      ulog (LOG_ERROR, "Line disconnected");
	      return FALSE;
	    }
	}
      else
	{
	  czero = 0;

	  cwrite -= cdid;
	  zwrite += cdid;
	}
    }

  return TRUE;
}

/* Write to a stdin port.  */

boolean
fsysdep_stdin_write (qport, zwrite, cwrite)
     struct sport *qport;
     const char *zwrite;
     int cwrite;
{
  return fsserial_write (&qport->u.sstdin.s.s, zwrite, cwrite);
}

/* Write to a modem port.  */

boolean
fsysdep_modem_write (qport, zwrite, cwrite)
     struct sport *qport;
     const char *zwrite;
     int cwrite;
{
  return fsserial_write (&qport->u.smodem.s.s, zwrite, cwrite);
}

/* Write to a direct port.  */

boolean
fsysdep_direct_write (qport, zwrite, cwrite)
     struct sport *qport;
     const char *zwrite;
     int cwrite;
{
  return fsserial_write (&qport->u.sdirect.s.s, zwrite, cwrite);
}

/* The fsysdep_io routine is supposed to both read and write data
   until it has either filled its read buffer or written out all the
   data it was given.  This lets us write out large packets without
   losing incoming data.  */

static boolean
fsserial_io (q, zwrite, pcwrite, zread, pcread)
     struct ssysdep_serial_port *q;
     const char *zwrite;
     int *pcwrite;
     char *zread;
     int *pcread;
{
  int cwrite, cread, czero;

  cwrite = *pcwrite;
  *pcwrite = 0;
  cread = *pcread;
  *pcread = 0;

  czero = 0;

  while (TRUE)
    {
      int cgot, cdo, cdid;

      /* If we've received a signal, don't continue.  */
      if (FGOT_QUIT_SIGNAL ())
	return FALSE;

      /* This used to always use nonblocking writes, but it turns out
	 that some systems don't support them on terminals.

	 The current algorithm is:
	     loop:
	       unblocked read
	       if read buffer full, return
	       if nothing to write, return
	       if HAVE_UNBLOCKED_WRITES
	         write all data
	       else
	         write up to SINGLE_WRITE bytes
	       if all data written, return
	       if no data written
	         blocked write of up to SINGLE_WRITE bytes

	 This algorithm should work whether the system supports
	 unblocked writes on terminals or not.  If the system supports
	 unblocked writes but HAVE_UNBLOCKED_WRITES is 0, then it will
	 call write more often than it needs to.  If the system does
	 not support unblocked writes but HAVE_UNBLOCKED_WRITES is 1,
	 then the write may hang so long that incoming data is lost.
	 This is actually possible at high baud rates on any system
	 when a blocking write is done; there is no solution, except
	 hardware handshaking.  */

      /* Do an unblocked read.  */

      if (! fsblock (q, FALSE))
	return FALSE;

      /* Loop until we get something (error or data) other than an
	 acceptable EINTR.  */
      while ((cgot = read (q->oread, zread, cread)) < 0
	     && errno == EINTR)
	{
	  /* Log the signal.  */
	  ulog (LOG_ERROR, (const char *) NULL);
	  if (FGOT_QUIT_SIGNAL ())
	    return FALSE;
	}

      if (cgot < 0)
	{
	  if (errno != EAGAIN && errno != EWOULDBLOCK)
	    {
	      ulog (LOG_ERROR, "read: %s", strerror (errno));
	      return FALSE;
	    }
	  cgot = 0;
	}

      cread -= cgot;
      zread += cgot;
      *pcread += cgot;

      /* If we've filled the read buffer, or we have nothing left to
	 write, return out.  */

      if (cread <= 0 || cwrite <= 0)
	return TRUE;

      /* The port is currently unblocked.  Do a write.  */

      cdo = cwrite;

#if ! HAVE_UNBLOCKED_WRITES
      if (cdo > SINGLE_WRITE)
	cdo = SINGLE_WRITE;
#endif

      /* Loop until we get something besides EINTR.  */
      while ((cdid = write (q->owrite, zwrite, cdo)) < 0
	     && errno == EINTR)
	{
	  /* Log the signal.  */
	  ulog (LOG_ERROR, (const char *) NULL);
	  if (FGOT_QUIT_SIGNAL ())
	    return FALSE;
	}

      if (cdid < 0)
	{
	  if (errno != EWOULDBLOCK && errno != EAGAIN)
	    {
	      ulog (LOG_ERROR, "write: %s", strerror (errno));
	      return FALSE;
	    }
	  cdid = 0;
	}

      if (cdid > 0)
	{
	  /* We wrote some data.  If we wrote everything, return out.
	     Otherwise loop around and do another read.  */
	  cwrite -= cdid;
	  zwrite += cdid;
	  *pcwrite += cdid;

	  if (cwrite <= 0)
	    return TRUE;

	  czero = 0;
	}
      else
	{
	  /* We didn't write any data.  Do a blocking write.  */

	  if (! fsblock (q, TRUE))
	    return FALSE;

	  cdo = cwrite;
	  if (cdo > SINGLE_WRITE)
	    cdo = SINGLE_WRITE;

	  DEBUG_MESSAGE1 (DEBUG_PORT,
			  "fsserial_io: Blocking write of %d", cdo);

	  /* Loop until we get something besides EINTR.  */
	  while ((cdid = write (q->owrite, zwrite, cdo)) < 0
		 && errno == EINTR)
	    {
	      /* Log the signal.  */
	      ulog (LOG_ERROR, (const char *) NULL);
	      if (FGOT_QUIT_SIGNAL ())
		return FALSE;
	    }
	  
	  if (cdid < 0)
	    {
	      ulog (LOG_ERROR, "write: %s", strerror (errno));
	      return FALSE;
	    }

	  if (cdid == 0)
	    {
	      /* On some systems write will return 0 if carrier is
		 lost.  If we fail to write anything ten times in a
		 row, we assume that this has happened.  This is
		 hacked in like this because there seems to be no
		 reliable way to tell exactly why the write returned
		 0.  */
	      ++czero;
	      if (czero >= 10)
		{
		  ulog (LOG_ERROR, "Line disconnected");
		  return FALSE;
		}
	    }
	  else
	    {
	      cwrite -= cdid;
	      zwrite += cdid;
	      *pcwrite += cdid;
	      czero = 0;
	    }
	}
    }
}

/* I/O to a stdin port.  */

boolean
fsysdep_stdin_io (qport, zwrite, pcwrite, zread, pcread)
     struct sport *qport;
     const char *zwrite;
     int *pcwrite;
     char *zread;
     int *pcread;
{
  return fsserial_io (&qport->u.sstdin.s.s, zwrite, pcwrite, zread, pcread);
}

/* I/O to a modem port.  */

boolean
fsysdep_modem_io (qport, zwrite, pcwrite, zread, pcread)
     struct sport *qport;
     const char *zwrite;
     int *pcwrite;
     char *zread;
     int *pcread;
{
  return fsserial_io (&qport->u.smodem.s.s, zwrite, pcwrite, zread, pcread);
}

/* I/O to a direct port.  */

boolean
fsysdep_direct_io (qport, zwrite, pcwrite, zread, pcread)
     struct sport *qport;
     const char *zwrite;
     int *pcwrite;
     char *zread;
     int *pcread;
{
  return fsserial_io (&qport->u.sdirect.s.s, zwrite, pcwrite, zread, pcread);
}

/* Send a break character to a serial port.  */

static boolean
fsserial_break (q)
     struct ssysdep_serial_port *q;
{
#if HAVE_BSD_TTY
  ioctl (q->owrite, TIOCSBRK, 0);
  sleep (1);
  ioctl (q->owrite, TIOCCBRK, 0);
  return TRUE;
#endif /* HAVE_BSD_TTY */
#if HAVE_SYSV_TERMIO
  ioctl (q->owrite, TCSBRK, 0);
  return TRUE;
#endif /* HAVE_SYSV_TERMIO */
#if HAVE_POSIX_TERMIOS
  return tcsendbreak (q->owrite, 0) == 0;
#endif /* HAVE_POSIX_TERMIOS */
}

/* Send a break character to a stdin port.  */

boolean
fsysdep_stdin_break (qport)
     struct sport *qport;
{
  return fsserial_break (&qport->u.sstdin.s.s);
}

/* Send a break character to a modem port.  */

boolean
fsysdep_modem_break (qport)
     struct sport *qport;
{
  return fsserial_break (&qport->u.smodem.s.s);
}

/* Send a break character to a direct port.  */

boolean
fsysdep_direct_break (qport)
     struct sport *qport;
{
  return fsserial_break (&qport->u.sdirect.s.s);
}

/* Change the setting of a serial port.  */

static boolean
fsserial_set (q, tset)
     struct ssysdep_serial_port *q;
     enum tportsetting tset;
{
  if (! q->fterminal)
    return TRUE;

  switch (tset)
    {
    case PORTSETTING_EIGHT:
#if HAVE_BSD_TTY
      if (q->snew.sg_flags == (RAW | ANYP))
	return TRUE;
      q->snew.sg_flags = RAW | ANYP;
#endif
#if HAVE_SYSV_TERMIO || HAVE_POSIX_TERMIOS
      if ((q->snew.c_iflag & ICLEAR_IFLAG) == 0)
	return TRUE;
      q->snew.c_iflag &=~ ICLEAR_IFLAG;
#endif
      if (! fsetterminfodrain (q->oread, &q->snew))
	{
	  ulog (LOG_ERROR, "Can't go to raw mode: %s", strerror (errno));
	  return FALSE;
	}
      return TRUE;

    case PORTSETTING_SEVEN:
#if HAVE_BSD_TTY
      if (q->snew.sg_flags == (CBREAK | ANYP | TANDEM))
	return TRUE;
      q->snew.sg_flags = CBREAK | ANYP | TANDEM;
#endif /* HAVE_BSD_TTY */
#if HAVE_SYSV_TERMIO | HAVE_POSIX_TERMIOS
      {
	int iwant;

#ifdef CRTSCTS
	/* It would be nice to do this is in a more portable fashion,
	   but in any case this is apparently correct for SunOS.  If
	   we are doing hardware flow control, we don't also send
	   start and stop characters; however, we do recognize
	   incoming start and stop characters.  */
	if ((q->snew.c_cflag & CRTSCTS) != 0)
	  iwant = ISTRIP | IXON;
	else
	  iwant = ISTRIP | IXON | IXOFF;
#else /* ! defined (CRTSCTS) */
	iwant = ISTRIP | IXON | IXOFF;
#endif /* ! defined (CRTSCTS) */

	if ((q->snew.c_iflag & ICLEAR_IFLAG) == iwant)
	  return TRUE;

	q->snew.c_iflag &=~ ICLEAR_IFLAG;
	q->snew.c_iflag |= iwant;
      }
#endif /* HAVE_SYSV_TERMIO | HAVE_POSIX_TERMIOS */

      if (! fsetterminfodrain (q->oread, &q->snew))
	{
	  ulog (LOG_ERROR, "Can't go to seven bit mode: %s",
		strerror (errno));
	  return FALSE;
	}
      return TRUE;

    default:
#if DEBUG > 0
      ulog (LOG_FATAL, "fsserial_set: Can't happen");
#endif
      return FALSE;
    }
}

/* Change settings of a stdin port.  */

boolean
fsysdep_stdin_set (qport, tset)
     struct sport *qport;
     enum tportsetting tset;
{
  return fsserial_set (&qport->u.sstdin.s.s, tset);
}

/* Change settings of a modem port.  */

boolean
fsysdep_modem_set (qport, tset)
     struct sport *qport;
     enum tportsetting tset;
{
  return fsserial_set (&qport->u.smodem.s.s, tset);
}

/* Change settings of a direct port.  */

boolean
fsysdep_direct_set (qport, tset)
     struct sport *qport;
     enum tportsetting tset;
{
  return fsserial_set (&qport->u.sdirect.s.s, tset);
}

/* Run a chat program.  */

static boolean
fsrun_chat (oread, owrite, zprog)
     int oread;
     int owrite;
     const char *zprog;
{
  int cargs;
  const char **azargs;
  char *zcopy, *zarg;
  int aidescs[3];
  FILE *e;
  pid_t ipid;
  char *z;

  /* Get the arguments into an array to pass to isspawn.  */
  zcopy = (char *) alloca (strlen (zprog) + 1);
  strcpy (zcopy, zprog);
  cargs = 0;
  for (zarg = strtok (zcopy, " \t");
       zarg != NULL;
       zarg = strtok ((char *) NULL, " \t"))
    ++cargs;

  azargs = (const char **) alloca ((cargs + 1) * sizeof (const char *));

  strcpy (zcopy, zprog);
  cargs = 0;
  for (zarg = strtok (zcopy, " \t");
       zarg != NULL;
       zarg = strtok ((char *) NULL, " \t"))
    {
      azargs[cargs] = zarg;
      ++cargs;
    }
  azargs[cargs] = NULL;

  aidescs[0] = oread;
  aidescs[1] = owrite;
  aidescs[2] = SPAWN_READ_PIPE;

  /* Pass fkeepuid, fkeepenv and fshell as TRUE.  This puts the
     responsibility of maintaing security on the chat program.  */
  ipid = isspawn (azargs, aidescs, TRUE, TRUE, (const char *) NULL,
		  FALSE, TRUE, (const char *) NULL,
		  (const char *) NULL, (const char *) NULL);
  if (ipid < 0)
    {
      ulog (LOG_ERROR, "isspawn (%s): %s", azargs[0], strerror (errno));
      return FALSE;
    }

  e = fdopen (aidescs[2], (char *) "r");
  if (e == NULL)
    {
      ulog (LOG_ERROR, "fdopen: %s", strerror (errno));
      (void) close (aidescs[2]);
      (void) kill (ipid, SIGKILL);
      (void) iswait ((unsigned long) ipid, (const char *) NULL);
      return FALSE;
    }

  /* The FILE e now is attached to stderr of the program.  Forward
     every line the program outputs to the log file.  */
  while ((z = zfgets (e, FALSE)) != NULL)
    {
      int clen;

      clen = strlen (z);
      if (z[clen - 1] == '\n')
	z[clen - 1] = '\0';
      if (*z != '\0')
	ulog (LOG_NORMAL, "chat: %s", z);
      xfree ((pointer) z);
    }

  (void) fclose (e);

  return iswait ((unsigned long) ipid, "Chat program") == 0;
}

/* Run a chat program on a stdin port.  */

boolean
fsysdep_stdin_chat (qport, zprog)
     struct sport *qport;
     const char *zprog;
{
  return fsrun_chat (qport->u.sstdin.s.s.oread,
		     qport->u.sstdin.s.s.owrite,
		     zprog);
}

/* Run a chat program on a modem port.  */

boolean
fsysdep_modem_chat (qport, zprog)
     struct sport *qport;
     const char *zprog;
{
  return fsrun_chat (qport->u.smodem.s.s.oread,
		     qport->u.smodem.s.s.owrite,
		     zprog);
}

/* Run a chat program on a direct port.  */

boolean
fsysdep_direct_chat (qport, zprog)
     struct sport *qport;
     const char *zprog;
{
  return fsrun_chat (qport->u.sdirect.s.s.oread,
		     qport->u.sdirect.s.s.owrite,
		     zprog);
}

#if HAVE_TCP

/* Run a chat program on a TCP port.  */

boolean
fsysdep_tcp_chat (qport, zprog)
     struct sport *qport;
     const char *zprog;
{
  return fsrun_chat (qport->u.stcp.o, qport->u.stcp.o, zprog);
}

#endif /* HAVE_TCP */

/* Functions to return baud rates.  */

/* Return baud rate of a stdin port.  */

long
isysdep_stdin_baud (qport)
     struct sport *qport;
{
  return qport->u.sstdin.s.s.ibaud;
}

/* Return baud rate of a modem port.  */

long
isysdep_modem_baud (qport)
     struct sport *qport;
{
  return qport->u.smodem.s.s.ibaud;
}

/* Return baud rate of a direct port.  */

long
isysdep_direct_baud (qport)
     struct sport *qport;
{
  return qport->u.sdirect.s.s.ibaud;
}

#if HAVE_TCP

/* Some system dependent routines for TCP ports.  These work by
   setting up an ssysdep_serial_port structure to fake out the serial
   port routines.  I'm doing it this way to avoid having to write the
   complicated timeout code twice, and because the serial port code
   will work fine.  It does mean that if the serial port code changes
   this code will have to be considered.  */

/* Read data from a TCP port.  */

boolean
fsysdep_tcp_read (qport, zread, pclen, cmin, ctimeout, freport)
     struct sport *qport;
     char *zread;
     int *pclen;
     int cmin;
     int ctimeout;
     boolean freport;
{
  struct ssysdep_serial_port s;

  s.oread = s.owrite = qport->u.stcp.o;
  s.fread_blocking = TRUE;
  s.fterminal = FALSE;
  return fsserial_read (&s, zread, pclen, cmin, ctimeout, freport, FALSE);
}

/* Write data to a TCP port.  */

boolean
fsysdep_tcp_write (qport, zwrite, cwrite)
     struct sport *qport;
     const char *zwrite;
     int cwrite;
{
  struct ssysdep_serial_port s;

  s.oread = s.owrite = qport->u.stcp.o;
  s.fread_blocking = TRUE;
  s.fterminal = FALSE;
  return fsserial_write (&s, zwrite, cwrite);
}

/* Read and write data to and from a TCP port.  We actually don't
   bother to really implement this, since the system will buffer up
   plenty of TCP data (only 256 bytes are buffered for a terminal,
   so losing data becomes a real possibility).  */

boolean
fsysdep_tcp_io (qport, zwrite, pcwrite, zread, pcread)
     struct sport *qport;
     const char *zwrite;
     int *pcwrite;
     char *zread;
     int *pcread;
{
  struct ssysdep_serial_port s;

  s.oread = s.owrite = qport->u.stcp.o;
  s.fread_blocking = TRUE;
  s.fterminal = FALSE;
  *pcread = 0;
  return fsserial_write (&s, zwrite, *pcwrite);
}

#endif /* HAVE_TCP */

/*
  Local variables:
  mode:c
  End:
  */
