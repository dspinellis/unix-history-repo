/* sys1.unx
   The basic system dependent routines for UNIX.

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

   $Log:	sys1.unx,v $
# Revision 1.2  92/05/13  05:42:07  rich
# ported to 386bsd
# 
# Revision 1.1  1992/05/10  17:36:33  rich
# Initial revision
#
   Revision 1.68  1992/04/03  05:37:11  ian
   Minor cleanups for gcc 2.1

   Revision 1.67  1992/03/31  23:53:34  ian
   Use $PWD to get the current directory if it's defined and correct

   Revision 1.66  1992/03/31  23:42:59  ian
   Brian W. Antoine: use name from getpwnam rather than getlogin

   Revision 1.65  1992/03/26  17:17:25  ian
   Gerben Wierda: various cleanups

   Revision 1.64  1992/03/18  06:00:25  ian
   Open the controlling terminal in non delay mode

   Revision 1.63  1992/03/17  15:35:28  ian
   Log signals when they happen, even if we continue looping

   Revision 1.62  1992/03/17  01:28:18  ian
   Undefine remove in uucp.h if ! HAVE_REMOVE

   Revision 1.61  1992/03/16  22:40:01  ian
   Undefine remove before function definition

   Revision 1.60  1992/03/16  22:22:35  ian
   Adjusted external declarations

   Revision 1.59  1992/03/16  22:01:58  ian
   Don't declare sigemptyset

   Revision 1.58  1992/03/16  01:23:08  ian
   Make blocking writes optional

   Revision 1.57  1992/03/15  04:51:17  ian
   Keep an array of signals we've received rather than a single variable

   Revision 1.56  1992/03/15  01:54:46  ian
   All execs are now done in isspawn, all waits are done in iswait

   Revision 1.55  1992/03/12  19:54:43  ian
   Debugging based on types rather than number

   Revision 1.54  1992/03/11  02:09:57  ian
   Franc,ois Pinard: retry fork several times before giving up

   Revision 1.53  1992/03/11  00:18:50  ian
   Save temporary file if file send fails

   Revision 1.52  1992/03/08  02:06:28  ian
   Let setpgrp fail silently

   Revision 1.51  1992/03/04  01:40:51  ian
   Thomas Fischer: tweaked a bit for the NeXT

   Revision 1.50  1992/03/03  21:01:20  ian
   Use strict timeout in fsserial_read, eliminate all race conditions

   Revision 1.49  1992/02/29  01:06:59  ian
   Chip Salzenberg: recheck file permissions before sending

   Revision 1.48  1992/02/28  05:06:15  ian
   T. William Wells: fsysdep_catch must be a macro

   Revision 1.47  1992/02/27  19:51:09  ian
   Added some new extern definitions

   Revision 1.46  1992/02/27  05:40:54  ian
   T. William Wells: detach from controlling terminal, handle signals safely

   Revision 1.45  1992/02/24  20:07:43  ian
   John Theus: some systems don't have <fcntl.h>

   Revision 1.44  1992/02/24  04:58:47  ian
   Only permit files to be received into directories that are world-writeable

   Revision 1.43  1992/02/23  03:26:51  ian
   Overhaul to use automatic configure shell script

   Revision 1.42  1992/02/19  19:36:07  ian
   Rearranged time functions

   Revision 1.41  1992/02/09  05:10:50  ian
   Added HAVE_MKDIR configuration parameter and mkdir emulation

   Revision 1.40  1992/02/09  03:14:48  ian
   Added HAVE_OLD_DIRECTORIES for systems without readdir routines

   Revision 1.39  1992/02/09  02:41:58  ian
   Added HAVE_DUP2 configuration parameter and dup2 emulation function

   Revision 1.38  1992/02/08  23:38:17  ian
   Put utsname on stack rather than making it static

   Revision 1.37  1992/02/08  23:34:41  ian
   If we have neither getcwd nor getwd, fork /bin/pwd to get the cwd

   Revision 1.36  1992/02/08  22:33:32  ian
   Only get the current working directory if it's going to be needed

   Revision 1.35  1992/02/08  03:54:18  ian
   Include <string.h> only in <uucp.h>, added 1992 copyright

   Revision 1.34  1992/01/29  04:27:11  ian
   Jay Vassos-Libove: removed some conflicting declarations

   Revision 1.33  1992/01/22  05:08:21  ian
   Call execl with correct first argument

   Revision 1.32  1992/01/21  19:39:12  ian
   Chip Salzenberg: uucp and uux start uucico for right system, not any

   Revision 1.31  1992/01/21  00:30:48  ian
   Don't try to create a directory with no name

   Revision 1.30  1992/01/16  03:38:20  ian
   Put \n at end of fsysdep_run error message

   Revision 1.29  1992/01/15  21:06:11  ian
   Mike Park: some systems can't include <sys/time.h> and <time.h> together

   Revision 1.28  1992/01/13  19:38:16  ian
   Chip Salzenberg: can't declare execl, since it is varadic

   Revision 1.27  1992/01/13  06:11:39  ian
   David Nugent: can't declare open or fcntl

   Revision 1.26  1992/01/11  17:30:10  ian
   John Antypas: use memcpy instead of relying on structure assignment

   Revision 1.25  1992/01/04  23:23:57  ian
   usysdep_localtime can't use usysdep_full_time if HAVE_TIMES

   Revision 1.24  1992/01/04  22:56:22  ian
   Added extern definition

   Revision 1.23  1991/12/29  15:45:46  ian
   Don't take the address of a cast value

   Revision 1.22  1991/12/29  04:04:18  ian
   Added a bunch of extern definitions

   Revision 1.21  1991/12/29  02:59:50  ian
   Lele Gaifax: put full year in log file

   Revision 1.20  1991/12/28  17:08:47  ian
   John Theus: offer HAVE_GETWD as an alternative to using getcwd

   Revision 1.19  1991/12/28  07:01:15  ian
   Added HAVE_FTIME configuration option

   Revision 1.18  1991/12/22  22:14:19  ian
   Monty Solomon: added HAVE_UNISTD_H configuration parameter

   Revision 1.17  1991/12/21  21:34:14  ian
   Moved fsysdep_file_exists from sys5.unx to sys1.unx

   Revision 1.16  1991/12/21  21:04:42  ian
   Use real program name in fsysdep_run error messages

   Revision 1.15  1991/12/17  07:09:58  ian
   Record statistics in fractions of a second

   Revision 1.14  1991/12/12  18:35:47  ian
   Do locking with link to avoid races and to permit running as root

   Revision 1.13  1991/12/12  17:39:40  ian
   Set the GID as well as the UID for extra safety

   Revision 1.12  1991/12/11  03:59:19  ian
   Create directories when necessary; don't just assume they exist

   Revision 1.11  1991/12/06  22:50:01  ian
   Franc,ois Pinard: getcwd may legitimately fail in usysdep_initialize

   Revision 1.10  1991/12/01  02:23:12  ian
   Niels Baggesen: don't multiply include <unistd.h>

   Revision 1.9  1991/11/21  20:59:32  ian
   Brian Campbell: ttyname takes an argument

   Revision 1.8  1991/11/14  19:11:25  ian
   Add extern for ttyname

   Revision 1.7  1991/11/14  03:40:10  ian
   Try to figure out whether stdin is a TCP port

   Revision 1.6  1991/11/11  18:55:52  ian
   Get protocol parameters from port and dialer for incoming calls

   Revision 1.5  1991/09/19  17:49:39  ian
   Chip Salzenberg: the log file has been closed before calling fsysdep_run

   Revision 1.4  1991/09/19  15:46:48  ian
   Chip Salzenberg: Make sure getlogin () uid matches process uid

   Revision 1.3  1991/09/19  03:23:34  ian
   Chip Salzenberg: append to private debugging file, don't overwrite it

   Revision 1.2  1991/09/11  02:33:14  ian
   Added ffork argument to fsysdep_run

   Revision 1.1  1991/09/10  19:45:50  ian
   Initial revision
 
   */

#include "uucp.h"

#if USE_RCS_ID
char sys1_unx_rcsid[] = "$Id: sys1.unx,v 1.2 92/05/13 05:42:07 rich Exp Locker: root $";
#endif

#include <errno.h>

#if USE_STDIO && HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "system.h"
#include "sysdep.h"

#include <pwd.h>

#if HAVE_GETGRENT
#include <grp.h>
extern struct group *getgrent ();
#endif

#if HAVE_LIMITS_H
#include <limits.h>
#endif

#if HAVE_SYS_PARAM_H
#include <sys/param.h>
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

#ifndef O_APPEND
#ifdef FAPPEND
#define O_APPEND FAPPEND
#endif
#endif

#ifndef O_NOCTTY
#define O_NOCTTY 0
#endif

#if ! HAVE_GETHOSTNAME && HAVE_UNAME
#include <sys/utsname.h>
extern int uname ();
#endif

#if HAVE_TIME_H && (HAVE_SYS_TIME_AND_TIME_H || ! HAVE_GETTIMEOFDAY)
#include <time.h>
#endif

#if HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif

/* If we have getcwd, forget about getwd.  */

#if HAVE_GETCWD
#undef HAVE_GETWD
#define HAVE_GETWD 0
#endif

#if HAVE_GETWD
/* If we didn't get MAXPATHLEN, make it up.  */
#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif
extern char *getwd ();
#endif /* HAVE_GETWD */

/* Prefer gettimeofday to ftime to times.  */

#if HAVE_GETTIMEOFDAY || HAVE_FTIME
#undef HAVE_TIMES
#define HAVE_TIMES 0
#endif

#if HAVE_GETTIMEOFDAY
#undef HAVE_FTIME
#define HAVE_FTIME 0
#endif

#if HAVE_GETTIMEOFDAY
#include <sys/time.h>
extern int gettimeofday ();
#endif

#if HAVE_FTIME
#include <sys/timeb.h>
extern int ftime ();
#endif

#if HAVE_TIMES
#include <sys/times.h>
#if TIMES_DECLARATION_OK
/* We use a macro to protect this because times really returns clock_t
   and on some systems, such as Ultrix 4.0, clock_t is int.  We don't
   leave it out entirely because on some systems, such as System III,
   the declaration is necessary for correct compilation.  */
extern long times ();
#endif

#if TIMES_TICK == 0
/* We don't have a value for TIMES_TICK.  Look for one.  */
#ifdef CLK_TCK
#undef TIMES_TICK
#define TIMES_TICK CLK_TCK
#else /* ! defined (CLK_TCK) */
#ifdef HZ
#undef TIMES_TICK
#define TIMES_TICK HZ
#endif /* defined (HZ) */
#endif /* ! defined (CLK_TCK) */
#endif /* TIMES_TICK == 0 */

#endif /* HAVE_TIMES */

/* We need the access macros.  */

#ifndef R_OK
#define R_OK 4
#define W_OK 2
#define X_OK 1
#define F_OK 0
#endif /* ! defined (R_OK) */

/* We need wait status information.  */

#if HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

/* We use a typedef wait_status for wait and related functions to put
   results into.  We define the POSIX examination functions we need if
   they are not already defined (if they aren't defined, I assume that
   we have a standard wait status).  */

#if HAVE_UNION_WAIT
typedef union wait wait_status;
#ifndef WIFEXITED
#define WIFEXITED(u) ((u).w_termsig == 0)
#endif
#ifndef WEXITSTATUS
#define WEXITSTATUS(u) ((u).w_retcode)
#endif
#ifndef WTERMSIG
#define WTERMSIG(u) ((u).w_termsig)
#endif
#else /* ! HAVE_UNION_WAIT */
typedef int wait_status;
#ifndef WIFEXITED
#define WIFEXITED(i) (((i) & 0xff) == 0)
#endif
#ifndef WEXITSTATUS
#define WEXITSTATUS(i) (((i) >> 8) & 0xff)
#endif
#ifndef WTERMSIG
#define WTERMSIG(i) ((i) & 0x7f)
#endif
#endif /* ! HAVE_UNION_WAIT */

/* External variables.  */
extern char **environ;

/* External functions.  */
#ifndef __386BSD__
extern int kill ();
#endif __386BSD__
extern int chdir (), access (), stat (), unlink (), execve ();
extern int close (), pipe (), dup2 ();
extern int fputs ();
extern void _exit ();
extern time_t time ();
extern char *getlogin (), *ttyname ();
extern pid_t getpid (), getppid (), fork (), getpgrp ();
extern uid_t getuid (), geteuid (), getgid (), getegid ();
extern struct tm *localtime ();
#ifndef __386BSD__
extern struct passwd *getpwuid ();
#endif __386BSD__
extern struct passwd *getpwnam ();
#if HAVE_GETHOSTNAME
extern int gethostname ();
#endif
#if HAVE_GETCWD
extern char *getcwd ();
#else
#if ! HAVE_GETWD
static char *getcwd P((char *zbuf, int cbuf));
#endif /* ! HAVE_GETWD */
#endif /* ! HAVE_GETCWD */
#if HAVE_GETDTABLESIZE
extern int getdtablesize ();
#endif
#if HAVE_SYSCONF
extern long sysconf ();
#endif
#if HAVE_SETPGRP
#ifndef __386BSD__
extern int setpgrp ();
#endif __386BSD__
#endif
#if HAVE_SETSID
#ifndef __386BSD__
extern int setsid ();
#endif __386BSD__
#endif
#if HAVE_SIGACTION
extern int sigaction ();
#endif
#if HAVE_SIGVEC
extern int sigvec ();
#endif
#if HAVE_TCP
extern int getsockname ();
#endif

/* Initialize the system dependent routines.  We will probably be running
   suid to uucp, so we make sure that nothing is obviously wrong.  We
   save the login name since we will be losing the real uid.  */
static char *zSlogin;

/* We save the current directory since we will do a chdir to the
   spool directory.  */
char *zScwd;

/* The maximum length of a system name is controlled by the type of spool
   directory we use.  */

#if SPOOLDIR_V2 | SPOOLDIR_BSD42 | SPOOLDIR_BSD43 | SPOOLDIR_ULTRIX
int cSysdep_max_name_len = 7;
#endif /* SPOOLDIR_V2 | SPOOLDIR_BSD42 | SPOOLDIR_BSD43 | SPOOLDIR_ULTRIX */
#if SPOOLDIR_BNU
int cSysdep_max_name_len = 14;
#endif /* SPOOLDIR_BNU */
#if SPOOLDIR_TAYLOR
#if HAVE_LONG_NAMES
int cSysdep_max_name_len = 255;
#else /* ! HAVE_LONG_NAMES */
int cSysdep_max_name_len = 14;
#endif /* ! HAVE_LONG_NAMES */
#endif /* SPOOLDIR_TAYLOR */

/* The number of available file descriptors.  */

static int cSdescriptors;

/* Local functions.  */

static void xmkdir P((const char *zdir));
static void usmake_spool_dir P((void));

void
usysdep_initialize (fdaemon, fgetcwd)
     boolean fdaemon;
     boolean fgetcwd;
{
  int o;
  char *z;
  struct passwd *q;

  ulog_id (getpid ());

#if HAVE_GETDTABLESIZE
  cSdescriptors = getdtablesize ();
#else
#if HAVE_SYSCONF
  cSdescriptors = sysconf (_SC_OPEN_MAX);
#else
#ifdef OPEN_MAX
  cSdescriptors = OPEN_MAX;
#else
#ifdef NOFILE
  cSdescriptors = NOFILE;
#else
  cSdescriptors = 20;
#endif /* ! defined (NOFILE) */
#endif /* ! defined (OPEN_MAX) */
#endif /* ! HAVE_SYSCONF */
#endif /* ! HAVE_GETDTABLESIZE */

  /* Close everything but stdin, stdout and stderr.  */

  for (o = 3; o < cSdescriptors; o++)
    (void) close (o);

  /* Make sure stdin, stdout and stderr are open.  Otherwise, newly
     opened files will appear to be them and confusion will result.  */
  if (fcntl (0, F_GETFD, 0) == -1
      && open ("/dev/null", O_RDONLY, 0) != 0)
    exit (EXIT_FAILURE);
  if (fcntl (1, F_GETFD, 0) == -1
      && open ("/dev/null", O_WRONLY, 0) != 1)
    exit (EXIT_FAILURE);
  if (fcntl (2, F_GETFD, 0) == -1
      && open ("/dev/null", O_WRONLY, 0) != 2)
    exit (EXIT_FAILURE);

  /* We always set our file modes to exactly what we want.  */
  umask (0);

  /* Get the login name, making sure that it matches the uid.  Many
     systems truncate the getlogin return value to 8 characters, but
     keep the full name in the password file, so we prefer the name in
     the password file.  */
  z = getlogin ();
  if (z == NULL)
    q = NULL;
  else
    {
      q = getpwnam (z);
      if (q != NULL)
	z = q->pw_name;
    }
  if (q == NULL || q->pw_uid != getuid ())
    {
      q = getpwuid (getuid ());
      if (q == NULL)
	ulog (LOG_FATAL, "Can't get login name");
      z = q->pw_name;
    }
  zSlogin = xstrdup (z);

  if (fdaemon)
    {
      /* Set our uid to our effective uid.  There is no point in
	 remembering who originally ran the program.  This won't work
	 on System V, but there's nothing to be done about that and it
	 doesn't make all that much difference.  */
      (void) setuid (geteuid ());
      (void) setgid (getegid ());
    }

  if (fgetcwd)
    {
      const char *zenv;
      struct stat senv, sdot;

      /* Get the current working directory.  We have to get it now,
	 since we're about to do a chdir.  We use PWD if it's defined
	 and if it really names the working directory, since if it's
	 not the same as whatever getcwd returns it's probably more
	 appropriate.  */
      zenv = getenv ("PWD");
      if (zenv != NULL
	  && stat (zenv, &senv) == 0
	  && stat (".", &sdot) == 0
	  && senv.st_ino == sdot.st_ino
	  && senv.st_dev == sdot.st_dev)
	zScwd = xstrdup (zenv);
      else
	{

#if HAVE_GETCWD || ! HAVE_GETWD
	  {
	    int c;

	    c = 128;
	    while (TRUE)
	      {
		zScwd = (char *) xmalloc (c);
		if (getcwd (zScwd, c) != NULL)
		  break;
		if (errno != ERANGE)
		  ulog (LOG_FATAL, "getcwd: %s", strerror (errno));
		xfree ((pointer) zScwd);
		c <<= 1;
	      }
	  }
#endif /* HAVE_GETCWD */

#if HAVE_GETWD
	  zScwd = (char *) xmalloc (MAXPATHLEN);
	  /* The getwd function puts in an error message in the
	     buffer, rather than setting errno.  */
	  if (getwd (zScwd) == NULL)
	    ulog (LOG_FATAL, "getwd: %s", zScwd);
#endif /* HAVE_GETWD */

	  zScwd = (char *) xrealloc ((pointer) zScwd, strlen (zScwd) + 1);
	}
    }

  /* Connect to the spool directory, and create it if is doesn't
     exist.  */
  if (chdir (zSpooldir) < 0)
    {
      if (errno != ENOENT)
	ulog (LOG_FATAL, "chdir (%s): %s", zSpooldir,
	      strerror (errno));
      usmake_spool_dir ();
    }
}

/* Exit the program.  */

void
usysdep_exit (fsuccess)
     boolean fsuccess;
{
  exit (fsuccess ? EXIT_SUCCESS : EXIT_FAILURE);
}

/* This is called when a non-standard configuration file is used, to
   make sure the program doesn't hand out privileged file access.
   This means that to test non-standard configuration files, you
   should be logged in as uucp.  This is called before
   usysdep_initialize.  It ensures that someone can't simply use an
   alternate configuration file to steal UUCP transfers from other
   systems.  This will still permit people to set up their own
   configuration file and pretend to be whatever system they choose.
   The only real security is to use a high level of protection on the
   modem ports.  */

/*ARGSUSED*/
boolean fsysdep_other_config (z)
     const char *z;
{
  (void) setuid (getuid ());
  (void) setgid (getgid ());
  return TRUE;
}

/* Detach from the controlling terminal.  This is called by uucico if
   it is calling out to another system, so that it can receive SIGHUP
   signals from the port it calls out on.  It is also called by uucico
   just before it starts uuxqt, so that uuxqt is completely
   independent of the terminal.  */

#ifdef TIOCNOTTY
#define HAVE_TIOCNOTTY 1
#else
#define HAVE_TIOCNOTTY 0 
#endif

void
usysdep_detach ()
{
  int o;

#if ! HAVE_BSD_PGRP || ! HAVE_TIOCNOTTY

  pid_t igrp;

  /* First make sure we are not a process group leader.  If we have
     TIOCNOTTY, this doesn't matter, since TIOCNOTTY sets our process
     group to 0 anyhow.  */

#if HAVE_BSD_PGRP
  igrp = getpgrp (0);
#else
  igrp = getpgrp ();
#endif

  if (igrp == getpid ())
    {
      boolean fignored;
      pid_t ipid;

      /* Ignore SIGHUP, since our process group leader is about to
	 die.  */
      usset_signal (SIGHUP, SIG_IGN, FALSE, &fignored);

      ipid = isfork ();
      if (ipid < 0)
	ulog (LOG_FATAL, "fork: %s", strerror (errno));

      if (ipid != 0)
	_exit (EXIT_SUCCESS);

      /* We'll always wind up as a child of process number 1, right?
	 Right?  We have to wait for our parent to die before
	 reenabling SIGHUP.  */
      while (getppid () != 1)
	sleep (1);

      /* Restore SIGHUP catcher if it wasn't being ignored.  */
      if (! fignored)
	usset_signal (SIGHUP, ussignal, TRUE, (boolean *) NULL);
    }

#endif /* ! HAVE_BSD_PGRP || ! HAVE_TIOCNOTTY */

  /* Close all open files.  */

  ulog_close ();

  for (o = 0; o < cSdescriptors; o++)
    (void) close (o);

  /* Reopen stdin, stdout and stderr.  */

  if (open ("/dev/null", O_RDONLY) != 0
      || open ("/dev/null", O_WRONLY) != 1
      || open ("/dev/null", O_WRONLY) != 2)
    ulog (LOG_FATAL, "open (/dev/null): %s", strerror (errno));

#if HAVE_BSD_PGRP

#if HAVE_TIOCNOTTY
  /* Lose our controlling terminal.  */

#ifndef O_NDELAY
#define O_NDELAY FNDELAY
#endif

  o = open ("/dev/tty", O_RDWR | O_NDELAY, 0);
  if (o >= 0)
    {
      (void) ioctl (o, TIOCNOTTY, (char *) NULL);
      (void) close (o);
    }
#endif /* HAVE_TIOCNOTTY */

  /* Make sure our process group ID is set to 0.  On BSD TIOCNOTTY
     should already have set it 0, so this will do no harm.  On System
     V we presumably did not execute the TIOCNOTTY call, but the
     System V setpgrp will detach the controlling terminal anyhow.
     This lets us use the same code on both BSD and System V, provided
     it compiles correctly, which life easier for the configure
     script.  We don't output an error if we got EPERM because some
     BSD variants don't permit this usage of setpgrp (which means they
     don't provide any way to pick up a new controlling terminal).  */

  if (setpgrp (0, 0) < 0)
    {
      if (errno != EPERM)
	ulog (LOG_ERROR, "setpgrp: %s", strerror (errno));
    }

#else /* ! HAVE_BSD_PGRP */

#if HAVE_SETSID

  /* Under POSIX the setsid call creates a new session for which we
     are the process group leader.  It also detaches us from our
     controlling terminal.  I'm using the BSD setpgrp call first
     because they should be equivalent for my purposes, but it turns
     out that on Ultrix 4.0 setsid prevents us from ever acquiring
     another controlling terminal (it does not change our process
     group, and Ultrix 4.0 prevents us from setting our process group
     to 0).  */

  if (setsid () < 0)
    ulog (LOG_ERROR, "setsid: %s", strerror (errno));

#else /* ! HAVE_SETSID */

#if HAVE_SETPGRP

  /* Now we assume we have the System V setpgrp, which takes no
     arguments, and we couldn't compile the HAVE_BSD_PGRP code above
     because there was a prototype somewhere in scope.  On System V
     setpgrp makes us the leader of a new process group and also
     detaches the controlling terminal.  */

  if (setpgrp () < 0)
    ulog (LOG_ERROR, "setpgrp: %s", strerror (errno));

#else /* ! HAVE_SETPGRP */

 #error Must detach from controlling terminal

#endif /* HAVE_SETPGRP */
#endif /* ! HAVE_SETSID */
#endif /* ! HAVE_BSD_PGRP */

  /* At this point we have completely detached from our controlling
     terminal.  The next terminal device we open will probably become
     our controlling terminal.  */
}

/* Get the node name to use if it was not specified in the configuration
   file.  */

const char *
zsysdep_local_name ()
{
#if HAVE_GETHOSTNAME
  char ab[256];

  if (gethostname (ab, sizeof ab) < 0)
    {
      ulog (LOG_ERROR, "gethostname: %s", strerror (errno));
      return NULL;
    }
  ab[sizeof ab - 1] = '\0';
  ab[strcspn (ab, ".")] = '\0';
  return xstrdup (ab);
#else /* ! HAVE_GETHOSTNAME */
#if HAVE_UNAME
  struct utsname s;

  if (uname (&s) < 0)
    {
      ulog (LOG_ERROR, "uname: %s", strerror (errno));
      return NULL;
    }
  return xstrdup (s.nodename);
#else /* ! HAVE_UNAME */
  return NULL;
#endif /* ! HAVE_UNAME */
#endif /* ! HAVE_GETHOSTNAME */
}

/* Get the login name.  We actually get the login name in
   usysdep_initialize, because after that we will lost the real uid.  */

const char *
zsysdep_login_name ()
{
  return zSlogin;
}

/* Get the port name of standard input.  I assume that Unix systems
   generally support ttyname.  If they don't, this function can just
   return NULL.  It uses getsockname to see whether standard input is
   a TCP connection.  */

const char *
zsysdep_port_name (ftcp_port)
     boolean *ftcp_port;
{
  const char *z;

  *ftcp_port = FALSE;

#if HAVE_TCP
  {
    int clen;

    clen = 0;
    if (getsockname (0, (struct sockaddr *) NULL, &clen) == 0)
      *ftcp_port = TRUE;
  }
#endif /* HAVE_TCP */

  z = ttyname (0);
  if (z == NULL)
    return NULL;
  if (strncmp (z, "/dev/", 5) == 0)
    return z + 5;
  else
    return z;
}

/* Signal handling routines.  When we catch a signal, we want to set
   the appropriate elements of afSignal and afLog_signal to TRUE.  If
   we are on a system which restarts system calls, we may also want to
   longjmp out.  On a system which does not restart system calls,
   these signal handling routines are well-defined by ANSI C.  */

#if HAVE_RESTARTABLE_SYSCALLS
volatile sig_atomic_t fSjmp;
volatile jmp_buf sSjmp_buf;
#endif /* HAVE_RESTARTABLE_SYSCALLS */

/* The SVR3 sigset function can be called just like signal, unless
   system calls are restarted which is extremely unlikely; we prevent
   this case in sysh.unx.  */
#if HAVE_SIGSET && ! HAVE_SIGACTION && ! HAVE_SIGVEC
#define signal sigset
#endif

/* Catch a signal.  Reinstall the signal handler if necessary, set the
   appropriate variables, and do a longjmp if necessary.  */

SIGtype
ussignal (isig)
     int isig;
{
  int iindex;

#if ! HAVE_SIGACTION && ! HAVE_SIGVEC && ! HAVE_SIGSET
  (void) signal (isig, ussignal);
#endif

  switch (isig)
    {
    default: iindex = INDEXSIG_SIGHUP; break;
#ifdef SIGINT
    case SIGINT: iindex = INDEXSIG_SIGINT; break;
#endif
#ifdef SIGQUIT
    case SIGQUIT: iindex = INDEXSIG_SIGQUIT; break;
#endif
#ifdef SIGTERM
    case SIGTERM: iindex = INDEXSIG_SIGTERM; break;
#endif
#ifdef SIGPIPE
    case SIGPIPE: iindex = INDEXSIG_SIGPIPE; break;
#endif
    }

  afSignal[iindex] = TRUE;
  afLog_signal[iindex] = TRUE;

#if HAVE_RESTARTABLE_SYSCALLS
  if (fSjmp)
    longjmp (sSjmp_buf, 1);
#endif /* HAVE_RESTARTABLE_SYSCALLS */
}

/* Prepare to catch a signal.  This is basically the ANSI C routine
   signal, but it uses sigaction or sigvec instead if they are
   available.  If fforce is FALSE, we do not set the signal if it is
   currently being ignored.  If pfignored is not NULL and fforce is
   FALSE, then *pfignored will be set to TRUE if the signal was
   previously being ignored (if fforce is TRUE the value of *pfignored
   is meaningless).  If we can't change the signal handler we give a
   fatal error.  */

void
usset_signal (isig, pfn, fforce, pfignored)
     int isig;
     SIGtype (*pfn) P((int));
     boolean fforce;
     boolean *pfignored;
{
#if HAVE_SIGACTION

  struct sigaction s;

  if (! fforce)
    {
      sigemptyset (&s.sa_mask);
      if (sigaction (isig, (struct sigaction *) NULL, &s) != 0)
	ulog (LOG_FATAL, "sigaction (%d): %s", isig, strerror (errno));

      if (s.sa_handler == SIG_IGN)
	{
	  if (pfignored != NULL)
	    *pfignored = TRUE;
	  return;
	}

      if (pfignored != NULL)
	*pfignored = FALSE;
    }

  s.sa_handler = pfn;
  sigemptyset (&s.sa_mask);
  s.sa_flags = 0;

  if (sigaction (isig, &s, (struct sigaction *) NULL) != 0)
    ulog (LOG_FATAL, "sigaction (%d): %s", isig, strerror (errno));

#else /* ! HAVE_SIGACTION */
#if HAVE_SIGVEC

  struct sigvec s;

  if (! fforce)
    {
      if (sigvec (isig, (struct sigvec *) NULL, &s) != 0)
	ulog (LOG_FATAL, "sigvec (%d): %s", isig, strerror (errno));

      if (s.sv_handler == SIG_IGN)
	{
	  if (pfignored != NULL)
	    *pfignored = TRUE;
	  return;
	}

      if (pfignored != NULL)
	*pfignored = FALSE;
    }

  s.sv_handler = pfn;
  s.sv_mask = 0;
#ifdef SV_INTERRUPT
  s.sv_flags = SV_INTERRUPT;
#else
  s.sv_flags = 0;
#endif

  if (sigvec (isig, &s, (struct sigvec *) NULL) != 0)
    ulog (LOG_FATAL, "sigvec (%d): %s", isig, strerror (errno));

#else /* ! HAVE_SIGVEC */

  if (! fforce)
    {
      if (signal (isig, SIG_IGN) == SIG_IGN)
	{
	  if (pfignored != NULL)
	    *pfignored = TRUE;
	  return;
	}

      if (pfignored != NULL)
	*pfignored = FALSE;
    }

  (void) signal (isig, pfn);

#endif /* ! HAVE_SIGVEC */
#endif /* ! HAVE_SIGACTION */
}

/* The routine called by the system independent code, which always
   uses the same signal handler.  */

void
usysdep_signal (isig)
     int isig;
{
  usset_signal (isig, ussignal, FALSE, (boolean *) NULL);
}

/* Get the time in seconds since the epoch, with optional
   microseconds.  We use usysdep_process_time to get the microseconds
   if it will work (it won't is it uses times, since that returns a
   time based only on the process).  */

long
isysdep_time (pimicros)
     long *pimicros;
{
#if HAVE_GETTIMEOFDAY || HAVE_FTIME
  return isysdep_process_time (pimicros);
#else
  if (pimicros != NULL)
    *pimicros = 0;
  return time ((time_t *) NULL);
#endif
}

/* Get the time in seconds and microseconds; this need only work
   within the process when called from the system independent code.
   It is also called by isysdep_time, above.  */

long
isysdep_process_time (pimicros)
     long *pimicros;
{
#if HAVE_GETTIMEOFDAY
  struct timeval stime;
  struct timezone stz;

  (void) gettimeofday (&stime, &stz);
  if (pimicros != NULL)
    *pimicros = stime.tv_usec;
  return stime.tv_sec;
#endif /* HAVE_GETTIMEOFDAY */

#if HAVE_FTIME
  struct timeb stime;

  (void) ftime (&stime);
  if (pimicros != NULL)
    *pimicros = stime.millitm * 1000;
  return stime.time;
#endif /* HAVE_FTIME */

#if HAVE_TIMES
  struct tms s;
  long i;
  static int itick;

  if (itick == 0)
    {
#if TIMES_TICK != 0
      itick = TIMES_TICK;
#else /* TIMES_TICK == 0 */
      const char *z;

      z = getenv ("HZ");
      if (z != NULL)
	itick = atoi (z);

      /* If we really couldn't get anything, just use 60.  */
      if (itick == 0)
	itick = 60;
#endif /* TIMES_TICK == 0 */
    }

  i = (long) times (&s);
  if (pimicros != NULL)
    *pimicros = (i % (long) itick) * ((long) 1000000 / (long) itick);
  return i / (long) itick;
#endif /* HAVE_TIMES */

#if ! HAVE_GETTIMEOFDAY && ! HAVE_FTIME && ! HAVE_TIMES
  if (pimicros != NULL)
    *pimicros = 0;
  return time ((time_t *) NULL);
#endif /* ! HAVE_GETTIMEOFDAY && ! HAVE_FTIME && ! HAVE_TIMES  */
}

/* Fill in a struct tm.  */

void
usysdep_localtime (itime, q)
     long itime;
     struct tm *q;
{
  time_t i;

  i = (time_t) itime;
  memcpy (q, localtime (&i), sizeof (struct tm));
}

/* Sleep for a number of seconds.  */

void
usysdep_sleep (c)
     int c;
{
  sleep (c);
}

/* Check whether a file exists.  */

boolean
fsysdep_file_exists (zfile)
     const char *zfile;
{
  struct stat s;

  return stat (zfile, &s) == 0;
}

/* Open a stdio file with appropriate permissions.  */

FILE *
esysdep_fopen (zfile, fpublic, fappend, fmkdirs)
     const char *zfile;
     boolean fpublic;
     boolean fappend;
     boolean fmkdirs;
{
  int imode;
  int o;
  FILE *e;

  if (fpublic)
    imode = IPUBLIC_FILE_MODE;
  else
    imode = IPRIVATE_FILE_MODE;

  if (! fappend)
    o = creat (zfile, imode);
  else
    {
#ifdef O_CREAT
      o = open (zfile, O_WRONLY | O_APPEND | O_CREAT | O_NOCTTY, imode);
#else
      o = open (zfile, O_WRONLY | O_NOCTTY);
      if (o < 0 && errno == ENOENT)
	o = creat (zfile, imode);
#endif /* ! defined (O_CREAT) */
    }

  if (o < 0)
    {
      if (errno == ENOENT && fmkdirs)
	{
	  if (! fsysdep_make_dirs (zfile, fpublic))
	    return NULL;
	  if (! fappend)
	    o = creat (zfile, imode);
	  else
	    {
#ifdef O_CREAT
	      o = open (zfile, O_WRONLY | O_APPEND | O_CREAT, imode);
#else
	      o = creat (zfile, imode);
#endif
	    }
	}
      if (o < 0)
	{
	  ulog (LOG_ERROR, "open (%s): %s", zfile, strerror (errno));
	  return NULL;
	}
    }

#ifndef O_CREAT
#ifdef O_APPEND
  if (fappend)
    {
      if (fcntl (o, F_SETFL, O_APPEND) != 0)
	{
	  ulog (LOG_ERROR, "fcntl (%s, O_APPEND): %s", zfile,
		strerror (errno));
	  (void) close (o);
	  return NULL;
	}
    }
#endif /* defined (O_APPEND) */
#endif /* ! defined (O_CREAT) */

  if (fappend)
    e = fdopen (o, (char *) "a");
  else
    e = fdopen (o, (char *) "w");

  if (e == NULL)
    {
      ulog (LOG_ERROR, "fdopen: %s", strerror (errno));
      (void) close (o);
    }

  return e;
}

/* See whether a directory exists.  */

boolean
fsdirectory_exists (z)
     const char *z;
{
  struct stat s;

  if (stat (z, &s) < 0)
    return FALSE;
  return S_ISDIR (s.st_mode);
}

/* Create any directories needed for a file name.  */

boolean
fsysdep_make_dirs (zfile, fpublic)
     const char *zfile;
     boolean fpublic;
{
  char *zcopy, *z;
  int imode;

  zcopy = (char *) alloca (strlen (zfile) + 1);
  strcpy (zcopy, zfile);

  if (fpublic)
    imode = IPUBLIC_DIRECTORY_MODE;
  else
    imode = IDIRECTORY_MODE;

  for (z = zcopy; *z != '\0'; z++)
    {
      if (*z == '/' && z != zcopy)
	{
	  *z = '\0';
	  if (! fsdirectory_exists (zcopy))
	    {
	      if (mkdir (zcopy, imode) != 0)
		{
		  ulog (LOG_ERROR, "mkdir (%s): %s", zcopy,
			strerror (errno));
		  return FALSE;
		}
	    }
	  *z = '/';
	}
    }

  return TRUE;
}

/* Tilde expand a file or directory name.  */

/*ARGSUSED*/
const char *
zstilde_expand (qsys, zfile)
     const struct ssysteminfo *qsys;
     const char *zfile;
{
  static int calc;
  static char *zalc;
  const char *zdir;
  int clen;

  if (zfile[0] != '~')
    return zfile;
  else if (zfile[1] == '\0' || zfile[1] == '/')
    {
      const char *zpub;

      if (qsys->zpubdir == NULL)
	zpub = zPubdir;
      else
	zpub = qsys->zpubdir;
      if (zfile[1] == '\0')
	return zpub;
      else
	{
	  zdir = zpub;
	  zfile += 2;
	}
    } 
  else
    {
      int cuserlen;
      char *zcopy;
      struct passwd *q;

      ++zfile;
      cuserlen = strcspn (zfile, "/");
      zcopy = (char *) alloca (cuserlen + 1);
      strncpy (zcopy, zfile, cuserlen);
      zcopy[cuserlen] = '\0';
      
      q = getpwnam (zcopy);
      if (q == NULL)
	{
	  ulog (LOG_ERROR, "User %s not found", zcopy);
	  return NULL;
	}

      if (zfile[cuserlen] == '\0')
	return q->pw_dir;
      else
	{
	  zdir = q->pw_dir;
	  zfile += cuserlen + 1;
	}
    }

  clen = strlen (zdir) + strlen (zfile) + 2;
  if (clen > calc)
    {
      zalc = (char *) xrealloc ((pointer) zalc, clen);
      calc = clen;
    }

  sprintf (zalc, "%s/%s", zdir, zfile);
  return zalc;
}

/* Do access(2) on a stat structure, except that the user name is
   provided.  If the user name in zuser is NULL, require the file to
   be accessible to the world.  Return TRUE if access is permitted,
   FALSE otherwise.  This does not log an error message.  */

boolean
fsuser_access (q, imode, zuser)
     const struct stat *q;
     int imode;
     const char *zuser;
{
  static char *zuser_hold;
  static uid_t iuid_hold;
  static gid_t igid_hold;
  static int cgroups_hold;
  static gid_t *paigroups_hold;
  int ir, iw, ix, iand;

  if (imode == F_OK)
    return TRUE;

  if (zuser != NULL)
    {
      /* We keep static variables around for the last user we did, to
	 avoid looking up a user multiple times.  */
      if (zuser_hold == NULL || strcmp (zuser_hold, zuser) != 0)
	{
	  struct passwd *qpwd;

	  if (zuser_hold != NULL)
	    {
	      xfree ((pointer) zuser_hold);
	      zuser_hold = NULL;
	      cgroups_hold = 0;
	      xfree ((pointer) paigroups_hold);
	      paigroups_hold = NULL;
	    }

	  qpwd = getpwnam ((char *) zuser);
	  if (qpwd == NULL)
	    {
	      /* Check this as a remote request.  */
	      zuser = NULL;
	    }
	  else
	    {
#if HAVE_GETGRENT
	      struct group *qg;
#endif

	      zuser_hold = xstrdup (zuser);

	      iuid_hold = qpwd->pw_uid;
	      igid_hold = qpwd->pw_gid;

#if HAVE_GETGRENT
	      /* Get the list of groups for this user.  This is
		 definitely more appropriate for BSD than for System
		 V.  It may just be a waste of time, and perhaps it
		 should be configurable.  */
	      setgrent ();
	      while ((qg = getgrent ()) != NULL)
		{
		  const char **pz;

		  if (qg->gr_gid == igid_hold)
		    continue;
		  for (pz = (const char **) qg->gr_mem; *pz != NULL; pz++)
		    {
		      if ((*pz)[0] == *zuser
			  && strcmp (*pz, zuser) == 0)
			{
			  paigroups_hold = ((gid_t *)
					    (xrealloc
					     ((pointer) paigroups_hold,
					      ((cgroups_hold + 1)
					       * sizeof (gid_t)))));
			  paigroups_hold[cgroups_hold] = qg->gr_gid;
			  ++cgroups_hold;
			  break;
			}
		    }
		}
	      endgrent ();
#endif
	    }
	}
    }


  /* Now do the actual access check.  */

  if (zuser != NULL)
    {
      /* The superuser can do anything.  */
      if (iuid_hold == 0)
	return TRUE;

      /* If this is the uid we're running under, there's no point to
	 checking access further, because when we actually try the
	 operation the system will do the checking for us.  */
      if (iuid_hold == geteuid ())
	return TRUE;
    }

  ir = S_IROTH;
  iw = S_IWOTH;
  ix = S_IXOTH;

  if (zuser != NULL)
    {
      if (iuid_hold == q->st_uid)
	{
	  ir = S_IRUSR;
	  iw = S_IWUSR;
	  ix = S_IXUSR;
	}
      else
	{
	  boolean fgroup;

	  fgroup = FALSE;
	  if (igid_hold == q->st_gid)
	    fgroup = TRUE;
	  else
	    {
	      int i;

	      for (i = 0; i < cgroups_hold; i++)
		{
		  if (paigroups_hold[i] == q->st_gid)
		    {
		      fgroup = TRUE;
		      break;
		    }
		}
	    }

	  if (fgroup)
	    {
	      ir = S_IRGRP;
	      iw = S_IWGRP;
	      ix = S_IXGRP;
	    }
	}
    }

  iand = 0;
  if ((imode & R_OK) != 0)
    iand |= ir;
  if ((imode & W_OK) != 0)
    iand |= iw;
  if ((imode & X_OK) != 0)
    iand |= ix;

  return (q->st_mode & iand) == iand;
}

/* See whether a file is in a directory, and optionally check access.  */

boolean
fsysdep_in_directory (qsys, zfile, zdir, fcheck, freadable, zuser)
     const struct ssysteminfo *qsys;
     const char *zfile;
     const char *zdir;
     boolean fcheck;
     boolean freadable;
     const char *zuser;
{
  int c;
  char *zcopy, *zslash;
  struct stat s;

  if (*zdir == '~')
    {
      zdir = zstilde_expand (qsys, zdir);
      if (zdir == NULL)
	return FALSE;
    }
  c = strlen (zdir);
  if (zdir[c - 1] == '/')
    c--;
  if (strncmp (zfile, zdir, c) != 0
      || (zfile[c] != '/' && zfile[c] != '\0'))
    return FALSE;
  if (strstr (zfile + c, "/../") != NULL)
    return FALSE;

  /* If we're not checking access, get out now.  */

  if (! fcheck)
    return TRUE;

  zcopy = (char *) alloca (strlen (zfile) + 1);
  strcpy (zcopy, zfile);

  /* Start checking directories after zdir.  Otherwise, we would
     require that all directories down to /usr/spool/uucppublic be
     publically searchable; they probably are but it should not be
     requirement.  */

  zslash = zcopy + c;
  do
    {
      char b;
      struct stat shold;

      b = *zslash;
      *zslash = '\0';

      shold = s;
      if (stat (zcopy, &s) != 0)
	{
	  if (errno != ENOENT)
	    {
	      ulog (LOG_ERROR, "stat (%s): %s", zcopy, strerror (errno));
	      return FALSE;
	    }

	  /* If this is the top directory, any problems will be caught
	     later when we try to open it.  */
	  if (zslash == zcopy + c)
	    return TRUE;

	  /* Go back and check the last directory for read or write
	     access.  */
	  s = shold;
	  break;
	}

      /* If this is not a directory, get out of the loop.  */
      if (! S_ISDIR (s.st_mode))
	break;

      /* Make sure the directory is searchable.  */
      if (! fsuser_access (&s, X_OK, zuser))
	{
	  ulog (LOG_ERROR, "%s: %s", zcopy, strerror (EACCES));
	  return FALSE;
	}

      /* If we've reached the end of the string, get out.  */
      if (b == '\0')
	break;

      *zslash = b;
    }
  while ((zslash = strchr (zslash + 1, '/')) != NULL);

  /* At this point s holds a stat on the last component of the path.
     We must check it for readability or writeability.  */

  if (! fsuser_access (&s, freadable ? R_OK : W_OK, zuser))
    {
      ulog (LOG_ERROR, "%s: %s", zcopy, strerror (EACCES));
      return FALSE;
    }

  return TRUE;
}

/* Start up a new program and end the current one.  We always go
   through isspawn, and never exec directly.  We don't have to worry
   about SIGHUP because the current process is either not a process
   group leader (uucp, uux) or it does not have a controlling terminal
   (uucico).  */

boolean
fsysdep_run (ffork, zprogram, zarg1, zarg2)
     boolean ffork;
     const char *zprogram;
     const char *zarg1;
     const char *zarg2;
{
  char *zlib;
  const char *azargs[4];
  int aidescs[3];
  pid_t ipid;

  zlib = (char *) alloca (sizeof LIBDIR + sizeof "/" + strlen (zprogram));
  sprintf (zlib, "%s/%s", LIBDIR, zprogram);

  azargs[0] = zlib;
  azargs[1] = zarg1;
  azargs[2] = zarg2;
  azargs[3] = NULL;

  aidescs[0] = SPAWN_NULL;
  aidescs[1] = SPAWN_NULL;
  aidescs[2] = SPAWN_NULL;

  /* We pass fshell as TRUE, which permits uucico and uuxqt to be
     replaced by shell scripts.  */
  ipid = isspawn (azargs, aidescs, FALSE, FALSE, (const char *) NULL,
		  FALSE, TRUE, (const char *) NULL,
		  (const char *) NULL, (const char *) NULL);
  if (ipid < 0)
    {
      ulog (LOG_ERROR, "isspawn: %s", strerror (errno));
      return FALSE;
    }

  if (ffork)
    return TRUE;

  exit (EXIT_SUCCESS);
}

/* Mail a message to a user.  */

boolean
fsysdep_mail (zto, zsubject, cstrs, paz)
     const char *zto;
     const char *zsubject;
     int cstrs;
     const char **paz;
{
  const char *az[3];
  FILE *e;
  pid_t ipid;
  time_t itime;
  int i;

  az[0] = MAIL_PROGRAM;
  az[1] = zto;
  az[2] = NULL;

  e = espopen (az, FALSE, &ipid);
  if (e == NULL)
    {
      ulog (LOG_ERROR, "espopen (%s): %s", MAIL_PROGRAM,
	    strerror (errno));
      return FALSE;
    }

  fprintf (e, "Subject: %s\n", zsubject);
  fprintf (e, "To: %s\n", zto);

  /* We should probably put in a Date: header as well.  */

  fprintf (e, "\n");

  (void) time (&itime);
  /* Remember that ctime includes a \n, so this skips a line.  */
  fprintf (e, "Message from UUCP on %s %s\n", zLocalname,
	   ctime (&itime));

  for (i = 0; i < cstrs; i++)
    fputs (paz[i], e);

  (void) fclose (e);

  return iswait ((unsigned long) ipid, MAIL_PROGRAM) == 0;
}

/* Make a directory with error checking.  */

static void
xmkdir (zdir)
     const char *zdir;
{
  if (mkdir ((char *) zdir, IDIRECTORY_MODE) < 0)
    ulog (LOG_FATAL, "mkdir (%s): %s", zdir, strerror (errno));
}

/* Make the spool directory.  */

static void
usmake_spool_dir ()
{
  xmkdir (zSpooldir);

  if (chdir (zSpooldir) < 0)
    ulog (LOG_FATAL, "chdir (%s): %s", zSpooldir, strerror (errno));

#if SPOOLDIR_BSD42 | SPOOLDIR_BSD43
  xmkdir ("C.");
  xmkdir ("D.");
#if SPOOLDIR_BSD43
  xmkdir ("X.");
#endif /* SPOOLDIR_BSD43 */
  {
    char ab[sizeof "D.1234567X"];

    sprintf (ab, "D.%.7s", zLocalname);
    xmkdir (ab);
#if SPOOLDIR_BSD43
    strcat (ab, "X");
    xmkdir (ab);
#endif /* SPOOLDIR_BSD43 */
  }
#endif /* SPOOLDIR_BSD42 | SPOOLDIR_BSD43 */

#if SPOOLDIR_ULTRIX
  xmkdir ("sys");
  xmkdir ("sys/DEFAULT");
  xmkdir ("sys/DEFAULT/C.");
  xmkdir ("sys/DEFAULT/D.");
  xmkdir ("sys/DEFAULT/X.");
  {
    char ab[sizeof "sys/DEFAULT/D.1234567X"];

    sprintf (ab, "sys/DEFAULT/D.%.7s", zLocalname);
    xmkdir (ab);
    strcat (ab, "X");
    xmkdir (ab);
  }
#endif /* SPOOLDIR_ULTRIX */

#if SPOOLDIR_BSD43 | SPOOLDIR_ULTRIX | SPOOLDIR_TAYLOR
  xmkdir (".Temp");
#endif /* SPOOLDIR_BSD43 | SPOOLDIR_ULTRIX | SPOOLDIR_TAYLOR */

  xmkdir (".Status");
  xmkdir (".Sequence");
  xmkdir (XQTDIR);
  xmkdir (PRESERVEDIR);
}

/* Retry fork several times before giving up.  */

pid_t
isfork ()
{
  int i;
  pid_t iret;

  for (i = 0; i < 10; i++)
    {
      iret = fork ();
      if (iret >= 0 || errno != EAGAIN)
	return iret;
      sleep (5);
    }

  return iret;
}

/* Spawn a child in a fairly secure fashion.  This returns the process
   ID of the child or -1 on error.  It takes far too many arguments:

   pazargs -- arguments (element 0 is command)
   aidescs -- file descriptors for stdin, stdout and stderr
   fkeepuid -- TRUE if euid should be left unchanged
   fkeepenv -- TRUE if environment should be left unmodified
   zchdir -- directory to chdir to
   fnosigs -- TRUE if child should ignore SIGHUP, SIGINT and SIGQUIT
   fshell -- TRUE if should try /bin/sh if execve gets ENOEXEC
   zpath -- value for environment variable PATH
   zuu_machine -- value for environment variable UU_MACHINE
   zuu_user -- value for environment variable UU_USER

   The aidescs array is three elements long.  0 is stdin, 1 is stdout
   and 2 is stderr.  The array may contain either file descriptor
   numbers to dup appropriately, or one of the following:

   SPAWN_NULL -- set descriptor to /dev/null
   SPAWN_READ_PIPE -- set aidescs element to pipe for parent to read
   SPAWN_WRITE_PIPE -- set aidescs element to pipe for parent to write

   If fkeepenv is FALSE, a standard environment is created.  The
   environment arguments (zpath, zuu_machine and zuu_user) are only
   used if fkeepenv is FALSE; any of them may be NULL.  */

pid_t
isspawn (pazargs, aidescs, fkeepuid, fkeepenv, zchdir, fnosigs, fshell,
	 zpath, zuu_machine, zuu_user)
     const char **pazargs;
     int aidescs[3];
     boolean fkeepuid;
     boolean fkeepenv;
     const char *zchdir;
     boolean fnosigs;
     boolean fshell;
     const char *zpath;
     const char *zuu_machine;
     const char *zuu_user;
{
  char *zshcmd = NULL;
  int i;
  char *azenv[9];
  char **pazenv;
  boolean ferr;
  int ierr = 0;
  int onull;
  int aichild_descs[3];
  int cpar_close;
  int aipar_close[4];
  int cchild_close;
  int aichild_close[3];
  pid_t iret = 0;
  const char *zcmd;

  /* If we might have to use the shell, allocate enough space for the
     quoted command before forking.  Otherwise the allocation might
     modify the data segment and we could not safely use vfork.  */
  if (fshell)
    {
      int clen;

      clen = 0;
      for (i = 0; pazargs[i] != NULL; i++)
	clen += strlen (pazargs[i]);
      zshcmd = (char *) alloca (2 * clen + i);
    }

  /* Set up a standard environment.  This is again done before forking
     because it might modify the data segment.  */

  if (fkeepenv)
    pazenv = environ;
  else
    {
      const char *zterm, *ztz;
      char *zspace;
      int ienv;

      if (zpath == NULL)
	zpath = CMDPATH;

      azenv[0] = (char *) alloca (sizeof "PATH=" + strlen (zpath));
      sprintf (azenv[0], "PATH=%s", zpath);
      zspace = azenv[0] + sizeof "PATH=" - 1;
      while ((zspace = strchr (zspace, ' ')) != NULL)
	*zspace = ':';
    
      azenv[1] = (char *) alloca (sizeof "HOME=" + strlen (zSpooldir));
      sprintf (azenv[1], "HOME=%s", zSpooldir);

      zterm = getenv ("TERM");
      if (zterm == NULL)
	zterm = "unknown";
      azenv[2] = (char *) alloca (sizeof "TERM=" + strlen (zterm));
      sprintf (azenv[2], "TERM=%s", zterm);

      azenv[3] = (char *) "SHELL=/bin/sh";
  
      azenv[4] = (char *) alloca (sizeof "USER=" + strlen (OWNER));
      sprintf (azenv[4], "USER=%s", OWNER);

      ienv = 5;

      ztz = getenv ("TZ");
      if (ztz != NULL)
	{
	  azenv[ienv] = (char *) alloca (sizeof "TZ=" + strlen (ztz));
	  sprintf (azenv[ienv], "TZ=%s", ztz);
	  ++ienv;
	}

      if (zuu_machine != NULL)
	{
	  azenv[ienv] = (char *) alloca (sizeof "UU_MACHINE="
					 + strlen (zuu_machine));
	  sprintf (azenv[ienv], "UU_MACHINE=%s", zuu_machine);
	  ++ienv;
	}

      if (zuu_user != NULL)
	{
	  azenv[ienv] = (char *) alloca (sizeof "UU_USER="
					 + strlen (zuu_user));
	  sprintf (azenv[ienv], "UU_USER=%s", zuu_user);
	  ++ienv;
	}

      azenv[ienv] = NULL;
      pazenv = azenv;
    }

  /* Set up any needed pipes.  */

  ferr = FALSE;
  onull = -1;
  cpar_close = 0;
  cchild_close = 0;

  for (i = 0; i < 3; i++)
    {
      if (aidescs[i] == SPAWN_NULL)
	{
	  if (onull < 0)
	    {
	      onull = open ("/dev/null", O_RDWR);
	      if (onull < 0)
		{
		  ierr = errno;
		  ferr = TRUE;
		  break;
		}
	      aipar_close[cpar_close] = onull;
	      ++cpar_close;
	    }
	  aichild_descs[i] = onull;
	}
      else if (aidescs[i] != SPAWN_READ_PIPE
	       && aidescs[i] != SPAWN_WRITE_PIPE)
	aichild_descs[i] = aidescs[i];
      else
	{
	  int aipipe[2];

	  if (pipe (aipipe) < 0)
	    {
	      ierr = errno;
	      ferr = TRUE;
	      break;
	    }

	  if (aidescs[i] == SPAWN_READ_PIPE)
	    {
	      aidescs[i] = aipipe[0];
	      aichild_close[cchild_close] = aipipe[0];
	      aichild_descs[i] = aipipe[1];
	      aipar_close[cpar_close] = aipipe[1];
	    }
	  else
	    {
	      aidescs[i] = aipipe[1];
	      aichild_close[cchild_close] = aipipe[1];
	      aichild_descs[i] = aipipe[0];
	      aipar_close[cpar_close] = aipipe[0];
	    }

	  ++cpar_close;
	  ++cchild_close;
	}
    }

#if DEBUG > 1
  if (! ferr && FDEBUGGING (DEBUG_EXECUTE))
    {
      ulog (LOG_DEBUG_START, "Forking %s", pazargs[0]);
      for (i = 1; pazargs[i] != NULL; i++)
	ulog (LOG_DEBUG_CONTINUE, " %s", pazargs[i]);
      ulog (LOG_DEBUG_END, "%s", "");
    }
#endif

  if (! ferr)
    {
      /* This should really be vfork if available.  */
      iret = isfork ();
      if (iret < 0)
	{
	  ferr = TRUE;
	  ierr = errno;
	}
    }

  if (ferr)
    {
      for (i = 0; i < cpar_close; i++)
	(void) close (aipar_close[i]);
      for (i = 0; i < cchild_close; i++)
	(void) close (aichild_close[i]);
      errno = ierr;
      return -1;
    }

  /* Here the fork has succeeded and all the pipes have been done.  */

  if (iret != 0)
    {
      /* The parent.  Close the child's ends of the pipes and return
	 the process ID.  */
      for (i = 0; i < cpar_close; i++)
	(void) close (aipar_close[i]);
      return iret;
    }

  /* The child.  */

#ifdef STDIN_FILENO
#if STDIN_FILENO != 0 || STDOUT_FILENO != 1 || STDERR_FILENO != 2
 #error The following code makes invalid assumptions
#endif
#endif

  for (i = 0; i < 3; i++)
    if (aichild_descs[i] != i)
      (void) dup2 (aichild_descs[i], i);

  for (i = 3; i < cSdescriptors; i++)
    (void) close (i);

  zcmd = pazargs[0];
  pazargs[0] = strrchr (zcmd, '/');
  if (pazargs[0] == NULL)
    pazargs[0] = zcmd;
  else
    ++pazargs[0];

  if (! fkeepuid)
    {
      (void) setuid (getuid ());
      (void) setgid (getgid ());
    }

  if (zchdir != NULL)
    (void) chdir (zchdir);

  if (fnosigs)
    {
#ifdef SIGHUP
      (void) signal (SIGHUP, SIG_IGN);
#endif
#ifdef SIGINT
      (void) signal (SIGINT, SIG_IGN);
#endif
#ifdef SIGQUIT
      (void) signal (SIGQUIT, SIG_IGN);
#endif
    }

  (void) execve ((char *) zcmd, (char **) pazargs, pazenv);

  /* The exec failed.  If permitted, try using /bin/sh to execute a
     shell script.  */

  if (errno == ENOEXEC && fshell)
    {
      char *zto;
      const char *azshargs[4];
      
      pazargs[0] = zcmd;
      zto = zshcmd;
      for (i = 0; pazargs[i] != NULL; i++)
	{
	  const char *zfrom;

	  for (zfrom = pazargs[i]; *zfrom != '\0'; zfrom++)
	    {
	      /* Some versions of /bin/sh appear to have a bug such
		 that quoting a '/' sometimes causes an error.  I
		 don't know exactly when this happens (I can recreate
		 it on Ultrix 4.0), but in any case it is harmless to
		 not quote a '/'.  */
	      if (*zfrom != '/')
		*zto++ = '\\';
	      *zto++ = *zfrom;
	    }
	  *zto++ = ' ';
	}
      *(zto - 1) = '\0';

      azshargs[0] = "sh";
      azshargs[1] = "-c";
      azshargs[2] = zshcmd;
      azshargs[3] = NULL;

      (void) execve ((char *) "/bin/sh", (char **) azshargs, pazenv);
    }

  _exit (EXIT_FAILURE);

  /* Avoid compiler warning.  */
  return -1;
}

/* A version of popen that goes through isspawn.  This actually takes
   an array of arguments rather than a string, and takes a boolean
   read/write value rather than a string.  It sets *pipid to the
   process ID of the child.  */

FILE *
espopen (pazargs, frd, pipid)
     const char **pazargs;
     boolean frd;
     pid_t *pipid;
{
  int aidescs[3];
  pid_t ipid;
  FILE *eret;

  if (frd)
    {
      aidescs[0] = SPAWN_NULL;
      aidescs[1] = SPAWN_READ_PIPE;
    }
  else
    {
      aidescs[0] = SPAWN_WRITE_PIPE;
      aidescs[1] = SPAWN_NULL;
    }
  aidescs[2] = SPAWN_NULL;

  ipid = isspawn (pazargs, aidescs, FALSE, FALSE,
		  (const char *) NULL, FALSE, TRUE,
		  (const char *) NULL, (const char *) NULL,
		  (const char *) NULL);
  if (ipid < 0)
    return NULL;

  if (frd)
    eret = fdopen (aidescs[1], (char *) "r");
  else
    eret = fdopen (aidescs[0], (char *) "w");

  if (eret == NULL)
    {
      int ierr;

      ierr = errno;
      (void) close (frd ? aidescs[1] : aidescs[0]);
      (void) kill (ipid, SIGKILL);
      (void) iswait ((unsigned long) ipid, (const char *) NULL);
      errno = ierr;
      return NULL;
    }
    
  *pipid = ipid;

  return eret;
}

/* Wait for a particular process to finish.  The ipid argument should
   be pid_t, but then we couldn't have a prototype.  If the zreport
   argument is not NULL, then a wait error will be logged, and if the
   exit status is non-zero it will be logged with zreport as the
   header of the log message.  If the zreport argument is NULL, no
   errors will be logged.  This function returns the exit status if
   the process exited normally, or -1 on error or if the process was
   killed by a signal (I don't just always return the exit status
   because then the calling code would have to prepared to handle
   union wait status vs. int status, and none of the callers care
   which signal killed the program anyhow).

   This functions keeps waiting until the process finished, even if it
   is interrupted by a signal.  I think this is right for all uses.
   The controversial one would be when called from uuxqt to wait for a
   requested process.  Hitting uuxqt with SIGKILL will approximate the
   actions taken if we return from here with an error anyhow.  If we
   do get a signal, we call ulog with a NULL argument to get it in the
   log file at about the right time.  */

int
iswait (ipid, zreport)
     unsigned long ipid;
     const char *zreport;
{
  wait_status istat;

#if HAVE_WAITPID
  while (waitpid ((pid_t) ipid, &istat, 0) < 0)
    {
      if (errno != EINTR)
	{
	  if (zreport != NULL)
	    ulog (LOG_ERROR, "waitpid: %s", strerror (errno));
	  return -1;
	}
      ulog (LOG_ERROR, (const char *) NULL);
    }
#else /* ! HAVE_WAITPID */
#if HAVE_WAIT4
  while (wait4 ((pid_t) ipid, &istat, 0, (struct rusage *) NULL) < 0)
    {
      if (errno != EINTR)
	{
	  if (zreport != NULL)
	    ulog (LOG_ERROR, "wait4: %s", strerror (errno));
	  return -1;
	}
      ulog (LOG_ERROR, (const char *) NULL);
    }
#else /* ! HAVE_WAIT4 */
  pid_t igot;

  /* We could theoretically get the wrong child here if we're in some
     kind of weird pipeline, so we don't give any error messages for
     it.  */
  while ((igot = wait (&istat)) != (pid_t) ipid)
    {
      if (igot < 0)
	{
	  if (errno != EINTR)
	    {
	      if (zreport != NULL)
		ulog (LOG_ERROR, "wait: %s", strerror (errno));
	      return -1;
	    }
	  ulog (LOG_ERROR, (const char *) NULL);
	}
    }
#endif /* ! HAVE_WAIT4 */
#endif /* ! HAVE_WAITPID */  

  DEBUG_MESSAGE2 (DEBUG_EXECUTE, "%s %d",
		  WIFEXITED (istat) ? "Exit status" : "Signal",
		  WIFEXITED (istat) ? WEXITSTATUS (istat) : WTERMSIG (istat));

  if (WIFEXITED (istat) && WEXITSTATUS (istat) == 0)
    return 0;

  if (zreport != NULL)
    {
      if (! WIFEXITED (istat))
	ulog (LOG_ERROR, "%s: Got signal %d", zreport, WTERMSIG (istat));
      else
	ulog (LOG_ERROR, "%s: Exit status %d", zreport,
	      WEXITSTATUS (istat));
    }

  if (WIFEXITED (istat))
    return WEXITSTATUS (istat);
  else
    return -1;
}

#if ! HAVE_REMOVE

/* Remove a file.  */

int
remove (z)
     const char *z;
{
  return unlink (z);
}

#endif /* ! HAVE_REMOVE */

#if ! HAVE_STRERROR

/* Some systems don't have a strerror definition, so we provide one.
   This function is, of course, system dependent.  */

char *
strerror (ierr)
     int ierr;
{
  extern int sys_nerr;
  extern char *sys_errlist[];

  if (ierr >= 0 && ierr < sys_nerr)
    return sys_errlist[ierr];
  return (char *) "unknown error";
}

#endif /* ! HAVE_STRERROR */

#if ! HAVE_GETCWD && ! HAVE_GETWD

/* Implement a simple getcwd that just calls /bin/pwd.  I probably
   should include Roland McGrath's getcwd implementation here, since
   it doesn't fork, but it requires readdir support that I don't feel
   like contemplating just now.  */

static char *
getcwd (zbuf, cbuf)
     char *zbuf;
     int cbuf;
{
  const char *azargs[2];
  FILE *e;
  pid_t ipid;
  int cread;
  int ierr;

  azargs[0] = PWD_PROGRAM;
  azargs[1] = NULL;
  e = espopen (azargs, TRUE, &ipid);
  if (e == NULL)
    return NULL;

  ierr = 0;

  cread = fread (zbuf, sizeof (char), cbuf, e);
  if (cread == 0)
    ierr = errno;

  (void) fclose (e);

  if (iswait ((unsigned long) ipid, (const char *) NULL) != 0)
    {
      ierr = EACCES;
      cread = 0;
    }

  if (cread != 0)
    {
      if (zbuf[cread - 1] == '\n')
	zbuf[cread - 1] = '\0';
      else
	{
	  ierr = ERANGE;
	  cread = 0;
	}
    }
  
  if (cread == 0)
    {
      errno = ierr;
      return NULL;
    }

  return zbuf;
}

#endif /* ! HAVE_GETCWD && ! HAVE_GETWD */

#if ! HAVE_DUP2

/* Emulate the dup2 call.  I basically took this from the emacs 18.57
   distribution, although I cleaned it up a bit and made it POSIX
   compliant.  */

int
dup2 (oold, onew)
     int oold;
     int onew;
{
  if (oold == onew)
    return onew;
  (void) close (onew);
  
#ifdef F_DUPFD
  return fcntl (oold, F_DUPFD, onew);
#else
  {
    int onext, oret, isave;

    onext = dup (oold);
    if (onext == onew)
      return onext;
    if (onext < 0)
      return -1;
    oret = dup2 (oold, onew);
    isave = errno;
    (void) close (onext);
    errno = isave;
    return oret;
  }
#endif
}

#endif /* ! HAVE_DUP2 */

#if ! HAVE_OPENDIR

/* Simple emulations of opendir/readdir/closedir for systems which
   have the original format of Unix directories.  It's probably better
   to get Doug Gwyn's public domain set of emulation functions.  */

DIR *
opendir (zdir)
     const char *zdir;
{
  int o;
  struct stat s;
  DIR *qret;

  o = open (zdir, O_RDONLY | O_NOCTTY, 0);
  if (o == -1)
    return NULL;
  if (fstat (o, &s) < 0)
    {
      (void) close (o);
      return NULL;
    }
  if (! S_ISDIR (s.st_mode))
    {
      (void) close (o);
      errno = ENOTDIR;
      return NULL;
    }
  qret = (DIR *) xmalloc (sizeof (DIR));
  qret->o = o;
  return qret;
}

struct dirent *
readdir (q)
    DIR *q;
{
  struct direct sdir;
  int cgot;

  do
    {
      cgot = read (q->o, &sdir, sizeof (struct direct));
      if (cgot <= 0)
	return NULL;
      if (cgot != sizeof (struct direct))
	{
	  errno = ENOENT;
	  return NULL;
	}
    }
  while (sdir.d_ino == 0);

  strncpy (q->s.d_name, sdir.d_name, DIRSIZ);
  q->s.d_name[DIRSIZ] = '\0';
  return &q->s;
}

int
closedir (q)
    DIR *q;
{
  int iret, isave;

  iret = close (q->o);
  isave = errno;
  xfree (q);
  errno = isave;
  return iret;
}

#endif /* ! HAVE_OPENDIR */

#if ! HAVE_MKDIR

/* We don't have the mkdir system call, so we invoke the suid program
   uudir to create the directory with the correct owner.  */

int
mkdir (zdir, imode)
     const char *zdir;
     int imode;
{
  const char *azargs[2];
  int aidescs[3];
  pid_t ipid;

  /* /bin/mkdir will create the directory with mode 777, so we set our
     umask to get the mode we want.  */
  (void) umask ((~ imode) & (S_IRWXU | S_IRWXG | S_IRWXO));

  azargs[0] = UUDIR_PROGRAM;
  azargs[1] = NULL;
  aidescs[0] = SPAWN_NULL;
  aidescs[1] = SPAWN_NULL;
  aidescs[2] = SPAWN_NULL;

  ipid = isspawn (azargs, aidescs, FALSE, FALSE, (const char *) NULL,
		  TRUE, FALSE, (const char *) NULL,
		  (const char *) NULL, (const char *) NULL);

  (void) umask (0);

  if (ipid < 0)
    return -1;

  if (iswait ((unsigned long) ipid, (const char *) NULL) != 0)
    {
      /* Make up an errno value.  */
      errno = EACCES;
      return -1;
    }

  return 0;
}

#endif /* ! HAVE_MKDIR */

/*
  Local variables:
  mode:c
  End:
  */
