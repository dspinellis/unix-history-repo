/* sysh.unx
   The header file for the UNIX system dependent routines.

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

   $Log: sysh.unx,v $
   Revision 1.53  1992/03/30  15:29:58  ian
   Added HAVE_SVR4_LOCKFILES

   Revision 1.52  1992/03/16  01:23:08  ian
   Make blocking writes optional

   Revision 1.51  1992/03/15  07:15:42  ian
   T. William Wells: don't use unblocked writes

   Revision 1.50  1992/03/15  04:51:17  ian
   Keep an array of signals we've received rather than a single variable

   Revision 1.49  1992/03/15  01:54:46  ian
   All execs are now done in isspawn, all waits are done in iswait

   Revision 1.48  1992/03/12  21:52:21  ian
   Corrected readdir prototype

   Revision 1.47  1992/03/11  22:06:37  ian
   Marty Shannon: added max-uuxqts command

   Revision 1.46  1992/03/11  02:09:57  ian
   Franc,ois Pinard: retry fork several times before giving up

   Revision 1.45  1992/03/11  00:18:50  ian
   Save temporary file if file send fails

   Revision 1.44  1992/03/10  17:39:57  ian
   Some systems don't permit both <termios.h> and <sys/ioctl.h> to be included

   Revision 1.43  1992/03/09  22:07:36  ian
   Wait for terminal output to drain at various points

   Revision 1.42  1992/03/03  21:01:20  ian
   Use strict timeout in fsserial_read, eliminate all race conditions

   Revision 1.41  1992/03/02  04:53:07  ian
   Marc Unangst: added HAVE_SCO_LOCKFILES configuration parameter

   Revision 1.40  1992/02/29  01:06:59  ian
   Chip Salzenberg: recheck file permissions before sending

   Revision 1.39  1992/02/28  05:06:15  ian
   T. William Wells: fsysdep_catch must be a macro

   Revision 1.38  1992/02/27  05:40:54  ian
   T. William Wells: detach from controlling terminal, handle signals safely

   Revision 1.37  1992/02/23  03:26:51  ian
   Overhaul to use automatic configure shell script

   Revision 1.36  1992/02/20  04:18:59  ian
   Added uustat

   Revision 1.35  1992/02/09  05:10:50  ian
   Added HAVE_MKDIR configuration parameter and mkdir emulation

   Revision 1.34  1992/02/09  03:14:48  ian
   Added HAVE_OLD_DIRECTORIES for systems without readdir routines

   Revision 1.33  1992/02/09  02:41:58  ian
   Added HAVE_DUP2 configuration parameter and dup2 emulation function

   Revision 1.32  1992/02/08  23:34:41  ian
   If we have neither getcwd nor getwd, fork /bin/pwd to get the cwd

   Revision 1.31  1992/02/08  20:02:36  ian
   Added HAVE_SETRET configuration option for systems without setjmp

   Revision 1.30  1992/02/08  03:54:18  ian
   Include <string.h> only in <uucp.h>, added 1992 copyright

   Revision 1.29  1992/01/15  21:06:11  ian
   Mike Park: some systems can't include <sys/time.h> and <time.h> together

   Revision 1.28  1992/01/15  19:40:35  ian
   Mike Park: handle HAVE_UNION_WAIT correctly and completely

   Revision 1.27  1992/01/15  07:06:29  ian
   Set configuration directory in Makefile rather than sysdep.h

   Revision 1.26  1992/01/14  04:18:47  ian
   Chip Salzenberg: added HAVE_USLEEP configuration parameter

   Revision 1.25  1992/01/13  19:59:29  ian
   Define LIBDIR in Makefile rather than in sysh.unx

   Revision 1.24  1992/01/13  05:55:59  ian
   Touched up a few comments

   Revision 1.23  1992/01/13  05:53:04  ian
   Mike Park: added HAVE_WAITPID and HAVE_WAIT4 configuration parameters

   Revision 1.22  1992/01/11  17:11:11  ian
   Hannu Strang: avoid compiler bug by not using -> in address constant

   Revision 1.21  1992/01/04  21:43:24  ian
   Chip Salzenberg: added ALLOW_FILENAME_ARGUMENTS to permit them

   Revision 1.20  1991/12/29  00:55:23  ian
   Monty Solomon: added HAVE_UNION_WAIT

   Revision 1.19  1991/12/28  17:08:47  ian
   John Theus: offer HAVE_GETWD as an alternative to using getcwd

   Revision 1.18  1991/12/28  07:01:15  ian
   Added HAVE_FTIME configuration option

   Revision 1.17  1991/12/22  22:14:19  ian
   Monty Solomon: added HAVE_UNISTD_H configuration parameter

   Revision 1.16  1991/12/19  04:25:57  ian
   Terry Gardner: configuration parameter to not use both NONBLOCK and NDELAY

   Revision 1.15  1991/12/17  07:09:58  ian
   Record statistics in fractions of a second

   Revision 1.14  1991/12/11  04:21:37  ian
   Arne Ludwig: merge in Arne Ludwig's patches for V2 and BNU style logging

   Revision 1.13  1991/12/09  19:07:07  ian
   Richard Todd: add HAVE_V2_LOCKFILES--binary number in lock file

   Revision 1.12  1991/11/30  23:28:26  ian
   Marty Shannon: some systems need a fake version of the rename system call

   Revision 1.11  1991/11/26  01:45:42  ian
   Marty Shannon: configuration option to not include <sys/wait.h>

   Revision 1.10  1991/11/24  04:15:13  ian
   Removed extraneous comment left by last revision

   Revision 1.9  1991/11/21  21:49:38  ian
   Brian Campbell: define all macros to avoid compiler warnings

   Revision 1.8  1991/11/21  21:07:46  ian
   Brian Campbell: offer ltrunc as an alternative to ftruncate

   Revision 1.7  1991/11/10  19:24:22  ian
   Added pffile protocol entry point for file level control

   Revision 1.6  1991/11/08  22:52:34  ian
   Brian Campbell: only include <sys/time.h> and <sys/ioctl.h> when needed

   Revision 1.5  1991/11/08  22:11:45  ian
   Brian Campbell: allow sigsetjmp as configuration option

   Revision 1.4  1991/11/07  19:32:28  ian
   Chip Salzenberg: allow LOCKDIR, and check that locking process exists

   Revision 1.3  1991/09/19  16:15:58  ian
   Chip Salzenberg: configuration option for permitting execution via sh

   Revision 1.2  1991/09/19  03:06:04  ian
   Chip Salzenberg: put BNU temporary files in system's directory

   Revision 1.1  1991/09/10  19:45:50  ian
   Initial revision

   */

#ifndef SYSH_UNX_H

#define SYSH_UNX_H

/* We're might redefine fsysdep_catch, so make sure we've gotten the
   prototype already.  */

#ifndef SYSTEM_H
#include "system.h"
#endif

/* Make sure the defines do not conflict.  These are in this file
   because they are Unix dependent.  */

#if HAVE_V2_LOCKFILES + HAVE_BNU_LOCKFILES + HAVE_SCO_LOCKFILES + HAVE_SVR4_LOCKFILES != 1
 #error LOCKFILES define not set or duplicated
#endif

/* SCO and SVR4 lockfiles are basically just like BNU lockfiles.  */
#if HAVE_SCO_LOCKFILES || HAVE_SVR4_LOCKFILES
#undef HAVE_BNU_LOCKFILES
#define HAVE_BNU_LOCKFILES 1
#endif

#if HAVE_BSD_TTY + HAVE_SYSV_TERMIO + HAVE_POSIX_TERMIOS != 1
 #error Terminal driver define not set or duplicated
#endif

#if SPOOLDIR_V2 + SPOOLDIR_BSD42 + SPOOLDIR_BSD43 + SPOOLDIR_BNU + SPOOLDIR_ULTRIX + SPOOLDIR_TAYLOR != 1
 #error Spool directory define not set or duplicated
#endif

/* On Unix, binary files are the same as text files.  */
#define BINREAD "r"
#define BINWRITE "w"

/* If we have sigaction, we can force system calls to not be
   restarted.  */
#if HAVE_SIGACTION
#undef HAVE_RESTARTABLE_SYSCALLS
#define HAVE_RESTARTABLE_SYSCALLS 0
#endif

/* If we have sigvec and SV_INTERRUPT is defined, we can force system
   calls to not be restarted (signal.h is included by uucp.h before
   this point, so SV_INTERRUPT will be defined by now if it it ever
   is).  */
#if HAVE_SIGVEC
#ifdef SV_INTERRUPT
#undef HAVE_RESTARTABLE_SYSCALLS
#define HAVE_RESTARTABLE_SYSCALLS 0
#endif
#endif

/* We don't handle sigset in combination with restartable system
   calls, so we check for it although this combination will never
   happen.  */
#if ! HAVE_SIGACTION && ! HAVE_SIGVEC && HAVE_SIGSET
#if HAVE_RESTARTABLE_SYSCALLS
#undef HAVE_SIGSET
#define HAVE_SIGSET 0
#endif
#endif

/* If we don't have restartable system calls, we can ignore
   fsysdep_catch, usysdep_start_catch and usysdep_end_catch.
   Otherwise fsysdep_catch has to do a setjmp.  */

#if ! HAVE_RESTARTABLE_SYSCALLS

#define fsysdep_catch() (TRUE)
#define usysdep_start_catch()
#define usysdep_end_catch()
#define CATCH_PROTECT

#else /* HAVE_RESTARTABLE_SYSCALLS */

#if HAVE_SETRET && ! HAVE_SIGSETJMP
#include <setret.h>
#define setjmp setret
#define longjmp longret
#define jmp_buf ret_buf
#else /* ! HAVE_SETRET || HAVE_SIGSETJMP */
#include <setjmp.h>
#if HAVE_SIGSETJMP
#undef setjmp
#undef longjmp
#undef jmp_buf
#define setjmp(s) sigsetjmp ((s), TRUE)
#define longjmp siglongjmp
#define jmp_buf sigjmp_buf
#endif /* HAVE_SIGSETJMP */
#endif /* ! HAVE_SETRET || HAVE_SIGSETJMP */

extern volatile sig_atomic_t fSjmp;
extern volatile jmp_buf sSjmp_buf;

#define fsysdep_catch() (setjmp (sSjmp_buf) == 0)

#define usysdep_start_catch() (fSjmp = TRUE)

#define usysdep_end_catch() (fSjmp = FALSE)

#define CATCH_PROTECT volatile

#endif /* HAVE_RESTARTABLE_SYSCALLS */

/* Get definitions for the terminal driver.  */

#if HAVE_BSD_TTY
#include <sgtty.h>
typedef struct sgttyb sterminal;
#define fgetterminfo(o, q) (ioctl ((o), TIOCGETP, (q)) == 0)
#define fsetterminfo(o, q) (ioctl ((o), TIOCSETN, (q)) == 0)
#define fsetterminfodrain(o, q) (ioctl ((o), TIOCSETP, (q)) == 0)
#endif /* HAVE_BSD_TTY */

#if HAVE_SYSV_TERMIO
#include <termio.h>
typedef struct termio sterminal;
#define fgetterminfo(o, q) (ioctl ((o), TCGETA, (q)) == 0)
#define fsetterminfo(o, q) (ioctl ((o), TCSETA, (q)) == 0)
#define fsetterminfodrain(o, q) (ioctl ((o), TCSETAW, (q)) == 0)
#endif /* HAVE_SYSV_TERMIO */

#if HAVE_POSIX_TERMIOS
#include <termios.h>
typedef struct termios sterminal;
#define fgetterminfo(o, q) (tcgetattr ((o), (q)) == 0)
#define fsetterminfo(o, q) (tcsetattr ((o), TCSANOW, (q)) == 0)
#define fsetterminfodrain(o, q) (tcsetattr ((o), TCSADRAIN, (q)) == 0)

/* On some systems it is not possible to include both <sys/ioctl.h>
   and <termios.h> in the same source files; I don't really know why.
   On such systems, we pretend that we don't have <sys/ioctl.h>.  */
#if ! HAVE_TERMIOS_AND_SYS_IOCTL_H
#undef HAVE_SYS_IOCTL_H
#define HAVE_SYS_IOCTL_H 0
#endif

#endif /* HAVE_POSIX_TERMIOS */

/* The root directory (this is needed by the system independent stuff
   as the default for local-send).  */
#define ZROOTDIR "/"

/* The name of the execution directory within the spool directory
   (this is need by the system independent uuxqt.c).  */
#define XQTDIR ".Xqtdir"

/* The name of the directory in which we preserve file transfers that
   failed.  */
#define PRESERVEDIR ".Preserve"

#if HAVE_TAYLOR_CONFIG

/* The names of the configuration files.  These are appended to
   NEWCONFIGLIB which is defined in Makefile.  */
#define CONFIGFILE "/config"
#define SYSFILE "/sys"
#define PORTFILE "/port"
#define DIALFILE "/dial"
#define CALLFILE "/call"
#define PASSWDFILE "/passwd"
#define DIALCODEFILE "/dialcode"

#endif /* HAVE_TAYLOR_CONFIG */

#if HAVE_V2_CONFIG

/* The names of the various V2 configuration files.  These are
   appended to OLDCONFIGLIB which is defined in Makefile.  */
#define V2_SYSTEMS "/L.sys"
#define V2_DEVICES "/L-devices"
#define V2_USERFILE "/USERFILE"
#define V2_CMDS "/L.cmds"
#define V2_DIALCODES "/L-dialcodes"

#endif /* HAVE_V2_CONFIG */

#if HAVE_BNU_CONFIG

/* The names of the BNU configuration files.  These are appended to
   OLDCONFIGLIB which is defined in Makefile.  */
#define BNU_SYSFILES "/Sysfiles"
#define BNU_SYSTEMS "/Systems"
#define BNU_PERMISSIONS "/Permissions"
#define BNU_DIALERS "/Dialers"
#define BNU_DEVICES "/Devices"
#define BNU_PERMISSIONS "/Permissions"
#define BNU_DIALCODES "/Dialcodes"
#define BNU_MAXUUXQTS "/Maxuuxqts"

#endif /* HAVE_BNU_CONFIG */

/* Get some standard definitions.  */

#include <sys/types.h>
#include <sys/stat.h>

/* Get definitions for the file permission bits.  */

#ifndef S_IRUSR

#define S_IRWXU 0700
#define S_IRUSR 0400
#define S_IWUSR 0200
#define S_IXUSR 0100

#define S_IRWXG 0070
#define S_IRGRP 0040
#define S_IWGRP 0020
#define S_IXGRP 0010

#define S_IRWXO 0007
#define S_IROTH 0004
#define S_IWOTH 0002
#define S_IXOTH 0001

#endif /* ! defined (S_IRUSR) */

#ifndef S_ISDIR
#ifdef S_IFDIR
#define S_ISDIR(i) (((i) & S_IFMT) == S_IFDIR)
#else /* ! defined (S_IFDIR) */
#define S_ISDIR(i) (((i) & 0170000) == 040000)
#endif /* ! defined (S_IFDIR) */
#endif /* ! defined (S_ISDIR) */

/* We create files with these modes (should this be configurable?).  */
#define IPRIVATE_FILE_MODE (S_IRUSR | S_IWUSR)
#define IPUBLIC_FILE_MODE (S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)

/* We create directories with this mode (should this be configurable?).  */
#define IDIRECTORY_MODE (S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH)
#define IPUBLIC_DIRECTORY_MODE (S_IRWXU | S_IRWXG | S_IRWXO)

#if ! HAVE_OPENDIR

/* Define some structures to use if we don't have opendir, etc.  These
   will only work if we have the old Unix filesystem, with a 2 byte
   inode and a 14 byte filename.  */

#include <sys/dir.h>

struct dirent
{
  char d_name[DIRSIZ + 1];
};

typedef struct
{
  int o;
  struct dirent s;
} DIR;

extern DIR *opendir P((const char *zdir));
extern struct dirent *readdir P((DIR *));
extern int closedir P((DIR *));

#endif /* ! HAVE_OPENDIR */

/* Information we need for a UNIX serial port.  */

struct ssysdep_serial_port
{
  /* File descriptor to read from.  */
  int oread;
  /* Whether the read descriptor is blocking.  */
  boolean fread_blocking;
  /* File descriptor to write to.  */
  int owrite;
  /* Whether the write descriptor is blocking.  */
  boolean fwrite_blocking;
  /* Whether setting the read descriptor to blocking affects the write
     descriptor and vice-versa.  */
  boolean fblock_both;
  /* Hold the real read descriptor when using a dialer device.  */
  int oholdread;
  /* Hold the real write descriptor when using a dialer device.  */
  int oholdwrite;
  /* TRUE if this is a terminal and the remaining fields are valid.  */
  boolean fterminal;
  /* Baud rate.  */
  long ibaud;
  /* Original terminal settings.  */
  sterminal sorig;
  /* New terminal settings (raw mode, etc.).  */
  sterminal snew;
};

/* Information we need for a UNIX stdin port.  */

struct ssysdep_stdin_port
{
  struct ssysdep_serial_port s;
  boolean fpty;
};

/* For debugging it is helpful to know whether we are connected
   through a pty; the fsserial_read routine uses this information to
   know whether to sleep or not.  */
#define SYSDEP_STDIN_CMDS(s) \
 { "pty", CMDTABTYPE_BOOLEAN, (pointer) &(s).fpty, NULL }

#define SYSDEP_STDIN_INIT(q) ((q)->fpty = FALSE, (q)->s.oread = -1)

/* Information we need for a UNIX modem port.  */

struct ssysdep_modem_port
{
  struct ssysdep_serial_port s;
};

#define SYSDEP_MODEM_INIT(q) ((q)->s.oread = -1)

/* Information we need for a UNIX direct port.  */

struct ssysdep_direct_port
{
  struct ssysdep_serial_port s;
};

#define SYSDEP_DIRECT_INIT(q) ((q)->s.oread = -1)

/* Set a signal handler.  */
extern void usset_signal P((int isig, SIGtype (*pfn) P((int)),
			    boolean fforce, boolean *pfignored));

/* Default signal handler.  This sets the appropriate element of the
   afSignal array.  If system calls are automatically restarted, it
   may do a longjmp to an fsysdep_catch.  */
extern SIGtype ussignal P((int isig));

/* Try to fork, repeating several times.  */
extern pid_t isfork P((void));

/* Spawn a job.  Returns the process ID of the spawned job or -1 on
   error.  The following macros may be passed in aidescs.  */

/* Set descriptor to /dev/null.  */
#define SPAWN_NULL (-1)
/* Set element of aidescs to a pipe for caller to read from.  */
#define SPAWN_READ_PIPE (-2)
/* Set element of aidescs to a pipe for caller to write to.  */
#define SPAWN_WRITE_PIPE (-3)

extern pid_t isspawn P((const char **pazargs, int *aidescs,
			boolean fkeepuid, boolean fkeepenv,
			const char *zchdir, boolean fnosigs,
			boolean fshell, const char *zpath,
			const char *zuu_machine,
			const char *zuu_user));

/* Do a form of popen using isspawn.  */
extern FILE *espopen P((const char **pazargs, boolean frd,
			pid_t *pipid));

/* Wait for a particular process to finish, returning the exit status.
   The process ID should be pid_t, but we can't put that in a
   prototype.  */
extern int iswait P((unsigned long ipid, const char *zreport));

/* Lock a string.  */
extern boolean fsdo_lock P((const char *, boolean fspooldir));

/* Unlock a string.  */
extern boolean fsdo_unlock P((const char *, boolean fspooldir));

/* See whether a directory exists.  */
extern boolean fsdirectory_exists P((const char *));

/* Expand a leading tilde to the public directory or a user's home
   directory.  Each system may have a different public directory, so
   the system information must be passed in.  */
extern const char *zstilde_expand P((const struct ssysteminfo *qsys,
				     const char *zfile));

/* Check access for a particular user name, or NULL to check access
   for any user.  */
extern boolean fsuser_access P((const struct stat *, int imode,
				const char *zuser));

/* Copy a string to the common static buffer.  */
extern const char *zscopy P((const char *z));

/* Prepend a directory to a file name, returning the common static
   buffer.  */
extern const char *zsappend P((const char *zdir, const char *zfile));

/* Get a temporary file name.  */
extern const char *zstemp_file P((const struct ssysteminfo *qsys));

/* Get a jobid from a system and file name.  */
extern const char *zsfile_to_jobid P((const struct ssysteminfo *qsys,
				     const char *zfile));

/* Get a file name from a jobid.  This also returns the associated system
   in *pzsystem.  */
extern const char *zsjobid_to_file P((const char *zid,
				      const char **pzsystem));

#if SPOOLDIR_ULTRIX
/* See whether there is a spool directory for a system.  */
extern boolean fsultrix_has_spool P((const char *zsystem));
#endif /* SPOOLDIR_ULTRIX */

/* The working directory from which the program was run (this is set
   by usysdep_initialize if called with fgetcwd TRUE).  */
extern char *zScwd;

/*
  Local variables:
  mode:c
  End:
  */
#endif /* ! defined (SYSH_UNX_H) */
