case $CONFIG in
'') . ./config.sh ;;
esac
case "$0" in
*/*) cd `expr X$0 : 'X\(.*\)/'` ;;
esac
echo "Extracting config.h (with variable substitutions)"
sed <<!GROK!THIS! >config.h -e 's!^#undef!/\*#define!'
/*
 * This file was produced by running the config.h.SH script, which
 * gets its values from config.sh, which is generally produced by
 * running Configure.
 *
 * Feel free to modify any of this as the need arises.  Note, however,
 * that running config.h.SH again will wipe out any changes you've made.
 * For a more permanent change edit config.sh and rerun config.h.SH.
 *
 * \$Id: Config.h.U,v 2.8.1.2 91/10/11 10:07:56 ram Exp Locker: ram $
 */

/* Configuration time: $cf_time
 * Configured by: $cf_by
 * Target system: $myuname
 */

#ifndef _config_h_
#define _config_h_

/* EUNICE:
 *	This symbol, if defined, indicates that the program is being compiled
 *	under the EUNICE package under VMS.  The program will need to handle
 *	things like files that don't go away the first time you unlink them,
 *	due to version numbering.  It will also need to compensate for lack
 *	of a respectable link() command.
 */
/* VMS:
 *	This symbol, if defined, indicates that the program is running under
 *	VMS.  It is currently only set in conjunction with the EUNICE symbol.
 */
#$d_eunice EUNICE		/**/
#$d_eunice VMS		/**/

/* bcmp:
 *	This symbol is maped to memcmp if the bcmp() routine is not
 *	available to compare strings.
 */
#$d_bcmp bcmp(s,d,l) memcmp((s),(d),(l))	/* mapped to memcmp */

/* bcopy:
 *	This symbol is maped to memcpy if the bcopy() routine is not
 *	available to copy strings.
 */
#$d_bcopy bcopy(s,d,l) memcpy((d),(s),(l))		/* mapped to memcpy */

/* bzero:
 *	This symbol is maped to memset if the bzero() routine is not
 *	available to set memory to 0.
 */
#$d_bzero bzero(s,l) memset((s),0,(l))	/* mapped to memset */

/* HAS_GETPWENT
 *	This symbol, if defined, indicates that the getpwent() routine
 *	should be used instead of the getpw() routine.
 */
#$d_getpwent HAS_GETPWENT	/**/

/* HAS_TERMLIB
 *	This symbol, when defined, indicates that termlib-style routines
 *	are available.  There is nothing to include.
 */
#$d_havetlib	HAS_TERMLIB	/**/

/* NEWS_ADMIN
 *	This symbol, if defined, contains the login name of the news
 *	administrator.
 */
#$d_newsadm NEWS_ADMIN "$newsadmin"		/**/

/* NOLINEBUF:
 *	This symbol, if defined, indicates that stdout is not buffered, so that
 *	the program can call setbuf() or setlinebuf() for efficiency.
 */
#$d_nolnbuf	NOLINEBUF	/**/

/* NORMSIG:
 *	This symbol, if defined, indicates that normal signal handling routines
 *	should be used, as opposed to the ones in 4.1bsd (sigset, etc.).
 */
#$d_normsig NORMSIG		/**/

/* HAS_RDCHK
 *	This symbol, if defined, indicates that the rdchk routine is available
 *	to find out if there is input pending on an IO channel.  Generally
 *	the routine is used only if FIONREAD and O_NDELAY aren't available.
 */
#$d_rdchk HAS_RDCHK		/**/

/* HAS_RENAME
 *	This symbol, if defined, indicates that the rename routine is available
 *	to rename files.  Otherwise you should do the unlink(), link(), unlink()
 *	trick.
 */
#$d_rename HAS_RENAME	/**/

/* HAS_SIGHOLD:
 *	This symbol, if defined, indicates that the sighold routine is
 *	available to hold signals.
 */
#$d_sighold HAS_SIGHOLD	/**/

/* Signal_t
 *	This symbol's value is either "void" or "int", corresponding to the
 *	appropriate return type of a signal handler.  Thus, you can declare
 *	a signal handler using "Signal_t (*handler)()", and define the
 *	handler using "Signal_t handler(sig)".
 */
#define Signal_t $signal_t	/* Signal handler's return type */

/* I_DIRENT:
 *	This symbol, if defined, indicates to the C program that it should
 *	include <dirent.h>.
 */
#$i_dirent I_DIRENT		/**/

/* I_FCNTL
 *	This manifest constant tells the C program to include <fcntl.h>.
 */
#$i_fcntl I_FCNTL	/**/

/* I_SYS_DIR
 *	This symbol, if defined, indicates to the C program that it should
 *	include <sys/dir.h>.
 */
#$i_sysdir I_SYS_DIR		/**/

/* I_SYS_IOCTL
 *	This symbol, if defined, indicates that <sys/ioctl.h> exists and should
 *	be included. Otherwise, include <sgtty.h> or <termio.h>.
 */
#$i_sysioctl	I_SYS_IOCTL		/**/

/* I_SYS_NDIR
 *	This symbol, if defined, indicates to the C program that it should
 *	include <sys/ndir.h>.
 */
#$i_sysndir I_SYS_NDIR	/**/

/* I_VFORK:
 *	This symbol, if defined, indicates to the C program that it should
 *	include vfork.h.
 */
#$i_vfork I_VFORK	/**/

/* MAILFILE:
 *	This symbol contains the interpretable name of the mail spool file
 *	for the current user.  The program must be prepared to substitute
 *	the HOME directory for %~, and the login id for %L.
 */
#define MAILFILE "$mailfile"		/**/

/* MBOXCHAR:
 *	This symbol contains a character which will match the beginning
 *	of a mailbox file.
 */
#define MBOXCHAR '$mboxchar'		/**/

/* ORGNAME:
 *	This symbol contains either the organizaton name or the full pathname
 *	of a file containing the organization name, which the program must
 *	be prepared to open and substitute the contents of.
 */
#define ORGNAME "$orgname"		/**/

/* PREFSHELL:
 *	This symbol contains the full name of the preferred user shell on this
 *	system.  Usual values are /bin/csh, /bin/ksh, /bin/sh.
 */
#define PREFSHELL "$prefshell"		/**/

/* ROOTID:
 *	This symbol contains the uid of root, normally 0.
 */
#define ROOTID $rootid		/**/

/* ACTIVE:
 *	The name of the active file for the news system.  This file contains
 *	the list of active newsgroups.  The name may have ~ on the front.
 */
/* ACTIVE_TIMES:
 *	The name of the active.times file for the news system.
 */
#define ACTIVE "$active"		/**/
#$d_acttimes ACTIVE_TIMES "$acttimes"		/**/

/* HAS_FTIME:
 *	This symbol, if defined, indicates that the ftime() routine exists.
 *	It is basically a sub-second accuracy clock, but is less accurate
 *	than gettimeofday(2) anyway. The type "Timeval" should be used to
 *	refer to "struct timeb".
 */
#$d_ftime HAS_FTIME		/**/

/* HAS_GETHOSTNAME
 *	This symbol, if defined, indicates that the C program may use the
 *	gethostname() routine to derive the host name.  See also HAS_UNAME
 *	and PHOSTCMD.
 */
/* HAS_UNAME
 *	This symbol, if defined, indicates that the C program may use the
 *	uname() routine to derive the host name.  See also HAS_GETHOSTNAME
 *	and PHOSTCMD.
 */
/* PHOSTCMD:
 *	This symbol, if defined, indicates that the C program may use the
 *	contents of PHOSTCMD as a command to feed to the popen() routine
 *	to derive the host name.  See also HAS_GETHOSTNAME and HAS_UNAME.
 *	Note that the command uses a fully qualified path, so that it is safe
 *	even if used by a process with super-user privileges.
 */
#$d_gethname HAS_GETHOSTNAME	/**/
#$d_uname HAS_UNAME		/**/
#$d_phostcmd PHOSTCMD "$aphostcmd"	/* How to get the host name */

/* HAS_GETWD
 *	This symbol, if defined, indicates that the getwd routine is
 *	available to get the working directory.
 */
/* HAS_GETCWD:
 *	This symbol, if defined, indicates that the getcwd routine is
 *	available.  This is never defined if getwd is found first.
 */
#$d_getwd HAS_GETWD		/**/
#$d_getcwd HAS_GETCWD		/**/

/* IGNOREORG:
 *	This symbol, if defined, indicates that the ORGANIZATION environment
 *	variable does not contain an organization name.
 */
#$d_ignoreorg IGNOREORG		/**/

/* INTERNET:
 *	This symbol, if defined, indicates that there is a mailer available
 *	which supports internet-style addresses (user@site.domain).
 */
#$d_internet	INTERNET	/**/

/* USE_NNTP:
 *	This symbol, if defined, indicates that NNTP should be used.
 */
/* SERVER_NAME:
 *	When using NNTP, this symbol indicates the server name or a
 *	file to open to read the server name.
 */
/* USE_XTHREAD:
 *	This symbol, if defined, indicates that .thread files can be
 *	gotten via NNTP.  It is never defined if USE_NNTP is undefined.
 */
/* USE_XOVER:
 *	This symbol, if defined, indicates that .overview files can be
 *	gotten via NNTP.  It is never defined if USE_NNTP is undefined.
 */
#$d_nntp	USE_NNTP	/**/
#define SERVER_NAME "$servername"  	/**/
#$d_xdata USE_XTHREAD  	/**/
#$d_xdata USE_XOVER	/**/

/* vfork:
 *	This symbol, if defined, remaps the vfork routine to fork if the
 *	vfork() routine isn't supported here.
 */
#$d_novfork vfork fork	/**/

/* void:
 *	This symbol is used for void functions.  On implementations which
 *	support void appropriately, its value is "void".  Otherwise, its
 *	value should be set to "int".
 */
#$d_novoid void int	/**/

/* HAS_SIGBLOCK:
 *	This symbol, if defined, indicates that the sigblock routine is
 *	available to block signal reception.
 */
#$d_sigblock HAS_SIGBLOCK	/**/

/* size_t:
 *	This symbol is defined as an int if no size_t definition exists.
 */
#$d_sizet	size_t int		/**/

/* index:
 *	This preprocessor symbol is defined, along with rindex, if the system
 *	uses the strchr and strrchr routines instead.
 */
/* rindex:
 *	This preprocessor symbol is defined, along with index, if the system
 *	uses the strchr and strrchr routines instead.
 */
#$d_strchr	index strchr	/**/
#$d_strchr	rindex strrchr	/**/

/* HAS_STRFTIME:
 *	This symbol, if defined, indicates that the strftime routine is
 *	available.
 */
#$d_strftime	HAS_STRFTIME		/**/

/* EMULATE_NDIR:
 *	This symbol, if defined, indicates that the program should compile
 *	the ndir.c code provided with the package.
 */
/* I_NDIR:
 *	This symbol, if defined, indicates that the program should include the
 *	system's version of ndir.h, rather than the one with this package.
 */
#$d_usendir	EMULATE_NDIR		/**/
#$d_libndir	I_NDIR		/**/

/* DEFEDITOR:
 *	This symbol contains the full pathname of the default editor.
 */
#define DEFEDITOR "$defeditor"		/**/

/* I_PTEM:
 *	This symbol, if defined, indicates to the C program that it should
 *	include ptem.h.
 */
#$i_ptem I_PTEM		/**/

/* I_TERMIO:
 *	This symbol, if defined, indicates that the program should include
 *	<termio.h> rather than <sgtty.h>.  There are also differences in
 *	the ioctl() calls that depend on the value of this symbol.
 */
/* I_TERMIOS:
 *	This symbol, if defined, indicates that the program should include
 *	the POSIX termios.h rather than sgtty.h or termio.h.
 *	There are also differences in the ioctl() calls that depend on the
 *	value of this symbol.
 */
/* I_SGTTY:
 *	This symbol, if defined, indicates that the program should include
 *	<sgtty.h> rather than <termio.h>.  There are also differences in
 *	the ioctl() calls that depend on the value of this symbol.
 */
#$i_termio I_TERMIO		/**/
#$i_termios I_TERMIOS		/**/
#$i_sgtty I_SGTTY		/**/

/* PASSNAMES:
 *	This symbol, if defined, indicates that full names are stored in
 *	the /etc/passwd file.
 */
/* BERKNAMES:
 *	This symbol, if defined, indicates that full names are stored in
 *	the /etc/passwd file in Berkeley format (name first thing, everything
 *	up to first comma, with & replaced by capitalized login id, yuck).
 */
#$d_passnames PASSNAMES /*  (undef to take name from ~/.fullname) */
#$d_berknames BERKNAMES /* (that is, ":name,stuff:") */

/* NEWSLIB:
 *	This symbol contains the name of the directory serving as the news
 *	library.  The program must be prepared to do ~ expansion on it.
 */
#define NEWSLIB "$newslib"		/**/

/* NEWSSPOOL:
 *	This symbol contains the directory name where news articles are
 *	spooled.  The program must be prepared to do ~ expansion on it.
 */
#define NEWSSPOOL "$newsspool"		/**/

/* PHOSTNAME:
 *	This symbol contains the posting host's name or a file from which
 *	to read its name.
 */
#define PHOSTNAME "$phost"		/**/

/* PRIVLIB:
 *	This symbol contains the name of the private library for this package.
 *	The library is private in the sense that it needn't be in anyone's
 *	execution path, but it should be accessible by the world.  The program
 *	should be prepared to do ~ expansion.
 */
#define PRIVLIB "$privlib"		/**/

/* THREAD_DIR:
 *	This symbol indicates where the .thread files go.
 */
/* OVERVIEW_DIR:
 *	This symbol indicates where the .overview files go.
 */
/* USE_MT:
 *	This symbol indicates if .thread file support is desired.
 */
/* USE_OV:
 *	This symbol indicates if .overview file support is desired.
 */
#define THREAD_DIR	"$threaddir"		/**/
#define OVERVIEW_DIR	"$overviewdir"		/**/
#$d_usemt USE_MT		/**/
#$d_useov USE_OV		/**/

/* THREAD_INIT:
 *	This symbol indicates we act like trn no matter what our name is.
 */
/* SELECT_INIT:
 *	This symbol indicates we default to the selector for group entry.
 */
#define THREAD_INIT	$trn_init
#define SELECT_INIT	$trn_select

#undef	LONG_THREAD_NAMES	/**/

#define CANCEL	"$inewsloc -h <%h"
#define SPEED_OVER_MEM	/* use more memory to run faster */
/*#define ANCIENT_NEWS	/* if your B news system is <= 2.10.1 */

#endif
!GROK!THIS!
