/*	@(#)mach.h	4.6	(Berkeley)	%G%	*/

/* sccs id variable */
static char *mach_h_sid = "@(#)mach.h	1.11";

/*

	mach.h	-- define machine-dependent things

*** Pre-processor Flags ***

This set of code is controlled by this set of conditional
compilation flags:

TESTING		if defined, do not generate tests, etc. which require
		super-user status.

OLDTTY		if defined, compile for old 1 character TTY names
CCTTY		if defined, compile for CC tty name format
		if neither is defined, use v7 ttyname format

PASSWDF		compile in code to handle /etc/passwdf - split passwd files

V6		Assume the v6 features instead of the v7 ones.

FUID		use the funny uid's present on CC V6

SENDMAIL	Uses the sendmail program

HPASSWD		The local machine has the hashed password stuff

OLDMAIL		mail is in home-directory/.mail
USRMAIL		mail is in /usr/mail/name
		(mail is in /usr/spool/mail/name)

CC		Machine is a Computer Center machine
NUID		userid (as returned by getuid()) on this machine
UID1CHAR	Uses vanila Version6 userid's (only 1 byte for uid)
NOEUID		Does not have the geteuid() system call
NFREECMD	doesn't allow any "network" free commands
NOFP		floating point just doesn't work on this machine
NOREMACCT	allows netlpr's on remote machines without an account
CSH		use the /bin/csh shell (Paths.h sets BINSH to this path ).
CRN		CC crn's are passed
DONTHOLDBIG	large (size > MAXDAYFILE ) jobs wont be held until night for
		transmission

SWAB		this machine has byte-ordering reversed from the DEC VAX 
		and PDP-11 standard (the only current example is Onyx)
The conditonal flags are first defined
in "local.h" in this directory.

For "normal" version 6 machines, there is a dummy machine
definition for "VANILLA6" which indicates the v6 UNIX options.

For "normal" version 7 machines, there is a dummy machine
definition for "VANILLA7" which indicates the v7 UNIX options.
(VM/UNIX and Berkeley VM/VAX/UNIX can use this)
*/
/* be sure to include <stdio.h> before these defns */

# include "local.h"
# include <sysexits.h>

/* note NUID is only used in mmail.c */

# ifdef RAND
/* definitions for Rand-Unix */
# ifdef VAX
# define LOCAL 'v'
# define NUID (-1)
# endif VAX

# ifdef GRAPHICS
# define V6
# define OLDTTY
# define UID1CHAR
# define OLDMAIL
# define LOCAL 'g'
# define NUID (-1)
# endif GRAPHICS

# ifdef TP
# define LOCAL 't'
# define V6
# define OLDTTY
# define UID1CHAR
# define OLDMAIL
# define NUID (-1)
# endif TP

/* end of definitions for Rand */
# endif RAND


# ifdef NOSC
/* definitions for Naval Ocean Systems Center NOSC */
# ifdef ATTS
# define LOCAL 'a'
# define V6
# define OLDTTY
# define UID1CHAR
# define OLDMAIL
# define NUID (-1)
# endif ATTS

# ifdef CCMM
# define LOCAL 'c'
# define V6
# define OLDTTY
# define UID1CHAR
# define OLDMAIL
# define NUID (-1)
# endif CCMM

# ifdef MSSF
# define V6
# define OLDTTY
# define UID1CHAR
# define OLDMAIL
# define LOCAL 'm'
# define NUID (-1)
# endif MSSF

/* end of definitions for NOSC */

# endif NOSC

# ifdef BERKELEY

/* all berkeley sites use sendmail.....         (someday?) */
# define SENDMAIL

/* CFO - A */
# ifdef A
# define CCV7
# define LOCAL 'a'
# endif A

/* CFO - B */
# ifdef B
# define CCV7
# define LOCAL 'b'
# endif B

/* CFO - C */
# ifdef C
# define CCV7
# define LOCAL 'c'
# endif C

/* CFO - D */
# ifdef D
# define CCV7
# define LOCAL 'd'
# endif D

/* CFO - E */
# ifdef E
# define CCV7
# define LOCAL 'e'
# endif E

/* CFO - F */
# ifdef F
# define CCV7
# define LOCAL 'f'
# endif F

/* CFO - G */
# ifdef G
# define LOCAL 'g'
# define NUID (501)
# endif G

/* CFO - Jade */
# ifdef H
# define LOCAL 'h'
# define NUID (501)
# endif H

/* CSSG - ucberl70 */
# ifdef ERL70
# define LOCAL 'i'
# define NUID (174)
# endif ERL70

/* Ingres Group - ucbingres */
# ifdef INGVAX
# define LOCAL 'j'
# define NUID (37)
# define FREELPR
# endif INGVAX

/* CS network hub - ucbvax */
# ifdef UCBVAX
# define LOCAL 'k'
# define NUID (35)
# define MAXSENDQ 35
# endif UCBVAX

/* Brodersen - ucboz */
# ifdef OZ
# define LOCAL 'l'
# define NUID (501)
# endif OZ

/* EE-Signal Proccessing - ucbmedea */
# ifdef MEDEA
# define LOCAL 'm'
# define NUID (501)
# endif MEDEA

/* Fateman - ucbkim */
# ifdef KIM
# define LOCAL 'n'
# define NUID (501)
# endif KIM

/* EECS-Research - ucbesvax */
# ifdef ESVAX
# define LOCAL 'o'
# define NUID (67)
# endif ESVAX

/* Newton CAD - ucbcad */
# ifdef CAD
# define LOCAL 'p'
# define NUID (67)
# endif CAD

/* currently unused */
# ifdef Q
# define LOCAL 'q'
# endif Q

/* Fabry CSRG - ucbarpa */
# ifdef ARPAVAX
# define LOCAL 'r'
# define NUID (501)
# define MAXSENDQ 35
# endif ARPAVAX

/* CFO & SRC - SRC */
# ifdef SRC
# define LOCAL 's'
# define NUID  38
# endif SRC

/* Math/Stat Dept - MathStat */
# ifdef MATHSTAT
# define LOCAL 't'
# define NUID (-1)
# endif MATHSTAT

/* Fabry CSRG - c70 */
# ifdef C70
# define LOCAL 'u'
# define NUID (501)
# define MAXSENDQ 35
# endif C70

/* CS Research - ucbernie */
# ifdef CSVAX
# define LOCAL 'v'
# define NUID (501)
# define MAXSENDQ 35
# endif CSVAX

/* Stat Dept - statvax */
# ifdef STATVAX
# define LOCAL 'w'
# define NUID
# endif STATVAX

/* CS Research - Onyx */
# ifdef ONYX
# define LOCAL 'x'
# define NUID (10)
# define NOFP
# define SWAB
# undef PARMLIST
# define PARMLIST 20
# endif ONYX

/* EECS Dept - Cory */
# ifdef CORY
# define LOCAL 'y'
# define NUID (10)
# define MAXSENDQ 35
# include <signal.h>
# undef NETLDISC
# endif CORY

/* EECS Dept Administrative - ucbear */
# ifdef EARVAX
# define LOCAL 'z'
# define NUID ((1 << 8) | 104)
# endif EARVAX

/* end of Berkeley definitions */
# endif BERKELEY

/*
	the CC V6 machines are all the same.
	splitting their type into a separate group will
	allow the binary patching program "patchd" to be
	used to patch the binaries so the sources can be compiled
	on one CC machine and the binaries shipped around
	to the other CC machines.
*/
# ifdef CCV7
# undef CC
# define NOREMACCT
# define NUID (10)
# define MAXSENDQ 35
# define CSH
# define CRN
# define MAGICCRN	"3700"		/* default CC crn */
# endif CCV7

# ifdef CC
# define V6
# define CCV6
# define CCTTY
# define PASSWDF
# define FUID
# define USRMAIL
# define NUID (115)
# define MAXSENDQ 35
# define NOREMACCT
# define CSH
# define CRN
# define MAGICCRN	"3700"		/* default CC crn */
# endif CC

/* default version 6 options */
# ifdef VANILLA6
# define V6
# define UID1CHAR
# define OLDTTY
# define OLDMAIL
# endif VANILLA6

/* default version 7 options */
# ifdef VANILLA7
# endif VANILLA7

# ifndef V6
# include <sys/param.h>
# include <sys/stat.h>
# include <dir.h>
# include <sys/times.h>
# include <ctype.h>
# include <sgtty.h>
# include <errno.h>
# include <pwd.h>
# include <assert.h>
# include <setjmp.h>
# define getsize(S)	((S)->st_size)
# define gettime()	(time(0))
int	(*signal())();
/*
#define	SIG_DFL	(int (*)())0
#define	SIG_IGN	(int (*)())1
*/

# else V6

# define ETXTBSY 26
# define S_IREAD 0400
# define S_IFMT 060000
# define S_IFDIR 0040000
# define ANYP 0300
# define ECHO 010
# define ROOTINO 1
# define TIOCEXCL 0
# define SIGHUP		1
# define SIGINT 	2
# define SIGQUIT 	3
# define SIGKILL		9
# define SIGALRM 	14
# define SIGTERM 	15
# define SIG_IGN	1
# define ASSERT "Assertion failed: file %s, line %d\n", __FILE__, __LINE__);exit(1);}}
# define assert(ex) {if (!(ex)){fprintf(stderr,ASSERT
# define isprint(c) (040 <= c && c <= 0176)
# define longjmp(a,b) reset()
# define setjmp(a) setexit()


struct tms {				/* see times - sect 2 */
	int	tms_utime;		/* user time */
	int	tms_stime;		/* system time */
	long	tms_cutime;		/* user time, children */
	long	tms_cstime;		/* system time, children */
	};
struct sgttyb {
	char sg_ispeed;
	char sg_ospeed;
	char sg_erase;
	char sg_kill;
	int sg_flags;
	};
struct	passwd { /* see getpwent(3) */
	char	*pw_name;
	char	*pw_passwd;
	int	pw_uid;
	int	pw_gid;
	int	pw_quota;
	char	*pw_comment;
	char	*pw_gecos;
	char	*pw_dir;
	char	*pw_shell;
	};
/* /usr/include/varargs.h */
typedef char *va_list;
# define va_dcl int va_alist;
# define va_start(list) list = (char *) &va_alist
# define va_end(list)
# define va_arg(list,mode) ((mode *)(list += sizeof(mode)))[-1]

typedef int jmp_buf[10];
long gettime(), getsize();
# endif V6
/* end of non-v7 defns */

# ifdef FUID
# define getgid(s) (0)
# endif FUID

# ifdef UID1CHAR
# define uidmask(S) (S & 0377)
# define geteuid() ((getuid()>>8)&0377)
# else UID1CHAR
# define uidmask(S) (S)
# endif UID1CHAR

# ifdef NOEUID
# define geteuid(S) (-1)
# endif NOEUID

# ifdef CCTTY
# define ttyname(S) myttyname(S)
# endif CCTTY

#ifdef CRN
#include	<gecos.h>
#endif CRN

# ifdef OLDTTY
/* this is the version 7 utmp structure. the getutmp() procedure
   converts the v6 structure into this format
*/
struct utmp {
	char	ut_line[8];		/* tty name */
	char	ut_name[8];		/* user id */
	long	ut_time;		/* time on */
};
# else OLDTTY
# include <utmp.h>
# endif OLDTTY

# define chfromf(S) (S ? 'T' : 'F')

/* functions */

char *hgethome(), *calloc(), *ctime(), *getenv();
char *getname(), *getun(), *getlogin();
char *SnFromUid(), *ttyname();
struct passwd *getpwnam(), *getpwuid();
long atol(),time();
struct utmp *getutmp();
