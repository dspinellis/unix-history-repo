/*	@(#)mach.h	4.1	(Berkeley)	%G%	*/

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

DELIVERM	Uses the delivermail program

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
in "/usr/include/whoami.h" on the local machine.

For "normal" version 6 machines, there is a dummy machine
definition for "VANILLA6" which indicates the v6 UNIX options.

For "normal" version 7 machines, there is a dummy machine
definition for "VANILLA7" which indicates the v7 UNIX options.
(VM/UNIX and Berkeley VM/VAX/UNIX can use this)
*/
/* be sure to include <stdio.h> before these defns */

# include <whoami.h>
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
/* definitions for Berkeley */
# ifdef A
# define CCV7
# define LOCAL 'a'
# endif A

# ifdef B
# define CCV7
# define LOCAL 'b'
# endif B

# ifdef C
# define CCV7
# define LOCAL 'c'
# endif C

# ifdef D
# define CCV7
# define LOCAL 'd'
# endif D

# ifdef E
# define CCV7
# define LOCAL 'e'
# endif E

# ifdef F
# define CCV7
# define LOCAL 'f'
# endif F

# ifdef G
# define LOCAL 'g'
# define NUID (501)
# endif G

# ifdef ING70
# define V6
# define OLDTTY
# define UID1CHAR
# define PASSWDF
# define DELIVERM
# define MULTNAMS
# define FREELPR
# define LOCAL 'i'
# define NUID (174)
/* correct vers. 7 = LOCAL, NUID */
# endif ING70

# ifdef INGVAX
# define LOCAL 'j'
# define NUID (37)
# define FREELPR
# define DELIVERM
# endif INGVAX

# ifdef VIRUS
# define LOCAL 'k'
# define NUID (-1)
# endif VIRUS

# ifdef IMAGE
# define LOCAL 'm'
# define NUID (120)
# define MAXSENDQ 35
# include <signal.h>
/* on some v7 cory-derivative systems, this is defined AFTER this point
   so you have to give the -l flag to the netdaemon, or remove it.
   it is usually in /usr/include/sys/ioctl.h
*/
# undef NETLDISC
# define DELIVERM
# endif IMAGE

# ifdef KIM
# define LOCAL 'n'
# define NUID (XXX)
# endif KIM

# ifdef ESVAX
# define LOCAL 'o'
# define NUID (67)
# endif ESVAX

# ifdef CAD
# define LOCAL 'p'
# define NUID (67)
# endif CAD

# ifdef Q
# define V6
# define CCV6
# define OLDTTY
# define FUID
# define PASSWDF
# define USRMAIL
# define NOEUID
# define LOCAL 'q'
# define NOREMACCT
# define MAXSENDQ
# define NUID ((11 << 8) | 38)
# define MAXSENDQ 35
# define CRN
# define MAGICCRN	"3700"		/* default CC crn */
/* correct vers. 7 = LOCAL, NUID */
# endif Q

# ifdef ARPAVAX
# define LOCAL 'r'
# define NUID (501)
# define DELIVERM
# define MAXSENDQ 35
# endif ARPAVAX

# ifdef SRC
# define V6
# define OLDTTY
# define FUID
# define NOEUID
# define LOCAL 's'
# define NUID  38
# define USRMAIL
/* correct vers. 7 = LOCAL, NUID */
# endif SRC

# ifdef MATHSTAT
# define LOCAL 't'
# define MAXSENDQ 35
# define NUID (31)
# include <signal.h>
/* on some v7 cory-derivative systems, this is defined AFTER this point
   so you have to give the -l flag to the netdaemon, or remove it.
   it is usually in /usr/include/sys/ioctl.h
*/
# undef NETLDISC
# define DELIVERM
# endif MATHSTAT

# ifdef CSVAX
# define LOCAL 'v'
# define NUID (501)
# define DELIVERM
# define MAXSENDQ 35
# endif CSVAX

# ifdef ONYX
# define LOCAL 'x'
# define NUID (10)
# define NOFP
# define SWAB
/* on the version 6 systems at Berkeley, the best versions of cc (e.g. ncc)
   can't take this nested undef so you must delete it when compiling on
   version 6 systems */
# undef PARMLIST
# define PARMLIST 20
# endif ONYX

# ifdef CORY
# define LOCAL 'y'
# define NUID (10)
# define MAXSENDQ 35
# include <signal.h>
/* on some v7 cory-derivative systems, this is defined AFTER this point
   so you have to give the -l flag to the netdaemon, or remove it.
   it is usually in /usr/include/sys/ioctl.h
*/
# undef NETLDISC
# define DELIVERM
# endif CORY

# ifdef EECS40
# define V6
# define OLDTTY
# define PASSWDF
# define UID1CHAR
# define LOCAL 'z'
# define NUID ((1 << 8) | 104)
# define NFREECMD
# define NOFP
/* this is necessary on 11/40's since the netdaemon is too
   big without split I/D when parmlst is 2000
   */
# undef PARMLIST
# define PARMLIST 50
/* correct vers. 7 = LOCAL, NUID */
# endif EECS40

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
# include <sys/dir.h>
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
# define DIRSIZ 14
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


struct stat {
	int st_dev;
	int st_ino;
	int st_mode;
	char st_nlink:8;
	char st_uid:8;
	char st_gid:8;
	char st_size0;
	int st_size1;
	int st_addr[8];
	long st_atime;
	long st_mtime;
	};
struct direct {
	int d_ino;
	char d_name[DIRSIZ];
	};
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
