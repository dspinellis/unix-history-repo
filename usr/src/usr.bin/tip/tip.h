/*	tip.h	4.1	81/05/09	*/
/*
 * tip - terminal interface program
 *
 * Samuel J. Leffler
 */

#include <sgtty.h>
#include <signal.h>
#include <stdio.h>
#include <pwd.h>
#include <sys/types.h>

/*
 * Remote host attributes
 */
char	*DV;			/* UNIX device(s) to open */
char	*EL;			/* chars marking an EOL */
char	*CM;			/* initial connection message */
char	*IE;			/* EOT to expect on input */
char	*OE;			/* EOT to send to complete FT */
char	*CU;			/* call unit if making a phone call */
char	*AT;			/* acu type */
char	*PN;			/* phone number(s) */

char	*PH;			/* phone number file */
char	*RM;			/* remote file name */
char	*HO;			/* host name */

int	BR;			/* line speed for conversation */
int	FS;			/* frame size for transfers */

char	DU;			/* this host is dialed up */

/*
 * String value table
 */
typedef
	struct {
		char	*v_name;	/* whose name is it */
		char	v_type;		/* for interpreting set's */
		char	v_access;	/* protection of touchy ones */
		char	*v_abrev;	/* possible abreviation */
		char	*v_value;	/* casted to a union later */
	}
	value_t;

#define STRING	01		/* string valued */
#define BOOL	02		/* true-false value */
#define NUMBER	04		/* numeric value */
#define CHAR	010		/* character value */

#define WRITE	01		/* write access to variable */
#define	READ	02		/* read access */

#define CHANGED	01		/* low bit is used to show modification */
#define PUBLIC	1		/* public access rights */
#define PRIVATE	03		/* private to definer */
#define ROOT	05		/* root defined */

#define	TRUE	1
#define FALSE	0

#define ENVIRON	020		/* initialize out of the environment */
#define IREMOTE	040		/* initialize out of remote structure */
#define INIT	0100		/* static data space used for initialization */
#define TMASK	017

/*
 * Definition of ACU line description
 */
typedef
	struct {
		char	*acu_name;
		int	(*acu_dialer)();
		int	(*acu_disconnect)();
		int	(*acu_abort)();
	}
	acu_t;

#define	equal(a, b)	(strcmp(a,b)==0)/* A nice function to string compare */
#define islower(c)	((c)>='a'&&(c)<='z')
#define toupper(c)	(c)-=('a'-'A')
#define isnum(c)	((c)>='0'&&(c)<= '9')
#define CTRL(c)		('c'&037)

/*
 * variable manipulation stuff --
 *   if we defined the value entry in value_t, then we couldn't
 *   initialize it in vars.c, so we cast it as needed to keep lint
 *   happy.
 */
typedef
	union {
		int	zz_number;
		short	zz_boolean;
		char	zz_character;
		int	*zz_address;
	}
	zzhack;

#define value(v)	vtable[v].v_value

#define boolean(v)	((((zzhack *)(&(v))))->zz_boolean)
#define number(v)	((((zzhack *)(&(v))))->zz_number)
#define character(v)	((((zzhack *)(&(v))))->zz_character)
#define address(v)	((((zzhack *)(&(v))))->zz_address)

/*
 * Escape command table definitions --
 *   lookup in this table is performed when ``escapec'' is recognized
 *   at the begining of a line (as defined by the eolmarks variable).
*/

typedef
	struct {
		char	e_char;		/* char to match on */
		char	e_flags;	/* experimental, priviledged */
		char	*e_help;	/* help string */
		int 	(*e_func)();	/* command */
	}
	esctable_t;

#define NORM	00		/* normal protection, execute anyone */
#define EXP	01		/* experimental, mark it with a `*' on help */
#define PRIV	02		/* priviledged, root execute only */

extern int	vflag;		/* verbose during reading of .tiprc file */
extern value_t	vtable[];	/* variable table */

#ifndef ACULOG
#define logent(a, b, c, d)
#define loginit()
#endif

/*
 * Definition of indices into variable table so
 *  value(DEFINE) turns into a static address.
 */

#define BEAUTIFY	0
#define BAUDRATE	1
#define DIALTIMEOUT	2
#define EOFREAD		3
#define EOFWRITE	4
#define EOL		5
#define ESCAPE		6
#define EXCEPTIONS	7
#define FORCE		8
#define FRAMESIZE	9
#define HOST		10
#if ACULOG
#define LOCK		11
#define LOG		12
#define PHONES		13
#define PROMPT		14
#define RAISE		15
#define RAISECHAR	16
#define RECORD		17
#define REMOTE		18
#define SCRIPT		19
#define TABEXPAND	20
#define VERBOSE		21
#define SHELL		22
#define HOME		23
#else
#define PHONES		11
#define PROMPT		12
#define RAISE		13
#define RAISECHAR	14
#define RECORD		15
#define REMOTE		16
#define SCRIPT		17
#define TABEXPAND	18
#define VERBOSE		19
#define SHELL		20
#define HOME		21
#endif

#define NOVAL	((value_t *)NULL)
#define NOACU	((acu_t *)NULL)
#define NOSTR	((char *)NULL)
#define NOFILE	((FILE *)NULL)
#define NOPWD	((struct passwd *)0)

struct sgttyb	arg;		/* current mode of local terminal */
struct sgttyb	defarg;		/* initial mode of local terminal */
struct tchars	tchars;		/* current state of terminal */
struct tchars	defchars;	/* initial state of terminal */

FILE	*fscript;		/* FILE for scripting */

int	fildes[2];		/* file transfer synchronization channel */
int	repdes[2];		/* read process sychronization channel */
int	FD;			/* open file descriptor to remote host */
int	vflag;			/* print .tiprc initialization sequence */
int	sfd;			/* for ~< operation */
int	pid;			/* pid of tipout */
int	stop;			/* stop transfer session flag */
int	quit;			/* same; but on other end */
int	intflag;		/* recognized interrupt */
int	stoprompt;		/* for interrupting a prompt session */
int	timedout;		/* ~> transfer timedout */

char	fname[80];		/* file name buffer for ~< */
char	copyname[80];		/* file name buffer for ~> */
char	ccc;			/* synchronization character */
char	ch;			/* for tipout */
char	*uucplock;		/* name of lock file for uucp's */

/*
 * From <sys/tty.h> (PDP-11 V7) ... it's put in here to avoid lots
 *  of naming conflicts with stuff we have to pull in to use tty.h
 */
#ifndef TIOCFLUSH
#	define TIOCFLUSH	(('t'<<8)|16)
#endif

/*
 * On PDP-11 V7 systems with Rand's capacity call use this
 *  stuff, otherwise <assuming it's a VM system> use FIONREAD
 */
#ifndef FIONREAD
#define	FIOCAPACITY	(('f'<<8)|3)

struct capacity {
	off_t	cp_nbytes;
	char	cp_eof;
};
#endif

#ifdef VMUNIX
int	odisc;				/* initial tty line discipline */
#endif

extern char		*ctrl();
extern char		*ctime();
extern long		time();
extern struct passwd 	*getpwuid();
extern char		*getlogin();
extern char		*vinterp();
extern char		*getenv();
extern char		*rindex();
extern char		*index();
extern char		*malloc();
extern char		*connect();
