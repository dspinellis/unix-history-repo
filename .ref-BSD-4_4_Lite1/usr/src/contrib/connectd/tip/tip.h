/*
 * tip - terminal interface program
 */

#include <sys/types.h>
#include <sys/file.h>

#include <sgtty.h>
#include <signal.h>
#include <stdio.h>
#include <pwd.h>
#include <ctype.h>
#include <setjmp.h>
#include <phonenumber.h>
#include <connect.h>
#include <errno.h>

/*
 * Remote host attributes
 */
char	*EL;			/* chars marking an EOL */
char	*CM;			/* initial connection message */
char	*IE;			/* EOT to expect on input */
char	*OE;			/* EOT to send to complete FT */
char	*PA;			/* parity to be generated */

int	FS;			/* frame size for transfers */

char	*ES;			/* escape character */
char	*EX;			/* exceptions */
char	*FO;			/* force (literal next) char*/
char	*RC;			/* raise character */
char	*RE;			/* script record file */
char	*PR;			/* remote prompt */
int	DL;			/* line delay for file transfers to remote */
int	CL;			/* char delay for file transfers to remote */
int	ET;			/* echocheck timeout */
char	HD;			/* this host is half duplex - do local echo */

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

#define	equal(a, b)	(strcmp(a,b)==0)/* A nice function to string compare */

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

/*
 * Definition of indices into variable table so
 *  value(DEFINE) turns into a static address.
 */

#define BEAUTIFY	0
#define DIALTIMEOUT	1
#define EOFREAD		2
#define EOFWRITE	3
#define EOL		4
#define ESCAPE		5
#define EXCEPTIONS	6
#define FORCE		7
#define FRAMESIZE	8
#define LOG		9
#define PROMPT		10
#define RAISE		11
#define RAISECHAR	12
#define RECORD		13
#define SCRIPT		14
#define TABEXPAND	15
#define VERBOSE		16
#define SHELL		17
#define HOME		18
#define ECHOCHECK	19
#define TAND		20
#define LDELAY		21
#define CDELAY		22
#define ETIMEOUT	23
#define RAWFTP		24
#define HALFDUPLEX	25
#define	LECHO		26
#define	PARITY		27

#define NOVAL	((value_t *)NULL)
#define NOACU	((acu_t *)NULL)
#define NOSTR	((char *)NULL)
#define NOFILE	((FILE *)NULL)
#define NOPWD	((struct passwd *)0)

struct sgttyb	arg;		/* current mode of local terminal */
struct sgttyb	defarg;		/* initial mode of local terminal */
struct tchars	tchars;		/* current state of terminal */
struct tchars	defchars;	/* initial state of terminal */
struct ltchars	ltchars;	/* current local characters of terminal */
struct ltchars	deflchars;	/* initial local characters of terminal */

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
int	cumode;			/* simulating the "cu" program */

char	fname[80];		/* file name buffer for ~< */
char	copyname[80];		/* file name buffer for ~> */
char	ccc;			/* synchronization character */
char	ch;			/* for tipout */

int	odisc;				/* initial tty line discipline */
extern	int disc;			/* current tty discpline */

extern	char *ctrl();
extern	char *ctime();
extern	long time();
extern	struct passwd *getpwuid();
extern	char *getlogin();
extern	char *vinterp();
extern	char *getenv();
extern	char *rindex();
extern	char *index();
extern	char *malloc();
extern	char *connect();
