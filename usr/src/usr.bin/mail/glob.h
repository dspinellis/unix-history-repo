/*
 * A bunch of global variable declarations lie herein.
 * def.h must be included first.
 */

/*
 * Sccs Id = "@(#)glob.h	2.7 %G%";
 */

int	msgCount;			/* Count of messages read in */
int	mypid;				/* Current process id */
int	rcvmode;			/* True if receiving mail */
int	sawcom;				/* Set after first command */
int	hflag;				/* Sequence number for network -h */
char	*rflag;				/* -r address for network */
char	*Tflag;				/* -T temp file for netnews */
char	nosrc;				/* Don't source /usr/lib/Mail.rc */
char	noheader;			/* Suprress initial header listing */
int	selfsent;			/* User sent self something */
int	senderr;			/* An error while checking */
int	edit;				/* Indicates editing a file */
int	readonly;			/* Will be unable to rewrite file */
int	noreset;			/* String resets suspended */
int	sourcing;			/* Currently reading variant file */
int	loading;			/* Loading user definitions */
int	shudann;			/* Print headers when possible */
int	cond;				/* Current state of conditional exc. */
FILE	*itf;				/* Input temp file buffer */
FILE	*otf;				/* Output temp file buffer */
FILE	*pipef;				/* Pipe file we have opened */
int	image;				/* File descriptor for image of msg */
FILE	*input;				/* Current command input file */
char	*editfile;			/* Name of file being edited */
char	*sflag;				/* Subject given from non tty */
int	outtty;				/* True if standard output a tty */
int	intty;				/* True if standard input a tty */
int	baud;				/* Output baud rate */
char	mbox[PATHSIZE];			/* Name of mailbox file */
char	mailname[PATHSIZE];		/* Name of system mailbox */
int	uid;				/* The invoker's user id */
char	mailrc[PATHSIZE];		/* Name of startup file */
char	deadletter[PATHSIZE];		/* Name of #/dead.letter */
char	homedir[PATHSIZE];		/* Path name of home directory */
char	myname[PATHSIZE];		/* My login id */
off_t	mailsize;			/* Size of system mailbox */
int	lexnumber;			/* Number of TNUMBER from scan() */
char	lexstring[STRINGLEN];		/* String from TSTRING, scan() */
int	regretp;			/* Pointer to TOS of regret tokens */
int	regretstack[REGDEP];		/* Stack of regretted tokens */
char	*stringstack[REGDEP];		/* Stack of regretted strings */
int	numberstack[REGDEP];		/* Stack of regretted numbers */
struct	message	*dot;			/* Pointer to current message */
struct	message	*message;		/* The actual message structure */
struct	var	*variables[HSHSIZE];	/* Pointer to active var list */
struct	grouphead	*groups[HSHSIZE];/* Pointer to active groups */
char	**altnames;			/* List of alternate names for user */
int	debug;				/* Debug flag set */
int	rmail;				/* Being called as rmail */

#include <setjmp.h>

jmp_buf	srbuf;


/*
 * The pointers for the string allocation routines,
 * there are NSPACE independent areas.
 * The first holds STRINGSIZE bytes, the next
 * twice as much, and so on.
 */

#define	NSPACE	25			/* Total number of string spaces */
struct strings {
	char	*s_topFree;		/* Beginning of this area */
	char	*s_nextFree;		/* Next alloctable place here */
	unsigned s_nleft;		/* Number of bytes left here */
} stringdope[NSPACE];
