/* Copyright (c) 1979 Regents of the University of California */
/*
 * A bunch of global variable declarations lie herein.
 * def.h must be included first.
 */

int	msgCount;			/* Count of messages read in */
int	mypid;				/* Current process id */
int	rcvmode;			/* True if receiving mail */
int	sawcom;				/* Set after first command */
int	selfsent;			/* User sent self something */
int	senderr;			/* An error while checking */
int	edit;				/* Indicates editing a file */
int	sourcing;			/* Currently reading variant file */
FILE	*itf;				/* Input temp file buffer */
FILE	*otf;				/* Output temp file buffer */
FILE	*input;				/* Current command input file */
char	*editfile;			/* Name of file being edited */
int	outtty;				/* True if standard output a tty */
int	intty;				/* True if standard input a tty */
char	mbox[PATHSIZE];			/* Name of mailbox file */
char	*mailname;			/* Name of system mailbox */
char	mailspace[PATHSIZE];		/* Space norm alloc'd for name */
int	uid;				/* The invoker's user id */
char	mailrc[PATHSIZE];		/* Name of startup file */
char	deadletter[PATHSIZE];		/* Name of #/dead.letter */
char	homedir[PATHSIZE];		/* Path name of home directory */
char	myname[9];			/* My login id */
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

/*
 * The pointers for the string allocation routines,
 * there are NSPACE independent areas.
 * The first holds STRINGSIZE bytes, the next
 * twice as much, and so on.
 */

#define	NSPACE	5			/* Total number of string spaces */
struct strings {
	char	*s_topFree;		/* Beginning of this area */
	char	*s_nextFree;		/* Next alloctable place here */
	unsigned s_nleft;		/* Number of bytes left here */
} stringdope[NSPACE];
