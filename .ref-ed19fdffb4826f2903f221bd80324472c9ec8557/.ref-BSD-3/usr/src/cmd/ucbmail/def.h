#

#include "local.h"
#include <sys/types.h>
#include <signal.h>
#include <stdio.h>

#undef isalpha
#undef isdigit

/*
 * Mail -- a mail program
 *
 * Commands are:
 *	t <message list>		print out these messages
 *	r <message list>		reply to messages
 *	m <user list>			mail to users (analogous to send)
 *	e <message list>		edit messages
 *	c [directory]			chdir to dir or home if none
 *	x				exit quickly
 *	w <message list> file		save messages in file
 *	q				quit, save remaining stuff in mbox
 *	d <message list>		delete messages
 *	u <message list>		undelete messages
 *	h				print message headers
 *
 * Author: Kurt Shoens (UCB) March 25, 1978
 */


#define	ESCAPE		'~'		/* Default escape for sending */
#define	NMLSIZE		20		/* max names in a message list */
#define	PATHSIZE	35		/* Size of pathnames throughout */
#define	NAMESIZE	20		/* Max size of user name */
#define	HSHSIZE		19		/* Hash size for aliases and vars */
#define	HDRFIELDS	3		/* Number of header fields */
#define	LINESIZE	512		/* max readable line width */
#define	SCREEN		18		/* screen size in lines (effective) */
#define	STRINGSIZE	((unsigned) 128)/* Dynamic allocation units */
#define	MAXARGC		20		/* Maximum list of raw strings */
#define	NOSTR		((char *) 0)	/* Null string pointer */
#define	equal(a, b)	(strcmp(a,b)==0)/* A nice function to string compare */

struct message {
	short	m_flag;			/* flags, see below */
	short	m_block;		/* block number of this message */
	short	m_offset;		/* offset in block of message */
	unsigned	m_size;		/* Bytes in the message */
	short	m_lines;		/* Lines in the message */
};

/*
 * flag bits.
 */

#define	MUSED		1		/* entry is used, but this bit isn't */
#define	MDELETED	2		/* entry has been deleted */
#define	MSAVED		4		/* entry has been saved */
#define	MTOUCH		8		/* entry has been noticed */
#define	MPRESERVE	16		/* keep entry in sys mailbox */
#define	MMARK		32		/* message is marked! */
#define	MODIFY		64		/* message has been modified */

/*
 * Format of the command description table.
 * The actual table is declared and initialized
 * in lex.c
 */

struct cmd {
	char	*c_name;		/* Name of command */
	int	(*c_func)();		/* Implementor of the command */
	short	c_argtype;		/* Type of arglist (see below) */
	short	c_msgflag;		/* Required flags of messages */
	short	c_msgmask;		/* Relevant flags of messages */
};

/* Yechh, can't initialize unions */

#define	c_minargs c_msgflag		/* Minimum argcount for RAWLIST */
#define	c_maxargs c_msgmask		/* Max argcount for RAWLIST */

/*
 * Argument types.
 */

#define	MSGLIST	 0		/* Message list type */
#define	STRLIST	 1		/* A pure string */
#define	RAWLIST	 2		/* Shell string list */
#define	NOLIST	 3		/* Just plain 0 */
#define	NDMLIST	 4		/* Message list, no defaults */

#define	P	040		/* Autoprint dot after command */
#define	I	0100		/* Interactive command bit */
#define	M	0200		/* Illegal from send mode bit */

/*
 * Oft-used mask values
 */

#define	MMNORM		(MDELETED|MSAVED)/* Look at both save and delete bits */
#define	MMNDEL		MDELETED	/* Look only at deleted bit */

/*
 * Structure used to return a break down of a head
 * line (hats off to Bill Joy!)
 */

struct headline {
	char	*l_from;	/* The name of the sender */
	char	*l_tty;		/* His tty string (if any) */
	char	*l_date;	/* The entire date string */
};

#define	GTO	1		/* Grab To: line */
#define	GSUBJECT 2		/* Likewise, Subject: line */
#define	GCC	4		/* And the Cc: line */
#define	GBCC	8		/* And also the Bcc: line */

/*
 * Structure used to pass about the current
 * state of the user-typed message header.
 */

struct header {
	char	*h_to;			/* Dynamic "To:" string */
	char	*h_subject;		/* Subject string */
	char	*h_cc;			/* Carbon copies string */
	char	*h_bcc;			/* Blind carbon copies */
	int	h_seq;			/* Sequence for optimization */
};

/*
 * Structure of namelist nodes used in processing
 * the recipients of mail and aliases and all that
 * kind of stuff.
 */

struct name {
	struct	name *n_flink;		/* Forward link in list. */
	struct	name *n_blink;		/* Backward list link */
	char	*n_name;		/* This fella's name */
};

/*
 * Structure of a variable node.  All variables are
 * kept on a singly-linked list of these, rooted by
 * "variables"
 */

struct var {
	struct	var *v_link;		/* Forward link to next variable */
	char	*v_name;		/* The variable's name */
	char	*v_value;		/* And it's current value */
};

struct group {
	struct	group *ge_link;		/* Next person in this group */
	char	*ge_name;		/* This person's user name */
};

struct grouphead {
	struct	grouphead *g_link;	/* Next grouphead in list */
	char	*g_name;		/* Name of this group */
	struct	group *g_list;		/* Users in group. */
};

#define	NIL	((struct name *) 0)	/* The nil pointer for namelists */
#define	NONE	((struct cmd *) 0)	/* The nil pointer to command tab */
#define	NOVAR	((struct var *) 0)	/* The nil pointer to variables */
#define	NOGRP	((struct grouphead *) 0)/* The nil grouphead pointer */
#define	NOGE	((struct group *) 0)	/* The nil group pointer */

/*
 * Token values returned by the scanner used for argument lists.
 * Also, sizes of scanner-related things.
 */

#define	TEOL	0			/* End of the command line */
#define	TNUMBER	1			/* A message number */
#define	TDASH	2			/* A simple dash */
#define	TSTRING	3			/* A string (possibly containing -) */
#define	TDOT	4			/* A "." */
#define	TUP	5			/* An "^" */
#define	TDOLLAR	6			/* A "$" */
#define	TSTAR	7			/* A "*" */
#define	TOPEN	8			/* An '(' */
#define	TCLOSE	9			/* A ')' */

#define	REGDEP	2			/* Maximum regret depth. */
#define	STRINGLEN	16		/* Maximum length of string token */

/*
 * Kludges to handle the change from setexit / reset to setjmp / longjmp
 */

#define	setexit()	setjmp(srbuf)
#define	reset(x)	longjmp(srbuf, x)

/*
 * If we have a system with the vfork() system call, define VFORK
 * otherwise . . .
 */

#ifndef VFORK
#define	vfork()	fork()
#endif

/*
 * Forward declarations of routine types to keep lint and cc happy.
 */

FILE	*Fdopen();
FILE	*collect();
FILE	*infix();
FILE	*mesedit();
FILE	*mespipe();
FILE	*setinput();
char	**unpack();
char	*addto();
char	*calloc();
char	*copy();
char	*copyin();
char	*detract();
char	*expand();
char	*gets();
char	*hfield();
char	*index();
char	*nameof();
char	*nextword();
char	*getenv();
char	*hcontents();
char	*netmap();
char	*netname();
char	*readtty();
char	*rindex();
char	*rpair();
char	*salloc();
char	*savestr();
char	*savestr();
char	*snarf();
char	*value();
char	*vcopy();
char	*yankword();
off_t	fsize();
struct	cmd	*lex();
struct	grouphead	*findgroup();
struct	name	*cat();
struct	name	*delname();
struct	name	*elide();
struct	name	*extract();
struct	name	*map();
struct	name	*outof();
struct	name	*put();
struct	name	*usermap();
struct	name	*verify();
struct	var	*lookup();
unsigned	int	msize();
