/*
**  DLVRMAIL.H -- Global definitions for delivermail.
**
**	Most of these are actually allocated in globals.c
**
**	@(#)dlvrmail.h	2.2	11/21/80
*/




# include "useful.h"

/*
**  Manifest constants.
*/

# define MAXLINE	256	/* maximum line length */
# define MAXNAME	128	/* maximum length of a name */
# define MAXFIELD	2500	/* maximum total length of a header field */
# define MAXPV		15	/* maximum # of parms to mailers */
# define MAXHOP		30	/* maximum value of HopCount */
# define ALIASFILE	"/usr/lib/aliases"	/* location of alias file */





/*
**  Mailer definition structure.
**	Every mailer known to the system is declared in this
**	structure.  It defines the pathname of the mailer, some
**	flags associated with it, and the argument vector to
**	pass to it.  The flags are defined in conf.c
**
**	The argument vector is expanded before actual use.  Every-
**	thing is passed through except for things starting with "$".
**	"$x" defines some interpolation, as described in conf.c
**	"$x" where x is unknown expands to "x", so use "$$" to get "$".
*/

struct mailer
{
	char	*m_mailer;	/* pathname of the mailer to use */
	short	m_flags;	/* status flags, see below */
	short	m_badstat;	/* the status code to use on unknown error */
	char	**m_local;	/* list of local names for this host */
	char	*m_argv[MAXPV];	/* template argument vector */
};

# define M_FOPT		0001	/* mailer takes picky -f flag */
# define M_ROPT		0002	/* mailer takes picky -r flag */
# define M_QUIET	0004	/* don't print error on bad status */
# define M_RESTR	0010	/* must be daemon to execute */
# define M_HDR		0020	/* insert From line */
# define M_NOHOST	0040	/* ignore host in comparisons */
# define M_STRIPQ	0100	/* strip quote characters from user/host */
# define M_FHDR		0200	/* force good From line */

extern struct mailer Mailer[];


/*
**  Address structure.
**	Addresses are stored internally in this structure.
*/

struct address
{
	char		*q_paddr;	/* the printname for the address */
	char		*q_user;	/* user name */
	char		*q_host;	/* host name */
	struct mailer	*q_mailer;	/* mailer to use */
	struct address	*q_next;	/* chain */
	struct address	*q_prev;	/* back pointer */
};

typedef struct address addrq;

/* some other primitives */
# define nxtinq(q)	((q)->q_next)
# define clearq(q)	(q)->q_next = (q)->q_prev = NULL

extern addrq SendQ;		/* queue of people to send to */
extern addrq AliasQ;		/* queue of people that are aliases */


/*
**  Parse structure.
**	This table drives the parser which determines the network
**	to send the mail to.
*/

struct parsetab
{
	char	p_char;		/* trigger character */
	char	p_mailer;	/* the index of the mailer to call */
	short	p_flags;	/* see below */
	char	*p_arg;		/* extra info needed for some flags */
};

# define P_MAP		0001	/* map p_char -> p_arg[0] */
# define P_HLAST	0002	/* host is last, & right associative */
# define P_ONE		0004	/* can only be one p_char in addr */
# define P_MOVE		0010	/* send untouched to host p_arg */
# define P_USR_UPPER	0020	/* don't map UPPER->lower in user names */
# define P_HST_UPPER	0040	/* don't map UPPER->lower in host names */




/*
**  Global variables.
*/

extern bool	ArpaFmt;	/* if set, message is in arpanet fmt */
extern bool	FromFlag;	/* if set, "From" person is explicit */
extern bool	Debug;		/* if set, debugging info */
extern bool	MailBack;	/* mail back response on error */
extern bool	BerkNet;	/* called from BerkNet */
extern bool	WriteBack;	/* write back response on error */
extern bool	NoAlias;	/* if set, don't do any aliasing */
extern bool	ForceMail;	/* if set, mail even if already got a copy */
extern bool	MeToo;		/* send to the sender also */
extern bool	UseMsgId;	/* put msg-id's in all msgs [conf.c] */
extern bool	IgnrDot;	/* don't let dot end messages */
extern bool	SaveFrom;	/* save leading "From" lines */
extern int	Errors;		/* set if errors */
extern int	ExitStat;	/* exit status code */
extern char	InFileName[];	/* input file name */
extern char	Transcript[];	/* the transcript file name */
extern char	MsgId[];	/* the message id for this message */
extern addrq	From;		/* the person it is from */
extern char	*To;		/* the target person */
extern int	HopCount;	/* hop count */


# include	<sysexits.h>

# define flagset(bits, word)	((bits) & (word))
# define setstat(s)		{ if (ExitStat == EX_OK) ExitStat = s; }
