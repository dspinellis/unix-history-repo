/*
**  DELIVERMAIL.H -- Global definitions for delivermail.
**
**	Most of these are actually allocated in globals.c
**
**	History:
**		12/26/79 -- written.
*/




/*
**  Manifest constants.
*/

# define MAXLINE	256	/* maximum line length */
# define MAXNAME	128	/* maximum length of a name */
# define MAXPV		15	/* maximum # of parms to mailers */
# define MAXHOP		30	/* maximum value of HopCount */





/*
**  Mailer definition structure.
**	Every mailer known to the system is declared in this
**	structure.  It defines the pathname of the mailer, some
**	flags associated with it, and the argument vector to
**	pass to it.
**
**	The flags are as follows:
**		M_FOPT -- if set, the mailer has a picky "-f"
**			option.  In this mode, the mailer will only
**			accept the "-f" option if the sender is
**			actually "root", "network", and possibly
**			(but not necessarily) if the -f argument
**			matches the real sender.  The effect is
**			that if the "-f" option is given to
**			delivermail then it will be passed through
**			(as arguments 1 & 2) to the mailer.
**		M_ROPT -- identical to M_FOPT, except uses -r instead.
**			UGH!
**		M_QUIET -- if set, don't print a message if the mailer
**			returns bad status.
**		M_RESTR -- if set, this mailer is restricted to use
**			by "daemon"; otherwise, we do a
**			setuid(getuid()) before calling the mailer.
**		M_HDR -- if set, the mailer wants us to insert a
**			UNIX "From" line before outputting.
**		M_NOHOST -- if set, this mailer doesn't care about
**			the host part (e.g., the local mailer).
**		M_STRIPQ -- if set, strip quote (`"') characters
**			out of parameters as you transliterate them
**			into the argument vector.  For example, the
**			local mailer is called directly, so these
**			should be stripped, but the program-mailer
**			(i.e., csh) should leave them in.
**
**	The argument vector is expanded before actual use.  Every-
**	thing is passed through except for things starting with "$".
**	"$x" defines some interpolation, as defined by x:
**		$f	The "from" person.
**		$h	The host being sent to.
**		$u	The user being sent to.
**		$c	The current hop count.
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

extern char	ArpaFmt;	/* if set, message is in arpanet fmt */
extern char	FromFlag;	/* if set, "From" person is explicit */
extern char	Debug;		/* if set, debugging info */
extern char	MailBack;	/* mail back response on error */
extern char	BerkNet;	/* called from BerkNet */
extern char	WriteBack;	/* write back response on error */
extern char	NoAlias;	/* if set, don't do any aliasing */
extern char	ForceMail;	/* if set, mail even if already got a copy */
extern char	MeToo;		/* send to the sender also */
extern char	Error;		/* set if errors */
extern int	ExitStat;	/* exit status code */
extern char	InFileName[];	/* input file name */
extern char	Transcript[];	/* the transcript file name */
extern addrq	From;		/* the person it is from */
extern char	*To;		/* the target person */
extern int	HopCount;	/* hop count */


# include	<sysexits.h>

# define flagset(bits, word)	((bits) & (word))
# define setstat(s)		{ if (ExitStat == EX_OK) ExitStat = s; }

# include "useful.h"

# define BADMAIL	YES	/* mail doesn't know about new returncodes */
