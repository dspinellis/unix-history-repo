/*
**  SENDMAIL.H -- Global definitions for sendmail.
*/



# ifdef _DEFINE
# define EXTERN
# ifndef lint
static char SmailSccsId[] =	"@(#)sendmail.h	3.71		%G%";
# endif lint
# else  _DEFINE
# define EXTERN extern
# endif _DEFINE

# include <stdio.h>
# include <ctype.h>
# include "conf.h"
# include "useful.h"

/*
**  Configuration constants.
**	There shouldn't be much need to change these....
*/

# define MAXLINE	256		/* max line length */
# define MAXNAME	128		/* max length of a name */
# define MAXFIELD	2500		/* max total length of a hdr field */
# define MAXPV		40		/* max # of parms to mailers */
# define MAXHOP		30		/* max value of HopCount */
# define MAXATOM	30		/* max atoms per address */
# define MAXMAILERS	10		/* maximum mailers known to system */
# define SPACESUB	('.'|0200)	/* substitution for <lwsp> */

extern char	Arpa_Info[];	/* the message number for Arpanet info */
/*
**  Address structure.
**	Addresses are stored internally in this structure.
**
**	Each address is on two chains and in one tree.
**		The q_next chain is used to link together addresses
**		  for one mailer (and is rooted in a mailer).
**		The q_chain chain is used to maintain a list of
**		  addresses originating from one call to sendto, and
**		  is used primarily for printing messages.
**		The q_alias, q_sibling, and q_child tree maintains
**		  a complete tree of the aliases.  q_alias points to
**		  the parent -- obviously, there can be several, and
**		  so this points to "one" of them.  Ditto for q_sibling.
*/

struct address
{
	char		*q_paddr;	/* the printname for the address */
	char		*q_user;	/* user name */
	char		*q_host;	/* host name */
	struct mailer	*q_mailer;	/* mailer to use */
	short		q_rmailer;	/* real mailer (before mapping) */
	u_short		q_flags;	/* status flags, see below */
	short		q_uid;		/* user-id of receiver (if known) */
	short		q_gid;		/* group-id of receiver (if known) */
	char		*q_home;	/* home dir (local mailer only) */
	char		*q_fullname;	/* full name if known */
	char		*q_fullname;	/* full name of this person */
	time_t		q_timeout;	/* timeout for this address */
	struct address	*q_next;	/* chain */
	struct address	*q_alias;	/* parent in alias tree */
	struct address	*q_sibling;	/* sibling in alias tree */
	struct address	*q_child;	/* child in alias tree */
};

typedef struct address ADDRESS;

# define QDONTSEND	000001	/* don't send to this address */
# define QBADADDR	000002	/* this address is verified bad */
# define QGOODUID	000004	/* the q_uid q_gid fields are good */
# define QPRIMARY	000010	/* set from argv */
# define QQUEUEUP	000020	/* queue for later transmission */
# define QPSEUDO	000040	/* only on the list for verification */
/*
**  Mailer definition structure.
**	Every mailer known to the system is declared in this
**	structure.  It defines the pathname of the mailer, some
**	flags associated with it, and the argument vector to
**	pass to it.  The flags are defined in conf.c
**
**	The argument vector is expanded before actual use.  All
**	words except the first are passed through the macro
**	processor.
*/

struct mailer
{
	char	*m_name;	/* symbolic name of this mailer */
	char	*m_mailer;	/* pathname of the mailer to use */
	u_long	m_flags;	/* status flags, see below */
	short	m_badstat;	/* the status code to use on unknown error */
	short	m_mno;		/* mailer number internally */
	char	*m_from;	/* pattern for From: header */
	char	**m_argv;	/* template argument vector */
};

typedef struct mailer	MAILER;

/* bits for m_flags */
# define M_FOPT		000000001L	/* mailer takes picky -f flag */
# define M_ROPT		000000002L	/* mailer takes picky -r flag */
# define M_QUIET	000000004L	/* don't print error on bad status */
# define M_RESTR	000000010L	/* must be daemon to execute */
# define M_NHDR		000000020L	/* don't insert From line */
# define M_LOCAL	000000040L	/* delivery is to this host */
# define M_STRIPQ	000000100L	/* strip quote chars from user/host */
# define M_MUSER	000000200L	/* can handle multiple users at once */
# define M_NEEDFROM	000000400L	/* need arpa-style From: line */
# define M_NEEDDATE	000001000L	/* need arpa-style Date: line */
# define M_MSGID	000002000L	/* need Message-Id: field */
# define M_RELRCPT	000004000L	/* make recipient addresses relative */
# define M_USR_UPPER	000010000L	/* preserve user case distinction */
# define M_HST_UPPER	000020000L	/* preserve host case distinction */
# define M_FULLNAME	000040000L	/* want Full-Name field */
# define M_UGLYUUCP	000100000L	/* this wants an ugly UUCP from line */
# define M_EXPENSIVE	000200000L	/* it costs to use this mailer.... */
# define M_FULLSMTP	000400000L	/* must run full SMTP, inc. limits */

# define M_ARPAFMT	(M_NEEDDATE|M_NEEDFROM|M_MSGID)

EXTERN MAILER	*Mailer[MAXMAILERS+1];

EXTERN MAILER	*LocalMailer;		/* ptr to local mailer */
EXTERN MAILER	*ProgMailer;		/* ptr to program mailer */
/*
**  Header structure.
**	This structure is used internally to store header items.
*/

struct header
{
	char		*h_field;	/* the name of the field */
	char		*h_value;	/* the value of that field */
	struct header	*h_link;	/* the next header */
	u_short		h_flags;	/* status bits, see below */
	u_long		h_mflags;	/* m_flags bits needed */
};

typedef struct header	HDR;

/*
**  Header information structure.
**	Defined in conf.c, this struct declares the header fields
**	that have some magic meaning.
*/

struct hdrinfo
{
	char	*hi_field;	/* the name of the field */
	u_short	hi_flags;	/* status bits, see below */
	u_short	hi_mflags;	/* m_flags needed for this field */
};

extern struct hdrinfo	HdrInfo[];

/* bits for h_flags and hi_flags */
# define H_EOH		00001	/* this field terminates header */
# define H_RCPT		00002	/* contains recipient addresses */
# define H_DEFAULT	00004	/* if another value is found, drop this */
# define H_USED		00010	/* indicates that this has been output */
# define H_CHECK	00020	/* check h_mflags against m_flags */
# define H_ACHECK	00040	/* ditto, but always (not just default) */
# define H_FORCE	00100	/* force this field, even if default */
# define H_ADDR		00200	/* this field contains addresses */
/*
**  Envelope structure.
**	This structure defines the message itself.  There is usually
**	only one of these -- for the message that we originally read
**	and which is our primary interest -- but other envelopes can
**	be generated during processing.  For example, error messages
**	will have their own envelope.
*/

struct envelope
{
	HDR		*e_header;	/* head of header list */
	long		e_msgpriority;	/* adjusted priority of this message */
	bool		e_queueup;	/* queue this message */
	bool		e_oldstyle;	/* use spaces (not commas) in hdrs */
	bool		e_retreceipt;	/* give a return receipt */
	bool		e_sendreceipt;	/* actually send a receipt back */
	char		*e_origfrom;	/* the From: line first read */
	char		*e_to;		/* the target person */
	ADDRESS		e_from;		/* the person it is from */
	ADDRESS		*e_sendqueue;	/* list of message recipients */
	long		e_msgsize;	/* size of the message in bytes */
	short		e_class;	/* msg class (priority, junk, etc.) */
	int		(*e_puthdr)();	/* function to put header of message */
	int		(*e_putbody)();	/* function to put body of message */
	struct envelope	*e_parent;	/* the message this one encloses */
	char		*e_df;		/* location of temp file */
	char		*e_macro[128];	/* macro definitions */
};

typedef struct envelope	ENVELOPE;

EXTERN ENVELOPE	*CurEnv;	/* envelope currently being processed */
/*
**  Work queue.
*/

struct work
{
	char		*w_name;	/* name of control file */
	long		w_pri;		/* priority of message, see below */
	struct work	*w_next;	/* next in queue */
};

typedef struct work	WORK;

EXTERN WORK	*WorkQ;			/* queue of things to be done */


/*
**  Message priorities.
**	Priorities > 0 should be preemptive.
**
**	CurEnv->e_msgpriority is the number of bytes in the message adjusted
**	by the message priority and the amount of time the message
**	has been sitting around.  Each priority point is worth
**	WKPRIFACT bytes of message, and each time we reprocess a
**	message the size gets reduced by WKTIMEFACT.
**
**	The "class" is this number, unadjusted by the age or size of
**	this message.  Classes with negative representations will have
**	error messages thrown away if they are not local.
*/

# define PRI_ALERT	50
# define PRI_QUICK	30
# define PRI_FIRSTCL	10
# define PRI_NORMAL	0
# define PRI_SECONDCL	-10
# define PRI_THIRDCL	-40
# define PRI_JUNK	-100

# define WKPRIFACT	1800		/* bytes each pri point is worth */
# define WKTIMEFACT	400		/* bytes each time unit is worth */
/*
**  Rewrite rules.
*/

struct rewrite
{
	char	**r_lhs;	/* pattern match */
	char	**r_rhs;	/* substitution value */
	struct rewrite	*r_next;/* next in chain */
};

extern struct rewrite	*RewriteRules[];

# define MATCHANY	'\020'	/* match one or more tokens */
# define MATCHONE	'\021'	/* match exactly one token */
# define MATCHCLASS	'\022'	/* match one token in a class */
# define MATCHREPL	'\023'	/* replacement on RHS for above */

# define CANONNET	'\025'	/* canonical net, next token */
# define CANONHOST	'\026'	/* canonical host, next token */
# define CANONUSER	'\027'	/* canonical user, next N tokens */

# define CONDIF		'\030'	/* conditional if-then */
# define CONDELSE	'\031'	/* conditional else */
# define CONDFI		'\032'	/* conditional fi */
/*
**  Symbol table definitions
*/

struct symtab
{
	char		*s_name;	/* name to be entered */
	char		s_type;		/* general type (see below) */
	struct symtab	*s_next;	/* pointer to next in chain */
	union
	{
		long	sv_class;	/* bit-map of word classes */
		ADDRESS	*sv_addr;	/* pointer to address header */
		MAILER	*sv_mailer;	/* pointer to mailer */
		char	*sv_alias;	/* alias */
	}	s_value;
};

typedef struct symtab	STAB;

/* symbol types */
# define ST_UNDEF	0	/* undefined type */
# define ST_CLASS	1	/* class map */
# define ST_ADDRESS	2	/* an address in parsed format */
# define ST_MAILER	3	/* a mailer header */
# define ST_ALIAS	4	/* an alias */

# define s_class	s_value.sv_class
# define s_address	s_value.sv_addr
# define s_mailer	s_value.sv_mailer
# define s_alias	s_value.sv_alias

extern STAB	*stab();

/* opcodes to stab */
# define ST_FIND	0	/* find entry */
# define ST_ENTER	1	/* enter if not there */
/*
**  Statistics structure.
*/

struct statistics
{
	time_t	stat_itime;		/* file initialization time */
	short	stat_size;		/* size of this structure */
	long	stat_nf[MAXMAILERS];	/* # msgs from each mailer */
	long	stat_bf[MAXMAILERS];	/* kbytes from each mailer */
	long	stat_nt[MAXMAILERS];	/* # msgs to each mailer */
	long	stat_bt[MAXMAILERS];	/* kbytes to each mailer */
};

EXTERN struct statistics	Stat;
extern long			kbytes();	/* for _bf, _bt */
/*
**  Operation modes
**	The default operation mode can be safely changed (except
**	that the default cannot be MD_DAEMON).
*/

EXTERN char	Mode;		/* operation mode, see below */

#define MD_DELIVER	'a'		/* collect and deliver */
#define MD_FORK		'f'		/* verify & fork before delivery */
#define MD_QUEUE	'q'		/* collect & queue, don't deliver */
#define MD_DAEMON	'd'		/* run as a daemon */
#define MD_VERIFY	'v'		/* verify: don't collect or deliver */

#define MD_DEFAULT	MD_DELIVER	/* default operation mode */
/*
**  Global variables.
*/

EXTERN bool	FromFlag;	/* if set, "From" person is explicit */
EXTERN bool	MailBack;	/* mail back response on error */
EXTERN bool	BerkNet;	/* called from BerkNet */
EXTERN bool	WriteBack;	/* write back response on error */
EXTERN bool	NoAlias;	/* if set, don't do any aliasing */
EXTERN bool	ForceMail;	/* if set, mail even if already got a copy */
EXTERN bool	MeToo;		/* send to the sender also */
EXTERN bool	IgnrDot;	/* don't let dot end messages */
EXTERN bool	SaveFrom;	/* save leading "From" lines */
EXTERN bool	Verbose;	/* set if blow-by-blow desired */
EXTERN bool	GrabTo;		/* if set, get recipients from msg */
EXTERN bool	DontSend;	/* mark recipients as QDONTSEND */
EXTERN bool	NoReturn;	/* don't return letter to sender */
EXTERN bool	Smtp;		/* using SMTP over connection */
EXTERN bool	SuprErrs;	/* set if we are suppressing errors */
EXTERN bool	QueueRun;	/* currently running message from the queue */
EXTERN bool	HoldErrs;	/* only output errors to transcript */
EXTERN bool	ArpaMode;	/* set if running arpanet protocol */
EXTERN bool	NoConnect;	/* don't connect to non-local mailers */
EXTERN bool	FatalErrors;	/* set if fatal errors during processing */
extern time_t	TimeOut;	/* time until timeout */
EXTERN FILE	*InChannel;	/* input connection */
EXTERN FILE	*OutChannel;	/* output connection */
EXTERN FILE	*TempFile;	/* mail temp file */
EXTERN FILE	*Xscript;	/* mail transcript file */
EXTERN int	RealUid;	/* when Daemon, real uid of caller */
EXTERN int	RealGid;	/* when Daemon, real gid of caller */
extern int	DefUid;		/* default uid to run as */
extern int	DefGid;		/* default gid to run as */
EXTERN int	OldUmask;	/* umask when sendmail starts up */
EXTERN int	Debug;		/* debugging level */
EXTERN int	Errors;		/* set if errors (local to single pass) */
EXTERN int	ExitStat;	/* exit status code */
EXTERN int	HopCount;	/* hop count */
EXTERN int	AliasLevel;	/* depth of aliasing */
EXTERN time_t	QueueIntvl;	/* intervals between running the queue */
EXTERN char	*HostName;	/* name of this host for SMTP messages */
EXTERN char	*Transcript;	/* the transcript file name */
extern char	*XcriptFile;	/* template for Transcript */
extern char	*AliasFile;	/* location of alias file */
extern char	*ConfFile;	/* location of configuration file */
extern char	*StatFile;	/* location of statistics summary */
extern char	*QueueDir;	/* location of queue directory */
EXTERN time_t	CurTime;	/* time of this message */


# include	<sysexits.h>

# define setstat(s)		{ if (ExitStat == EX_OK) ExitStat = s; }


/* useful functions */

extern char	*newstr();
extern ADDRESS	*parse();
extern char	*xalloc();
extern bool	sameaddr();
extern FILE	*dfopen();
