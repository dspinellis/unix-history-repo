/*
**  SENDMAIL.H -- Global definitions for sendmail.
**
**	@(#)sendmail.h	3.35	%G%
*/




# include <stdio.h>
# include <ctype.h>
# include "useful.h"

/*
**  Configuration constants.
**	There shouldn't be much need to change these....
*/

# define MAXLINE	256	/* maximum line length */
# define MAXNAME	128	/* maximum length of a name */
# define MAXFIELD	2500	/* maximum total length of a header field */
# define MAXPV		40	/* maximum # of parms to mailers */
# define MAXHOP		30	/* maximum value of HopCount */
# define MAXATOM	15	/* max atoms per address */
# define MAXMAILERS	10	/* maximum mailers known to system */

/* values for ArpaMode -- these are ordered!! */
# define ARPA_NONE	0	/* not in arpanet mode */
# define ARPA_OLD	1	/* in old arpanet mode */
# define ARPA_MAIL	2	/* in regular arpanet mail */
# define ARPA_FILE	3	/* reading over data connection */

extern char	Arpa_Info[];	/* the message number for Arpanet info */






/*
**  Address structure.
**	Addresses are stored internally in this structure.
*/

struct address
{
	char		*q_paddr;	/* the printname for the address */
	char		*q_user;	/* user name */
	char		*q_host;	/* host name */
	short		q_mailer;	/* mailer to use */
	short		q_rmailer;	/* real mailer (before mapping) */
	u_short		q_flags;	/* status flags, see below */
	short		q_uid;		/* user-id of receiver (if known) */
	char		*q_home;	/* home dir (local mailer only) */
	struct address	*q_next;	/* chain */
};

typedef struct address ADDRESS;

# define QDONTSEND	000001	/* don't send to this address */
# define QBADADDR	000002	/* this address is verified bad */





/*
**  Mailer definition structure.
**	Every mailer known to the system is declared in this
**	structure.  It defines the pathname of the mailer, some
**	flags associated with it, and the argument vector to
**	pass to it.  The flags are defined in conf.c
**
**	The host map is a list of lists of strings.  Within each
**	list, any host is mapped to the last host in the list.
**	This allows multiple names, as well as doing clever
**	mail grouping in point-to-point networks.  Note: this
**	is only used internally, so the apparent host is still
**	kept around.
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
	char	*m_from;	/* pattern for From: header */
	char	**m_argv;	/* template argument vector */
	ADDRESS	*m_sendq;	/* list of addresses to send to */
};

typedef struct mailer	MAILER;

# define M_FOPT		000001	/* mailer takes picky -f flag */
# define M_ROPT		000002	/* mailer takes picky -r flag */
# define M_QUIET	000004	/* don't print error on bad status */
# define M_RESTR	000010	/* must be daemon to execute */
# define M_NHDR		000020	/* don't insert From line */
# define M_LOCAL	000040	/* delivery is to this host */
# define M_STRIPQ	000100	/* strip quote characters from user/host */
# define M_MUSER	000200	/* mailer can handle multiple users at once */
# define M_NEEDFROM	000400	/* need arpa-style From: line */
# define M_NEEDDATE	001000	/* need arpa-style Date: line */
# define M_MSGID	002000	/* need Message-Id: field */
# define M_USR_UPPER	010000	/* preserve user case distinction */
# define M_HST_UPPER	020000	/* preserve host case distinction */
# define M_FULLNAME	040000	/* want Full-Name field */

# define M_ARPAFMT	(M_NEEDDATE|M_NEEDFROM|M_NEEDDATE)

extern MAILER *Mailer[];

/* special mailer numbers */
# define MN_LOCAL	0	/* local mailer */
# define MN_PROG	1	/* program mailer */
/* mailers from 2 on are arbitrary */



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

extern HDR	*Header;	/* head of header list */

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
# define H_DEFAULT	00004	/* if another value is found, drop this */
# define H_USED		00010	/* indicates that this has been output */
# define H_CHECK	00020	/* check h_mflags against m_flags */
# define H_ACHECK	00040	/* ditto, but always (not just default) */
# define H_FORCE	00100	/* force this field, even if default */
# define H_ADDR		00200	/* this field contains addresses */


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

# define CANONNET	'\025'	/* canonical net, next token */
# define CANONHOST	'\026'	/* canonical host, next token */
# define CANONUSER	'\027'	/* canonical user, next N tokens */



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
# define s_addr		s_value.sv_addr
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

extern struct statistics	Stat;
extern long			kbytes();	/* for _bf, _bt */




/*
**  Global variables.
*/

extern bool	FromFlag;	/* if set, "From" person is explicit */
extern bool	MailBack;	/* mail back response on error */
extern bool	BerkNet;	/* called from BerkNet */
extern bool	WriteBack;	/* write back response on error */
extern bool	NoAlias;	/* if set, don't do any aliasing */
extern bool	ForceMail;	/* if set, mail even if already got a copy */
extern bool	MeToo;		/* send to the sender also */
extern bool	IgnrDot;	/* don't let dot end messages */
extern bool	SaveFrom;	/* save leading "From" lines */
extern bool	Verbose;	/* set if blow-by-blow desired */
extern bool	GrabTo;		/* if set, get recipients from msg */
extern bool	DontSend;	/* mark recipients as QDONTSEND */
extern int	Debug;		/* debugging level */
extern int	Errors;		/* set if errors */
extern int	ExitStat;	/* exit status code */
extern int	ArpaMode;	/* ARPANET handling mode */
extern long	MsgSize;	/* size of the message in bytes */
extern char	InFileName[];	/* input file name */
extern char	Transcript[];	/* the transcript file name */
extern FILE	*TempFile;	/* mail temp file */
extern ADDRESS	From;		/* the person it is from */
extern char	*To;		/* the target person */
extern int	HopCount;	/* hop count */
extern long	CurTime;	/* time of this message */
extern int	AliasLevel;	/* depth of aliasing */


# include	<sysexits.h>

# define setstat(s)		{ if (ExitStat == EX_OK) ExitStat = s; }


/* useful functions */

extern char	*newstr();
extern ADDRESS	*parse();
extern char	*xalloc();
extern char	*expand();
extern bool	sameaddr();
