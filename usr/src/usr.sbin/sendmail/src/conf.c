# include <stdio.h>
# include <pwd.h>
# include "postbox.h"

/*
**  CONF.C -- Postbox Configuration Tables.
**
**	Defines the configuration of this installation.
**
**	Compilation Flags:
**		HASARPA -- set if this machine has a connection to
**			the Arpanet.
**		HASUUCP -- set if this machine has a connection to
**			the UUCP network.
**		NETV6MAIL -- set if you want to use "v6mail" that
**			comes with the Berkeley network.  Normally
**			/bin/mail will work fine, but around Berkeley
**			we use v6mail because it is a "fixed target".
**			Also, only v6mail has the "/dev/mail" stuff
**			in it (for biff(1)).
**		V6 -- running on a version 6 system.  This determines
**			whether to define certain routines between
**			the two systems.  If you are running a funny
**			system, e.g., V6 with long tty names, this
**			should be checked carefully.
**		DUMBMAIL -- set if your /bin/mail doesn't have the
**			-d flag.
**
**	Configuration Variables:
**		ArpaHost -- the arpanet name of the host through
**			which arpanet mail will be sent.
**		MyLocName -- the name of the host on a local network.
**			This is used to disambiguate the contents of
**			ArpaHost among many hosts who may be sharing
**			a gateway.
**		DaemonName -- the name of this agent for use in
**			error messages, typically "~MAILER~DAEMON~"
**			at this host on the local net.
**		ArpaLocal -- a list of local names for this host on
**			the arpanet.  Only functional if HASARPA set.
**		UucpLocal -- ditto for the Arpanet.
**		BerkLocal -- ditto for the Berknet.
**		Mailer -- a table of mailers known to the system.
**			The fields are:
**			- the pathname of the mailer.
**			- a list of flags describing the properties
**			  of this mailer:
**			   M_FOPT -- if set, the mailer has a picky "-f"
**				option.  In this mode, the mailer will
**				only accept the "-f" option if the
**				sender is actually "root", "network",
**				and possibly (but not necessarily) if
**				the -f argument matches the real sender.
**				The effect is that if the "-f" option
**				is given to postbox then it will be
**				passed through (as arguments 1 & 2) to
**				the mailer.
**			   M_ROPT -- identical to M_FOPT, except uses
**				-r instead.
**			   M_QUIET -- if set, don't print a message if
**				the mailer returns bad status.
**			   M_RESTR -- if set, this mailer is restricted
**				to use by "daemon"; otherwise, we do a
**				setuid(getuid()) before calling the
**				mailer.
**			   M_HDR -- if set, the mailer wants us to
**				insert a UNIX "From" line before
**				outputing.
**			   M_FHDR -- if set, the header that we
**				generate will be used literally, so
**				we must force it to be correct.  The
**				effect is that we generate a header
**				even if one exists.
**			   M_NOHOST -- if set, this mailer doesn't care
**				about the host part (e.g., the local
**				mailer).
**			   M_STRIPQ -- if set, strip quote (`"')
**				characters out of parameters as you
**				transliterate them into the argument
**				vector.  For example, the local mailer
**				is called directly, so these should be
**				stripped, but the program-mailer (i.e.,
**				csh) should leave them in.
**			   M_NEEDDATE -- this mailer requires a Date:
**				field in the message.
**			   M_NEEDFROM -- this mailer requires a From:
**				field in the message.
**			   M_MSGID -- this mailer requires a Message-Id
**				field in the message.
**			   M_COMMAS -- this mailer wants comma-
**				seperated To: and Cc: fields.
**			   M_ARPAFMT == M_NEEDDATE|M_NEEDFROM|M_MSGID|
**				M_COMMAS.
**			- an exit status to use as the code for the
**			  error message print if the mailer returns
**			  something we don't understand.
**			- A list of names that are to be considered
**			  "local" (and hence are stripped off) for
**			  this mailer.
**			- An argument vector to be passed to the
**			  mailer with the following substitutions:
**			   $f - the from person name.
**			   $u - the target user name.
**			   $h - the target user host.
**			   $c - the hop count.
**			>>>>>>>>>> Entry zero must be for the local
**			>> NOTE >> mailer and entry one must be for
**			>>>>>>>>>> the shell.
**		ParseTab -- a table driving the parsing process.  Each
**			entry contains:
**			- a character that will trigger this entry.
**			- an index into the Mailer table.
**			- a word of flags, described in postbox.h.
**			- an argument.  If we have P_MAP, it is the
**			  character to turn the trigger character into.
**			  If we have P_MOVE, it is the site to send it
**			  to, using the mailer specified above.
**			This table will almost certainly have to be
**			changed on your site if you have anything more
**			than the UUCP net.
**		HdrInfo -- a table describing well-known header fields.
**			Each entry has the field name and some flags,
**			which can be:
**			- H_EOH -- this field is equivalent to a blank
**			  line; i.e., it signifies end of header.
**			- H_DELETE -- delete this field.
**			There is also a field pointing to a pointer
**			that should be set to point to this header.
*/




static char SccsId[] = "@(#)conf.c	3.6	%G%";


# include <whoami.h>		/* definitions of machine id's at berkeley */

# ifdef BERKELEY

char	*ArpaHost = "Berkeley";	/* host name of gateway on Arpanet */
# define BerkHost   MyLocName	/* host name of gateway on Berk net */
# define NETV6MAIL		/* use /usr/net/bin/v6mail for local delivery */

/* Specific Configurations for Berkeley Machines */

/* Berkeley people: mail changes to ingvax:eric or they will be lost! */

# ifdef ING70
# include "c.ing70.h"
# endif ING70

# ifdef INGVAX
# include "c.ingvax.h"
# endif INGVAX

# ifdef CSVAX
# include "c.csvax.h"
# endif CSVAX

# ifdef ARPAVAX
# include "c.arpavax.h"
# endif ARPAVAX

# ifdef CORY
# include "c.cory.h"
# endif CORY

# ifdef ONYX
# include "c.onyx.h"
# endif ONYX

# ifdef IMAGE
# include "c.image.h"
# endif IMAGE

# ifdef ESVAX
# include "c.esvax.h"
# endif ESVAX

# ifdef EECS40
# include "c.eecs40.h"
# endif EECS40

# else BERKELEY

char	*ArpaHost = "[unknown]";
char	*MyLocName = sysname;
# define HASUUCP		/* default to having UUCP net */
char	*UucpLocal[] = { sysname, NULL };
# define BerkMerge	NULL	/* don't merge any berknet sites */
# define UucpMerge	NULL	/* don't merge any UUCP sites */

# endif BERKELEY



# ifndef HASARPA
# define ArpaLocal	NULL
# endif HASARPA

# ifndef HASUUCP
# define UucpLocal	NULL
# endif HASUUCP


/* local mail -- must be #0 */
static char	*LocalArgv[] =
{
	"...local%mail",
	"-d",
	"$u",
	NULL
};

static struct mailer	LocalMailer =
{
# ifdef NETV6MAIL
	"/usr/net/bin/v6mail",
# else
	"/bin/mail",
# endif
	M_ROPT|M_NOHOST|M_STRIPQ|M_ARPAFMT,	EX_NOUSER,	NULL,
	"$f",		NULL,			LocalArgv,
};

/* pipes through programs -- must be #1 */
static char	*ProgArgv[] =
{
	"...prog%mail",
	"-fc",
	"$u",
	NULL
};

static struct mailer	ProgMailer =
{
	"/bin/csh",
	M_HDR|M_FHDR|M_NOHOST,			EX_UNAVAILABLE,	NULL,
	"$f",		NULL,			ProgArgv,
};

/* local berkeley mail */
static char	*BerkArgv[] =
{
	"...berk%mail",
	"-m",
	"$h",
	"-t",
	"$u",
	"-h",
	"$c",
	NULL
};

static struct mailer	BerkMailer =
{
	"/usr/net/bin/sendberkmail",
	M_FOPT|M_HDR|M_STRIPQ,			EX_UNAVAILABLE,	BerkLocal,
	"$B:$f",	BerkMerge,		BerkArgv,
};

/* arpanet mail */
static char	*ArpaArgv[] =
{
	"...arpa%mail",
	"$f",
	"$h",
	"$u",
	NULL
};

static struct mailer	ArpaMailer =
{
	"/usr/lib/mailers/arpa",
	M_STRIPQ|M_ARPAFMT,			0,		ArpaLocal,
	"$f@$A",	NULL,			ArpaArgv,
};

/* uucp mail (cheat & use Bell's v7 mail) */
static char	*UucpArgv[] =
{
	"...uucp%mail",
# ifdef DUMBMAIL
	"-d",
# endif DUMBMAIL
	"$h!$u",
	NULL
};

static struct mailer	UucpMailer =
{
	"/bin/mail",
	M_ROPT|M_STRIPQ,			EX_NOUSER,	UucpLocal,
	"$U!$f",	UucpMerge,		UucpArgv,
};

struct mailer	*Mailer[] =
{
	&LocalMailer,		/* 0 -- must be 0 */
	&ProgMailer,		/* 1 -- must be 1 */
	&BerkMailer,		/* 2 */
	&ArpaMailer,		/* 3 */
	&UucpMailer,		/* 4 */
};

# define NMAILERS	(sizeof Mailer / sizeof Mailer[0])

# define M_LOCAL	0
# define M_PROG		1
# define M_BERK		2
# define M_ARPA		3
# define M_UUCP		4

/* list of messages for each mailer (sorted by host) */
ADDRESS		MailList[NMAILERS];



# ifdef BERKELEY
struct parsetab ParseTab[] =
{
	':',	M_BERK,		P_ONE,				NULL,
# ifdef HASARPA
	'@',	M_ARPA,		P_HLAST|P_USR_UPPER,		NULL,
# else
	'@',	M_BERK,		P_HLAST|P_USR_UPPER|P_MOVE,	"ing70",
# endif HASARPA
	'^',	-1,		P_MAP,				"!",
# ifdef HASUUCP
	'!',	M_UUCP,		0,				NULL,
# else
	'!',	M_BERK,		P_MOVE,				"csvax",
# endif HASUUCP
	'.',	-1,		P_MAP|P_ONE,			":",
	'\0',	M_LOCAL,	P_MOVE,				"",
};
# else BERKELEY
struct parsetab ParseTab[] =
{
# ifdef HASARPA
	'@',	M_ARPA,		P_HLAST|P_USR_UPPER,		NULL,
# endif HASARPA
# ifdef HASUUCP
	'^',	-1,		P_MAP,				"!",
	'!',	M_UUCP,		0,				NULL,
# endif HASUUCP
	'\0',	M_LOCAL,	P_MOVE,				"",
};
# endif BERKELEY


/*
**  Header info table
**	Final (null) entry contains the flags used for any other field.
*/

struct hdrinfo	HdrInfo[] =
{
	"date",		0,			NULL,
	"from",		0,			NULL,
	"to",		0,			NULL,
	"cc",		0,			NULL,
	"subject",	0,			NULL,
	"message-id",	0,			&MsgId,
	"message",	H_EOH,			NULL,
	NULL,		0,			NULL,
};
/*
**  INITMACS -- initialize predefined macros.
**
**	Parameters:
**		none.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Macros array gets initialized.
*/

char	*Macro[26];

# define MACRO(x)	Macro[x - 'A']

initmacs()
{
	MACRO('A') = ArpaHost;
	MACRO('B') = BerkHost;
	MACRO('U') = UucpHost;
}

# ifdef V6
/*
**  TTYPATH -- Get the path of the user's tty -- Version 6 version.
**
**	Returns the pathname of the user's tty.  Returns NULL if
**	the user is not logged in or if s/he has write permission
**	denied.
**
**	Parameters:
**		none
**
**	Returns:
**		pathname of the user's tty.
**		NULL if not logged in or write permission denied.
**
**	Side Effects:
**		none.
**
**	WARNING:
**		Return value is in a local buffer.
**
**	Called By:
**		savemail
*/

# include <sys/types.h>
# include <sys/stat.h>

char *
ttypath()
{
	struct stat stbuf;
	register int i;
	static char pathn[] = "/dev/ttyx";
	extern int errno;

	/* compute the pathname of the controlling tty */
	if ((i = ttyn(2)) == 'x' && (i = ttyn(1)) == 'x' && (i = ttyn(0)) == 'x')
	{
		errno = 0;
		return (NULL);
	}
	pathn[8] = i;

	/* see if we have write permission */
	if (stat(pathn, &stbuf) < 0 || !bitset(02, stbuf.st_mode))
	{
		errno = 0;
		return (NULL);
	}

	/* see if the user is logged in */
	if (getlogin() == NULL)
		return (NULL);

	/* looks good */
	return (pathn);
}
/*
**  FDOPEN -- Open a stdio file given an open file descriptor.
**
**	This is included here because it is standard in v7, but we
**	need it in v6.
**
**	Algorithm:
**		Open /dev/null to create a descriptor.
**		Close that descriptor.
**		Copy the existing fd into the descriptor.
**
**	Parameters:
**		fd -- the open file descriptor.
**		type -- "r", "w", or whatever.
**
**	Returns:
**		The file descriptor it creates.
**
**	Side Effects:
**		none
**
**	Called By:
**		deliver
**
**	Notes:
**		The mode of fd must match "type".
*/

FILE *
fdopen(fd, type)
	int fd;
	char *type;
{
	register FILE *f;

	f = fopen("/dev/null", type);
	close(fileno(f));
	fileno(f) = fd;
	return (f);
}
/*
**  INDEX -- Return pointer to character in string
**
**	For V7 compatibility.
**
**	Parameters:
**		s -- a string to scan.
**		c -- a character to look for.
**
**	Returns:
**		If c is in s, returns the address of the first
**			instance of c in s.
**		NULL if c is not in s.
**
**	Side Effects:
**		none.
*/

index(s, c)
	register char *s;
	register char c;
{
	while (*s != '\0')
	{
		if (*s++ == c)
			return (--s);
	}
	return (NULL);
}
# endif V6

# ifndef V6
/*
**  TTYPATH -- Get the path of the user's tty -- Version 7 version.
**
**	Returns the pathname of the user's tty.  Returns NULL if
**	the user is not logged in or if s/he has write permission
**	denied.
**
**	Parameters:
**		none
**
**	Returns:
**		pathname of the user's tty.
**		NULL if not logged in or write permission denied.
**
**	Side Effects:
**		none.
**
**	WARNING:
**		Return value is in a local buffer.
**
**	Called By:
**		savemail
*/

# include <sys/types.h>
# include <sys/stat.h>

char *
ttypath()
{
	struct stat stbuf;
	register char *pathn;
	extern int errno;
	extern char *ttyname();

	/* compute the pathname of the controlling tty */
	if ((pathn = ttyname(2)) == NULL && (pathn = ttyname(1)) == NULL && (pathn = ttyname(0)) == NULL)
	{
		errno = 0;
		return (NULL);
	}

	/* see if we have write permission */
	if (stat(pathn, &stbuf) < 0 || !bitset(02, stbuf.st_mode))
	{
		errno = 0;
		return (NULL);
	}

	/* see if the user is logged in */
	if (getlogin() == NULL)
		return (NULL);

	/* looks good */
	return (pathn);
}
# endif V6
/*
**  CHECKCOMPAT -- check for From and To person compatible.
**
**	This routine can be supplied on a per-installation basis
**	to determine whether a person is allowed to send a message.
**	This allows restriction of certain types of internet
**	forwarding or registration of users.
**
**	If the hosts are found to be incompatible, an error
**	message should be given using "usrerr" and FALSE should
**	be returned.
**
**	Parameters:
**		to -- the person being sent to.
**
**	Returns:
**		TRUE -- ok to send.
**		FALSE -- not ok.
**
**	Side Effects:
**		none (unless you include the usrerr stuff)
*/

bool
checkcompat(to)
	register ADDRESS *to;
{
	return (TRUE);
}
