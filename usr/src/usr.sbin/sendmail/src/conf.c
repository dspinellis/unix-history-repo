# include <stdio.h>
# include <pwd.h>
# include "sendmail.h"

/*
**  CONF.C -- Sendmail Configuration Tables.
**
**	Defines the configuration of this installation.
**
**	Compilation Flags:
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
**
**	Configuration Variables:
**		Mailer -- a table of mailers known to the system.
**			This should be fairly static.  The fields are:
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
**				is given to sendmail then it will be
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
**			   M_NHDR -- if set, the mailer doesn't want us
**				to insert a UNIX "From" line before
**				outputing.
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
**			   M_ARPAFMT == M_NEEDDATE|M_NEEDFROM|M_MSGID.
**			- an exit status to use as the code for the
**			  error message print if the mailer returns
**			  something we don't understand.
**			- A list of names that are to be considered
**			  "local" (and hence are stripped off) for
**			  this mailer.
**			- An argument vector to be passed to the
**			  mailer; this is macro substituted.
**			>>>>>>>>>> Entry zero must be for the local
**			>> NOTE >> mailer and entry one must be for
**			>>>>>>>>>> the shell.
**		HdrInfo -- a table describing well-known header fields.
**			Each entry has the field name and some flags,
**			which can be:
**			- H_EOH -- this field is equivalent to a blank
**			  line; i.e., it signifies end of header.
**			- H_DELETE -- delete this field.
**			There is also a field pointing to a pointer
**			that should be set to point to this header.
*/




static char SccsId[] = "@(#)conf.c	3.13	%G%";


# include <whoami.h>		/* definitions of machine id's at berkeley */

# ifdef BERKELEY
# define NETV6MAIL		/* use /usr/net/bin/v6mail for local delivery */
# endif BERKELEY



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
	"local",	"/usr/net/bin/v6mail",
# else
	"local",	"/bin/mail",
# endif
	M_ROPT|M_NOHOST|M_STRIPQ|M_ARPAFMT|M_MUSER|M_NHDR,
	EX_NOUSER,	"$f",		LocalArgv,	NULL,
};

/* pipes through programs -- must be #1 -- also used for files */
static char	*ProgArgv[] =
{
	"...prog%mail",
	"-fc",
	"$u",
	NULL
};

static struct mailer	ProgMailer =
{
	"prog",		"/bin/csh",
	M_NOHOST|M_ARPAFMT,
	EX_UNAVAILABLE, "$f",		ProgArgv,	NULL,
};

/* local berkeley mail */
static char	*BerkArgv[] =
{
	"...berk%mail",
	"-m",
	"$h",
	"-h",
	"$c",
	"-t",
	"$u",
	NULL
};

static struct mailer	BerkMailer =
{
	"berk",		"/usr/net/bin/sendberkmail",
	M_FOPT|M_NEEDDATE|M_FULLNAME|M_STRIPQ,
	EX_UNAVAILABLE,	"$B:$f",	BerkArgv,	NULL,
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
	"arpa",		"/usr/lib/mailers/arpa",
	M_STRIPQ|M_ARPAFMT|M_USR_UPPER,
	0,		"$f@$A",	ArpaArgv,	NULL,
};

/* uucp mail (cheat & use Bell's v7 mail) */
static char	*UucpArgv[] =
{
	"...uucp%mail",
	"-",
	"$h!rmail",
	"($u)",
	NULL
};

static struct mailer	UucpMailer =
{
	"uucp",		"/usr/bin/uux",
	M_ROPT|M_STRIPQ|M_NEEDDATE|M_FULLNAME|M_MUSER,
	EX_NOUSER,	"$U!$f",	UucpArgv,	NULL,
};

struct mailer	*Mailer[] =
{
	&LocalMailer,		/* 0 -- must be 0 */
	&ProgMailer,		/* 1 -- must be 1 */
	&BerkMailer,		/* 2 */
	&ArpaMailer,		/* 3 */
	&UucpMailer,		/* 4 */
	NULL
};

# define M_LOCAL	0
# define M_PROG		1
# define M_BERK		2
# define M_ARPA		3
# define M_UUCP		4





/*
**  Header info table
**	Final (null) entry contains the flags used for any other field.
*/

struct hdrinfo	HdrInfo[] =
{
	"date",		H_CHECK,		M_NEEDDATE,
	"from",		H_CHECK,		M_NEEDFROM,
	"full-name",	H_ACHECK,		M_FULLNAME,
	"to",		0,			NULL,
	"cc",		0,			NULL,
	"subject",	0,			NULL,
	"message-id",	H_CHECK,		M_MSGID,
	"message",	H_EOH,			NULL,
	NULL,		0,			NULL,
};

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
