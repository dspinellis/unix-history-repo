# include <stdio.h>
# include <pwd.h>
# include "dlvrmail.h"

/*
**  CONF.C -- Delivermail Configuration Tables.
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
**			error messages, typically "MAILER-DAEMON"
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
**				is given to delivermail then it will be
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
**			- a word of flags, described in dlvrmail.h.
**			- an argument.  If we have P_MAP, it is the
**			  character to turn the trigger character into.
**			  If we have P_MOVE, it is the site to send it
**			  to, using the mailer specified above.
**			This table will almost certainly have to be
**			changed on your site if you have anything more
**			than the UUCP net.
*/




static char SccsId[] = "@(#)conf.c	2.6	2/15/81";

/************ BEGIN CONFIGURATION SECTION ************/

bool	UseMsgId = FALSE;	/* don't put message id's in anywhere */

# include <whoami.h>		/* definitions of machine id's at berkeley */

char	*ArpaHost = "[unknown]";
char	*MyLocName = sysname;
char	*DaemonName = "MAILER-DAEMON";
# define HASUUCP		/* default to having UUCP net */
char	*UucpLocal[] = { sysname, NULL };
/* if you define HASARPA you must include a declaration for ArpaLocal */

/************ END CONFIGURATION SECTION ************/

# ifndef HASARPA
# define ArpaLocal	NULL
# endif HASARPA

# ifndef HASUUCP
# define UucpLocal	NULL
# endif HASUUCP

# ifndef HASBERK
# define BerkLocal	NULL
# endif HASBERK


struct mailer Mailer[] =
{
	/* local mail -- must be #0 */
	{
# ifdef NETV6MAIL
		"/usr/net/bin/v6mail",
# else
		"/bin/mail",
# endif
		M_ROPT|M_NOHOST|M_STRIPQ,	EX_NOUSER,	NULL,
		{ "...local%mail", "-d", "$u", NULL }
	},
	/* pipes through programs -- must be #1 */
	{
		"/bin/csh",
		M_HDR|M_FHDR|M_NOHOST,		EX_UNAVAILABLE,	NULL,
		{ "...prog%mail", "-fc", "$u", NULL }
	},
	/* local berkeley mail */
	{
		"/usr/net/bin/sendberkmail",
		M_FOPT|M_HDR|M_STRIPQ,		EX_UNAVAILABLE,	BerkLocal,
		{ "...berk%mail", "-m", "$h", "-t", "$u", "-h", "$c", NULL }
	},
	/* arpanet mail */
	{
		"/usr/lib/mailers/arpa",
		M_STRIPQ,			0,		ArpaLocal,
		{ "...arpa%mail", "$f", "$h", "$u", NULL }
	},
	/* uucp mail (cheat & use Bell's v7 mail) */
	{
		"/bin/mail",
		M_ROPT|M_STRIPQ,		EX_NOUSER,	UucpLocal,
# ifdef DUMBMAIL
		{ "...uucp%mail", "$h!$u", NULL }
# else
		{ "...uucp%mail", "-d", "$h!$u", NULL }
# endif DUMBMAIL
	},
};

# define M_LOCAL	0
# define M_BERK		2
# define M_ARPA		3
# define M_UUCP		4



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
/*
**  GETNAME -- Get the current users login name.
**
**	This is in config.c because it is somewhat machine dependent.
**	Examine it carefully for your installation.
**
**	Algorithm:
**		See if the person is logged in.  If so, return
**			the name s/he is logged in as.
**		Look up the user id in /etc/passwd.  If found,
**			return that name.
**		Return NULL.
**
**	Parameters:
**		none
**
**	Returns:
**		The login name of this user.
**		NULL if this person is noone.
**
**	Side Effects:
**		none
**
**	Called By:
**		main
*/

char *
getname()
{
	register char *p;
	register struct passwd *w;
	extern char *getlogin();
	extern struct passwd *getpwuid();
	static char namebuf[9];

	p = getlogin();
	if (p != NULL && p[0] != '\0')
		return (p);
# ifdef V6
	w = getpwuid(getuid() & 0377);
# else
	w = getpwuid(getuid());
# endif V6
	if (w != NULL)
	{
		strcpy(namebuf, w->pw_name);
		return (namebuf);
	}
	return (NULL);
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
	if (stat(pathn, &stbuf) < 0 || !flagset(02, stbuf.st_mode))
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
	if (stat(pathn, &stbuf) < 0 || !flagset(02, stbuf.st_mode))
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
