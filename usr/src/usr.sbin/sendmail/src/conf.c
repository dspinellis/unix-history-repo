# include <pwd.h>
# include "sendmail.h"

/*
**  CONF.C -- Sendmail Configuration Tables.
**
**	Defines the configuration of this installation.
**
**	Compilation Flags:
**		V6 -- running on a version 6 system.  This determines
**			whether to define certain routines between
**			the two systems.  If you are running a funny
**			system, e.g., V6 with long tty names, this
**			should be checked carefully.
**
**	Configuration Variables:
**		HdrInfo -- a table describing well-known header fields.
**			Each entry has the field name and some flags,
**			which are described in sendmail.h.
**		StdTimezone -- name of local timezone in standard time
**			(V6 only).
**		DstTimezone -- name of local timezone in daylight savings
**			time (V6 only).
**
**	Notes:
**		I have tried to put almost all the reasonable
**		configuration information into the configuration
**		file read at runtime.  My intent is that anything
**		here is a function of the version of UNIX you
**		are running, or is really static -- for example
**		the headers are a superset of widely used
**		protocols.  If you find yourself playing with
**		this file too much, you may be making a mistake!
*/




static char SccsId[] = "@(#)conf.c	3.35	%G%";
/*
**  Header info table
**	Final (null) entry contains the flags used for any other field.
**
**	Not all of these are actually handled specially by sendmail
**	at this time.  They are included as placeholders, to let
**	you know that "someday" I intend to have sendmail do
**	something with them.
*/

struct hdrinfo	HdrInfo[] =
{
	"date",			H_CHECK,		M_NEEDDATE,
	"from",			H_CHECK,		M_NEEDFROM,
	"original-from",	0,			0,
	"sender",		0,			0,
	"full-name",		H_ACHECK,		M_FULLNAME,
	"to",			H_ADDR,			0,
	"cc",			H_ADDR,			0,
	"bcc",			H_ADDR|H_ACHECK,	0,
	"message-id",		H_CHECK,		M_MSGID,
	"message",		H_EOH,			0,
	"text",			H_EOH,			0,
	"posted-date",		0,			0,
	"return-receipt-to",	0,			0,
	"received-date",	H_CHECK,		M_LOCAL,
	"received-from",	H_CHECK,		M_LOCAL,
	"precedence",		0,			0,
	"via",			H_FORCE,		0,
	NULL,			0,			0,
};


/*
**  ARPANET error message numbers.
*/

# ifdef NEWFTP
/* these are almost all unchecked */
char	Arpa_Info[] =	"010";	/* arbitrary info: this is WRONG! */
char	Arpa_Enter[] =	"354";	/* start mail input */
char	Arpa_Mmsg[] =	"250";	/* mail successful (MAIL cmd) */
char	Arpa_Fmsg[] =	"250";	/* mail successful (MLFL cmd) */
char	Arpa_Syserr[] =	"450";	/* some (transient) system error */
char	Arpa_Usrerr[] =	"550";	/* some (fatal) user error */
# else NEWFTP
char	Arpa_Info[] =	"050";	/* arbitrary info */
char	Arpa_Enter[] =	"350";	/* start mail input */
char	Arpa_Mmsg[] =	"256";	/* mail successful (MAIL cmd) */
char	Arpa_Fmsg[] =	"250";	/* mail successful (MLFL cmd) */
char	Arpa_Syserr[] =	"455";	/* some (transient) system error */
char	Arpa_Usrerr[] =	"450";	/* some (fatal) user error */
# endif NEWFTP





/*
**  Location of system files/databases/etc.
*/

char	*AliasFile =	"/usr/lib/aliases";	/* alias file */
char	*ConfFile =	"/usr/lib/sendmail.cf";	/* runtime configuration */
char	*StatFile =	"/usr/lib/mailstats";	/* statistics summary */
char	*HelpFile =	"/usr/lib/sendmail.hf";	/* help file */


/*
**  Other configuration.
*/

int	DefUid = 1;		/* the uid to execute mailers as */
int	DefGid = 1;		/* ditto for gid */



/*
**  V6 system configuration.
*/

# ifdef V6
char	*StdTimezone =	"PST";		/* std time timezone */
char	*DstTimezone =	"PDT";		/* daylight time timezone */
# endif V6

# ifdef V6
/*
**  TTYNAME -- return name of terminal.
**
**	Parameters:
**		fd -- file descriptor to check.
**
**	Returns:
**		pointer to full path of tty.
**		NULL if no tty.
**
**	Side Effects:
**		none.
*/

char *
ttyname(fd)
	int fd;
{
	register char tn;
	static char pathn[] = "/dev/ttyx";

	/* compute the pathname of the controlling tty */
	if ((tn = ttyn(fd)) == NULL)
	{
		errno = 0;
		return (NULL);
	}
	pathn[8] = tn;
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
	(void) close(fileno(f));
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

char *
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
/*
**  UMASK -- fake the umask system call.
**
**	Since V6 always acts like the umask is zero, we will just
**	assume the same thing.
*/

/*ARGSUSED*/
umask(nmask)
{
	return (0);
}


/*
**  GETRUID -- get real user id.
*/

getruid()
{
	return (getuid() & 0377);
}


/*
**  GETRGID -- get real group id.
*/

getrgid()
{
	return (getgid() & 0377);
}


/*
**  GETEUID -- get effective user id.
*/

geteuid()
{
	return ((getuid() >> 8) & 0377);
}


/*
**  GETEGID -- get effective group id.
*/

getegid()
{
	return ((getgid() >> 8) & 0377);
}

# endif V6

# ifndef V6

/*
**  GETRUID -- get real user id (V7)
*/

getruid()
{
	if (Daemon)
		return (RealUid);
	else
		return (getuid());
}


/*
**  GETRGID -- get real group id (V7).
*/

getrgid()
{
	if (Daemon)
		return (RealGid);
	else
		return (getgid());
}

# endif V6
/*
**  TTYPATH -- Get the path of the user's tty
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

# include <sys/stat.h>

char *
ttypath()
{
	struct stat stbuf;
	register char *pathn;
	extern char *ttyname();
	extern char *getlogin();

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
**	'NoReturn' can be set to suppress the return-to-sender
**	function; this should be done on huge messages.
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
	register STAB *s;

	if (to->q_mailer != MN_LOCAL && MsgSize > 100000)
	{
		usrerr("Message exceeds 100000 bytes");
		NoReturn++;
		return (FALSE);
	}
# ifdef ING70
	s = stab("arpa", ST_MAILER, ST_FIND);
	if (s != NULL && From.q_mailer != MN_LOCAL && to->q_mailer == s->s_mailer->m_mno)
	{
		usrerr("No ARPA mail through this machine: see your system administration");
		return (FALSE);
	}
# endif ING70
	return (TRUE);
}
