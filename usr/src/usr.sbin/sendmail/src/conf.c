# include <stdio.h>
# include <pwd.h>
# include "dlvrmail.h"
# include <whoami.h>

/*
**  CONF.C -- Delivermail Configuration Tables.
**
**	Defines the configuration of this installation.
**
**	The first table describes available mailers.  This is
**	just a list of argument vectors, with the following
**	codes embedded:
**		$u -- insert the user name.
**		$h -- insert the host name.
**		$f -- insert the from person name.
**		$c -- insert the hop count.
**	This stuff is interpreted in buildmail.  There are two
**	important conventions here: entry zero must be the
**	local mailer & entry one must be the shell.
**
**	The second table gives a list of special characters.  This
**	table is scanned linearly by parse() until an entry is
**	found using one of the magic characters.  Other fields
**	give more information on how to handle it.
**
**	Defined Constants:
**		M_* -- indices into Mailer, used only in this module.
**
**	Defines:
**		Mailer -- the mailer descriptor table.
**		ParseTab -- the parse table.
**
**	Notes:
**		Ingres 11/70 version.
**
**	History:
**		3/5/80 -- Generalized to use <whoami.h>.
**		12/26/79 -- written for Ingres 11/70.
*/





# ifdef ING70
static char	*BerkLocal[] = { "i", "ingres", "ing70", NULL };
char		*MyLocNam = "Ing70";
# define HASARPA
# define V6
# endif ING70

# ifdef INGVAX
/* untested */
static char	*BerkLocal[] = { "j", "ingvax", NULL };
char		*MyLocNam = "IngVax";
# endif INGVAX

# ifdef CSVAX
/* untested */
static char	*BerkLocal[] = { "v", "csvax", "vax", NULL };
char		*MyLocNam = "CSVax";
# define HASUUCP
# define NETV6MAIL
# endif CSVAX

# ifdef CORY
/* untested */
static char	*BerkLocal[] = { "y", "cory", NULL };
char		*MyLocNam = "Cory";
# endif CORY

# ifdef IMAGE
/* untested */
static char	*BerkLocal[] = { "m", "image", NULL };
char		*MyLocNam = "Image";
# define V6
# endif IMAGE

# ifdef ESVAX
/* untested */
static char	*BerkLocal[] = { "o", "esvax", NULL };
char		*MyLocNam = "ESVax";
# endif ESVAX

# ifdef EECS40
/* untested */
static char	*BerkLocal[] = { "z", "eecs40", NULL };
char		*MyLocNam = "EECS40";
# define V6
# endif EECS40

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
		M_HDR|M_NOHOST,			EX_UNAVAIL,	NULL,
		{ "...prog%mail", "-fc", "$u", NULL }
	},
	/* local berkeley mail */
	{
		"/usr/net/bin/sendberkmail",
		M_FOPT|M_HDR,			EX_UNAVAIL,	BerkLocal,
		{ "...berk%mail", "-m", "$h", "-t", "$u", "-h", "$c", NULL }
	},
	/* arpanet mail */
	{
		"/usr/lib/mailers/arpa",
		0,				0,		NULL,
		{ "...arpa%mail", "$f", "$h", "$u", NULL }
	},
	/* uucp mail (cheat & use Bell's v7 mail) */
	{
# ifdef UCKMAIL
		"/bin/badmail",
# else
		"/bin/mail",
# endif
		M_ROPT|M_NOHOST|M_STRIPQ,	EX_NOUSER,	NULL,
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
**	Requires:
**		getlogin (sys)
**		getpwuid (sys)
**		getuid (sys)
**
**	Called By:
**		main
**
**	History:
**		12/26/79 -- written.
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
**	Requires:
**		stat (sys)
**		ttyn (sys)
**		open (sys)
**		read (sys)
**		close (sys)
**		seek (sys)
**
**	Called By:
**		savemail
**
**	History:
**		1/12/80 -- written.
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
**	Requires:
**		fopen (sys)
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
**
**	Requires:
**		none.
**
**	History:
**		3/14/80 -- written.  Why isn't this in -lS?
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
**	Requires:
**		stat (sys)
**		ttyn (sys)
**		open (sys)
**		read (sys)
**		close (sys)
**		seek (sys)
**
**	Called By:
**		savemail
**
**	History:
**		1/12/80 -- written.
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
