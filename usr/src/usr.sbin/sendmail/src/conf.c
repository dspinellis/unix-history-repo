/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)conf.c	6.2 (Berkeley) %G%";
#endif /* not lint */

# include <sys/ioctl.h>
# include <pwd.h>
# include "sendmail.h"
# include "pathnames.h"

/*
**  CONF.C -- Sendmail Configuration Tables.
**
**	Defines the configuration of this installation.
**
**	Configuration Variables:
**		HdrInfo -- a table describing well-known header fields.
**			Each entry has the field name and some flags,
**			which are described in sendmail.h.
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
		/* originator fields, most to least significant  */
	"resent-sender",	H_FROM|H_RESENT,
	"resent-from",		H_FROM|H_RESENT,
	"resent-reply-to",	H_FROM|H_RESENT,
	"sender",		H_FROM,
	"from",			H_FROM,
	"reply-to",		H_FROM,
	"full-name",		H_ACHECK,
	"return-receipt-to",	H_FROM /* |H_RECEIPTTO */,
	"errors-to",		H_FROM|H_ERRORSTO,
		/* destination fields */
	"to",			H_RCPT,
	"resent-to",		H_RCPT|H_RESENT,
	"cc",			H_RCPT,
	"resent-cc",		H_RCPT|H_RESENT,
	"bcc",			H_RCPT|H_ACHECK,
	"resent-bcc",		H_RCPT|H_ACHECK|H_RESENT,
	"apparently-to",	H_RCPT,
		/* message identification and control */
	"message-id",		0,
	"resent-message-id",	H_RESENT,
	"message",		H_EOH,
	"text",			H_EOH,
		/* date fields */
	"date",			0,
	"resent-date",		H_RESENT,
		/* trace fields */
	"received",		H_TRACE|H_FORCE,
	"via",			H_TRACE|H_FORCE,
	"mail-from",		H_TRACE|H_FORCE,

	NULL,			0,
};


/*
**  ARPANET error message numbers.
*/

char	Arpa_Info[] =		"050";	/* arbitrary info */
char	Arpa_TSyserr[] =	"451";	/* some (transient) system error */
char	Arpa_PSyserr[] =	"554";	/* some (permanent) system error */
char	Arpa_Usrerr[] =		"554";	/* some (fatal) user error */



/*
**  Location of system files/databases/etc.
*/

char	*ConfFile =	_PATH_SENDMAILCF;	/* runtime configuration */
char	*FreezeFile =	_PATH_SENDMAILFC;	/* frozen version of above */



/*
**  Miscellaneous stuff.
*/

int	DtableSize =	50;		/* max open files; reset in 4.2bsd */
/*
**  SETDEFAULTS -- set default values
**
**	Because of the way freezing is done, these must be initialized
**	using direct code.
**
**	Parameters:
**		none.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Initializes a bunch of global variables to their
**		default values.
*/

setdefaults()
{
	QueueLA = 8;
	QueueFactor = 10000;
	RefuseLA = 12;
	SpaceSub = ' ';
	WkRecipFact = 1000;
	WkClassFact = 1800;
	WkTimeFact = 9000;
	FileMode = (getuid() != geteuid()) ? 0644 : 0600;
	DefUid = 1;
	DefGid = 1;
	CheckpointInterval = 10;
	MaxHopCount = 17;
	SendMode = SM_FORK;
	ErrorMode = EM_PRINT;
	EightBit = FALSE;
	MaxMciCache = 1;
	MciCacheTimeout = 300;
	setdefuser();
	setupmaps();
}


/*
**  SETDEFUSER -- set/reset DefUser using DefUid (for initgroups())
*/

setdefuser()
{
	struct passwd *defpwent;

	if (DefUser != NULL)
		free(DefUser);
	if ((defpwent = getpwuid(DefUid)) != NULL)
		DefUser = newstr(defpwent->pw_name);
	else
		DefUser = newstr("nobody");
}
/*
**  SETUPMAPS -- set up map classes
**
**	Since these are compiled in, they cannot be in the config file.
**
*/

setupmaps()
{
	register STAB *s;
	extern bool host_map_init();
	extern char *maphostname();

	/* set up host name lookup map */
	s = stab("host", ST_MAPCLASS, ST_ENTER);
	s->s_mapclass.map_init = host_map_init;
	s->s_mapclass.map_lookup = maphostname;

	/*
	**  Set up other map classes.
	*/

# ifdef DBM_MAP
	/* dbm file access */
	{
		extern bool dbm_map_init();
		extern char *dbm_map_lookup();

		s = stab("dbm", ST_MAPCLASS, ST_ENTER);
		s->s_mapclass.map_init = dbm_map_init;
		s->s_mapclass.map_lookup = dbm_map_lookup;
	}
# endif

# ifdef BTREE_MAP
	/* new database file access -- btree files */
	{
		extern bool bt_map_init();
		extern char *db_map_lookup();

		s = stab("btree", ST_MAPCLASS, ST_ENTER);
		s->s_mapclass.map_init = bt_map_init;
		s->s_mapclass.map_lookup = db_map_lookup;
	}
# endif

# ifdef HASH_MAP
	/* new database file access -- hash files */
	{
		extern bool hash_map_init();
		extern char *db_map_lookup();

		s = stab("hash", ST_MAPCLASS, ST_ENTER);
		s->s_mapclass.map_init = hash_map_init;
		s->s_mapclass.map_lookup = db_map_lookup;
	}
# endif

# ifdef NIS_MAP
	/* NIS map access */
	{
		extern bool nis_map_init();
		extern char *nis_map_lookup();

		s = stab("nis", ST_MAPCLASS, ST_ENTER);
		s->s_mapclass.map_init = nis_map_init;
		s->s_mapclass.map_lookup = nis_map_lookup;
	}
# endif

# ifdef USERDB_MAP
	/* user database */
	{
		extern bool udb_map_init();
		extern char *udb_map_lookup();

		s = stab("udb", ST_MAPCLASS, ST_ENTER);
		s->s_mapclass.map_init = udb_map_init;
		s->s_mapclass.map_lookup = udb_map_lookup;
	}
# endif
}
/*
**  HOST_MAP_INIT -- initialize host class structures
*/

bool
host_map_init(map, mapname, args)
	MAP *map;
	char *mapname;
	char *args;
{
	register char *p = args;

	for (;;)
	{
		while (isspace(*p))
			p++;
		if (*p != '-')
			break;
		switch (*++p)
		{
		  case 'a':
			map->map_app = ++p;
			break;
		}
		while (*p != '\0' && !isspace(*p))
			p++;
		if (*p != '\0')
			*p++ = '\0';
	}
	if (map->map_app != NULL)
		map->map_app = newstr(map->map_app);
	return TRUE;
}

/*
**  GETRUID -- get real user id (V7)
*/

getruid()
{
	if (OpMode == MD_DAEMON)
		return (RealUid);
	else
		return (getuid());
}


/*
**  GETRGID -- get real group id (V7).
*/

getrgid()
{
	if (OpMode == MD_DAEMON)
		return (RealGid);
	else
		return (getgid());
}
/*
**  USERNAME -- return the user id of the logged in user.
**
**	Parameters:
**		none.
**
**	Returns:
**		The login name of the logged in user.
**
**	Side Effects:
**		none.
**
**	Notes:
**		The return value is statically allocated.
*/

char *
username()
{
	static char *myname = NULL;
	extern char *getlogin();
	register struct passwd *pw;

	/* cache the result */
	if (myname == NULL)
	{
		myname = getlogin();
		if (myname == NULL || myname[0] == '\0')
		{

			pw = getpwuid(getruid());
			if (pw != NULL)
				myname = newstr(pw->pw_name);
		}
		else
		{

			myname = newstr(myname);
			if ((pw = getpwnam(myname)) == NULL ||
			      getuid() != pw->pw_uid)
			{
				pw = getpwuid(getuid());
				if (pw != NULL)
					myname = newstr(pw->pw_name);
			}
		}
		if (myname == NULL || myname[0] == '\0')
		{
			syserr("Who are you?");
			myname = "postmaster";
		}
	}

	return (myname);
}
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
	if ((pathn = ttyname(2)) == NULL && (pathn = ttyname(1)) == NULL &&
	    (pathn = ttyname(0)) == NULL)
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
checkcompat(to, e)
	register ADDRESS *to;
	register ENVELOPE *e;
{
# ifdef lint
	if (to == NULL)
		to++;
# endif lint
# ifdef EXAMPLE_CODE
	/* this code is intended as an example only */
	register STAB *s;

	s = stab("arpa", ST_MAILER, ST_FIND);
	if (s != NULL && e->e_from.q_mailer != LocalMailer &&
	    to->q_mailer == s->s_mailer)
	{
		usrerr("No ARPA mail through this machine: see your system administration");
		/* NoReturn = TRUE; to supress return copy */
		return (FALSE);
	}
# endif /* EXAMPLE_CODE */
	return (TRUE);
}
/*
**  HOLDSIGS -- arrange to hold all signals
**
**	Parameters:
**		none.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Arranges that signals are held.
*/

holdsigs()
{
}
/*
**  RLSESIGS -- arrange to release all signals
**
**	This undoes the effect of holdsigs.
**
**	Parameters:
**		none.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Arranges that signals are released.
*/

rlsesigs()
{
}
/*
**  GETLA -- get the current load average
**
**	This code stolen from la.c.
**
**	Parameters:
**		none.
**
**	Returns:
**		The current load average as an integer.
**
**	Side Effects:
**		none.
*/

/* try to guess what style of load average we have */
#define LA_ZERO		1	/* always return load average as zero */
#define LA_INT		2	/* read kmem for avenrun; interpret as int */
#define LA_FLOAT	3	/* read kmem for avenrun; interpret as float */
#define LA_SUBR		4	/* call getloadavg */

#ifndef LA_TYPE
#  if defined(sun)
#    define LA_TYPE		LA_INT
#  endif
#  if defined(mips)
     /* Ultrix or RISC/os */
#    define LA_TYPE		LA_INT
#    define LA_AVENRUN		"avenrun"
#  endif
#  if defined(hpux)
#    define LA_TYPE		LA_FLOAT
#  endif
#  if defined(BSD)
#    define LA_TYPE		LA_SUBR
#  endif

#  ifndef LA_TYPE
#    define LA_TYPE		LA_ZERO
#  endif
#endif

#if (LA_TYPE == LA_INT) || (LA_TYPE == LA_FLOAT)

#include <nlist.h>
#include <fcntl.h>

#ifndef LA_AVENRUN
#define LA_AVENRUN	"_avenrun"
#endif

/* _PATH_UNIX should be defined in <paths.h> */
#ifndef _PATH_UNIX
#  if defined(hpux)
#    define _PATH_UNIX		"/hp-ux"
#  endif
#  if defined(mips) && !defined(ultrix)
     /* powerful RISC/os */
#    define _PATH_UNIX		"/unix"
#  endif
#  ifndef _PATH_UNIX
#    define _PATH_UNIX		"/vmunix"
#  endif
#endif

struct	nlist Nl[] =
{
	{ LA_AVENRUN },
#define	X_AVENRUN	0
	{ 0 },
};

#if (LA_TYPE == LA_INT) && !defined(FSHIFT)
#  define FSHIFT	8
#  define FSCALE	(1 << FSHIFT)
#endif

getla()
{
	static int kmem = -1;
#if LA_TYPE == LA_INT
	long avenrun[3];
#else
	double avenrun[3];
#endif
	extern off_t lseek();

	if (kmem < 0)
	{
		kmem = open("/dev/kmem", 0, 0);
		if (kmem < 0)
			return (-1);
		(void) fcntl(kmem, F_SETFD, 1);
		nlist(_PATH_UNIX, Nl);
		if (Nl[0].n_type == 0)
			return (-1);
	}
	if (lseek(kmem, (off_t) Nl[X_AVENRUN].n_value, 0) == -1 ||
	    read(kmem, (char *) avenrun, sizeof(avenrun)) < sizeof(avenrun))
	{
		/* thank you Ian */
		return (-1);
	}
#if LA_TYPE == LA_INT
	return ((int) (avenrun[0] + FSCALE/2) >> FSHIFT);
#else
	return ((int) (avenrun[0] + 0.5));
#endif
}

#else
#if LA_TYPE == LA_SUBR

getla()
{
	double avenrun[3];

	if (getloadavg(avenrun, sizeof(avenrun) / sizeof(avenrun[0])) < 0)
		return (-1);
	return ((int) (avenrun[0] + 0.5));
}

#else

getla()
{
	return (0);
}

#endif
#endif
/*
**  SHOULDQUEUE -- should this message be queued or sent?
**
**	Compares the message cost to the load average to decide.
**
**	Parameters:
**		pri -- the priority of the message in question.
**
**	Returns:
**		TRUE -- if this message should be queued up for the
**			time being.
**		FALSE -- if the load is low enough to send this message.
**
**	Side Effects:
**		none.
*/

bool
shouldqueue(pri)
	long pri;
{
	if (CurrentLA < QueueLA)
		return (FALSE);
	return (pri > (QueueFactor / (CurrentLA - QueueLA + 1)));
}
/*
**  REFUSECONNECTIONS -- decide if connections should be refused
**
**	Parameters:
**		none.
**
**	Returns:
**		TRUE if incoming SMTP connections should be refused
**			(for now).
**		FALSE if we should accept new work.
**
**	Side Effects:
**		none.
*/

bool
refuseconnections()
{
	/* this is probably too simplistic */
	return (CurrentLA > RefuseLA);
}
/*
**  SETPROCTITLE -- set process title for ps
**
**	Parameters:
**		fmt -- a printf style format string.
**		a, b, c -- possible parameters to fmt.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Clobbers argv of our main procedure so ps(1) will
**		display the title.
*/

/*VARARGS1*/
setproctitle(fmt VA_ARG_FORMAL)
	char *fmt;
	VA_ARG_DECL
{
# ifdef SETPROCTITLE
	register char *p;
	register int i;
	char buf[MAXLINE];
	VA_LOCAL_DECL
	extern char **Argv;
	extern char *LastArgv;

	p = buf;

	/* print sendmail: heading for grep */
	(void) strcpy(p, "sendmail: ");
	p += strlen(p);

	/* print the argument string */
	VA_START(fmt);
	(void) vsprintf(p, fmt, ap);
	VA_END;

	i = strlen(buf);
	if (i > LastArgv - Argv[0] - 2)
	{
		i = LastArgv - Argv[0] - 2;
		buf[i] = '\0';
	}
	(void) strcpy(Argv[0], buf);
	p = &Argv[0][i];
	while (p < LastArgv)
		*p++ = ' ';
# endif /* SETPROCTITLE */
}
/*
**  REAPCHILD -- pick up the body of my child, lest it become a zombie
**
**	Parameters:
**		none.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Picks up extant zombies.
*/

# include <sys/wait.h>

void
reapchild()
{
# ifdef WNOHANG
	union wait status;

	while (wait3((int *)&status, WNOHANG, (struct rusage *) NULL) > 0)
		continue;
# else /* WNOHANG */
	auto int status;

	while (wait((int *)&status) > 0)
		continue;
# endif /* WNOHANG */
}
/*
**  UNSETENV -- remove a variable from the environment
**
**	Not needed on newer systems.
**
**	Parameters:
**		name -- the string name of the environment variable to be
**			deleted from the current environment.
**
**	Returns:
**		none.
**
**	Globals:
**		environ -- a pointer to the current environment.
**
**	Side Effects:
**		Modifies environ.
*/

#ifdef UNSETENV

void
unsetenv(name)
	char *name;
{
	extern char **environ;
	register char **pp;
	int len = strlen(name);

	for (pp = environ; *pp != NULL; pp++)
	{
		if (strncmp(name, *pp, len) == 0 &&
		    ((*pp)[len] == '=' || (*pp)[len] == '\0'))
			break;
	}

	for (; *pp != NULL; pp++)
		*pp = pp[1];
}

#endif /* UNSETENV */
/*
**  GETDTABLESIZE -- return number of file descriptors
**
**	Only on non-BSD systems
**
**	Parameters:
**		none
**
**	Returns:
**		size of file descriptor table
**
**	Side Effects:
**		none
*/

#ifdef SYSTEM5

int
getdtablesize()
{
	return NOFILE;
}

#endif
