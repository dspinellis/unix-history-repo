/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)conf.c	6.48 (Berkeley) %G%";
#endif /* not lint */

# include <sys/ioctl.h>
# include <sys/param.h>
# include <signal.h>
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
	"x400-received",	H_TRACE|H_FORCE,
	"via",			H_TRACE|H_FORCE,
	"mail-from",		H_TRACE|H_FORCE,

		/* miscellaneous fields */
	"comments",		H_FORCE,
	"return-path",		H_ACHECK,

	NULL,			0,
};



/*
**  Location of system files/databases/etc.
*/

char	*ConfFile =	_PATH_SENDMAILCF;	/* runtime configuration */
char	*FreezeFile =	_PATH_SENDMAILFC;	/* frozen version of above */
char	*PidFile =	_PATH_SENDMAILPID;	/* stores daemon proc id */



/*
**  Privacy values
*/

struct prival PrivacyValues[] =
{
	"public",		PRIV_PUBLIC,
	"needmailhelo",		PRIV_NEEDMAILHELO,
	"needexpnhelo",		PRIV_NEEDEXPNHELO,
	"needvrfyhelo",		PRIV_NEEDVRFYHELO,
	"noexpn",		PRIV_NOEXPN,
	"novrfy",		PRIV_NOVRFY,
	"restrictmailq",	PRIV_RESTRMAILQ,
	"authwarnings",		PRIV_AUTHWARNINGS,
	"goaway",		PRIV_GOAWAY,
	NULL,			0,
};



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
**		e -- the default envelope.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Initializes a bunch of global variables to their
**		default values.
*/

#define DAYS		* 24 * 60 * 60

setdefaults(e)
	register ENVELOPE *e;
{
	SpaceSub = ' ';				/* option B */
	QueueLA = 8;				/* option x */
	RefuseLA = 12;				/* option X */
	WkRecipFact = 30000L;			/* option y */
	WkClassFact = 1800L;			/* option z */
	WkTimeFact = 90000L;			/* option Z */
	QueueFactor = WkRecipFact * 20;		/* option q */
	FileMode = (getuid() != geteuid()) ? 0644 : 0600;
						/* option F */
	DefUid = 1;				/* option u */
	DefGid = 1;				/* option g */
	CheckpointInterval = 10;		/* option C */
	MaxHopCount = 25;			/* option h */
	e->e_sendmode = SM_FORK;		/* option d */
	e->e_errormode = EM_PRINT;		/* option e */
	EightBit = FALSE;			/* option 8 */
	MaxMciCache = 1;			/* option k */
	MciCacheTimeout = 300;			/* option K */
	LogLevel = 9;				/* option L */
	settimeouts(NULL);			/* option r */
	TimeOuts.to_q_return = 5 DAYS;		/* option T */
	TimeOuts.to_q_warning = 0;		/* option T */
	PrivacyFlags = 0;			/* option p */
	setdefuser();
	setupmaps();
	setupmailers();
}


/*
**  SETDEFUSER -- set/reset DefUser using DefUid (for initgroups())
*/

setdefuser()
{
	struct passwd *defpwent;
	static char defuserbuf[40];

	DefUser = defuserbuf;
	if ((defpwent = getpwuid(DefUid)) != NULL)
		strcpy(defuserbuf, defpwent->pw_name);
	else
		strcpy(defuserbuf, "nobody");
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

	/* host name lookup map */
	{
		extern bool host_map_init();
		extern char *maphostname();

		s = stab("host", ST_MAPCLASS, ST_ENTER);
		s->s_mapclass.map_init = host_map_init;
		s->s_mapclass.map_lookup = maphostname;
	}

	/* dequote map */
	{
		extern bool dequote_init();
		extern char *dequote_map();

		s = stab("dequote", ST_MAPCLASS, ST_ENTER);
		s->s_mapclass.map_init = dequote_init;
		s->s_mapclass.map_lookup = dequote_map;
	}

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
		while (isascii(*p) && isspace(*p))
			p++;
		if (*p != '-')
			break;
		switch (*++p)
		{
		  case 'a':
			map->map_app = ++p;
			break;
		}
		while (*p != '\0' && !(isascii(*p) && isspace(*p)))
			p++;
		if (*p != '\0')
			*p++ = '\0';
	}
	if (map->map_app != NULL)
		map->map_app = newstr(map->map_app);
	return TRUE;
}
/*
**  SETUPMAILERS -- initialize default mailers
*/

setupmailers()
{
	char buf[100];

	strcpy(buf, "prog, P=/bin/sh, F=lsD, A=sh -c $u");
	makemailer(buf);

	strcpy(buf, "*file*, P=/dev/null, F=lsDEu, A=FILE");
	makemailer(buf);

	strcpy(buf, "*include*, P=/dev/null, F=su, A=INCLUDE");
	makemailer(buf);
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
			uid_t uid = getuid();

			myname = newstr(myname);
			if ((pw = getpwnam(myname)) == NULL ||
			      (uid != 0 && uid != pw->pw_uid))
			{
				pw = getpwuid(uid);
				if (pw != NULL)
					myname = newstr(pw->pw_name);
			}
		}
		if (myname == NULL || myname[0] == '\0')
		{
			syserr("554 Who are you?");
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
**	message should be given using "usrerr" and 0 should
**	be returned.
**
**	'NoReturn' can be set to suppress the return-to-sender
**	function; this should be done on huge messages.
**
**	Parameters:
**		to -- the person being sent to.
**
**	Returns:
**		an exit status
**
**	Side Effects:
**		none (unless you include the usrerr stuff)
*/

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
		usrerr("553 No ARPA mail through this machine: see your system administration");
		/* NoReturn = TRUE; to supress return copy */
		return (EX_UNAVAILABLE);
	}
# endif /* EXAMPLE_CODE */
	return (EX_OK);
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
#  if defined(mips) || defined(__alpha)
     /* Ultrix or OSF/1 or RISC/os */
#    define LA_TYPE		LA_INT
#    define LA_AVENRUN		"avenrun"
#  endif
#  if defined(hpux)
#    define LA_TYPE		LA_FLOAT
#    define LA_AVENRUN		"avenrun"
#  endif

#  ifndef LA_TYPE
#   if defined(SYSTEM5)
#    define LA_TYPE		LA_INT
#    define LA_AVENRUN		"avenrun"
#   else
#    if defined(BSD)
#     define LA_TYPE		LA_SUBR
#    else
#     define LA_TYPE		LA_ZERO
#    endif
#   endif
#  endif
#endif

#if (LA_TYPE == LA_INT) || (LA_TYPE == LA_FLOAT)

#include <nlist.h>

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
#  if defined(SYSTEM5)
#    ifndef _PATH_UNIX
#      define _PATH_UNIX	"/unix"
#    endif
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

#if defined(unixpc)
# define FSHIFT		5
#endif

#if defined(__alpha)
# define FSHIFT		10
#endif

#if (LA_TYPE == LA_INT) && !defined(FSHIFT)
#  define FSHIFT	8
#endif
#if (LA_TYPE == LA_INT) && !defined(FSCALE)
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
	extern char *errstring();
	extern int errno;

	if (kmem < 0)
	{
		kmem = open("/dev/kmem", 0, 0);
		if (kmem < 0)
		{
			if (tTd(3, 1))
				printf("getla: open(/dev/kmem): %s\n",
					errstring(errno));
			return (-1);
		}
		(void) fcntl(kmem, F_SETFD, 1);
		if (nlist(_PATH_UNIX, Nl) < 0)
		{
			if (tTd(3, 1))
				printf("getla: nlist(%s): %s\n", _PATH_UNIX,
					errstring(errno));
			return (-1);
		}
		if (Nl[X_AVENRUN].n_value == 0)
		{
			if (tTd(3, 1))
				printf("getla: nlist(%s, %s) ==> 0\n",
					_PATH_UNIX, LA_AVENRUN);
			return (-1);
		}
	}
	if (tTd(3, 20))
		printf("getla: symbol address = %#x\n", Nl[X_AVENRUN].n_value);
	if (lseek(kmem, (off_t) Nl[X_AVENRUN].n_value, 0) == -1 ||
	    read(kmem, (char *) avenrun, sizeof(avenrun)) < sizeof(avenrun))
	{
		/* thank you Ian */
		if (tTd(3, 1))
			printf("getla: lseek or read: %s\n", errstring(errno));
		return (-1);
	}
#if LA_TYPE == LA_INT
	if (tTd(3, 5))
	{
		printf("getla: avenrun = %d", avenrun[0]);
		if (tTd(3, 15))
			printf(", %d, %d", avenrun[1], avenrun[2]);
		printf("\n");
	}
	if (tTd(3, 1))
		printf("getla: %d\n", (int) (avenrun[0] + FSCALE/2) >> FSHIFT);
	return ((int) (avenrun[0] + FSCALE/2) >> FSHIFT);
#else
	if (tTd(3, 5))
	{
		printf("getla: avenrun = %g", avenrun[0]);
		if (tTd(3, 15))
			printf(", %g, %g", avenrun[1], avenrun[2]);
		printf("\n");
	}
	if (tTd(3, 1))
		printf("getla: %d\n", (int) (avenrun[0] +0.5));
	return ((int) (avenrun[0] + 0.5));
#endif
}

#else
#if LA_TYPE == LA_SUBR

getla()
{
	double avenrun[3];

	if (getloadavg(avenrun, sizeof(avenrun) / sizeof(avenrun[0])) < 0)
	{
		if (tTd(3, 1))
			perror("getla: getloadavg failed:");
		return (-1);
	}
	if (tTd(3, 1))
		printf("getla: %d\n", (int) (avenrun[0] +0.5));
	return ((int) (avenrun[0] + 0.5));
}

#else

getla()
{
	if (tTd(3, 1))
		printf("getla: ZERO\n");
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
**		ctime -- the message creation time.
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
shouldqueue(pri, ctime)
	long pri;
	time_t ctime;
{
	if (CurrentLA < QueueLA)
		return (FALSE);
	if (CurrentLA >= RefuseLA)
		return (TRUE);
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
#ifdef XLA
	if (!xla_smtp_ok())
		return TRUE;
#endif

	/* this is probably too simplistic */
	return (CurrentLA >= RefuseLA);
}
/*
**  SETPROCTITLE -- set process title for ps
**
**	Parameters:
**		fmt -- a printf style format string.  If NULL, the first
**			parameter is a literal proctitle previously
**			returned by getproctitle.
**		va_alist -- possible parameters to fmt.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Clobbers argv of our main procedure so ps(1) will
**		display the title.
*/

#ifdef SETPROCTITLE
# ifdef __hpux
#  include <sys/pstat.h>
# endif
#endif

char	ProcTitleBuf[MAXLINE];

/*VARARGS1*/
#ifdef __STDC__
setproctitle(char *fmt, ...)
#else
setproctitle(fmt, va_alist)
	char *fmt;
	va_dcl
#endif
{
# ifdef SETPROCTITLE
	register char *p;
	register int i;
	VA_LOCAL_DECL
#  ifdef __hpux
	union pstun pst;
#  endif
	extern char **Argv;
	extern char *LastArgv;

	VA_START(fmt);
	if (fmt == NULL)
	{
		/* restore old proctitle */
		(void) strcpy(ProcTitleBuf, va_arg(ap, char *));
	}
	else
	{
		p = ProcTitleBuf;

		/* print sendmail: heading for grep */
		(void) strcpy(p, "sendmail: ");
		p += strlen(p);

		/* print the argument string */
		(void) vsprintf(p, fmt, ap);
	}
	VA_END;

	i = strlen(ProcTitleBuf);
	if (i > LastArgv - Argv[0] - 2)
	{
		i = LastArgv - Argv[0] - 2;
		ProcTitleBuf[i] = '\0';
	}
	(void) strcpy(Argv[0], ProcTitleBuf);
	p = &Argv[0][i];
	while (p < LastArgv)
		*p++ = ' ';
#  endif
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
# ifdef SYSTEM5
	(void) signal(SIGCHLD, reapchild);
# endif
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
# ifdef _SC_OPEN_MAX
	return sysconf(_SC_OPEN_MAX);
# else
	return NOFILE;
# endif
}

#endif
/*
**  UNAME -- get the UUCP name of this system.
*/

#ifndef HASUNAME

int
uname(name)
	struct utsname *name;
{
	FILE *file;
	char *n;

	name->nodename[0] = '\0';

	/* try /etc/whoami -- one line with the node name */
	if ((file = fopen("/etc/whoami", "r")) != NULL)
	{
		(void) fgets(name->nodename, NODE_LENGTH + 1, file);
		(void) fclose(file);
		n = strchr(name->nodename, '\n');
		if (n != NULL)
			*n = '\0';
		if (name->nodename[0] != '\0')
			return (0);
	}

	/* try /usr/include/whoami.h -- has a #define somewhere */
	if ((file = fopen("/usr/include/whoami.h", "r")) != NULL)
	{
		char buf[MAXLINE];

		while (fgets(buf, MAXLINE, file) != NULL)
			if (sscanf(buf, "#define sysname \"%*[^\"]\"",
					NODE_LENGTH, name->nodename) > 0)
				break;
		(void) fclose(file);
		if (name->nodename[0] != '\0')
			return (0);
	}

#ifdef TRUST_POPEN
	/*
	**  Popen is known to have security holes.
	*/

	/* try uuname -l to return local name */
	if ((file = popen("uuname -l", "r")) != NULL)
	{
		(void) fgets(name, NODE_LENGTH + 1, file);
		(void) pclose(file);
		n = strchr(name, '\n');
		if (n != NULL)
			*n = '\0';
		if (name->nodename[0] != '\0')
			return (0);
	}
#endif
	
	return (-1);
}
#endif /* HASUNAME */
/*
**  INITGROUPS -- initialize groups
**
**	Stub implementation for System V style systems
*/

#ifndef HASINITGROUPS
# if !defined(SYSTEM5) || defined(hpux)
#  define HASINITGROUPS
# endif
#endif

#ifndef HASINITGROUPS

initgroups(name, basegid)
	char *name;
	int basegid;
{
	return 0;
}

#endif
/*
**  SETSID -- set session id (for non-POSIX systems)
*/

#ifndef HASSETSID

setsid()
{
# ifdef SYSTEM5
	setpgrp();
# endif
}

#endif
/*
**  ENOUGHSPACE -- check to see if there is enough free space on the queue fs
**
**	Only implemented if you have statfs.
**
**	Parameters:
**		msize -- the size to check against.  If zero, we don't yet
**			know how big the message will be, so just check for
**			a "reasonable" amount.
**
**	Returns:
**		TRUE if there is enough space.
**		FALSE otherwise.
*/

#ifndef HASSTATFS
# if defined(BSD4_4) || defined(__osf__)
#  define HASSTATFS
# endif
#endif

#ifdef HASSTATFS
# undef HASUSTAT
#endif

#if defined(HASUSTAT)
# include <sys/stat.h>
# include <ustat.h>
#endif

#ifdef HASSTATFS
# if defined(sgi) || defined(apollo)
#  include <sys/statfs.h>
# else
#  if defined(sun) || defined(hpux)
#   include <sys/vfs.h>
#  else
#   include <sys/mount.h>
#  endif
# endif
#endif

bool
enoughspace(msize)
	long msize;
{
#if defined(HASSTATFS) || defined(HASUSTAT)
# if defined(HASUSTAT)
	struct ustat fs;
	struct stat statbuf;
#  define FSBLOCKSIZE	DEV_BSIZE
#  define f_bavail	f_tfree
# else
#  if defined(ultrix)
	struct fs_data fs;
#   define f_bavail	fd_bfreen
#   define FSBLOCKSIZE	fs.fd_bsize
#  else
	struct statfs fs;
#   define FSBLOCKSIZE	fs.f_bsize
#  endif
# endif
	long blocksneeded;
	extern int errno;
	extern char *errstring();

	if (MinBlocksFree <= 0 && msize <= 0)
	{
		if (tTd(4, 80))
			printf("enoughspace: no threshold\n");
		return TRUE;
	}

# if defined(HASUSTAT)
	if (stat(QueueDir, &statbuf) == 0 && ustat(statbuf.st_dev, &fs) == 0)
# else
#  if defined(sgi) || defined(apollo)
	if (statfs(QueueDir, &fs, sizeof fs, 0) == 0)
#  else
#   if defined(ultrix)
	if (statfs(QueueDir, &fs) > 0)
#   else
	if (statfs(QueueDir, &fs) == 0)
#   endif
#  endif
# endif
	{
		if (tTd(4, 80))
			printf("enoughspace: bavail=%ld, need=%ld\n",
				fs.f_bavail, msize);

		/* convert msize to block count */
		msize = msize / FSBLOCKSIZE + 1;
		if (MinBlocksFree >= 0)
			msize += MinBlocksFree;

		if (fs.f_bavail < msize)
		{
#ifdef LOG
			if (LogLevel > 0)
				syslog(LOG_ALERT, "%s: low on space (have %ld, need %ld)",
					QueueDir, fs.f_bavail, msize);
#endif
			return FALSE;
		}
	}
	else if (tTd(4, 80))
		printf("enoughspace failure: min=%ld, need=%ld: %s\n",
			MinBlocksFree, msize, errstring(errno));
#endif
	return TRUE;
}
/*
**  TRANSIENTERROR -- tell if an error code indicates a transient failure
**
**	This looks at an errno value and tells if this is likely to
**	go away if retried later.
**
**	Parameters:
**		err -- the errno code to classify.
**
**	Returns:
**		TRUE if this is probably transient.
**		FALSE otherwise.
*/

bool
transienterror(err)
	int err;
{
	switch (err)
	{
	  case EIO:			/* I/O error */
	  case ENXIO:			/* Device not configured */
	  case EAGAIN:			/* Resource temporarily unavailable */
	  case ENOMEM:			/* Cannot allocate memory */
	  case ENODEV:			/* Operation not supported by device */
	  case ENFILE:			/* Too many open files in system */
	  case EMFILE:			/* Too many open files */
	  case ENOSPC:			/* No space left on device */
#ifdef ETIMEDOUT
	  case ETIMEDOUT:		/* Connection timed out */
#endif
#ifdef ESTALE
	  case ESTALE:			/* Stale NFS file handle */
#endif
#ifdef ENETDOWN
	  case ENETDOWN:		/* Network is down */
#endif
#ifdef ENETUNREACH
	  case ENETUNREACH:		/* Network is unreachable */
#endif
#ifdef ENETRESET
	  case ENETRESET:		/* Network dropped connection on reset */
#endif
#ifdef ECONNABORTED
	  case ECONNABORTED:		/* Software caused connection abort */
#endif
#ifdef ECONNRESET
	  case ECONNRESET:		/* Connection reset by peer */
#endif
#ifdef ENOBUFS
	  case ENOBUFS:			/* No buffer space available */
#endif
#ifdef ESHUTDOWN
	  case ESHUTDOWN:		/* Can't send after socket shutdown */
#endif
#ifdef ECONNREFUSED
	  case ECONNREFUSED:		/* Connection refused */
#endif
#ifdef EHOSTDOWN
	  case EHOSTDOWN:		/* Host is down */
#endif
#ifdef EHOSTUNREACH
	  case EHOSTUNREACH:		/* No route to host */
#endif
#ifdef EDQUOT
	  case EDQUOT:			/* Disc quota exceeded */
#endif
#ifdef EPROCLIM
	  case EPROCLIM:		/* Too many processes */
#endif
#ifdef EUSERS
	  case EUSERS:			/* Too many users */
#endif
#ifdef EDEADLK
	  case EDEADLK:			/* Resource deadlock avoided */
#endif
#ifdef EISCONN
	  case EISCONN:			/* Socket already connected */
#endif
#ifdef EINPROGRESS
	  case EINPROGRESS:		/* Operation now in progress */
#endif
#ifdef EALREADY
	  case EALREADY:		/* Operation already in progress */
#endif
#ifdef EADDRINUSE
	  case EADDRINUSE:		/* Address already in use */
#endif
#ifdef EADDRNOTAVAIL
	  case EADDRNOTAVAIL:		/* Can't assign requested address */
#endif
#ifdef ENOSR
	  case ENOSR:			/* Out of streams resources */
#endif
		return TRUE;
	}

	/* nope, must be permanent */
	return FALSE;
}
/*
**  LOCKFILE -- lock a file using flock or (shudder) lockf
**
**	Parameters:
**		fd -- the file descriptor of the file.
**		filename -- the file name (for error messages).
**		type -- type of the lock.  Bits can be:
**			LOCK_EX -- exclusive lock.
**			LOCK_NB -- non-blocking.
**
**	Returns:
**		TRUE if the lock was acquired.
**		FALSE otherwise.
*/

bool
lockfile(fd, filename, type)
	int fd;
	char *filename;
	int type;
{
# ifdef LOCKF
	int action;
	struct flock lfd;

	if (bitset(LOCK_EX, type))
		lfd.l_type = F_WRLCK;
	else
		lfd.l_type = F_RDLCK;

	if (bitset(LOCK_NB, type))
		action = F_SETLK;
	else
		action = F_SETLKW;

	lfd.l_whence = lfd.l_start = lfd.l_len = 0;

	if (fcntl(fd, action, &lfd) >= 0)
		return TRUE;

	if (!bitset(LOCK_NB, type) || (errno != EACCES && errno != EAGAIN))
		syserr("cannot lockf(%s)", filename);
# else
	if (flock(fd, type) >= 0)
		return TRUE;

	if (!bitset(LOCK_NB, type) || errno != EWOULDBLOCK)
		syserr("cannot flock(%s)", filename);
# endif
	return FALSE;
}
