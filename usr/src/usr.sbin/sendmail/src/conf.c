/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)conf.c	6.46 (Berkeley) %G%";
#endif /* not lint */

# include <sys/ioctl.h>
# include <sys/param.h>
# include <signal.h>
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
**  Some other configuration....
*/

char	SpaceSub;		/* character to replace <lwsp> in addrs */
int	QueueLA;		/* load avg > QueueLA -> just queue */
int	RefuseLA;		/* load avg > RefuseLA -> refuse connections */

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
