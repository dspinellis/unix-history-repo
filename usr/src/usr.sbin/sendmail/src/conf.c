/*
 * Copyright (c) 1983, 1995 Eric P. Allman
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)conf.c	8.168 (Berkeley) %G%";
#endif /* not lint */

# include "sendmail.h"
# include "pathnames.h"
# include <sys/ioctl.h>
# include <sys/param.h>

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
	"resent-sender",		H_FROM|H_RESENT,
	"resent-from",			H_FROM|H_RESENT,
	"resent-reply-to",		H_FROM|H_RESENT,
	"sender",			H_FROM,
	"from",				H_FROM,
	"reply-to",			H_FROM,
	"full-name",			H_ACHECK,
	"return-receipt-to",		H_FROM|H_RECEIPTTO,
	"errors-to",			H_FROM|H_ERRORSTO,

		/* destination fields */
	"to",				H_RCPT,
	"resent-to",			H_RCPT|H_RESENT,
	"cc",				H_RCPT,
	"resent-cc",			H_RCPT|H_RESENT,
	"bcc",				H_RCPT|H_STRIPVAL,
	"resent-bcc",			H_RCPT|H_STRIPVAL|H_RESENT,
	"apparently-to",		H_RCPT,

		/* message identification and control */
	"message-id",			0,
	"resent-message-id",		H_RESENT,
	"message",			H_EOH,
	"text",				H_EOH,

		/* date fields */
	"date",				0,
	"resent-date",			H_RESENT,

		/* trace fields */
	"received",			H_TRACE|H_FORCE,
	"x400-received",		H_TRACE|H_FORCE,
	"via",				H_TRACE|H_FORCE,
	"mail-from",			H_TRACE|H_FORCE,

		/* miscellaneous fields */
	"comments",			H_FORCE,
	"return-path",			H_FORCE|H_ACHECK,
	"content-transfer-encoding",	H_CTE,
	"content-type",			H_CTYPE,
	"content-length",		H_ACHECK,

	NULL,			0,
};



/*
**  Location of system files/databases/etc.
*/

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
	"restrictmailq",	PRIV_RESTRICTMAILQ,
	"restrictqrun",		PRIV_RESTRICTQRUN,
	"authwarnings",		PRIV_AUTHWARNINGS,
	"noreceipts",		PRIV_NORECEIPTS,
	"goaway",		PRIV_GOAWAY,
	NULL,			0,
};



/*
**  Some other configuration....
*/

char	SpaceSub;		/* character to replace <lwsp> in addrs */
int	QueueLA;		/* load avg > QueueLA -> just queue */
int	RefuseLA;		/* load avg > RefuseLA -> refuse connections */
}
/*
**  HOST_MAP_INIT -- initialize host class structures
*/

bool	host_map_init __P((MAP *map, char *args));

bool
host_map_init(map, args)
	MAP *map;
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

void
setupmailers()
{
	char buf[100];
	extern void makemailer();

	strcpy(buf, "prog, P=/bin/sh, F=lsoD, T=X-Unix, A=sh -c $u");
	makemailer(buf);

	strcpy(buf, "*file*, P=[FILE], F=lsDFMPEou, T=X-Unix, A=FILE");
	makemailer(buf);

	strcpy(buf, "*include*, P=/dev/null, F=su, A=INCLUDE");
	makemailer(buf);
}
/*
**  SETUPMAPS -- set up map classes
*/

#define MAPDEF(name, ext, flags, parse, open, close, lookup, store) \
	{ \
		extern bool parse __P((MAP *, char *)); \
		extern bool open __P((MAP *, int)); \
		extern void close __P((MAP *)); \
		extern char *lookup __P((MAP *, char *, char **, int *)); \
		extern void store __P((MAP *, char *, char *)); \
		s = stab(name, ST_MAPCLASS, ST_ENTER); \
		s->s_mapclass.map_cname = name; \
		s->s_mapclass.map_ext = ext; \
		s->s_mapclass.map_cflags = flags; \
		s->s_mapclass.map_parse = parse; \
		s->s_mapclass.map_open = open; \
		s->s_mapclass.map_close = close; \
		s->s_mapclass.map_lookup = lookup; \
		s->s_mapclass.map_store = store; \
	}

void
setupmaps()
{
	register STAB *s;

#ifdef NEWDB
	MAPDEF("hash", ".db", MCF_ALIASOK|MCF_REBUILDABLE,
		map_parseargs, hash_map_open, db_map_close,
		db_map_lookup, db_map_store);

	MAPDEF("btree", ".db", MCF_ALIASOK|MCF_REBUILDABLE,
		map_parseargs, bt_map_open, db_map_close,
		db_map_lookup, db_map_store);
#endif

#ifdef NDBM
	MAPDEF("dbm", ".dir", MCF_ALIASOK|MCF_REBUILDABLE,
		map_parseargs, ndbm_map_open, ndbm_map_close,
		ndbm_map_lookup, ndbm_map_store);
#endif

#ifdef NIS
	MAPDEF("nis", NULL, MCF_ALIASOK,
		map_parseargs, nis_map_open, null_map_close,
		nis_map_lookup, null_map_store);
#endif

#ifdef NISPLUS
	MAPDEF("nisplus", NULL, MCF_ALIASOK,
		map_parseargs, nisplus_map_open, null_map_close,
		nisplus_map_lookup, null_map_store);
#endif

#ifdef HESIOD
	MAPDEF("hesiod", NULL, MCF_ALIASOK|MCF_ALIASONLY,
		map_parseargs, null_map_open, null_map_close,
		hes_map_lookup, null_map_store);
#endif

#ifdef NETINFO
	MAPDEF("netinfo", NULL, MCF_ALIASOK,
		map_parseargs, ni_map_open, null_map_close,
		ni_map_lookup, null_map_store);
#endif

#if 0
	MAPDEF("dns", NULL, 0,
		dns_map_init, null_map_open, null_map_close,
		dns_map_lookup, null_map_store);
#endif

#if NAMED_BIND
	/* best MX DNS lookup */
	MAPDEF("bestmx", NULL, MCF_OPTFILE,
		map_parseargs, null_map_open, null_map_close,
		bestmx_map_lookup, null_map_store);
#endif

	MAPDEF("host", NULL, 0,
		host_map_init, null_map_open, null_map_close,
		host_map_lookup, null_map_store);

	MAPDEF("text", NULL, MCF_ALIASOK,
		map_parseargs, text_map_open, null_map_close,
		text_map_lookup, null_map_store);

	MAPDEF("stab", NULL, MCF_ALIASOK|MCF_ALIASONLY,
		map_parseargs, stab_map_open, null_map_close,
		stab_map_lookup, stab_map_store);

	MAPDEF("implicit", NULL, MCF_ALIASOK|MCF_ALIASONLY|MCF_REBUILDABLE,
		map_parseargs, impl_map_open, impl_map_close,
		impl_map_lookup, impl_map_store);

	/* access to system passwd file */
	MAPDEF("user", NULL, MCF_OPTFILE,
		map_parseargs, user_map_open, null_map_close,
		user_map_lookup, null_map_store);

	/* dequote map */
	MAPDEF("dequote", NULL, 0,
		dequote_init, null_map_open, null_map_close,
		dequote_map, null_map_store);

#ifdef USERDB
	/* user database */
	MAPDEF("userdb", ".db", 0,
		map_parseargs, null_map_open, null_map_close,
		udb_map_lookup, null_map_store);
#endif

	/* arbitrary programs */
	MAPDEF("program", NULL, MCF_ALIASOK,
		map_parseargs, null_map_open, null_map_close,
		prog_map_lookup, null_map_store);

	/* sequenced maps */
	MAPDEF("sequence", NULL, MCF_ALIASOK,
		seq_map_parse, null_map_open, null_map_close,
		seq_map_lookup, seq_map_store);

	/* switched interface to sequenced maps */
	MAPDEF("switch", NULL, MCF_ALIASOK,
		map_parseargs, switch_map_open, null_map_close,
		seq_map_lookup, seq_map_store);
}

#undef MAPDEF
/*
**  INITHOSTMAPS -- initial host-dependent maps
**
**	This should act as an interface to any local service switch
**	provided by the host operating system.
**
**	Parameters:
**		none
**
**	Returns:
**		none
**
**	Side Effects:
**		Should define maps "host" and "users" as necessary
**		for this OS.  If they are not defined, they will get
**		a default value later.  It should check to make sure
**		they are not defined first, since it's possible that
**		the config file has provided an override.
*/

void
inithostmaps()
{
	register int i;
	int nmaps;
	char *maptype[MAXMAPSTACK];
	short mapreturn[MAXMAPACTIONS];
	char buf[MAXLINE];

	/*
	**  Set up default hosts maps.
	*/

#if 0
	nmaps = switch_map_find("hosts", maptype, mapreturn);
	for (i = 0; i < nmaps; i++)
	{
		if (strcmp(maptype[i], "files") == 0 &&
		    stab("hosts.files", ST_MAP, ST_FIND) == NULL)
		{
			strcpy(buf, "hosts.files text -k 0 -v 1 /etc/hosts");
			makemapentry(buf);
		}
#if NAMED_BIND
		else if (strcmp(maptype[i], "dns") == 0 &&
		    stab("hosts.dns", ST_MAP, ST_FIND) == NULL)
		{
			strcpy(buf, "hosts.dns dns A");
			makemapentry(buf);
		}
#endif
#ifdef NISPLUS
		else if (strcmp(maptype[i], "nisplus") == 0 &&
		    stab("hosts.nisplus", ST_MAP, ST_FIND) == NULL)
		{
			strcpy(buf, "hosts.nisplus nisplus -k name -v address -d hosts.org_dir");
			makemapentry(buf);
		}
#endif
#ifdef NIS
		else if (strcmp(maptype[i], "nis") == 0 &&
		    stab("hosts.nis", ST_MAP, ST_FIND) == NULL)
		{
			strcpy(buf, "hosts.nis nis -d -k 0 -v 1 hosts.byname");
			makemapentry(buf);
		}
#endif
	}
#endif

	/*
	**  Make sure we have a host map.
	*/

	if (stab("host", ST_MAP, ST_FIND) == NULL)
	{
		/* user didn't initialize: set up host map */
		strcpy(buf, "host host");
#if NAMED_BIND
		if (ConfigLevel >= 2)
			strcat(buf, " -a.");
#endif
		makemapentry(buf);
	}

	/*
	**  Set up default aliases maps
	*/

	nmaps = switch_map_find("aliases", maptype, mapreturn);
	for (i = 0; i < nmaps; i++)
	{
		if (strcmp(maptype[i], "files") == 0 &&
		    stab("aliases.files", ST_MAP, ST_FIND) == NULL)
		{
			strcpy(buf, "aliases.files implicit /etc/aliases");
			makemapentry(buf);
		}
#ifdef NISPLUS
		else if (strcmp(maptype[i], "nisplus") == 0 &&
		    stab("aliases.nisplus", ST_MAP, ST_FIND) == NULL)
		{
			strcpy(buf, "aliases.nisplus nisplus -kalias -vexpansion -d mail_aliases.org_dir");
			makemapentry(buf);
		}
#endif
#ifdef NIS
		else if (strcmp(maptype[i], "nis") == 0 &&
		    stab("aliases.nis", ST_MAP, ST_FIND) == NULL)
		{
			strcpy(buf, "aliases.nis nis -d mail.aliases");
			makemapentry(buf);
		}
#endif
	}
	if (stab("aliases", ST_MAP, ST_FIND) == NULL)
	{
		strcpy(buf, "aliases switch aliases");
		makemapentry(buf);
	}
	strcpy(buf, "switch:aliases");
	setalias(buf);

#if 0		/* "user" map class is a better choice */
	/*
	**  Set up default users maps.
	*/

	nmaps = switch_map_find("passwd", maptype, mapreturn);
	for (i = 0; i < nmaps; i++)
	{
		if (strcmp(maptype[i], "files") == 0 &&
		    stab("users.files", ST_MAP, ST_FIND) == NULL)
		{
			strcpy(buf, "users.files text -m -z: -k0 -v6 /etc/passwd");
			makemapentry(buf);
		}
#ifdef NISPLUS
		else if (strcmp(maptype[i], "nisplus") == 0 &&
		    stab("users.nisplus", ST_MAP, ST_FIND) == NULL)
		{
			strcpy(buf, "users.nisplus nisplus -m -kname -vhome -d passwd.org_dir");
			makemapentry(buf);
		}
#endif
#ifdef NIS
		else if (strcmp(maptype[i], "nis") == 0 &&
		    stab("users.nis", ST_MAP, ST_FIND) == NULL)
		{
			strcpy(buf, "users.nis nis -m -d passwd.byname");
			makemapentry(buf);
		}
#endif
#ifdef HESIOD
		else if (strcmp(maptype[i], "hesiod") == 0) &&
		    stab("users.hesiod", ST_MAP, ST_FIND) == NULL)
		{
			strcpy(buf, "users.hesiod hesiod");
			makemapentry(buf);
		}
#endif
	}
	if (stab("users", ST_MAP, ST_FIND) == NULL)
	{
		strcpy(buf, "users switch -m passwd");
		makemapentry(buf);
	}
#endif
}
/*
**  SWITCH_MAP_FIND -- find the list of types associated with a map
**
**	This is the system-dependent interface to the service switch.
**
**	Parameters:
**		service -- the name of the service of interest.
**		maptype -- an out-array of strings containing the types
**			of access to use for this service.  There can
**			be at most MAXMAPSTACK types for a single service.
**		mapreturn -- an out-array of return information bitmaps
**			for the map.
**
**	Returns:
**		The number of map types filled in, or -1 for failure.
*/

#ifdef SOLARIS
# include <nsswitch.h>
#endif

#if defined(ultrix) || defined(__osf__)
# include <sys/svcinfo.h>
#endif

int
switch_map_find(service, maptype, mapreturn)
	char *service;
	char *maptype[MAXMAPSTACK];
	short mapreturn[MAXMAPACTIONS];
{
	register FILE *fp;
	int svcno;
	static char buf[MAXLINE];

#ifdef SOLARIS
	struct __nsw_switchconfig *nsw_conf;
	enum __nsw_parse_err pserr;
	struct __nsw_lookup *lk;
	int nsw_rc;
	static struct __nsw_lookup lkp0 =
		{ "files", {1, 0, 0, 0}, NULL, NULL };
	static struct __nsw_switchconfig lkp_default =
		{ 0, "sendmail", 3, &lkp0 };

	if ((nsw_conf = __nsw_getconfig(service, &pserr)) == NULL)
		lk = lkp_default.lookups;
	else
		lk = nsw_conf->lookups;
	svcno = 0;
	while (lk != NULL)
	{
		maptype[svcno] = lk->service_name;
		if (lk->actions[__NSW_NOTFOUND] == __NSW_RETURN)
			mapreturn[MA_NOTFOUND] |= 1 << svcno;
		if (lk->actions[__NSW_TRYAGAIN] == __NSW_RETURN)
			mapreturn[MA_TRYAGAIN] |= 1 << svcno;
		if (lk->actions[__NSW_UNAVAIL] == __NSW_RETURN)
			mapreturn[MA_TRYAGAIN] |= 1 << svcno;
		svcno++;
		lk = lk->next;
	}
	return svcno;
#endif

#if defined(ultrix) || defined(__osf__)
	struct svcinfo *svcinfo;
	int svc;

	for (svcno = 0; svcno < MAXMAPACTIONS; svcno++)
		mapreturn[svcno] = 0;

	svcinfo = getsvc();
	if (svcinfo == NULL)
		goto punt;
	if (strcmp(service, "hosts") == 0)
		svc = SVC_HOSTS;
	else if (strcmp(service, "aliases") == 0)
		svc = SVC_ALIASES;
	else if (strcmp(service, "passwd") == 0)
		svc = SVC_PASSWD;
	else
		return -1;
	for (svcno = 0; svcno < SVC_PATHSIZE; svcno++)
	{
		switch (svcinfo->svcpath[svc][svcno])
		{
		  case SVC_LOCAL:
			maptype[svcno] = "files";
			break;

		  case SVC_YP:
			maptype[svcno] = "nis";
			break;

		  case SVC_BIND:
			maptype[svcno] = "dns";
			break;

#ifdef SVC_HESIOD
		  case SVC_HESIOD:
			maptype[svcno] = "hesiod";
			break;
#endif

		  case SVC_LAST:
			return svcno;
		}
	}
	return svcno;
#endif

#if !defined(SOLARIS) && !defined(ultrix) && !defined(__osf__)
	/*
	**  Fall-back mechanism.
	*/

	for (svcno = 0; svcno < MAXMAPACTIONS; svcno++)
		mapreturn[svcno] = 0;

	svcno = 0;
	fp = fopen(ServiceSwitchFile, "r");
	if (fp != NULL)
	{
		while (fgets(buf, sizeof buf, fp) != NULL)
		{
			register char *p;

			p = strpbrk(buf, "#\n");
			if (p != NULL)
				*p = '\0';
			p = strpbrk(buf, " \t");
			if (p != NULL)
				*p++ = '\0';
			if (strcmp(buf, service) != 0)
				continue;

			/* got the right service -- extract data */
			do
			{
				while (isspace(*p))
					p++;
				if (*p == '\0')
					break;
				maptype[svcno++] = p;
				p = strpbrk(p, " \t");
				if (p != NULL)
					*p++ = '\0';
			} while (p != NULL);
			break;
		}
		fclose(fp);
		return svcno;
	}
#endif

	/* if the service file doesn't work, use an absolute fallback */
  punt:
	for (svcno = 0; svcno < MAXMAPACTIONS; svcno++)
		mapreturn[svcno] = 0;
	svcno = 0;
	if (strcmp(service, "aliases") == 0)
	{
		maptype[svcno++] = "files";
#ifdef AUTO_NIS_ALIASES
# ifdef NISPLUS
		maptype[svcno++] = "nisplus";
# endif
# ifdef NIS
		maptype[svcno++] = "nis";
# endif
#endif
		return svcno;
	}
	if (strcmp(service, "hosts") == 0)
	{
# if NAMED_BIND
		maptype[svcno++] = "dns";
# else
#  if defined(sun) && !defined(BSD) && !defined(SOLARIS)
		/* SunOS */
		maptype[svcno++] = "nis";
#  endif
# endif
		maptype[svcno++] = "files";
		return svcno;
	}
	return -1;
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
			pw = sm_getpwuid(RealUid);
			if (pw != NULL)
				myname = newstr(pw->pw_name);
		}
		else
		{
			uid_t uid = RealUid;

			myname = newstr(myname);
			if ((pw = sm_getpwnam(myname)) == NULL ||
			      (uid != 0 && uid != pw->pw_uid))
			{
				pw = sm_getpwuid(uid);
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
**	message should be given using "usrerr" and an EX_ code
**	should be returned.  You can also set to->q_status to
**	a DSN-style status code.
**
**	EF_NO_BODY_RETN can be set in e->e_flags to suppress the
**	body during the return-to-sender function; this should be done
**	on huge messages.  This bit may already be set by the ESMTP
**	protocol.
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

int
checkcompat(to, e)
	register ADDRESS *to;
	register ENVELOPE *e;
{
# ifdef lint
	if (to == NULL)
		to++;
# endif /* lint */

	if (tTd(49, 1))
		printf("checkcompat(to=%s, from=%s)\n",
			to->q_paddr, e->e_from.q_paddr);

# ifdef EXAMPLE_CODE
	/* this code is intended as an example only */
	register STAB *s;

	s = stab("arpa", ST_MAILER, ST_FIND);
	if (s != NULL && strcmp(e->e_from.q_mailer->m_name, "local") != 0 &&
	    to->q_mailer == s->s_mailer)
	{
		usrerr("553 No ARPA mail through this machine: see your system administration");
		/* e->e_flags |= EF_NO_BODY_RETN; to supress body on return */
		to->q_status = "5.7.1";
		return (EX_UNAVAILABLE);
	}
# endif /* EXAMPLE_CODE */
	return (EX_OK);
}
/*
**  SETSIGNAL -- set a signal handler
**
**	This is essentially old BSD "signal(3)".
*/

sigfunc_t
setsignal(sig, handler)
	int sig;
	sigfunc_t handler;
{
#if defined(SYS5SIGNALS) || defined(BSD4_3) || defined(_AUX_SOURCE)
	return signal(sig, handler);
#else
	struct sigaction n, o;

	bzero(&n, sizeof n);
	n.sa_handler = handler;
# ifdef SA_RESTART
	n.sa_flags = SA_RESTART;
# endif
	if (sigaction(sig, &n, &o) < 0)
		return SIG_ERR;
	return o.sa_handler;
#endif
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

void
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

void
rlsesigs()
{
}
/*
**  INIT_MD -- do machine dependent initializations
**
**	Systems that have global modes that should be set should do
**	them here rather than in main.
*/

#ifdef _AUX_SOURCE
# include	<compat.h>
#endif

void
init_md(argc, argv)
	int argc;
	char **argv;
{
#ifdef _AUX_SOURCE
	setcompat(getcompat() | COMPAT_BSDPROT);
#endif

#ifdef VENDOR_DEFAULT
	VendorCode = VENDOR_DEFAULT;
#else
	VendorCode = VENDOR_BERKELEY;
#endif
}
/*
**  INIT_VENDOR_MACROS -- vendor-dependent macro initializations
**
**	Called once, on startup.
**
**	Parameters:
**		e -- the global envelope.
**
**	Returns:
**		none.
**
**	Side Effects:
**		vendor-dependent.
*/

void
init_vendor_macros(e)
	register ENVELOPE *e;
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
#define LA_INT		2	/* read kmem for avenrun; interpret as long */
#define LA_FLOAT	3	/* read kmem for avenrun; interpret as float */
#define LA_SUBR		4	/* call getloadavg */
#define LA_MACH		5	/* MACH load averages (as on NeXT boxes) */
#define LA_SHORT	6	/* read kmem for avenrun; interpret as short */
#define LA_PROCSTR	7	/* read string ("1.17") from /proc/loadavg */
#define LA_READKSYM	8	/* SVR4: use MIOC_READKSYM ioctl call */
#define LA_DGUX		9	/* special DGUX implementation */
#define LA_HPUX		10	/* special HPUX implementation */

/* do guesses based on general OS type */
#ifndef LA_TYPE
# define LA_TYPE	LA_ZERO
#endif

#ifndef FSHIFT
# if defined(unixpc)
#  define FSHIFT	5
# endif

# if defined(__alpha) || defined(IRIX)
#  define FSHIFT	10
# endif

# if defined(_AIX3)
#  define FSHIFT	16
# endif
#endif

#ifndef FSHIFT
# define FSHIFT		8
#endif

#ifndef FSCALE
# define FSCALE		(1 << FSHIFT)
#endif

#if (LA_TYPE == LA_INT) || (LA_TYPE == LA_FLOAT) || (LA_TYPE == LA_SHORT)

#include <nlist.h>

#ifdef IRIX64
# define nlist		nlist64
#endif

#ifndef LA_AVENRUN
# ifdef SYSTEM5
#  define LA_AVENRUN	"avenrun"
# else
#  define LA_AVENRUN	"_avenrun"
# endif
#endif

/* _PATH_UNIX should be defined in <paths.h> */
#ifndef _PATH_UNIX
# if defined(SYSTEM5)
#  define _PATH_UNIX	"/unix"
# else
#  define _PATH_UNIX	"/vmunix"
# endif
#endif

struct nlist	Nl[] =
{
	{ LA_AVENRUN },
#define	X_AVENRUN	0
	{ 0 },
};

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

#ifdef _AIX3
		if (knlist(Nl, 1, sizeof Nl[0]) < 0)
#else
		if (nlist(_PATH_UNIX, Nl) < 0)
#endif
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
#ifdef NAMELISTMASK
		Nl[X_AVENRUN].n_value &= NAMELISTMASK;
#endif
	}
	if (tTd(3, 20))
		printf("getla: symbol address = %#x\n", Nl[X_AVENRUN].n_value);
	if (lseek(kmem, (off_t) Nl[X_AVENRUN].n_value, SEEK_SET) == -1 ||
	    read(kmem, (char *) avenrun, sizeof(avenrun)) < sizeof(avenrun))
	{
		/* thank you Ian */
		if (tTd(3, 1))
			printf("getla: lseek or read: %s\n", errstring(errno));
		return (-1);
	}
# if (LA_TYPE == LA_INT) || (LA_TYPE == LA_SHORT)
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

#endif /* LA_TYPE == LA_INT or LA_SHORT or LA_FLOAT */

#if LA_TYPE == LA_READKSYM

# include <sys/ksym.h>

getla()
{
	static int kmem = -1;
	long avenrun[3];
	extern int errno;
	struct mioc_rksym mirk;

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
	}
	mirk.mirk_symname = LA_AVENRUN;
	mirk.mirk_buf = avenrun;
	mirk.mirk_buflen = sizeof(avenrun);
	if (ioctl(kmem, MIOC_READKSYM, &mirk) < 0)
	{
		if (tTd(3, 1))
			printf("getla: ioctl(MIOC_READKSYM) failed: %s\n",
				errstring(errno));
		return -1;
	}
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
}

#endif /* LA_TYPE == LA_READKSYM */

#if LA_TYPE == LA_DGUX

# include <sys/dg_sys_info.h>

int
getla()
{
	struct dg_sys_info_load_info load_info;

	dg_sys_info((long *)&load_info,
		DG_SYS_INFO_LOAD_INFO_TYPE, DG_SYS_INFO_LOAD_VERSION_0);

        if (tTd(3, 1))
                printf("getla: %d\n", (int) (load_info.one_minute + 0.5));

	return((int) (load_info.one_minute + 0.5));
}

#endif /* LA_TYPE == LA_DGUX */

#if LA_TYPE == LA_HPUX

struct pst_dynamic;

# include <sys/param.h>
# include <sys/pstat.h>

int
getla()
{
	struct pst_dynamic pstd;

	if (pstat_getdynamic(&pstd, sizeof(struct pst_dynamic),
			     (size_t) 1, 0) == -1)
		return 0;

        if (tTd(3, 1))
                printf("getla: %d\n", (int) (pstd.psd_avg_1_min + 0.5));

	return (int) (pstd.psd_avg_1_min + 0.5);
}

#endif /* LA_TYPE == LA_HPUX */

#if LA_TYPE == LA_SUBR

int
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

#endif /* LA_TYPE == LA_SUBR */

#if LA_TYPE == LA_MACH

/*
**  This has been tested on NEXTSTEP release 2.1/3.X.
*/

#if defined(NX_CURRENT_COMPILER_RELEASE) && NX_CURRENT_COMPILER_RELEASE > NX_COMPILER_RELEASE_3_0
# include <mach/mach.h>
#else
# include <mach.h>
#endif

getla()
{
	processor_set_t default_set;
	kern_return_t error;
	unsigned int info_count;
	struct processor_set_basic_info info;
	host_t host;

	error = processor_set_default(host_self(), &default_set);
	if (error != KERN_SUCCESS)
		return -1;
	info_count = PROCESSOR_SET_BASIC_INFO_COUNT;
	if (processor_set_info(default_set, PROCESSOR_SET_BASIC_INFO,
			       &host, (processor_set_info_t)&info,
			       &info_count) != KERN_SUCCESS)
	{
		return -1;
	}
	return (int) (info.load_average + (LOAD_SCALE / 2)) / LOAD_SCALE;
}

#endif /* LA_TYPE == LA_MACH */

#if LA_TYPE == LA_PROCSTR

/*
**  Read /proc/loadavg for the load average.  This is assumed to be
**  in a format like "0.15 0.12 0.06".
**
**	Initially intended for Linux.  This has been in the kernel
**	since at least 0.99.15.
*/

# ifndef _PATH_LOADAVG
#  define _PATH_LOADAVG	"/proc/loadavg"
# endif

int
getla()
{
	double avenrun;
	register int result;
	FILE *fp;

	fp = fopen(_PATH_LOADAVG, "r");
	if (fp == NULL)
	{
		if (tTd(3, 1))
			printf("getla: fopen(%s): %s\n",
				_PATH_LOADAVG, errstring(errno));
		return -1;
	}
	result = fscanf(fp, "%lf", &avenrun);
	fclose(fp);
	if (result != 1)
	{
		if (tTd(3, 1))
			printf("getla: fscanf() = %d: %s\n",
				result, errstring(errno));
		return -1;
	}

	if (tTd(3, 1))
		printf("getla(): %.2f\n", avenrun);

	return ((int) (avenrun + 0.5));
}

#endif /* LA_TYPE == LA_PROCSTR */

#if LA_TYPE == LA_ZERO

getla()
{
	if (tTd(3, 1))
		printf("getla: ZERO\n");
	return (0);
}

#endif /* LA_TYPE == LA_ZERO */


/*
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Authors:  Many and varied...
 */

/* Non Apollo stuff removed by Don Lewis 11/15/93 */
#ifndef lint
static char  rcsid[] = "@(#)$Id: getloadavg.c,v 1.16 1991/06/21 12:51:15 paul Exp $";
#endif /* !lint */

#ifdef apollo
# undef volatile
#    include <apollo/base.h>

/* ARGSUSED */
int getloadavg( call_data )
     caddr_t	call_data;	/* pointer to (double) return value */
{
     double *avenrun = (double *) call_data;
     int i;
     status_$t      st;
     long loadav[3];
     proc1_$get_loadav(loadav, &st);
     *avenrun = loadav[0] / (double) (1 << 16);
     return(0);
}
#   endif /* apollo */
