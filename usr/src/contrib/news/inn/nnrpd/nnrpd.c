/*  $Revision: 1.18 $
**
**  NNTP server for readers (NNRP) for InterNetNews.
**  This server doesn't do any real load-limiting, except for what has
**  proven empirically necesary (i.e., look at GRPscandir).
*/
#define MAINLINE
#include <signal.h>
#include "nnrpd.h"
#include <sys/time.h>
#include <netdb.h>
#if	defined(HPUX)
#include <sys/pstat.h>
#endif	/* defined(HPUX) */


#define CMDany		-1


typedef struct _CMDENT {
    STRING	Name;
    FUNCPTR	Function;
    BOOL	Needauth;
    int		Minac;
    int		Maxac;
    STRING	Help;
} CMDENT;


char	NOACCESS[] = NNTP_ACCESS;
char	ACTIVE[] = _PATH_ACTIVE;
char	ACTIVETIMES[] = _PATH_ACTIVETIMES;
char	HISTORY[] = _PATH_HISTORY;
char	NEWSGROUPS[] = _PATH_NEWSGROUPS;
    /* Default permission -- change with adb. */
BOOL	PERMdefault = FALSE;

STATIC double	STATstart;
STATIC double	STATfinish;
#if	!defined(HPUX)
STATIC char	*TITLEstart;
STATIC char	*TITLEend;
#endif	/* !defined(HPUX) */
STATIC SIGVAR	ChangeTrace;

extern FUNCTYPE	CMDauthinfo();
extern FUNCTYPE	CMDdate();
extern FUNCTYPE	CMDfetch();
extern FUNCTYPE	CMDgroup();
STATIC FUNCTYPE	CMDhelp();
extern FUNCTYPE	CMDlist();
extern FUNCTYPE	CMDmode();
extern FUNCTYPE	CMDnewgroups();
extern FUNCTYPE	CMDnewnews();
extern FUNCTYPE	CMDnextlast();
extern FUNCTYPE	CMDpost();
extern FUNCTYPE	CMDxgtitle();
extern FUNCTYPE	CMDxhdr();
extern FUNCTYPE	CMDxover();
extern FUNCTYPE	CMDxpat();
extern FUNCTYPE	CMDxpath();
extern FUNCTYPE	CMD_unimp();
#if	defined(DO_DO_XTHREAD)
extern FUNCTYPE	CMDxthread();
#endif	/* defined(DO_DO_XTHREAD) */

STATIC char	CMDfetchhelp[] = "[MessageID|Number]";

STATIC CMDENT	CMDtable[] = {
    {	"authinfo",	CMDauthinfo,	FALSE,	3,	3,
	"user Name|pass Password" },
    {	"article",	CMDfetch,	FALSE,	1,	2,
	CMDfetchhelp },
    {	"body",		CMDfetch,	FALSE,	1,	2,
	CMDfetchhelp },
    {	"date",		CMDdate,	FALSE,	1,	1,
	NULL },
    {	"group",	CMDgroup,	FALSE,	2,	2,
	"newsgroup" },
    {	"head",		CMDfetch,	FALSE,	1,	2,
	CMDfetchhelp },
    {	"help",		CMDhelp,	FALSE,	1,	CMDany,
	NULL },
    {	"ihave",	CMD_unimp,	TRUE,	1,	2,
	NULL },
    {	"last",		CMDnextlast,	FALSE,	1,	1,
	NULL },
    {	"list",		CMDlist,	FALSE,	1,	2,
	"[active|newsgroups|distributions|schema]" },
    {	"listgroup",	CMDgroup,	FALSE,	1,	2,
	"newsgroup" },
    {	"mode",		CMDmode,	FALSE,	2,	2,
	"reader" },
    {	"newgroups",	CMDnewgroups,	FALSE,	3,	5,
	"yymmdd hhmmss [\"GMT\"] [<distributions>]" },
    {	"newnews",	CMDnewnews,	FALSE,	4,	6,
	"newsgroups yymmdd hhmmss [\"GMT\"] [<distributions>]" },
    {	"next",		CMDnextlast,	FALSE,	1,	1,
	NULL },
    {	"post",		CMDpost,	TRUE,	1,	1,
	NULL },
    {	"slave",	CMD_unimp,	FALSE,	1,	1,
	NULL },
    {	"stat",		CMDfetch,	FALSE,	1,	2,
	CMDfetchhelp },
    {	"xgtitle",	CMDxgtitle,	FALSE,	1,	2,
	"[group_pattern]" },
    {	"xhdr",		CMDxhdr,	FALSE,	2,	3,
	"header [range|MessageID]" },
    {	"xover",	CMDxover,	FALSE,	1,	2,
	"[range]" },
    {	"xpat",		CMDxpat,	FALSE,	4,	CMDany,
	"header range|MessageID pat [morepat...]" },
    {	"xpath",	CMDxpath,	FALSE,	2,	2,
	"xpath MessageID" },
#if	defined(DO_DO_XTHREAD)
    {	"xthread",	CMDxthread,	FALSE,	1,	2,
	"[dbinit|thread]" },
#endif	/* defined(DO_DO_XTHREAD) */
    {	NULL }
};


/*
**  Log a summary status message and exit.
*/
NORETURN
ExitWithStats(x)
    int			x;
{
    TIMEINFO		Now;
    double		usertime;
    double		systime;

    (void)fflush(stdout);
    (void)GetTimeInfo(&Now);
    STATfinish = TIMEINFOasDOUBLE(Now);
    if (GetResourceUsage(&usertime, &systime) < 0) {
	usertime = 0;
	systime = 0;
    }

    GRPreport();
    syslog(L_NOTICE, "%s exit articles %ld groups %ld",
	   ClientHost, ARTcount, GRPcount);
    if (POSTreceived ||  POSTrejected)
	syslog(L_NOTICE, "%s posts received %ld rejected %ld",
	   ClientHost, POSTreceived, POSTrejected);
    syslog(L_NOTICE, "%s times user %.3f system %.3f elapsed %.3f",
	ClientHost, usertime, systime, STATfinish - STATstart);
    exit(x);
}
/*
**  The "help" command.
*/
/* ARGSUSED0 */
STATIC FUNCTYPE
CMDhelp(ac, av)
    int		ac;
    char	*av[];
{
    CMDENT	*cp;

    Reply("%s\r\n", NNTP_HELP_FOLLOWS);
    for (cp = CMDtable; cp->Name; cp++)
	if (cp->Help == NULL)
	    Printf("  %s\r\n", cp->Name);
	else
	    Printf("  %s %s\r\n", cp->Name, cp->Help);
    Printf("Report problems to <%s@%s>\r\n",
	NEWSMASTER, GetConfigValue(_CONF_FROMHOST));
    Reply(".\r\n");
}


/*
**  Unimplemented catch-all.
*/
/* ARGSUSED0 */
FUNCTYPE
CMD_unimp(ac, av)
    int		ac;
    char	*av[];
{
    if (caseEQ(av[0], "ihave"))
	Reply("%d Transfer permission denied\r\n", NNTP_AUTH_NEEDED_VAL);
    else if (caseEQ(av[0], "slave"))
	/* Somebody sends us this?  I don't believe it! */
	Reply("%d Unsupported\r\n", NNTP_SLAVEOK_VAL);
    else
	Reply("%d %s not implemented; try help\r\n",
	    NNTP_BAD_COMMAND_VAL, av[0]);
}


/*
**  Overwrite the original argv so that ps will show what's going on.
*/
STATIC void
TITLEset(what)
    char		*what;
{
#if	!defined(HPUX)
    register char	*p;
    register int	i;
    char		buff[BUFSIZ];

    /* Make ps think we're swapped out so we get "(nnrpd)" in the output. */
    p = TITLEstart;
    *p++ = '-';

    (void)sprintf(buff, "%s %s", ClientHost, what);
    i = strlen(buff);
    if (i > TITLEend - p - 2) {
	i = TITLEend - p - 2;
	buff[i] = '\0';
    }
    (void)strcpy(p, buff);
    for (p += i; p < TITLEend; )
	*p++ = ' ';
#else
    char		buff[BUFSIZ];

    (void)sprintf(buff, "(nnrpd) %s %s", ClientHost, what);
    (void)pstat(PSTAT_SETCMD, buff, 0, 0, 0);
#endif	/* defined(HPUX) */
}


#if	defined(DO_NNRP_GETHOSTBYADDR)
/*
**  Convert an IP address to a hostname.  Don't trust the reverse lookup,
**  since anyone can fake .in-addr.arpa entries.
*/
STATIC BOOL
Address2Name(ap, hostname, i)
    register INADDR		*ap;
    register char		*hostname;
    register int		i;
{
    register char		*p;
    register struct hostent	*hp;
#if	defined(h_addr)
    register char		**pp;
#endif

    /* Get the official hostname, store it away. */
    if ((hp = gethostbyaddr((char *)ap, sizeof *ap, AF_INET)) == NULL)
	return FALSE;
    (void)strncpy(hostname, hp->h_name, i);
    hostname[i - 1] = '\0';

    /* Get addresses for this host. */
    if ((hp = gethostbyname(hostname)) == NULL)
	return FALSE;

    /* Make sure one of those addresses is the address we got. */
#if	defined(h_addr)
    /* We have many addresses */
    for (pp = hp->h_addr_list; *pp; pp++)
	if (memcmp((POINTER)&ap->s_addr, (POINTER)*pp,
		(SIZE_T)hp->h_length) == 0)
	    break;
    if (*pp == NULL)
	return FALSE;
#else
    /* We have one address. */
    if (memcmp((POINTER)&ap->s_addr, (POINTER)hp->h_addr,
	    (SIZE_T)hp->h_length) != 0)
	return FALSE;
#endif

    /* Only needed for misconfigured YP/NIS systems. */
    if (strchr(hostname, '.') == NULL
     && (p = GetConfigValue(_CONF_DOMAIN)) != NULL) {
	(void)strcat(hostname, ".");
	(void)strcat(hostname, p);
    }

    /* Make all lowercase, for wildmat. */
    for (p = hostname; *p; p++)
	if (CTYPE(isupper, *p))
	    *p = tolower(*p);
    return TRUE;
}
#endif	/* defined(DO_NNRP_GETHOSTBYADDR) */


BOOL
PERMinfile(hp, ip, user, pass, accesslist)
    char		*hp;
    char		*ip;
    char		*user;
    char		*pass;
    char		*accesslist;
{
    static char		ACCESS[] = _PATH_NNRPACCESS;
    register FILE	*F;
    register char	*p;
    register BOOL	found;
    register int	i;
    char		buff[BIG_BUFFER];
    char		*fields[5];

    if ((F = fopen(ACCESS, "r")) == NULL) {
	syslog(L_ERROR, "%s cant fopen %s %m", ClientHost, ACCESS);
	return FALSE;
    }

    PERMcanread = FALSE;
    PERMcanpost = FALSE;
    found = FALSE;
    accesslist[0] = '\0';
    while (fgets(buff, sizeof buff, F) != NULL) {
	if ((p = strchr(buff, '\n')) != NULL)
	    *p = '\0';
	if ((p = strchr(buff, COMMENT_CHAR)) != NULL)
	    *p = '\0';
	if (buff[0] == '\0')
	    continue;

	/* Split "host:permissions:user:pass:groups" into fields. */
	for (fields[0] = buff, i = 0, p = buff; *p; p++)
	    if (*p == ':') {
		*p = '\0';
		fields[++i] = p + 1;
	    }
	if (i != 4)
	    /* Malformed line. */
	    continue;

	if (hp)
	    /* Got an address; try to match either the IP address or as
	     * a text hostname. */
	    if (!(ip && wildmat(ip, fields[0])) && !wildmat(hp, fields[0]))
		continue;
	/* Matching for a specific user or just the host? */
	if (user && (!EQ(user, fields[2]) || !EQ(pass, fields[3])))
	    continue;

	PERMcanread = strchr(fields[1], 'R') != NULL;
	PERMcanpost = strchr(fields[1], 'P') != NULL;
	(void)strcpy(PERMuser, fields[2]);
	(void)strcpy(PERMpass, fields[3]);
	(void)strcpy(accesslist, fields[4]);
	found = TRUE;
    }
    (void)fclose(F);
    return found;
}


/*
**  Determine access rights of the client.
*/
STATIC void
StartConnection(accesslist)
    char		*accesslist;
{
    struct sockaddr_in	sin;
    int			length;
    char		buff[SMBUF];
    char		*ClientAddr;

    /* Get the peer's name. */
    length = sizeof sin;
    ClientAddr = NULL;
    if (getpeername(STDIN, (struct sockaddr *)&sin, &length) < 0) {
	if (!isatty(STDIN)) {
	    syslog(L_ERROR, "%s cant getpeername %m", "?");
	    Printf("%d I can't get your name.  Goodbye.\r\n", NNTP_ACCESS_VAL);
	    ExitWithStats(1);
	}
	(void)strcpy(ClientHost, "stdin");
    }
    else {
	if (sin.sin_family != AF_INET) {
	    syslog(L_ERROR, "%s bad_address_family %ld",
		"?", (long)sin.sin_family);
	    Printf("%d Bad address family.  Goodbye.\r\n", NNTP_ACCESS_VAL);
	    ExitWithStats(1);
	}

	/* Get client's name. */
#if	defined(DO_NNRP_GETHOSTBYADDR)
	if (!Address2Name(&sin.sin_addr, ClientHost, sizeof ClientHost)) {
	    (void)strcpy(ClientHost, inet_ntoa(sin.sin_addr));
	    syslog(L_ERROR, "? cant gethostbyaddr %s %m", ClientHost);
	}
	else {
	    ClientAddr = buff;
	    (void)strcpy(buff, inet_ntoa(sin.sin_addr));
	}
#else
	(void)strcpy(ClientHost, inet_ntoa(sin.sin_addr));
#endif /* defined(DO_NNRP_GETHOSTBYADDR) */
    }

    syslog(L_NOTICE, "%s connect", ClientHost);
    if (!PERMinfile(ClientHost, ClientAddr, (char *)NULL, (char *)NULL,
	    accesslist)) {
	syslog(L_NOTICE, "%s no_access", ClientHost);
	Printf("%d You are not in my access file.  Goodbye.\r\n",
	    NNTP_ACCESS_VAL);
	ExitWithStats(1);
    }
}


#if	!defined(VAR_NONE)

#if	defined(VAR_VARARGS)
#if	defined(lint)
#define START_VARARG(fmt, vp, type)	va_start(vp); fmt = NULL
#else
#define START_VARARG(fmt, vp, type)	va_start(vp); fmt = va_arg(vp, type)
#endif	/* defined(lint) */
#endif	/* defined(VAR_VARARGS) */
#if	defined(VAR_STDARGS)
#define START_VARARG(fmt, vp, type)	va_start(vp, fmt)
#endif	/* defined(VAR_STDARGS) */

/*
**  Send a reply, possibly with debugging output.
*/
/*VARARGS*/
void
#if	defined(VAR_VARARGS)
Reply(va_alist)
    va_dcl
#endif	/* defined(VAR_VARARGS) */
#if	defined(VAR_STDARGS)
Reply(char *fmt, ...)
#endif	/* defined(VAR_STDARGS) */
{
    register int	oerrno;
    register char	*p;
    va_list		vp;
    char		buff[2048];
#if	defined(VAR_VARARGS)
    register char	*fmt;
#endif	/* defined(VAR_VARARGS) */

    START_VARARG(fmt, vp, char*);
    (void)vprintf(fmt, vp);
    va_end(vp);

    if (Tracing) {
	oerrno = errno;
	START_VARARG(fmt, vp, char*);

	/* Copy output, but strip trailing CR-LF. */
	(void)vsprintf(buff, fmt, vp);
	p = buff + strlen(buff) - 1;
	while (p >= buff && (*p == '\n' || *p == '\r'))
	    *p-- = '\0';
	syslog(LOG_DEBUG, "%s > %s", ClientHost, buff);

	va_end(vp);
	errno = oerrno;
    }
}
#endif	/* !defined(VAR_NONE) */


/*
**  Got a signal; toggle tracing.
*/
STATIC SIGHANDLER
ToggleTrace(s)
    int		s;
{
    ChangeTrace = TRUE;
    (void)signal(s, ToggleTrace);
}


/*
**  Print a usage message and exit.
*/
STATIC void
Usage()
{
    (void)fprintf(stderr, "Usage error.\n");
    exit(1);
}


/* ARGSUSED0 */
int
main(argc, argv)
    int			argc;
    char		*argv[];
{
#if	NNRP_LOADLIMIT > 0
    int			load;
#endif	/* NNRP_LOADLIMIT > 0 */
    CMDENT		*cp;
    char		buff[NNTP_STRLEN];
    char		**av;
    int			ac;
    READTYPE		r;
    TIMEINFO		Now;
    register int	i;
    char		*Reject;
    char		accesslist[BIG_BUFFER];

#if	!defined(HPUX)
    /* Save start and extent of argv for TITLEset. */
    TITLEstart = argv[0];
    TITLEend = argv[argc - 1] + strlen(argv[argc - 1]) - 1;
#endif	/* !defined(HPUX) */

    /* Parse arguments.   Must COPY() optarg if used because the
     * TITLEset() routine would clobber it! */
    Reject = NULL;
    while ((i = getopt(argc, argv, "S:r:s:t")) != EOF)
	switch (i) {
	default:
	    Usage();
	    /* NOTREACHED */
	case 'S':			/* We're a slave to NNTP master */
	    RemoteMaster = COPY(optarg);
	    break;
	case 's':			/* Unused title string */
	    break;
	case 't':			/* Tracing */
	    Tracing = TRUE;
	    break;
	case 'r':			/* Reject connection message */
	    Reject = COPY(optarg);
	    break;
	}
    argc -= optind;
    if (argc)
	Usage();

    /* Setup. */
    openlog("nnrpd", L_OPENLOG_FLAGS | LOG_PID, LOG_INN_PROG);
    if (GetTimeInfo(&Now) < 0) {
	syslog(L_FATAL, "cant gettimeinfo %m");
	exit(1);
    }
    STATstart = TIMEINFOasDOUBLE(Now);

    if ((MyHostName = GetConfigValue(_CONF_PATHHOST)) == NULL) {
	syslog(L_FATAL, "cant getconfigvalue %m");
	ExitWithStats(1);
    }
    MyHostName = COPY(MyHostName);

#if	NNRP_LOADLIMIT > 0
    if ((load = GetLoadAverage()) > NNRP_LOADLIMIT) {
	syslog(L_NOTICE, "load %d > %d", load, NNRP_LOADLIMIT);
	Reply("%d load at %d, try later\r\n", NNTP_GOODBYE_VAL, load);
	ExitWithStats(1);
    }
#endif	/* NNRP_LOADLIMIT > 0 */

    /* Ignore SIGPIPE, since we'll see closed connections with read. */
    (void)signal(SIGPIPE, SIG_IGN);

    /* Arrange to toggle tracing. */
    (void)signal(SIGHUP, ToggleTrace);

    /* Get permissions and see if we can talk to this client */
    StartConnection(accesslist);
    if (!PERMcanread && !PERMcanpost) {
	syslog(L_NOTICE, "%s no_permission", ClientHost);
	Printf("%d You have no permission to talk.  Goodbye.\r\n",
	       NNTP_ACCESS_VAL);
	ExitWithStats(1);
    }

    /* Proceed with initialization. */
    PERMneedauth = PERMuser[0] != '\0' && PERMpass != '\0';
    PERMspecified = NGgetlist(&PERMlist, accesslist);
    TITLEset("connect");

    /* Were we told to reject connections? */
    if (Reject) {
	syslog(L_NOTICE, "%s rejected %s", ClientHost, Reject);
	Reply("%s %s\r\n", NNTP_GOODBYE, Reject);
	ExitWithStats(0);
    }

    ARTreadschema();
    if (!GetGroupList()) {
	/* This shouldn't really happen. */
	syslog(L_NOTICE, "%s cant getgrouplist %m", ClientHost);
	Reply("%d NNTP server unavailable. Try later.\r\n", NNTP_TEMPERR_VAL);
	ExitWithStats(1);
    }

    Reply("%d %s InterNetNews NNRP server %s ready (%s).\r\n",
	   PERMcanpost ? NNTP_POSTOK_VAL : NNTP_NOPOSTOK_VAL,
	   MyHostName, INNVersion(),
	   PERMcanpost ? "posting ok" : "no posting");

    /* Main dispatch loop. */
    for (av = NULL; ; ) {
	(void)fflush(stdout);
	if (ChangeTrace) {
	    Tracing = Tracing ? FALSE : TRUE;
	    syslog(L_TRACE, "trace %sabled", Tracing ? "en" : "dis");
	    ChangeTrace = FALSE;
	}
	switch (r = READline(buff, (int)sizeof buff, CLIENT_TIMEOUT)) {
	default:
	    syslog(L_ERROR, "%s internal %d in main", ClientHost, r);
	    /* FALLTHROUGH */
	case RTtimeout:
	    syslog(L_NOTICE, "%s timeout", ClientHost);
	    Printf("%d Timeout after %d seconds, closing connection.\r\n",
		   NNTP_TEMPERR_VAL, CLIENT_TIMEOUT);
	    ExitWithStats(1);
	    break;
	case RTlong:
	    Reply("%d Line too long\r\n", NNTP_BAD_COMMAND_VAL);
	    continue;
	case RTok:
	    /* Do some input processing, check for blank line. */
	    if (buff[0] == '\0')
		continue;
	    if (Tracing)
		syslog(L_TRACE, "%s < %s", ClientHost, buff);
	    ac = Argify(buff, &av);
	    if (ac == 0)
		continue;
	    break;
	case RTeof:
	    /* Handled below. */
	    break;
	}

	/* Client gone? */
	if (r == RTeof || caseEQ(av[0], "quit"))
	    break;

	/* Find command. */
	for (cp = CMDtable; cp->Name; cp++)
	    if (caseEQ(cp->Name, av[0]))
		break;
	if (cp->Name == NULL) {
	    if ((int)strlen(buff) > 40)
		syslog(L_NOTICE, "%s unrecognized %.40s...", ClientHost, buff);
	    else
		syslog(L_NOTICE, "%s unrecognized %s", ClientHost, buff);
	    Reply("%d What?\r\n", NNTP_BAD_COMMAND_VAL);
	    continue;
	}

	/* Check usage. */
	if ((cp->Minac != CMDany && ac < cp->Minac)
	 || (cp->Maxac != CMDany && ac > cp->Maxac)) {
	    Reply("%d %s\r\n",
		NNTP_SYNTAX_VAL,  cp->Help ? cp->Help : "Usage error");
	    continue;
	}

	/* Check permissions and dispatch. */
	if (cp->Needauth && PERMneedauth) {
	    Reply("%d Authentication required for command\r\n",
		NNTP_AUTH_NEEDED_VAL);
	    continue;
	}
	TITLEset(av[0]);
	(*cp->Function)(ac, av);
    }

    Reply("%s\r\n", NNTP_GOODBYE_ACK);

    ExitWithStats(0);
    /* NOTREACHED */
}
