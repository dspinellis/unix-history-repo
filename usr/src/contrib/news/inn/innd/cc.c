/*  $Revision: 1.41 $
**
**  Routines for the control channel.  Create a Unix-domain datagram socket
**  that processes on the local server send messages to.  The control
**  channel is used only by ctlinnd to tell the server to perform
**  special functions.  We use datagrams so that we don't need to do an
**  accept() and tie up another descriptor.  Recvfrom seems to be broken on
**  several systems, so the client passes in the socket name.
**
**  This module completely rips away all pretense of software layering.
*/
#include "innd.h"
#include "inndcomm.h"
#if	defined(DO_HAVE_UNIX_DOMAIN)
#include <sys/un.h>
#endif	/* defined(DO_HAVE_UNIX_DOMAIN) */


/*
**  An entry in the dispatch table.  The name, and implementing function,
**  of every command we support.
*/
typedef struct _CCDISPATCH {
    char	Name;
    int		argc;
    STRING	(*Function)();
} CCDISPATCH;


STATIC STRING	CCaddhist();
STATIC STRING	CCallow();
STATIC STRING	CCbegin();
STATIC STRING	CCchgroup();
STATIC STRING	CCdrop();
STATIC STRING	CCflush();
STATIC STRING	CCflushlogs();
STATIC STRING	CCgo();
STATIC STRING	CChangup();
STATIC STRING	CCreserve();
STATIC STRING	CCmode();
STATIC STRING	CCname();
STATIC STRING	CCnewgroup();
STATIC STRING	CCparam();
STATIC STRING	CCpause();
STATIC STRING	CCreaders();
STATIC STRING	CCrefile();
STATIC STRING	CCreject();
STATIC STRING	CCreload();
STATIC STRING	CCrenumber();
STATIC STRING	CCrmgroup();
STATIC STRING	CCsend();
STATIC STRING	CCshutdown();
STATIC STRING	CCsignal();
STATIC STRING	CCthrottle();
STATIC STRING	CCtrace();
STATIC STRING	CCxabort();
STATIC STRING	CCxexec();

STATIC char		CCpath[] = _PATH_NEWSCONTROL;
STATIC char		**CCargv;
STATIC char		CCnosite[] = "1 No such site";
STATIC char		CCwrongtype[] = "1 Wrong site type";
STATIC char		CCnogroup[] = "1 No such group";
STATIC char		CCnochannel[] = "1 No such channel";
STATIC char		CCnoreason[] = "1 Empty reason";
STATIC char		CCnotrunning[] = "1 Must be running";
STATIC BUFFER		CCreply;
STATIC CHANNEL		*CCchan;
STATIC int		CCwriter;
STATIC CCDISPATCH	CCcommands[] = {
    {	SC_ADDHIST,	5, CCaddhist	},
    {	SC_ALLOW,	1, CCallow	},
    {	SC_BEGIN,	1, CCbegin	},
    {	SC_CANCEL,	1, CCcancel	},
    {	SC_CHANGEGROUP,	2, CCchgroup	},
    {	SC_CHECKFILE,	0, CCcheckfile	},
    {	SC_DROP,	1, CCdrop	},
    {	SC_FLUSH,	1, CCflush	},
    {	SC_FLUSHLOGS,	0, CCflushlogs	},
    {	SC_GO,		1, CCgo		},
    {	SC_HANGUP,	1, CChangup	},
    {	SC_MODE,	0, CCmode	},
    {	SC_NAME,	1, CCname	},
    {	SC_NEWGROUP,	3, CCnewgroup	},
    {	SC_PARAM,	2, CCparam	},
    {	SC_PAUSE,	1, CCpause	},
    {	SC_READERS,	2, CCreaders	},
    {	SC_REFILE,	2, CCrefile	},
    {	SC_REJECT,	1, CCreject	},
    {	SC_RENUMBER,	1, CCrenumber	},
    {	SC_RELOAD,	2, CCreload	},
    {	SC_RESERVE,	1, CCreserve	},
    {	SC_RMGROUP,	1, CCrmgroup	},
    {	SC_SEND,	2, CCsend	},
    {	SC_SHUTDOWN,	1, CCshutdown	},
    {	SC_SIGNAL,	2, CCsignal	},
    {	SC_THROTTLE,	1, CCthrottle	},
    {	SC_TRACE,	2, CCtrace	},
    {	SC_XABORT,	1, CCxabort	},
    {	SC_XEXEC,	1, CCxexec	}
};



void
CCcopyargv(av)
    char		*av[];
{
    register char	**v;
    register int	i;

    /* Get the vector size. */
    for (i = 0; av[i]; i++)
	continue;

    /* Get the vector, copy each element. */
    for (v = CCargv = NEW(char*, i + 1); *av; av++)
	*v++ = COPY(*av);
    *v = NULL;
}


/*
**  Add <> around Message-ID if needed.
*/
STATIC STRING
CCgetid(p, store)
    char		*p;
    char		**store;
{
    static char		NULLMESGID[] = "1 Empty Message-ID";
    static BUFFER	Save;
    int			i;

    if (*p == '\0')
	return NULLMESGID;
    if (*p == '<') {
	if (p[1] == '\0' || p[1] == '>')
	    return NULLMESGID;
	*store = p;
	return NULL;
    }

    /* Make sure the Message-ID buffer has room. */
    i = 1 + strlen(p) + 1 + 1;
    if (Save.Data == NULL) {
	Save.Size = i;
	Save.Data = NEW(char, Save.Size);
    }
    else if (Save.Size < i) {
	Save.Size = i;
	RENEW(Save.Data, char, Save.Size);
    }
    *store = Save.Data;
    (void)sprintf(*store, "<%s>", p);
    return NULL;
}


/*
**  Abort and dump core.
*/
STATIC STRING
CCxabort(av)
    char		*av[];
{
    syslog(L_FATAL, "%s abort %s", LogName, av[0]);
    abort();
    syslog(L_FATAL, "%s cant abort %m", LogName);
    CleanupAndExit(1, av[0]);
    /* NOTREACHED */
}


/*
**  Do the work needed to add a history entry.
*/
STATIC STRING
CCaddhist(av)
    char		*av[];
{
    static char		DIGITS[] = "0123456789";
    ARTDATA		Data;
    STRING		p;
    BOOL		ok;

    /* Check the fields. */
    if ((p = CCgetid(av[0], &Data.MessageID)) != NULL)
	return p;
    if (HIShavearticle(Data.MessageID))
	return "1 Duplicate";
    if (strspn(av[1], DIGITS) != strlen(av[1]))
	return "1 Bad arrival date";
    Data.Arrived = atol(av[1]);
    if (strspn(av[2], DIGITS) != strlen(av[2]))
	return "1 Bad expiration date";
    Data.Expires = atol(av[2]);
    if (strspn(av[3], DIGITS) != strlen(av[3]))
	return "1 Bad posted date";
    Data.Posted = atol(av[3]);

    if (Mode == OMrunning)
	ok = HISwrite(&Data, av[4]);
    else {
	/* Possible race condition, but documented in ctlinnd manpage. */
	HISsetup();
	ok = HISwrite(&Data, av[4]);
	HISclose();
    }
    return ok ? NULL : "1 Write failed";
}


/*
**  Do the work to allow foreign connectiosn.
*/
STATIC STRING
CCallow(av)
    char	*av[];
{
    char	*p;

    if (RejectReason == NULL)
	return "1 Already allowed";
    p = av[0];
    if (*p && !EQ(p, RejectReason))
	return "1 Wrong reason";
    DISPOSE(RejectReason);
    RejectReason = NULL;
    return NULL;
}


/*
**  Do the work needed to start feeding a (new) site.
*/
STATIC STRING
CCbegin(av)
    char		*av[];
{
    SITE		*sp;
    register int	i;
    register int	length;
    register STRING	p;
    register char	**strings;
    register NEWSGROUP	*ngp;
    STRING		error;
    char		*subbed;

    /* If site already exists, drop it. */
    if (SITEfind(av[0]) != NULL && (p = CCdrop(av)) != NULL)
	return p;

    /* Find the named site. */
    length = strlen(av[0]);
    for (strings = SITEreadfile(TRUE), i = 0; (p = strings[i]) != NULL; i++)
	if ((p[length] == NF_FIELD_SEP || p[length] == NF_SUBFIELD_SEP)
	 && caseEQn(p, av[0], length)) {
	    p = COPY(p);
	    break;
	}
    if (p == NULL)
	return CCnosite;

    if (p[0] == 'M' && p[1] == 'E' && p[2] == NF_FIELD_SEP)
	sp = &ME;
    else {
	/* Get space for the new site entry, and space for it in all
	 * the groups. */
	for (i = nSites, sp = Sites; --i >= 0; sp++)
	    if (sp->Name == NULL)
		break;
	if (i < 0) {
	    nSites++;
	    RENEW(Sites, SITE, nSites);
	    sp = &Sites[nSites - 1];
	    for (i = nGroups, ngp = Groups; --i >= 0; ngp++)
		RENEW(ngp->Sites, int, nSites);
	}
	SITElinkall();
    }

    /* Parse. */
    subbed = NEW(char, nGroups);
    error = SITEparseone(p, sp, subbed);
    DISPOSE(subbed);
    if (error != NULL) {
	DISPOSE(p);
	syslog(L_ERROR, "%s bad_newsfeeds %s", av[0], error);
	return "1 Parse error";
    }

    if (sp != &ME && (!SITEsetup(sp) || !SITEfunnelpatch()))
	return "1 Startup error";
    SITEforward(sp, "begin");
    return NULL;
}


/*
**  Common code to change a group's flags.
*/
STATIC STRING
CCdochange(ngp, Rest)
    register NEWSGROUP	*ngp;
    char		*Rest;
{
    int			length;
    char		*p;

    if (ngp->Rest[0] == Rest[0]) {
	length = strlen(Rest);
	if (ngp->Rest[length] == '\n' && EQn(ngp->Rest, Rest, length))
	    return "0 Group status unchanged";
    }
    if (Mode != OMrunning)
	return CCnotrunning;

    p = COPY(ngp->Name);
    if (!ICDchangegroup(ngp, Rest)) {
	syslog(L_NOTICE, "%s cant change_group %s to %s", LogName, p, Rest);
	DISPOSE(p);
	return "1 Change failed (probably can't write active?)";
    }
    syslog(L_NOTICE, "%s change_group %s to %s", LogName, p, Rest);
    DISPOSE(p);
    return NULL;
}


/*
**  Change the mode of a newsgroup.
*/
STATIC STRING
CCchgroup(av)
    char	*av[];
{
    NEWSGROUP	*ngp;
    char	*Rest;

    if ((ngp = NGfind(av[0])) == NULL)
	return CCnogroup;
    Rest = av[1];
    if (Rest[0] != NF_FLAG_ALIAS) {
	Rest[1] = '\0';
	if (CTYPE(isupper, Rest[0]))
	    Rest[0] = tolower(Rest[0]);
    }
    return CCdochange(ngp, Rest);
}


/*
**  Cancel a message.
*/
STRING
CCcancel(av)
    char	*av[];
{
    ARTDATA	Data;
    STRING	p;

    Data.Posted = Now.time;
    Data.Expires = 0;
    Data.Feedsite = "?";
    if ((p = CCgetid(av[0], &Data.MessageID)) != NULL)
	return p;
    if (Mode == OMrunning)
	ARTcancel(&Data, Data.MessageID, TRUE);
    else {
	/* Possible race condition, but documented in ctlinnd manpage. */
	HISsetup();
	ARTcancel(&Data, Data.MessageID, TRUE);
	HISclose();
    }
#if	defined(DO_LOG_CANCEL_COMMANDS)
    syslog(L_NOTICE, "%s cancelled %s", LogName, Data.MessageID);
#endif	/* defined(DO_LOG_CANCEL_COMMANDS) */
    return NULL;
}


/*
**  Syntax-check the newsfeeds file.
*/
/* ARGSUSED */
STRING
CCcheckfile(av)
    char		*av[];
{
    register char	**strings;
    register char	*p;
    register int	i;
    register int	errors;
    STRING		error;
    SITE		fake;

    /* Parse all site entries. */
    strings = SITEreadfile(FALSE);
    fake.Buffer.Size = 0;
    fake.Buffer.Data = NULL;
    for (errors = 0, i = 0; (p = strings[i]) != NULL; i++) {
	if ((error = SITEparseone(p, &fake, (char *)NULL)) != NULL) {
	    syslog(L_ERROR, "%s bad_newsfeeds %s", MaxLength(p, p), error);
	    errors++;
	}
	SITEfree(&fake);
    }
    DISPOSE(strings);

    if (errors == 0)
	return NULL;

    (void)sprintf(CCreply.Data, "1 Found %d errors -- see syslog", errors);
    return CCreply.Data;
}


/*
**  Drop a site.
*/
STATIC STRING
CCdrop(av)
    char		*av[];
{
    SITE		*sp;
    register NEWSGROUP	*ngp;
    register int	*ip;
    register int	idx;
    register int	i;
    register int	j;

    if ((sp = SITEfind(av[0])) == NULL)
	return CCnosite;

    SITEdrop(sp);

    /* Loop over all groups, and if the site is in a group, clobber it. */
    for (idx = sp - Sites, i = nGroups, ngp = Groups; --i >= 0; ngp++)
	for (j = ngp->nSites, ip = ngp->Sites; --j >= 0; ip++)
	    if (*ip == idx)
		*ip = NOSITE;

	return NULL;
}


/*
**  Flush all sites or one site.
*/
STATIC STRING
CCflush(av)
    char		*av[];
{
    register SITE	*sp;
    register int	i;
    register char	*p;

    p = av[0];
    if (*p == '\0') {
	ICDwrite();
	for (sp = Sites, i = nSites; --i >= 0; sp++)
	    SITEflush(sp, TRUE);
	syslog(L_NOTICE, "%s flush_all", LogName);
    }
    else  {
	if ((sp = SITEfind(p)) == NULL)
	    return CCnosite;
	syslog(L_NOTICE, "%s flush", sp->Name);
	SITEflush(sp, TRUE);
    }
    return NULL;
}


/*
**  Flush the log files.
*/
/* ARGSUSED0 */
STATIC STRING
CCflushlogs(av)
    char	*av[];
{
    if (Debug)
	return "1 In debug mode";

    ICDwrite();
    ReopenLog(Log);
    ReopenLog(Errlog);
    return NULL;
}


/*
**  Leave paused or throttled mode.
*/
STATIC STRING
CCgo(av)
    char	*av[];
{
    static char	YES[] = "y";
    char	*p;

    p = av[0];
    if (Reservation && EQ(p, Reservation)) {
	DISPOSE(Reservation);
	Reservation = NULL;
    }
    if (RejectReason && EQ(p, RejectReason)) {
	DISPOSE(RejectReason);
	RejectReason = NULL;
    }

    if (Mode == OMrunning)
	return "1 Already running";
    if (*p && !EQ(p, ModeReason))
	return "1 Wrong reason";
    DISPOSE(ModeReason);
    ModeReason = NULL;
    Mode = OMrunning;

    if (NNRPReason && NNRPFollows) {
	av[0] = YES;
	av[1] = p;
	(void)CCreaders(av);
    }
    if (ErrorCount < 0)
	ErrorCount = IO_ERROR_COUNT;
    HISsetup();
    syslog(L_NOTICE, "%s running", LogName);
    if (ICDneedsetup)
	ICDsetup(TRUE);
    SCHANwakeup((POINTER)&Mode);
    return NULL;
}


/*
**  Hangup a channel.
*/
STATIC STRING
CChangup(av)
    char		*av[];
{
    register CHANNEL	*cp;
    register int	fd;
    register char	*p;
    int			i;

    /* Parse the argument, a channel number. */
    for (p = av[0], fd = 0; *p; p++) {
	if (!CTYPE(isdigit, *p))
	    return "1 Bad channel number";
	fd = fd * 10 + *p - '0';
    }

    /* Loop over all channels for the desired one. */
    for (i = 0; (cp = CHANiter(&i, CTany)) != NULL; )
	if (cp->fd == fd) {
	    p = CHANname(cp);
	    switch (cp->Type) {
	    default:
		(void)sprintf(CCreply.Data, "1 Can't close %s", p);
		return CCreply.Data;
	    case CTexploder:
	    case CTprocess:
	    case CTfile:
	    case CTnntp:
		syslog(L_NOTICE, "%s hangup", p);
		CHANclose(cp, p);
		return NULL;
	    }
	}
    return "1 Not active";
}


/*
**  Return our operating mode.
*/
/* ARGSUSED */
STATIC STRING
CCmode(av)
    char		*av[];
{
    register char	*p;
    register int	i;
    int			h;
    char		buff[BUFSIZ];

    p = buff;
    p += strlen(strcpy(buff, "0 Server "));

    /* Server's mode. */
    switch (Mode) {
    default:
	(void)sprintf(p, "Unknown %d", Mode);
	p += strlen(p);
	break;
    case OMrunning:
	p += strlen(strcpy(p, "running"));
	break;
    case OMpaused:
	p += strlen(strcpy(p, "paused "));
	p += strlen(strcpy(p, ModeReason));
	break;
    case OMthrottled:
	p += strlen(strcpy(p, "throttled "));
	p += strlen(strcpy(p, ModeReason));
	break;
    }
    *p++ = '\n';
    if (RejectReason) {
	p += strlen(strcpy(p, "Rejecting "));
	p += strlen(strcpy(p, RejectReason));
    }
    else
	p += strlen(strcpy(p, "Allowing remote connections"));

    /* Server parameters. */
    for (i = 0, h = 0; CHANiter(&h, CTnntp) != NULL; )
	i++;
    *p++ = '\n';
    (void)sprintf(p, "Parameters c %ld i %d (%d) l %ld o %d t %ld %s %s",
	    (long)Cutoff / (24L * 60L * 60L),
	    MaxIncoming, i,
	    LargestArticle, MaxOutgoing, (long)TimeOut.tv_sec,
	    AmSlave ? "slave" : "normal",
	    AnyIncoming ? "any" : "specified");
    p += strlen(p);

    /* Reservation. */
    *p++ = '\n';
    if (Reservation) {
	(void)sprintf(p, "Reserved %s", Reservation);
	p += strlen(p);
    }
    else
	p += strlen(strcpy(p, "Not reserved"));

    /* Newsreaders. */
    *p++ = '\n';
    p += strlen(strcpy(p, "Readers "));
    if (NNRPFollows)
	p += strlen(strcpy(p, "follow "));
    else
	p += strlen(strcpy(p, "separate "));
    if (NNRPReason == NULL)
	p += strlen(strcpy(p, "enabled"));
    else {
	(void)sprintf(p, "disabled %s", NNRPReason);
	p += strlen(p);
    }
	
    i = strlen(buff);
    if (CCreply.Size <= i) {
	CCreply.Size = i;
	RENEW(CCreply.Data, char, CCreply.Size + 1);
    }
    (void)strcpy(CCreply.Data, buff);
    return CCreply.Data;
}


/*
**  Name the channels.  ("Name the bats -- simple names.")
*/
STATIC STRING
CCname(av)
    char		*av[];
{
    static char		NL[] = "\n";
    static char		NIL[] = "\0";
    register CHANNEL	*cp;
    register char	*p;
    register int	count;
    int			i;

    p = av[0];
    if (*p != '\0') {
	if ((cp = CHANfromdescriptor(atoi(p))) == NULL)
	    return COPY(CCnochannel);
	(void)sprintf(CCreply.Data, "0 %s", CHANname(cp));
	return CCreply.Data;
    }
    BUFFset(&CCreply, "0 ", 2);
    for (count = 0, i = 0; (cp = CHANiter(&i, CTany)) != NULL; ) {
	if (cp->Type == CTfree)
	    continue;
	if (++count > 1)
	    BUFFappend(&CCreply, NL, 1);
	p = CHANname(cp);
	BUFFappend(&CCreply, p, strlen(p));
    }
    BUFFappend(&CCreply, NIL, 1);
    return CCreply.Data;
}


/*
**  Create a newsgroup.
*/
STATIC STRING
CCnewgroup(av)
    char		*av[];
{
    static char		TIMES[] = _PATH_ACTIVETIMES;
    static char		WHEN[] = "updating active.times";
    register int	fd;
    register char	*p;
    NEWSGROUP		*ngp;
    char		*Name;
    char		*Rest;
    STRING		who;
    char		buff[SMBUF];

    Name = av[0];
    if (Name[0] == '.' || strspn(Name, "0123456789") == strlen(Name))
	return "1 Illegal newsgroup name";
    for (p = Name; *p; p++)
	if (*p == '.') {
	    if (p[1] == '.' || p[1] == '\0')
		return "1 Double or trailing period in newsgroup name";
	}
	else if (ISWHITE(*p) || *p == ':' || *p == '!')
	    return "1 Illegal character in newsgroup name";

    Rest = av[1];
    if (Rest[0] != NF_FLAG_ALIAS) {
	Rest[1] = '\0';
	if (CTYPE(isupper, Rest[0]))
	    Rest[0] = tolower(Rest[0]);
    }
    if ((ngp = NGfind(Name)) != NULL)
	return CCdochange(ngp, Rest);

    /* Update the log of groups created.  Don't use stdio because SunOS
     * 4.1 has broken libc which can't handle fd's greater than 127. */
    if ((fd = open(TIMES, O_WRONLY | O_APPEND | O_CREAT, 0664)) < 0) {
	IOError(WHEN);
	syslog(L_ERROR, "%s cant fopen %s %m", LogName, TIMES);
    }
    else {
	who = av[2];
	if (*who == '\0')
	    who = NEWSMASTER;
	(void)sprintf(buff, "%s %ld %s\n", Name, Now.time, who);
	if (xwrite(fd, buff, strlen(buff)) < 0) {
	    IOError(WHEN);
	    syslog(L_ERROR, "%s cant write %s %m", LogName, TIMES);
	}
	if (close(fd) < 0) {
	    IOError(WHEN);
	    syslog(L_ERROR, "%s cant close %s %m", LogName, TIMES);
	}
	if (AmRoot)
	    xchown(TIMES);
    }

    /* Update the in-core data. */
    if (!ICDnewgroup(Name, Rest))
	return "1 Failed";
    syslog(L_NOTICE, "%s newgroup %s as %s", LogName, Name, Rest);

    if (*Rest != NF_FLAG_ALIAS) {
	/* Create the spool directory. */
	for (p = Name; *p; p++)
	    if (*p == '.')
		*p = '/';
	if (!MakeSpoolDirectory(Name))
	    syslog(L_NOTICE, "%s cant mkdir %s %m", LogName, Name);
    }
    return NULL;
}


/*
**  Parse and set a boolean flag.
*/
STATIC BOOL
CCparsebool(name, bp, value)
    char	name;
    BOOL	*bp;
    char	value;
{
    switch (value) {
    default:
	return FALSE;
    case 'y':
	*bp = FALSE;
	break;
    case 'n':
	*bp = TRUE;
	break;
    }
    syslog(L_NOTICE, "%s changed -%c %c", LogName, name, value);
    return TRUE;
}


/*
**  Change a running parameter.
*/
STATIC STRING
CCparam(av)
    char	*av[];
{
    static char	BADVAL[] = "1 Bad value";
    char	*p;

    p = av[1];
    switch (av[0][0]) {
    default:
	return "1 Unknown parameter";
    case 'a':
	if (!CCparsebool('a', &AnyIncoming, *p))
	    return BADVAL;
	break;
    case 'c':
	Cutoff = atoi(p) * 24 * 60 * 60;
	syslog(L_NOTICE, "%s changed -c %d", LogName, Cutoff);
	break;
    case 'i':
	MaxIncoming = atoi(p);
	syslog(L_NOTICE, "%s changed -i %d", LogName, MaxIncoming);
	break;
    case 'l':
	LargestArticle = atol(p);
	syslog(L_NOTICE, "%s changed -l %ld", LogName, LargestArticle);
	break;
    case 'n':
	if (!CCparsebool('n', &NNRPFollows, *p))
	    return BADVAL;
	break;
    case 'o':
	MaxOutgoing = atoi(p);
	syslog(L_NOTICE, "%s changed -o %d", LogName, MaxOutgoing);
	break;
    case 't':
	TimeOut.tv_sec = atol(p);
	syslog(L_NOTICE, "%s changed -t %ld", LogName, (long)TimeOut.tv_sec);
	break;
    }
    return NULL;
}


/*
**  Common code to implement a pause or throttle.
*/
STRING
CCblock(NewMode, reason)
    OPERATINGMODE	NewMode;
    char		*reason;
{
    static char		NO[] = "n";
    STRING		av[2];

    if (*reason == '\0')
	return CCnoreason;

    if (Reservation) {
	if (!EQ(reason, Reservation)) {
	    (void)sprintf(CCreply.Data, "1 Reserved \"%s\"", Reservation);
	    return CCreply.Data;
	}
	DISPOSE(Reservation);
	Reservation = NULL;
    }
    ICDwrite();
    HISclose();
    Mode = NewMode;
    if (ModeReason)
	DISPOSE(ModeReason);
    ModeReason = COPY(reason);
    if (NNRPReason == NULL && NNRPFollows) {
	av[0] = NO;
	av[1] = ModeReason;
	(void)CCreaders(av);
    }
    syslog(L_NOTICE, "%s %s %s",
	LogName, NewMode == OMpaused ? "paused" : "throttled", reason);
    return NULL;
}


/*
**  Enter paused mode.
*/
STATIC STRING
CCpause(av)
    char	*av[];
{
    switch (Mode) {
    case OMrunning:
	return CCblock(OMpaused, av[0]);
    case OMpaused:
	return "1 Already paused";
    case OMthrottled:
	return "1 Already throttled";
    }
    return "1 Unknown mode";
}


/*
**  Allow or disallow newsreaders.
*/
STATIC STRING
CCreaders(av)
    char	*av[];
{
    char	*p;

    switch (av[0][0]) {
    default:
	return "1 Bad flag";
    case 'y':
	if (NNRPReason == NULL)
	    return "1 Already allowing readers";
	p = av[1];
	if (*p && !EQ(p, NNRPReason))
	    return "1 Wrong reason";
	DISPOSE(NNRPReason);
	NNRPReason = NULL;
	break;
    case 'n':
	if (NNRPReason)
	    return "1 Already not allowing readers";
	p = av[1];
	if (*p == '\0')
	    return CCnoreason;
	NNRPReason = COPY(p);
	break;
    }
    return NULL;
}


/*
**  Re-exec ourselves.
*/
STATIC STRING
CCxexec(av)
    char	*av[];
{
    static char	INND[] = _PATH_INND;
    static char	INNDSTART[] = _PATH_INNDSTART;
    char	*p;

    if (CCargv == NULL)
	return "1 no argv!";
    
    /* Get the pathname. */
    p = av[0];
    if (*p == '\0')
	CCargv[0] = AmRoot ? INND : INNDSTART;
    else if (EQ(p, "innd"))
	CCargv[0] = INND;
    else if (EQ(p, "inndstart"))
	CCargv[0] = INNDSTART;
    else
	return "1 Bad value";

    JustCleanup();
    syslog(L_FATAL, "%s execv %s", LogName, CCargv[0]);
    (void)execv(CCargv[0], CCargv);
    syslog(L_FATAL, "%s cant execv %s %m", LogName, CCargv[0]);
    _exit(1);
    /* NOTREACHED */
}


/*
**  Refile an article.
*/
STATIC STRING
CCrefile(av)
    char	*av[];
{
    char	*head;

    /* xxx multiple groups? */
    if (NGfind(av[1]) == NULL)
	return CCnogroup;

    head = ARTreadheader(av[0]);
    if (head == NULL)
	return "1 No such article";

    return "1 Not yet implemented";
}


/*
**  Reject remote readers.
*/
STATIC STRING
CCreject(av)
    char	*av[];
{
    if (RejectReason)
	return "1 Already rejecting";
    RejectReason = COPY(av[0]);
    return NULL;
}


/*
**  Re-read all in-core data.
*/
STATIC STRING
CCreload(av)
    char	*av[];
{
    static char	BADSCHEMA[] = "1 Can't read schema";
    STRING	p;

    p = av[0];
    if (*p == '\0' || EQ(p, "all")) {
	SITEflushall(FALSE);
	HISclose();
	RCreadlist();
	HISsetup();
	ICDwrite();
	ICDsetup(TRUE);
	if (!ARTreadschema())
	    return BADSCHEMA;
	p = "all";
    }
    else if (EQ(p, "active") || EQ(p, "newsfeeds")) {
	SITEflushall(FALSE);
	ICDwrite();
	ICDsetup(TRUE);
    }
    else if (EQ(p, "history")) {
	HISclose();
	HISsetup();
    }
    else if (EQ(p, "hosts.nntp"))
	RCreadlist();
    else if (EQ(p, "overview.fmt")) {
	if (!ARTreadschema())
	    return BADSCHEMA;
    }
    else
	return "1 Unknown reload type";

    syslog(L_NOTICE, "%s reload %s %s", LogName, p, av[1]);
    return NULL;
}


/*
**  Renumber the active file.
*/
STATIC STRING
CCrenumber(av)
    char	*av[];
{
    char	*p;
    NEWSGROUP	*ngp;

    if (Mode != OMrunning)
	return CCnotrunning;
    if (ICDneedsetup)
	return "1 Must first reload newsfeeds";
    p = av[0];
    if (*p) {
	if ((ngp = NGfind(p)) == NULL)
	    return CCnogroup;
	if (!NGrenumber(ngp))
	    return "1 Failed (see syslog)";
    }
    else
	ICDrenumberactive();
    return NULL;
}


/*
**  Reserve a lock.
*/
STATIC STRING
CCreserve(av)
    char	*av[];
{
    char	*p;

    if (Mode != OMrunning)
	return CCnotrunning;
    p = av[0];
    if (*p) {
	/* Trying to make a reservation. */
	if (Reservation)
	    return "1 Already reserved";
	Reservation = COPY(p);
    }
    else {
	/* Trying to remove a reservation. */
	if (Reservation == NULL)
	    return "1 Not reserved";
	DISPOSE(Reservation);
	Reservation = NULL;
    }
    return NULL;
}


/*
**  Remove a newsgroup.
*/
STATIC STRING
CCrmgroup(av)
    char	*av[];
{
    NEWSGROUP	*ngp;

    if ((ngp = NGfind(av[0])) == NULL)
	return CCnogroup;

    /* Update the in-core data. */
    if (!ICDrmgroup(ngp))
	return "1 Failed";
    syslog(L_NOTICE, "%s rmgroup %s", LogName, av[0]);
    return NULL;
}


/*
**  Send a command line to an exploder.
*/
STATIC STRING
CCsend(av)
    char		*av[];
{
    SITE		*sp;

    if ((sp = SITEfind(av[0])) == NULL)
	return CCnosite;
    if (sp->Type != FTexploder)
	return CCwrongtype;
    SITEwrite(sp, av[1]);
    return NULL;
}


/*
**  Shut down the system.
*/
STATIC STRING
CCshutdown(av)
    char	*av[];
{
    syslog(L_NOTICE, "%s shutdown %s", LogName, av[0]);
    CleanupAndExit(0, av[0]);
    /* NOTREACHED */
}


/*
**  Send a signal to a site's feed.
*/
STATIC STRING
CCsignal(av)
    char	*av[];
{
    register SITE	*sp;
    register char	*p;
    int			s;
    int			oerrno;

    /* Parse the signal. */
    p = av[0];
    if (*p == '-')
	p++;
    if (caseEQ(p, "HUP"))
	s = SIGHUP;
    else if (caseEQ(p, "INT"))
	s = SIGINT;
    else if (caseEQ(p, "TERM"))
	s = SIGTERM;
    else if ((s = atoi(p)) <= 0)
	return "1 Invalid signal";

    /* Parse the site. */
    p = av[1];
    if ((sp = SITEfind(p)) == NULL)
	return CCnosite;
    if (sp->Type != FTchannel && sp->Type != FTexploder)
	return CCwrongtype;
    if (sp->Spooling || sp->Process  < 0)
	return "1 Site has no process";

    /* Do it. */
    if (kill(sp->pid, s) < 0) {
	oerrno = errno;
	syslog(L_ERROR, "%s cant kill %d %d site %s, %m", sp->Process, s, p);
	(void)sprintf(CCreply.Data, "1 Can't signal process %d, %s",
		sp->Process, strerror(oerrno));
	return CCreply.Data;
    }

    return NULL;
}


/*
**  Enter throttled mode.
*/
STATIC STRING
CCthrottle(av)
    char	*av[];
{
    char	*p;

    p = av[0];
    switch (Mode) {
    case OMpaused:
	if (*p && !EQ(p, ModeReason))
	    return "1 Already paused";
	/* FALLTHROUGH */
    case OMrunning:
	return CCblock(OMthrottled, p);
    case OMthrottled:
	return "1 Already throttled";
    }
    return "1 unknown mode";
}


/*
**  Add or remove tracing.
*/
STATIC STRING
CCtrace(av)
    char	*av[];
{
    char	*p;
    BOOL	Flag;
    STRING	word;
    CHANNEL	*cp;

    /* Parse the flag. */
    p = av[1];
    switch (p[0]) {
    default:			return "1 Bad trace flag";
    case 'y': case 'Y':		Flag = TRUE;	word = "on";	break;
    case 'n': case 'N':		Flag = FALSE;	word = "off";	break;
    }

    /* Parse what's being traced. */
    p = av[0];
    switch (*p) {
    default:
	return "1 Bad trace item";
    case 'i': case 'I':
	Tracing = Flag;
	syslog(L_NOTICE, "%s trace innd %s", LogName, word);
	break;
    case 'n': case 'N':
	NNRPTracing = Flag;
	syslog(L_NOTICE, "%s trace nnrpd %s", LogName, word);
	break;
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
	if ((cp = CHANfromdescriptor(atoi(p))) == NULL)
	    return CCnochannel;
	CHANtracing(cp, Flag);
	break;
    }
    return NULL;
}



/*
**  Split up the text into fields and stuff them in argv.  Return the
**  number of elements or -1 on error.
*/
STATIC int
CCargsplit(p, end, argv, size)
    register char	*p;
    register char	*end;
    register char	**argv;
    register int	size;
{
    char		**save;

    for (save = argv, *argv++ = p, size--; p < end; p++)
	if (*p == SC_SEP) {
	    if (--size <= 0)
		return -1;
	    *p = '\0';
	    *argv++ = p + 1;
	}
    *argv = NULL;
    return argv - save;
}


/*
**  Read function.  Read and process the message.
*/
STATIC FUNCTYPE
CCreader(cp)
    CHANNEL		*cp;
{
    static char		TOOLONG[] = "0 Reply too long for server to send";
    register CCDISPATCH	*dp;
    register STRING	p;
    register char	*q;
#if	defined(DO_HAVE_UNIX_DOMAIN)
    struct sockaddr_un	client;
#else
    int			written;
#endif	/* defined(DO_HAVE_UNIX_DOMAIN) */
    int			i;
    char		buff[BUFSIZ + 2];
    char		copy[BUFSIZ + 2];
    char		*argv[SC_MAXFIELDS + 2];
    int			argc;
    int			len;

    if (cp != CCchan) {
	syslog(L_ERROR, "%s internal CCreader wrong channel 0x%x not 0x%x",
	    LogName, cp, CCchan);
	return;
    }

    /* Get the message. */
    i = RECVorREAD(CCchan->fd, buff, sizeof buff - 1);
    if (i < 0) {
	syslog(L_ERROR, "%s cant recv CCreader %m", LogName);
	return;
    }
    if (i == 0) {
	syslog(L_ERROR, "%s cant recv CCreader empty", LogName);
	return;
    }
    buff[i] = '\0';

    /* Copy to a printable buffer, and log. */
    (void)strcpy(copy, buff);
    for (p = NULL, q = copy; *q; q++)
	if (*q == SC_SEP) {
	    *q = ':';
	    if (p == NULL)
		p = q + 1;
	}
    syslog(L_CC_CMD, "%s", p ? p : copy);

    /* Split up the fields, get the command letter. */
    if ((argc = CCargsplit(buff, &buff[i], argv, SIZEOF(argv))) < 2
     || argc == SIZEOF(argv)) {
	syslog(L_ERROR, "%s bad_fields CCreader", LogName);
	return;
    }
    p = argv[1];
    i = *p;

    /* Dispatch to the command function. */
    for (argc -= 2, dp = CCcommands; dp < ENDOF(CCcommands); dp++)
	if (i == dp->Name) {
	    if (argc != dp->argc)
		p = "1 Wrong number of parameters";
	    else
		p = (*dp->Function)(&argv[2]);
	    break;
	}
    if (dp == ENDOF(CCcommands)) {
	syslog(L_NOTICE, "%s bad_message %c", LogName, i);
	p = "1 Bad command";
    }
    else if (p == NULL)
	p = "0 Ok";

    /* Build the reply address and send the reply. */
    len = strlen(p);

#if	defined(DO_HAVE_UNIX_DOMAIN)
    (void)memset((POINTER)&client, 0, sizeof client);
    client.sun_family = AF_UNIX;
    (void)strcpy(client.sun_path, argv[0]);
    if (sendto(CCwriter, p, len, 0,
	    (struct sockaddr *)&client, AF_UNIX_SOCKSIZE(client)) < 0) {
	i = errno;
	syslog(i == ENOENT ? L_NOTICE : L_ERROR,
	    "%s cant sendto CCreader bytes %d %m", LogName, len);
	if (i == EMSGSIZE)
	    (void)sendto(CCwriter, TOOLONG, STRLEN(TOOLONG), 0,
		(struct sockaddr *)&client, AF_UNIX_SOCKSIZE(client));
    }
#else
    if ((i = open(argv[0], O_WRONLY)) < 0)
	syslog(L_ERROR, "%s cant open %s %m", LogName, argv[0]);
    else {
	if ((written = write(i, p, len)) != len)
	    if (written < 0)
		syslog(L_ERROR, "%s cant write %s %m", LogName, argv[0]);
	    else
		syslog(L_ERROR, "%s cant write %s", LogName, argv[0]);
	if (close(i) < 0)
	    syslog(L_ERROR, "%s cant close %s %m", LogName, argv[0]);
    }
#endif	/* defined(DO_HAVE_UNIX_DOMAIN) */
}


/*
**  Called when a write-in-progress is done on the channel.  Shouldn't happen.
*/
STATIC FUNCTYPE
CCwritedone()
{
    syslog(L_ERROR, "%s internal CCwritedone", LogName);
}


/*
**  Create the channel.
*/
void
CCsetup()
{
    int			i;
#if	defined(DO_HAVE_UNIX_DOMAIN)
    struct sockaddr_un	server;
#endif	/* defined(DO_HAVE_UNIX_DOMAIN) */

    /* Remove old detritus. */
    if (unlink(CCpath) < 0 && errno != ENOENT) {
	syslog(L_FATAL, "%s cant unlink %s %m", LogName, CCpath);
	exit(1);
    }

#if	defined(DO_HAVE_UNIX_DOMAIN)
    /* Create a socket and name it. */
    if ((i = socket(AF_UNIX, SOCK_DGRAM, 0)) < 0) {
	syslog(L_FATAL, "%s cant socket %s %m", LogName, CCpath);
	exit(1);
    }
    (void)memset((POINTER)&server, 0, sizeof server);
    server.sun_family = AF_UNIX;
    (void)strcpy(server.sun_path, CCpath);
    if (bind(i, (struct sockaddr *)&server, AF_UNIX_SOCKSIZE(server)) < 0) {
	syslog(L_FATAL, "%s cant bind %s %m", LogName, CCpath);
	exit(1);
    }

    /* Create an unbound socket to reply on. */
    if ((CCwriter = socket(AF_UNIX, SOCK_DGRAM, 0)) < 0) {
	syslog(L_FATAL, "%s cant socket unbound %m", LogName);
	exit(1);
    }
#else
    /* Create a named pipe and open it. */
    if (mkfifo(CCpath, 0666) < 0) {
	syslog(L_FATAL, "%s cant mkfifo %s %m", LogName, CCpath);
	exit(1);
    }
    if ((i = open(CCpath, O_RDWR)) < 0) {
	syslog(L_FATAL, "%s cant open %s %m", LogName, CCpath);
	exit(1);
    }
#endif	/* defined(DO_HAVE_UNIX_DOMAIN) */

    CCchan = CHANcreate(i, CTcontrol, CSwaiting, CCreader, CCwritedone);
    syslog(L_NOTICE, "%s ccsetup %s", LogName, CHANname(CCchan));
    RCHANadd(CCchan);

    CCreply.Size = SMBUF;
    CCreply.Data = NEW(char, CCreply.Size);
}


/*
**  Cleanly shut down the channel.
*/
void
CCclose()
{
    CHANclose(CCchan, CHANname(CCchan));
    CCchan = NULL;
    if (unlink(CCpath) < 0)
	syslog(L_ERROR, "%s cant unlink %s %m", LogName, CCpath);
#if	defined(DO_HAVE_UNIX_DOMAIN)
    if (close(CCwriter) < 0)
	syslog(L_ERROR, "%s cant close unbound %m", LogName);
#endif	/* defined(DO_HAVE_UNIX_DOMAIN) */
}
