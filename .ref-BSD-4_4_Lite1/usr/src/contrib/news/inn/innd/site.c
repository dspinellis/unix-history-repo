/*  $Revision: 1.34 $
**
**  Routines to implement site-feeding.  Mainly working with channels to
**  do buffering and determine what to send.
*/
#include "innd.h"


#define SITEmovetohead(sp_) \
    if (SITEhead != sp_) { \
	if (SITEtail == sp_) { \
	    SITEtail = sp_->Prev; \
	    SITEtail->Next = NULL; \
	} \
	else { \
	    if (sp_->Prev) \
		sp_->Prev->Next = sp_->Next; \
	    if (sp_->Next) \
		sp_->Next->Prev = sp_->Prev; \
	} \
	sp_->Prev = NULL; \
	sp_->Next = SITEhead->Next; \
	SITEhead->Prev = sp_; \
	SITEhead = sp_; \
    } \
    else

STATIC int	SITEcount;
STATIC SITE	*SITEhead;
STATIC SITE	*SITEtail;
STATIC char	SITEshell[] = _PATH_SH;


/*
**  Called when input is ready to read.  Shouldn't happen.
*/
/* ARGSUSED0 */
STATIC FUNCTYPE
SITEreader(cp)
    CHANNEL	*cp;
{
    syslog(L_ERROR, "%s internal SITEreader", LogName);
}


/*
**  Called when write is done.  No-op.
*/
/* ARGSUSED0 */
STATIC FUNCTYPE
SITEwritedone(cp)
    CHANNEL	*cp;
{
}


/*
**  Make a site start spooling.
*/
STATIC BOOL
SITEspool(sp, cp)
    register SITE	*sp;
    CHANNEL		*cp;
{
    int			i;
    char		buff[SPOOLNAMEBUFF];
    char		*name;

    name = sp->SpoolName;
    i = open(name, O_APPEND | O_CREAT | O_WRONLY, BATCHFILE_MODE);
    if (i < 0 && errno == EISDIR) {
	FileGlue(buff, sp->SpoolName, '/', "togo");
	name = buff;
	i = open(buff, O_APPEND | O_CREAT | O_WRONLY, BATCHFILE_MODE);
    }
    if (i < 0) {
	IOError("site batch file");
	syslog(L_ERROR, "%s cant open %s %m", sp->Name, name);
	sp->Channel = NULL;
	return FALSE;
    }
    if (AmRoot)
	xchown(name);
    if (cp) {
	cp->fd = i;
	return TRUE;
    }
    sp->Channel = CHANcreate(i, CTfile, CSwriting, SITEreader, SITEwritedone);
    if (sp->Channel == NULL) {
	syslog(L_ERROR, "%s cant channel %m", sp->Name);
	(void)close(i);
	return FALSE;
    }
    WCHANset(sp->Channel, "", 0);
    sp->Spooling = TRUE;
    sp->Process = -1;
    return TRUE;
}


/*
**  Find the oldest "file feed" site and buffer it.
*/
STATIC void
SITEbufferoldest()
{
    register SITE	*sp;
    register BUFFER	*bp;
    register BUFFER	*out;

    /* Go backwards and find the oldest file. */
    for (sp = SITEtail; sp; sp = sp->Prev)
	if (sp->Type == FTfile)
	    break;
    if (sp == NULL) {
	syslog(L_ERROR, "%s internal no oldest site found", LogName);
	return;
    }

    /* Write out what we can. */
    (void)WCHANflush(sp->Channel);

    /* Get a buffer for the site. */
    sp->Buffered = TRUE;
    bp = &sp->Buffer;
    bp->Used = 0;
    bp->Left = 0;
    if (bp->Size == 0) {
	bp->Size = sp->Flushpoint;
	bp->Data = NEW(char, bp->Size);
    }
    else {
	bp->Size = sp->Flushpoint;
	RENEW(bp->Data, char, bp->Size);
    }

    /* If there's any unwritten data, copy it. */
    out = &sp->Channel->Out;
    if (out->Left)
	BUFFset(bp, &out->Data[out->Used], out->Left);

    /* Now close the original channel. */
    CHANclose(sp->Channel, sp->Name);
    SITEcount--;
}


/*
**  Check if we need to write out the site's buffer.  If we're buffered
**  or the feed is backed up, this gets a bit complicated.
*/
STATIC void
SITEflushcheck(sp, bp)
    register SITE	*sp;
    register BUFFER	*bp;
{
    register int	i;
    register CHANNEL	*cp;

    /* If we're buffered, and we hit the flushpoint, do an LRU. */
    if (sp->Buffered) {
	if (bp->Used < sp->Flushpoint)
	    return;
	if (SITEcount > MaxOutgoing)
	    SITEbufferoldest();
	if (!SITEsetup(sp) || sp->Buffered) {
	    syslog(L_ERROR, "%s cant unbuffer %m", sp->Name);
	    return;
	}
	WCHANsetfrombuffer(sp->Channel, bp);
	WCHANadd(sp->Channel);
    }

    if (PROCneedscan)
	PROCscan();

    /* Handle buffering. */
    cp = sp->Channel;
    i = cp->Out.Left;
    if (i < sp->StopWriting)
	WCHANremove(cp);
    if ((sp->StartWriting == 0 || i > sp->StartWriting)
     && !CHANsleeping(cp))
	WCHANadd(cp);

    /* If we're a channel that's getting big, see if we need to spool. */
    if (sp->Type == FTfile || sp->StartSpooling == 0 || i < sp->StartSpooling)
	return;

    if (!SITEspool(sp, (CHANNEL *)NULL)) {
	syslog(L_ERROR, "%s overflow %d bytes", sp->Name, i);
	return;
    }
    syslog(L_ERROR, "%s spooling %d bytes", sp->Name, i);
    WCHANsetfrombuffer(sp->Channel, &cp->Out);
    WCHANadd(sp->Channel);
    CHANclose(cp, CHANname(cp));
}


/*
**  Send a control line to an exploder.
*/
void
SITEwrite(sp, text)
    register SITE	*sp;
    STRING		text;
{
    static char		PREFIX[] = { EXP_CONTROL, '\0' };
    register BUFFER	*bp;

    if (sp->Buffered)
	bp = &sp->Buffer;
    else {
	if (sp->Channel == NULL)
	    return;
	bp = &sp->Channel->Out;
    }
    BUFFappend(bp, PREFIX, STRLEN(PREFIX));
    BUFFappend(bp, text, (int)strlen(text));
    BUFFappend(bp, "\n", 1);
    WCHANadd(sp->Channel);
}


/*
**  Send the desired data about an article down a channel.
*/
STATIC void
SITEwritefromflags(sp, Data)
    register SITE	*sp;
    ARTDATA		*Data;
{
    static char		ITEMSEP[] = " ";
    static char		NL[] = "\n";
    register char	*p;
    register BOOL	Dirty;
    register BUFFER	*bp;
    register SITE	*spx;
    register int	i;

    if (sp->Buffered)
	bp = &sp->Buffer;
    else {
	/* This should not happen, but if we tried to spool and failed,
	 * e.g., because of a bad F param for this site, we can get
	 * into this state.  We already logged a message so give up. */
	if (sp->Channel == NULL)
	    return;
	bp = &sp->Channel->Out;
    }
    for (Dirty = FALSE, p = sp->FileFlags; *p; p++) {
	switch (*p) {
	default:
	    syslog(L_ERROR, "%s internal SITEwritefromflags %c", sp->Name, p);
	    continue;
	case FEED_BYTESIZE:
	    if (Dirty)
		BUFFappend(bp, ITEMSEP, STRLEN(ITEMSEP));
	    BUFFappend(bp, Data->Size, Data->SizeLength);
	    break;
	case FEED_FULLNAME:
	    if (Dirty)
		BUFFappend(bp, ITEMSEP, STRLEN(ITEMSEP));
	    BUFFappend(bp, SPOOL, SPOOLlen);
	    BUFFappend(bp, "/", 1);
	    BUFFappend(bp, Data->Name, Data->NameLength);
	    break;
	case FEED_HDR_DISTRIB:
	    if (Dirty)
		BUFFappend(bp, ITEMSEP, STRLEN(ITEMSEP));
	    BUFFappend(bp, Data->Distribution, Data->DistributionLength);
	    break;
	case FEED_HDR_NEWSGROUP:
	    if (Dirty)
		BUFFappend(bp, ITEMSEP, STRLEN(ITEMSEP));
	    BUFFappend(bp, Data->Newsgroups, Data->NewsgroupsLength);
	    break;
	case FEED_HEADERS:
	    if (Dirty)
		BUFFappend(bp, NL, STRLEN(NL));
	    BUFFappend(bp, Data->Headers->Data, Data->Headers->Left);
	    break;
	case FEED_OVERVIEW:
	    if (Dirty)
		BUFFappend(bp, ITEMSEP, STRLEN(ITEMSEP));
	    BUFFappend(bp, Data->Overview->Data, Data->Overview->Left);
	    break;
	case FEED_REPLIC:
	    if (Dirty)
		BUFFappend(bp, ITEMSEP, STRLEN(ITEMSEP));
	    BUFFappend(bp, Data->Replic, Data->ReplicLength);
	    break;
	case FEED_TIMERECEIVED:
	    if (Dirty)
		BUFFappend(bp, ITEMSEP, STRLEN(ITEMSEP));
	    BUFFappend(bp, Data->TimeReceived, Data->TimeReceivedLength);
	    break;
	case FEED_MESSAGEID:
	    if (Dirty)
		BUFFappend(bp, ITEMSEP, STRLEN(ITEMSEP));
	    BUFFappend(bp, Data->MessageID, Data->MessageIDLength);
	    break;
	case FEED_FNLNAMES:
	    if (sp->FNLnames.Data) {
		/* Funnel; write names of our sites that got it. */
		if (Dirty)
		    BUFFappend(bp, ITEMSEP, STRLEN(ITEMSEP));
		BUFFappend(bp, sp->FNLnames.Data, sp->FNLnames.Used);
	    }
	    else {
		/* Not funnel; write names of all sites that got it. */
		for (spx = Sites, i = nSites; --i >= 0; spx++)
		    if (spx->Sendit) {
			if (Dirty)
			    BUFFappend(bp, ITEMSEP, STRLEN(ITEMSEP));
			BUFFappend(bp, spx->Name, spx->NameLength);
			Dirty = TRUE;
		    }
	    }
	    break;
	case FEED_NAME:
	    if (Dirty)
		BUFFappend(bp, ITEMSEP, STRLEN(ITEMSEP));
	    BUFFappend(bp, Data->Name, Data->NameLength);
	    break;
	case FEED_NEWSGROUP:
	    if (Dirty)
		BUFFappend(bp, ITEMSEP, STRLEN(ITEMSEP));
	    if (sp->ng)
		BUFFappend(bp, sp->ng->Name, sp->ng->NameLength);
	    else
		BUFFappend(bp, "?", 1);
	    break;
	case FEED_SITE:
	    if (Dirty)
		BUFFappend(bp, ITEMSEP, STRLEN(ITEMSEP));
	    BUFFappend(bp, Data->Feedsite, Data->FeedsiteLength);
	    break;
	}
	Dirty = TRUE;
    }
    if (Dirty) {
	BUFFappend(bp, "\n", 1);
	SITEflushcheck(sp, bp);
    }
}


/*
**  Send one article to a site.
*/
void
SITEsend(sp, Data)
    register SITE	*sp;
    ARTDATA		*Data;
{
    register int	i;
    register char	*p;
    char		*temp;
    char		buff[BUFSIZ];
    STRING		argv[MAX_BUILTIN_ARGV];
    int			fd;

    switch (sp->Type) {
    default:
	syslog(L_ERROR, "%s internal SITEsend type %d", sp->Name, sp->Type);
	break;
    case FTlogonly:
	break;
    case FTfunnel:
	syslog(L_ERROR, "%s funnel_send", sp->Name);
	break;
    case FTfile:
	if (SITEcount > MaxOutgoing)
	    SITEmovetohead(sp);
	/* FALLTHROUGH */
    case FTchannel:
    case FTexploder:
	SITEwritefromflags(sp, Data);
	break;
    case FTprogram:
	/* Set up the argument vector. */
	if (sp->FNLwantsnames) {
	    i = strlen(sp->Param) + sp->FNLnames.Used;
	    if (i + strlen(Data->Name) >= sizeof buff) {
		syslog(L_ERROR, "%s toolong need %d for %s",
		    sp->Name, i + strlen(Data->Name), Data->Name);
		break;
	    }
	    temp = NEW(char, i + 1);
	    p = strchr(sp->Param, '*');
	    *p = '\0';
	    (void)strcpy(temp, sp->Param);
	    (void)strcat(temp, sp->FNLnames.Data);
	    (void)strcat(temp, &p[1]);
	    *p = '*';
	    (void)sprintf(buff, temp, Data->Name);
	    DISPOSE(temp);
	}
	else
	    (void)sprintf(buff, sp->Param, Data->Name);

	if (NeedShell(buff, argv, ENDOF(argv))) {
	    argv[0] = SITEshell;
	    argv[1] = "-c";
	    argv[2] = buff;
	    argv[3] = NULL;
	}

	/* Feed the article on standard input. */
	fd = open(Data->Name, O_RDONLY);
	if (fd < 0) {
	    /* Unlikely, but we could check if the article is cross-posted
	     * and try it under other names... */
	    syslog(L_ERROR, "%s cant open %s %m", sp->Name, Data->Name);
	    fd = 0;
	}

	/* Start the process. */
	i = Spawn(fd, (int)fileno(Errlog), (int)fileno(Errlog), argv);
	if (i >= 0)
	    (void)PROCwatch(i, -1);
	if (fd != 0)
	    (void)close(fd);
	break;
    }
}


/*
**  The channel was sleeping because we had to spool our output to
**  a file.  Flush and restart.
*/
STATIC FUNCTYPE
SITEspoolwake(cp)
    CHANNEL	*cp;
{
    SITE	*sp;
    int		*ip;

    ip = CAST(int*, cp->Argument);
    sp = &Sites[*ip];
    DISPOSE(cp->Argument);
    cp->Argument = NULL;
    if (sp->Channel != cp) {
	syslog(L_ERROR, "%s internal SITEspoolwake %s got %d, not %d",
	    LogName, sp->Name, cp->fd, sp->Channel->fd);
        return;
    }
    syslog(L_NOTICE, "%s spoolwake", sp->Name);
    SITEflush(sp, TRUE);
}


/*
**  Start up a process for a channel, or a spool to a file if we can't.
**  Create a channel for the site to talk to.
*/
STATIC BOOL
SITEstartprocess(sp)
    SITE		*sp;
{
    register int	i;
    STRING		argv[MAX_BUILTIN_ARGV];
    char		*process;
    int			*ip;
    int			pan[2];

    /* Create a pipe. */
    if (pipe(pan) < 0) {
	syslog(L_ERROR, "%s cant pipe %m", sp->Name);
	return FALSE;
    }
    CloseOnExec(pan[PIPE_WRITE], TRUE);

    /* Set up the argument vector. */
    process = COPY(sp->Param);
    if (NeedShell(process, argv, ENDOF(argv))) {
	argv[0] = SITEshell;
	argv[1] = "-c";
	argv[2] = process;
	argv[3] = NULL;
    }

    /* Fork a child. */
    i = Spawn(pan[PIPE_READ], (int)fileno(Errlog), (int)fileno(Errlog), argv);
    if (i > 0) {
	sp->pid = i;
	sp->Spooling = FALSE;
	sp->Process = PROCwatch(i, sp - Sites);
	(void)close(pan[PIPE_READ]);
	sp->Channel = CHANcreate(pan[PIPE_WRITE],
			sp->Type == FTchannel ? CTprocess : CTexploder,
			CSwriting, SITEreader, SITEwritedone);
	DISPOSE(process);
	return TRUE;
    }
    DISPOSE(process);

    /* Error.  Switch to spooling. */
    syslog(L_ERROR, "%s spooling", sp->Name);
    (void)close(pan[PIPE_WRITE]);
    (void)close(pan[PIPE_READ]);
    if (!SITEspool(sp, (CHANNEL *)NULL))
	return FALSE;

    /* We'll try to restart the channel later. */
    syslog(L_ERROR, "%s cant spawn spooling %m", sp->Name);
    ip = NEW(int, 1);
    *ip = sp - Sites;
    SCHANadd(sp->Channel, (time_t)(Now.time + CHANNEL_RETRY_TIME),
	(POINTER)NULL, SITEspoolwake, (POINTER)ip);
    return TRUE;
}


/*
**  Set up a site for internal buffering.
*/
STATIC void
SITEbuffer(sp)
    register SITE	*sp;
{
    register BUFFER	*bp;

    sp->Buffered = TRUE;
    sp->Channel = NULL;
    bp = &sp->Buffer;
    if (bp->Size == 0) {
	bp->Size = sp->Flushpoint;
	bp->Data = NEW(char, bp->Size);
    }
    else {
	bp->Size = sp->Flushpoint;
	RENEW(bp->Data, char, bp->Size);
    }
    BUFFset(bp, "", 0);
    syslog(L_NOTICE, "%s buffered", sp->Name);
}


/*
**  Set up a site's feed.  This means opening a file or channel if needed.
*/
BOOL
SITEsetup(sp)
    register SITE	*sp;
{
    int			fd;

    switch (sp->Type) {
    default:
	syslog(L_ERROR, "%s internal SITEsetup %d",
	    sp->Name, sp->Type);
	return FALSE;
    case FTfunnel:
    case FTlogonly:
    case FTprogram:
	/* Nothing to do here. */
	break;
    case FTfile:
	SITEcount++;
	if (SITEcount > MaxOutgoing)
	    SITEbuffer(sp);
	else {
	    sp->Buffered = FALSE;
	    fd = open(sp->Param, O_APPEND | O_CREAT | O_WRONLY, BATCHFILE_MODE);
	    if (fd < 0) {
		if (errno == EMFILE) {
		    SITEbuffer(sp);
		    break;
		}
		IOError("site file");
		syslog(L_NOTICE, "%s cant open %s %m", sp->Name, sp->Param);
		return FALSE;
	    }
	    if (AmRoot)
		xchown(sp->Param);
	    sp->Channel = CHANcreate(fd, CTfile, CSwriting,
			    SITEreader, SITEwritedone);
	    syslog(L_NOTICE, "%s opened %s", sp->Name, CHANname(sp->Channel));
	    WCHANset(sp->Channel, "", 0);
	}
	break;
    case FTchannel:
    case FTexploder:
	if (!SITEstartprocess(sp))
	    return FALSE;
	syslog(L_NOTICE, "%s spawned %s", sp->Name, CHANname(sp->Channel));
	WCHANset(sp->Channel, "", 0);
	WCHANadd(sp->Channel);
	break;
    }
    return TRUE;
}


/*
**  A site's channel process died; restart it.
*/
void
SITEprocdied(sp, process, pp)
    SITE	*sp;
    int		process;
    PROCESS	*pp;
{
    syslog(pp->Status ? L_ERROR : L_NOTICE, "%s exit %d elapsed %ld pid %ld",
	sp->Name ? sp->Name : "?", pp->Status,
	(pp->Collected - pp->Started) / 60L, (long)pp->Pid);
    if (sp->Process != process || sp->Name == NULL)
	/* We already started a new process for this channel
	 * or this site has been dropped. */
	return;
    if (sp->Channel != NULL)
	CHANclose(sp->Channel, CHANname(sp->Channel));
    sp->Working = SITEsetup(sp);
    if (!sp->Working) {
	syslog(L_ERROR, "%s cant restart %m", sp->Name);
	return;
    }
    syslog(L_NOTICE, "%s restarted", sp->Name);
}

/*
**  A channel is about to be closed; see if any site cares.
*/
void
SITEchanclose(cp)
    register CHANNEL	*cp;
{
    register int	i;
    register SITE	*sp;
    int			*ip;

    for (i = nSites, sp = Sites; --i >= 0; sp++)
	if (sp->Channel == cp) {
	    /* Found the site that has this channel.  Start that
	     * site spooling, copy any data that might be pending,
	     * and arrange to retry later. */
	    if (!SITEspool(sp, (CHANNEL *)NULL)) {
		syslog(L_ERROR, "%s loss %d bytes", sp->Name, i);
		return;
	    }
	    WCHANsetfrombuffer(sp->Channel, &cp->Out);
	    WCHANadd(sp->Channel);
	    ip = NEW(int, 1);
	    *ip = sp - Sites;
	    SCHANadd(sp->Channel, (time_t)(Now.time + CHANNEL_RETRY_TIME),
		(POINTER)NULL, SITEspoolwake, (POINTER)ip);
	    break;
	}
}


/*
**  Flush any pending data waiting to be sent.
*/
void
SITEflush(sp, Restart)
    register SITE	*sp;
    BOOL		Restart;
{
    register CHANNEL	*cp;
    register BUFFER	*out;

    if (sp->Name == NULL)
	return;

    SITEforward(sp, "flush");
    switch (sp->Type) {
    default:
	syslog(L_ERROR, "%s internal SITEflush %d", sp->Name, sp->Type);
	return;

    case FTlogonly:
    case FTprogram:
    case FTfunnel:
	/* Nothing to do here. */
	return;

    case FTchannel:
    case FTexploder:
	/* If spooling, close the file right now. */
	if (sp->Spooling && (cp = sp->Channel) != NULL) {
	    (void)WCHANflush(cp);
	    CHANclose(cp, CHANname(cp));
	    sp->Channel = NULL;
	}
	break;

    case FTfile:
	break;
    }

    /* We're only dealing with files and channels now. */
    if ((cp = sp->Channel) != NULL)
	(void)WCHANflush(cp);

    /* Restart the site, copy any pending data. */
    if (Restart) {
	if (!SITEsetup(sp))
	    syslog(L_ERROR, "%s cant restart %m", sp->Name);
	else if (cp != NULL) {
	    if (sp->Buffered) {
		/* SITEsetup had to buffer us; save any residue. */
		out = &sp->Channel->Out;
	        if (out->Left)
		    BUFFset(&sp->Buffer, &out->Data[out->Used], out->Left);
	    }
	    else
		WCHANsetfrombuffer(sp->Channel, &cp->Out);
	}
    }
    else if (cp != NULL && cp->Out.Left) {
 	if (sp->Type == FTfile || sp->Spooling) {
	    /* Can't flush a file?  Hopeless. */
 	    syslog(L_ERROR, "%s dataloss %d", sp->Name, cp->Out.Left);
 	    return;
 	}
 	/* Must be a working channel; spool and retry. */
	syslog(L_ERROR, "%s spooling %d bytes", sp->Name, cp->Out.Left);
 	if (SITEspool(sp, cp))
	    SITEflush(sp, FALSE);
 	return;
    }

    /* Close the old channel if it was open. */
    if (cp != NULL) {
        /* Make sure we have no dangling pointers to it. */
	if (!Restart)
	    sp->Channel = NULL;
	CHANclose(cp, sp->Name);
	if (sp->Type == FTfile)
	    SITEcount--;
    }
}


/*
**  Flush all sites.
*/
void
SITEflushall(Restart)
    BOOL	Restart;
{
    register int	i;
    register SITE	*sp;

    for (i = nSites, sp = Sites; --i >= 0; sp++)
	if (sp->Name)
	    SITEflush(sp, Restart);
}


/*
**  Run down the site's pattern list and see if it wants the specified
**  newsgroup.
*/
BOOL
SITEwantsgroup(sp, name)
    register SITE	*sp;
    register char	*name;
{
    register BOOL	match;
    register BOOL	subvalue;
    register char	*pat;
    register char	**argv;

    match = SUB_DEFAULT;
    if (ME.Patterns) {
	for (argv = ME.Patterns; (pat = *argv++) != NULL; ) {
	    subvalue = *pat != SUB_NEGATE;
	    if (!subvalue)
		pat++;
	    if (wildmat(name, pat))
		match = subvalue;
	}
    }
    for (argv = sp->Patterns; (pat = *argv++) != NULL; ) {
	subvalue = *pat != SUB_NEGATE;
	if (!subvalue)
	    pat++;
	if (wildmat(name, pat))
	    match = subvalue;
    }
    return match;
}


/*
**  Find a site.
*/
SITE *
SITEfind(p)
    char	*p;
{
    register int	i;
    register SITE	*sp;

    for (i = nSites, sp = Sites; --i >= 0; sp++)
	if (sp->Name && caseEQ(p, sp->Name))
	    return sp;
    return NULL;
}


/*
**  Find the next site that matches this site.
*/
SITE *
SITEfindnext(p, sp)
    char		*p;
    register SITE	*sp;
{
    register SITE	*end;

    for (sp++, end = &Sites[nSites]; sp < end; sp++)
	if (sp->Name && caseEQ(p, sp->Name))
	    return sp;
    return NULL;
}


/*
**  Close a site down.
*/
void
SITEfree(sp)
    register SITE	*sp;
{
    register SITE	*s;
    register int	new;
    register int	i;

    if (sp->Channel) {
	CHANclose(sp->Channel, CHANname(sp->Channel));
	sp->Channel = NULL;
    }
    sp->Name = NULL;
    if (sp->Process > 0) {
	/* Kill the backpointer so PROCdied won't call us. */
	PROCunwatch(sp->Process);
	sp->Process = -1;
    }
    if (sp->Entry) {
	DISPOSE(sp->Entry);
	sp->Entry = NULL;
    }
    if (sp->Param) {
	DISPOSE(sp->Param);
	sp->Param = NULL;
    }
    if (sp->SpoolName) {
	DISPOSE(sp->SpoolName);
	sp->SpoolName = NULL;
    }
    if (sp->Patterns) {
	DISPOSE(sp->Patterns);
	sp->Patterns = NULL;
    }
    if (sp->Exclusions) {
	DISPOSE(sp->Exclusions);
	sp->Exclusions = NULL;
    }
    if (sp->Distributions) {
	DISPOSE(sp->Distributions);
	sp->Distributions = NULL;
    }
    if (sp->Buffer.Data) {
	DISPOSE(sp->Buffer.Data);
	sp->Buffer.Data = NULL;
	sp->Buffer.Size = 0;
    }
    if (sp->FNLnames.Data) {
	DISPOSE(sp->FNLnames.Data);
	sp->FNLnames.Data = NULL;
	sp->FNLnames.Size = 0;
    }

    /* If this site was a master, find a new one. */
    if (sp->IsMaster) {
	for (new = NOSITE, s = Sites, i = nSites; --i >= 0; s++)
	    if (&Sites[s->Master] == sp)
		if (new == NOSITE) {
		    s->Master = NOSITE;
		    s->IsMaster = TRUE;
		    new = s - Sites;
		}
		else
		    s->Master = new;
	sp->IsMaster = FALSE;
    }
}


/*
**  If a site is an exploder or funnels into one, forward a command
**  to it.
*/
void
SITEforward(sp, text)
    register SITE	*sp;
    char		*text;
{
    register SITE	*fsp;
    register char	*p;
    char		buff[SMBUF];

    fsp = sp;
    if (sp->Name == NULL || fsp->Name == NULL)
	return;
    if (fsp->Funnel != NOSITE)
	fsp = &Sites[fsp->Funnel];
    if (fsp->Type == FTexploder) {
	(void)strcpy(buff, text);
	if (fsp != sp && fsp->FNLwantsnames) {
	    p = buff + strlen(buff);
	    *p++ = ' ';
	    (void)strcpy(p, sp->Name);
	}
	SITEwrite(fsp, buff);
    }
}


/*
**  Drop a site.
*/
void
SITEdrop(sp)
    SITE		*sp;
{
    SITEforward(sp, "drop");
    SITEflush(sp, FALSE);
    SITEfree(sp);
    sp->Name = NULL;
    SITElinkall();
}


/*
**  Put all the feeds into a doubly-linked list.
*/
void
SITElinkall()
{
    register SITE	*sp;

    if (nSites < 2)
	return;

    SITEhead = &Sites[0];
    SITEtail = &Sites[nSites - 1];
    for (sp = SITEhead; ++sp < SITEtail; )
	sp[-1].Next = sp[1].Prev = sp;
    SITEhead->Prev = NULL;
    if (SITEhead->Next)
	SITEhead->Next->Prev = SITEhead;
    SITEtail->Next = NULL;
    if (SITEtail->Prev)
	SITEtail->Prev->Next = SITEtail;
}
