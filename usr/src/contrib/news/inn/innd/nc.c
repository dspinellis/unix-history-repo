/*  $Revision: 1.35 $
**
**  Routines for the NNTP channel.  Other channels get the descriptors which
**  we turn into NNTP channels, and over which we speak NNTP.
*/
#include "innd.h"
#include "dbz.h"


#define BAD_COMMAND_COUNT	10
#define WIP_CHECK		(1 * 60)
#define SAVE_AMT		10
#define ART_EOF(c, s)		\
    ((c) >= 5 && (s)[-5] == '\r' && (s)[-4] == '\n' && (s)[-3] == '.' \
     && (s)[-2] == '\r' && (s)[-1] == '\n')


/*
**  An entry in the dispatch table.  The name, and implementing function,
**  of every command we support.
*/
typedef struct _NCDISPATCH {
    STRING	Name;
    FUNCPTR	Function;
    int		Size;
} NCDISPATCH;


/*
**  Information about the work in progress on all our open channels.
*/
typedef struct _WIP {
    char	*MessageID;
    long	Size;
    time_t	Timestamp;
    BUFFER	Replic;
    BOOL	Wanted;
} WIP;


#if	0
static FUNCTYPE	NCarticle();
#endif	/* 0 */
static FUNCTYPE	NCauthinfo();
static FUNCTYPE	NChead();
static FUNCTYPE	NChelp();
static FUNCTYPE	NCihave();
static FUNCTYPE	NClist();
static FUNCTYPE	NCmode();
static FUNCTYPE	NCquit();
static FUNCTYPE	NCstat();
static FUNCTYPE	NCxpath();
static FUNCTYPE	NCxreplic();
static FUNCTYPE	NC_unimp();

STATIC int		NCcount;	/* Number of open connections	*/
STATIC int		NCwipsize;	/* Size of NCwip array		*/
STATIC char		*NCfreelist[5];	/* Free string list		*/
STATIC WIP		*NCwip;		/* Work-in-progress		*/
STATIC WIP		NCnullwip;
STATIC NCDISPATCH	NCcommands[] = {
#if	0
    {	"article",	NCarticle },
#else
    {	"article",	NC_unimp },
#endif	/* 0 */
    {	"authinfo",	NCauthinfo },
    {	"help",		NChelp	},
    {	"ihave",	NCihave	},
    {	"list",		NClist	},
    {	"mode",		NCmode	},
    {	"quit",		NCquit	},
    {	"head",		NChead	},
    {	"stat",		NCstat	},
    {	"body",		NC_unimp },
    {	"group",	NC_unimp },
    {	"last",		NC_unimp },
    {	"newgroups",	NC_unimp },
    {	"newnews",	NC_unimp },
    {	"next",		NC_unimp },
    {	"post",		NC_unimp },
    {	"slave",	NC_unimp },
    {	"xhdr",		NC_unimp },
    {	"xpath",	NCxpath	},
    {	"xreplic",	NCxreplic }
};
STATIC char		*NCquietlist[] = {
    INND_QUIET_BADLIST
};
STATIC char		NCterm[] = "\r\n";
STATIC char		NCdotterm[] = ".\r\n";
STATIC char		NCbadcommand[] = NNTP_BAD_COMMAND;
STATIC STRING		NCgreeting;


/*
**  Clear the work-in-progress entry and wake up anyone who might
**  have been waiting for us.
*/
STATIC void
NCclearwip(wp)
    register WIP	*wp;
{
    char		*p;

    if ((p = wp->MessageID) != NULL)
	*p = '\0';
    wp->Size = 0;
    if (wp->Wanted) {
        wp->Wanted = FALSE;
	SCHANwakeup((POINTER)wp);
    }
}


/*
**  Write an NNTP reply message.
*/
STATIC void
NCwritetext(cp, text)
    CHANNEL	*cp;
    char	*text;
{
    RCHANremove(cp);
    WCHANset(cp, text, (int)strlen(text));
    WCHANappend(cp, NCterm, STRLEN(NCterm));
    WCHANadd(cp);
    if (Tracing || cp->Tracing)
	syslog(L_TRACE, "%s > %s", CHANname(cp), text);
}


/*
**  Tell the NNTP channel to go away.
*/
STATIC void
NCwriteshutdown(cp, text)
    CHANNEL	*cp;
    char	*text;
{
    RCHANremove(cp);
    WCHANset(cp, NNTP_GOODBYE, STRLEN(NNTP_GOODBYE));
    WCHANappend(cp, " ", 1);
    WCHANappend(cp, text, (int)strlen(text));
    WCHANappend(cp, NCterm, STRLEN(NCterm));
    WCHANadd(cp);
    cp->State = CSwritegoodbye;
}


/*
**  If a Message-ID is bad, write a reject message and return TRUE.
*/
STATIC BOOL
NCbadid(cp, p)
    register CHANNEL	*cp;
    register char	*p;
{
    if (ARTidok(p))
	return FALSE;

    NCwritetext(cp, NNTP_HAVEIT_BADID);
    syslog(L_NOTICE, "%s bad_messageid %s", CHANname(cp), MaxLength(p, p));
    return TRUE;
}


/*
**  We have an entire article collected; try to post it.  If we're
**  not running, drop the article or just pause and reschedule.
*/
STATIC void
NCpostit(cp)
    register CHANNEL	*cp;
{
    STRING		response;
    WIP			*wp;

    /* Note that some use break, some use return here. */
    switch (Mode) {
    default:
	syslog(L_ERROR, "%s internal NCpostit mode %d", CHANname(cp), Mode);
	return;
    case OMpaused:
	SCHANadd(cp, (time_t)(Now.time + PAUSE_RETRY_TIME), (POINTER)&Mode,
	    NCpostit, (POINTER)NULL);
	return;
    case OMrunning:
	wp = &NCwip[cp->fd];
	response = ARTpost(cp, AmSlave ? &wp->Replic : NULL, wp->MessageID);
	if (atoi(response) == NNTP_TOOKIT_VAL)
	    cp->Received++;
	else
	    cp->Rejected++;
	cp->Reported++;
	if (cp->Reported >= NNTP_ACTIVITY_SYNC) {
	    syslog(L_NOTICE,
	    "%s checkpoint seconds %ld accepted %ld refused %ld rejected %ld",
		CHANname(cp), (long)(Now.time - cp->Started),
		cp->Received, cp->Refused, cp->Rejected);
	    cp->Reported = 0;
	}
	NCwritetext(cp, response);
	cp->State = CSgetcmd;
	break;

    case OMthrottled:
	NCwriteshutdown(cp, ModeReason);
	cp->Rejected++;
	break;
    }

    /* Clear the work-in-progress entry. */
    NCclearwip(&NCwip[cp->fd]);
}


/*
**  Write-done function.  Close down or set state for what we expect to
**  read next.
*/
STATIC FUNCTYPE
NCwritedone(cp)
    register CHANNEL	*cp;
{
    switch (cp->State) {
    default:
	syslog(L_ERROR, "%s internal NCwritedone state %d",
	    CHANname(cp), cp->State);
	break;

    case CSwritegoodbye:
	if (NCcount > 0)
	    NCcount--;
	CHANclose(cp, CHANname(cp));
	break;

    case CSgetcmd:
    case CSgetauth:
    case CSgetarticle:
    case CSgetrep:
	RCHANadd(cp);
	break;
    }
}



#if	0
/*
**  The "article" command.
*/
STATIC FUNCTYPE
NCarticle(cp)
    register CHANNEL	*cp;
{
    register char	*p;
    register char	*q;
    char		*art;

    /* Snip off the Message-ID. */
    for (p = cp->In.Data + STRLEN("article"); ISWHITE(*p); p++)
	continue;
    if (NCbadid(cp, p))
	return;

    /* Get the article filenames, and the article header+body. */
    if ((art = ARTreadarticle(HISfilesfor(p))) == NULL) {
	NCwritetext(cp, NNTP_DONTHAVEIT);
	return;
    }

    /* Write it. */
    NCwritetext(cp, NNTP_ARTICLE_FOLLOWS);
    for (p = art; ((q = strchr(p, '\n')) != NULL); p = q + 1) {
	if (*p == '.')
	    WCHANappend(cp, ".", 1);
	WCHANappend(cp, p, q - p);
	WCHANappend(cp, NCterm, STRLEN(NCterm));
    }

    /* Write the terminator. */
    WCHANappend(cp, NCdotterm, STRLEN(NCdotterm));
}
#endif	/* 0 */


/*
**  The "head" command.
*/
STATIC FUNCTYPE
NChead(cp)
    CHANNEL		*cp;
{
    register char	*p;
    register char	*q;
    char		*head;

    /* Snip off the Message-ID. */
    for (p = cp->In.Data + STRLEN("head"); ISWHITE(*p); p++)
	continue;
    if (NCbadid(cp, p))
	return;

    /* Get the article filenames, and the header. */
    if ((head = ARTreadheader(HISfilesfor(p))) == NULL) {
	NCwritetext(cp, NNTP_DONTHAVEIT);
	return;
    }

    /* Write it. */
    NCwritetext(cp, NNTP_HEAD_FOLLOWS);
    for (p = head; ((q = strchr(p, '\n')) != NULL); p = q + 1) {
	if (*p == '.')
	    WCHANappend(cp, ".", 1);
	WCHANappend(cp, p, q - p);
	WCHANappend(cp, NCterm, STRLEN(NCterm));
    }

    /* Write the terminator. */
    WCHANappend(cp, NCdotterm, STRLEN(NCdotterm));
}


/*
**  The "stat" command.
*/
STATIC FUNCTYPE
NCstat(cp)
    CHANNEL		*cp;
{
    register char	*p;
    char		buff[SMBUF];

    /* Snip off the Message-ID. */
    for (p = cp->In.Data + STRLEN("stat"); ISWHITE(*p); p++)
	continue;
    if (NCbadid(cp, p))
	return;

    /* Get the article filenames; read the header (to make sure not
     * the article is still here). */
    if (ARTreadheader(HISfilesfor(p)) == NULL) {
	NCwritetext(cp, NNTP_DONTHAVEIT);
	return;
    }

    /* Write the message. */
    (void)sprintf(buff, "%d 0 %s", NNTP_NOTHING_FOLLOWS_VAL, p);
    NCwritetext(cp, buff);
}


/*
**  The "authinfo" command.  Actually, we come in here whenever the
**  channel is in CSgetauth state and we just got a command.
*/
STATIC FUNCTYPE
NCauthinfo(cp)
    register CHANNEL	*cp;
{
    static char		AUTHINFO[] = "authinfo ";
    static char		PASS[] = "pass ";
    static char		USER[] = "user ";
    register char	*p;

    p = cp->In.Data;

    /* Allow the poor sucker to quit. */
    if (caseEQ(p, "quit")) {
	NCquit(cp);
	return;
    }

    /* Otherwise, make sure we're only getting "authinfo" commands. */
    if (!caseEQn(p, AUTHINFO, STRLEN(AUTHINFO))) {
	NCwritetext(cp, NNTP_AUTH_NEEDED);
	return;
    }
    for (p += STRLEN(AUTHINFO); ISWHITE(*p); p++)
	continue;

    /* Ignore "authinfo user" commands, since we only care about the
     * password. */
    if (caseEQn(p, USER, STRLEN(USER))) {
	NCwritetext(cp, NNTP_AUTH_NEXT);
	return;
    }

    /* Now make sure we're getting only "authinfo pass" commands. */
    if (!caseEQn(p, PASS, STRLEN(PASS))) {
	NCwritetext(cp, NNTP_AUTH_NEEDED);
	return;
    }
    for (p += STRLEN(PASS); ISWHITE(*p); p++)
	continue;

    /* Got the password -- is it okay? */
    if (!RCauthorized(cp, p)) {
	NCwritetext(cp, NNTP_AUTH_BAD);
	cp->State = CSwritegoodbye;
    }
    else {
	NCwritetext(cp, NNTP_AUTH_OK);
	cp->State = CSgetcmd;
    }
}


/*
**  Is someone already sending us this article?
*/
STATIC BOOL
NCinprogress(cp, id, who)
    CHANNEL		*cp;
    register char	*id;
    WIP			**who;
{
    register WIP	*wp;
    register char	*p;
    register int	i;

    for (i = NCwipsize, wp = NCwip; --i >= 0; wp++)
	if ((p = wp->MessageID) != NULL && *p == *id && EQ(p, id)
	 && Now.time - wp->Timestamp < WIP_CHECK) {
	    *who = wp;
	    return TRUE;
	}
    wp = &NCwip[cp->fd];
    if (wp->MessageID == NULL) {
	for (i = SIZEOF(NCfreelist); --i >= 0; )
	    if (NCfreelist[i] != NULL) {
		wp->MessageID = NCfreelist[i];
		NCfreelist[i] = NULL;
		break;
	    }
	if (i < 0)
	    wp->MessageID = NEW(char, DBZMAXKEY + 3);
    }
    (void)strcpy(wp->MessageID, id);
    return FALSE;
}


/*
**  The "help" command.
*/
STATIC FUNCTYPE
NChelp(cp)
    register CHANNEL	*cp;
{
    static char		LINE1[] = "For more information, contact \"";
    static char		LINE2[] = "\" at this machine.";
    register NCDISPATCH	*dp;

    NCwritetext(cp, NNTP_HELP_FOLLOWS);
    for (dp = NCcommands; dp < ENDOF(NCcommands); dp++)
	if (dp->Function != NC_unimp) {
	    WCHANappend(cp, "\t", 1);
	    WCHANappend(cp, dp->Name, dp->Size);
	    WCHANappend(cp, NCterm, STRLEN(NCterm));
	}
    WCHANappend(cp, LINE1, STRLEN(LINE1));
    WCHANappend(cp, NEWSMASTER, STRLEN(NEWSMASTER));
    WCHANappend(cp, LINE2, STRLEN(LINE2));
    WCHANappend(cp, NCterm, STRLEN(NCterm));
    WCHANappend(cp, NCdotterm, STRLEN(NCdotterm));
}


#if	!defined(NNTP_RESENDIT_LATER)
/*
**  We woke up because we got offered an article that was already in
**  progress somewhere else.  If the other channel finished, then we
**  don't want the article, otherwise let's accept it.
*/
STATIC FUNCTYPE
NCwaitfor(cp)
    register CHANNEL	*cp;
{
    WIP			*who;

    if (HIShavearticle(cp->Argument)) {
	NCwritetext(cp, NNTP_HAVEIT);
	DISPOSE(cp->Argument);
	cp->Argument = NULL;
    }
    else if (NCinprogress(cp, cp->Argument, &who)) {
	who->Wanted = TRUE;
	SCHANadd(cp, (time_t)(Now.time + WIP_CHECK / 2 + 1), (POINTER)who,
	    NCwaitfor, (POINTER)cp->Argument);
    }
    else {
	NCwritetext(cp, NNTP_SENDIT);
	cp->State = CSgetarticle;
	DISPOSE(cp->Argument);
	cp->Argument = NULL;
    }
}
#endif	/* !defined(NNTP_RESENDIT_LATER) */


/*
**  The "ihave" command.  Check the Message-ID, and see if we want the
**  article or not.  Set the state appropriately.
*/
STATIC FUNCTYPE
NCihave(cp)
    CHANNEL		*cp;
{
    register char	*p;
    WIP			*who;

    if (AmSlave) {
	NCwritetext(cp, NCbadcommand);
	return;
    }

    /* Snip off the Message-ID. */
    for (p = cp->In.Data + STRLEN("ihave"); ISWHITE(*p); p++)
	continue;
    if (NCbadid(cp, p))
	return;

    if (HIShavearticle(p)) {
	cp->Refused++;
	NCwritetext(cp, NNTP_HAVEIT);
    }
    else if (NCinprogress(cp, p, &who)) {
#if	defined(NNTP_RESENDIT_LATER)
	NCwritetext(cp, NNTP_RESENDIT_LATER);
#else
	/* Somebody else is sending it to us; wait until they're done. */
	who->Wanted = TRUE;
	SCHANadd(cp, (time_t)(Now.time + WIP_CHECK + 1), (POINTER)who,
	    NCwaitfor, (POINTER)COPY(p));
	/* Clear input buffer. */
	cp->In.Used = 0;
#endif	/* defined(NNTP_RESENDIT_LATER) */
    }
    else {
	NCwritetext(cp, NNTP_SENDIT);
	cp->State = CSgetarticle;
    }
}


/*
**  The "list" command.  Send the active file.
*/
STATIC FUNCTYPE
NClist(cp)
    register CHANNEL	*cp;
{
    register char	*p;
    register char	*q;
    char		*trash;
    char		*end;

    for (p = cp->In.Data + STRLEN("list"); ISWHITE(*p); p++)
	continue;
    if (caseEQ(p, "newsgroups")) {
	trash = p = ReadInFile(_PATH_NEWSGROUPS, (struct stat *)NULL);
	end = p + strlen(p);
    }
    else if (caseEQ(p, "active.times")) {
	trash = p = ReadInFile(_PATH_ACTIVETIMES, (struct stat *)NULL);
	end = p + strlen(p);
    }
    else if (*p == '\0' || (caseEQ(p, "active"))) {
	p = ICDreadactive(&end);
	trash = NULL;
    }
    else {
	NCwritetext(cp, NCbadcommand);
	return;
    }

    /* Loop over all lines, sending the text and \r\n. */
    NCwritetext(cp, NNTP_LIST_FOLLOWS);
    for (; p < end && (q = strchr(p, '\n')) != NULL; p = q + 1) {
	WCHANappend(cp, p, q - p);
	WCHANappend(cp, NCterm, STRLEN(NCterm));
    }
    WCHANappend(cp, NCdotterm, STRLEN(NCdotterm));
    if (trash)
	DISPOSE(trash);
}


/*
**  The "mode" command.  Hand off the channel.
*/
STATIC FUNCTYPE
NCmode(cp)
    register CHANNEL	*cp;
{
    register char	*p;
    HANDOFF		h;

    /* Skip the first word, get the argument. */
    for (p = cp->In.Data + STRLEN("mode"); ISWHITE(*p); p++)
	continue;

    if (caseEQ(p, "reader"))
	h = HOnnrpd;
    else if (caseEQ(p, "query"))
	h = HOnnrqd;
    else {
	NCwritetext(cp, NCbadcommand);
	return;
    }
    RChandoff(cp->fd, h);
    if (NCcount > 0)
	NCcount--;
    CHANclose(cp, CHANname(cp));
}


/*
**  The "quit" command.  Acknowledge, and set the state to closing down.
*/
STATIC FUNCTYPE
NCquit(cp)
    CHANNEL		*cp;
{
    register WIP	*wp;
    register int	i;

    wp = &NCwip[cp->fd];
    for (i = SIZEOF(NCfreelist); --i >= 0; )
	if (NCfreelist[i] == NULL) {
	    NCfreelist[i] = wp->MessageID;
	    wp->MessageID = NULL;
	    break;
	}
#if	0
    if (i < 0) {
	DISPOSE(wp->MessageID);
	wp->MessageID = NULL;
    }
#endif	/* 0 */
    NCwritetext(cp, NNTP_GOODBYE_ACK);
    cp->State = CSwritegoodbye;
}


/*
**  The "xpath" command.  Return the paths for an article is.
*/
STATIC FUNCTYPE
NCxpath(cp)
    CHANNEL		*cp;
{
    static BUFFER	Reply;
    register char	*p;
    register int	i;

    /* Nip off the Message-ID. */
    for (p = cp->In.Data + STRLEN("xpath"); ISWHITE(*p); p++)
	continue;
    if (NCbadid(cp, p))
	return;

    if ((p = HISfilesfor(p)) == NULL) {
	NCwritetext(cp, NNTP_DONTHAVEIT);
	return;
    }
    i = 3 + 1 + strlen(p);
    if (Reply.Data == NULL) {
	Reply.Size = i;
	Reply.Data = NEW(char, i + 1);
    }
    else if (Reply.Size < i) {
	Reply.Size = i;
	RENEW(Reply.Data, char, i + 1);
    }
    (void)sprintf(Reply.Data, "%d %s", NNTP_NOTHING_FOLLOWS_VAL, p);
    NCwritetext(cp, Reply.Data);
}


/*
**  The "xreplic" command.  Take an article and the places to file it.
*/
STATIC FUNCTYPE
NCxreplic(cp)
    CHANNEL		*cp;
{
    register char	*p;
    register BUFFER	*bp;
    register int	i;

    if (!RCismaster(cp->Address)) {
	NCwritetext(cp, NCbadcommand);
	return;
    }

    /* Stash the filename arguments. */
    for (p = cp->In.Data + STRLEN("xreplic"); ISWHITE(*p); p++)
	continue;
    i = cp->In.Used - (p - cp->In.Data) + 1;
    bp = &NCwip[cp->fd].Replic;
    if (bp->Data == NULL) {
	bp->Size = i;
	bp->Data = NEW(char, i);
    }
    BUFFset(bp, p, i);
    bp->Used = bp->Left;

    /* Tell master to send it to us. */
    NCwritetext(cp, NNTP_SENDIT);
    cp->State = CSgetrep;
}


/*
**  The catch-all for inimplemented commands.
*/
STATIC FUNCTYPE
NC_unimp(cp)
    CHANNEL		*cp;
{
    register char	*p;
    char		buff[SMBUF];

    /* Nip off the first word. */
    for (p = cp->In.Data; *p && !ISWHITE(*p); p++)
	continue;
    *p = '\0';
    (void)sprintf(buff, "%d \"%s\" not implemented; try \"help\".",
	    NNTP_BAD_COMMAND_VAL, MaxLength(cp->In.Data, cp->In.Data));
    NCwritetext(cp, buff);
}



/*
**  Remove the \r\n and leading dot escape that the NNTP protocol adds.
*/
STATIC void
NCclean(bp)
    register BUFFER	*bp;
{
    register char	*end;
    register char	*p;
    register char	*dest;

    for (p = bp->Data, dest = p, end = p + bp->Used; p < end; ) {
	if (p[0] == '\r' && p[1] == '\n') {
	    p += 2;
	    *dest++ = '\n';
	    if (p[0] == '.' && p[1] == '.') {
		p += 2;
		*dest++ = '.';
	    }
	}
	else
	    *dest++ = *p++;
    }
    *dest = '\0';
    bp->Used = dest - bp->Data;
}


/*
**  Read whatever data is available on the channel.  If we got the
**  full amount (i.e., the command or the whole article) process it.
*/
STATIC FUNCTYPE
NCreader(cp)
    register CHANNEL	*cp;
{
    register char	*p;
    register NCDISPATCH	*dp;
    register BUFFER	*bp;
    register WIP	*wp;
    STRING		q;
    char		buff[SMBUF];
    char		*av[2];
    int			i;

    /* Read any data that's there; ignore errors (retry next time it's our
     * turn) and if we got nothing, then it's EOF so mark it closed. */
    if ((i = CHANreadtext(cp)) < 0) {
	if (cp->BadReads++ >= BAD_IO_COUNT) {
	    if (NCcount > 0)
		NCcount--;
	    CHANclose(cp, CHANname(cp));
	}
	return;
    }
    if (i == 0) {
	NCcount--;
	return;
    }

    /* Update timestamp. */
    wp = &NCwip[cp->fd];
    wp->Timestamp = Now.time;

    bp = &cp->In;
    p = &bp->Data[bp->Used];
    switch (cp->State) {
    default:
	syslog(L_ERROR, "%s internal NCreader state %d",
	    CHANname(cp), cp->State);
	break;

    case CSgetcmd:
    case CSgetauth:
	/* Did we get the whole command, terminated with "\r\n"? */
	if (bp->Used < 2 || p[-2] != '\r' || p[-1] != '\n')
	    break;
	p[-2] = '\0';
	bp->Used -= 2;

	/* Ignore blank lines. */
	if (bp->Used == 0)
	    break;
	if (Tracing || cp->Tracing)
	    syslog(L_TRACE, "%s < %s", CHANname(cp), bp->Data);

	/* We got something -- stop sleeping (in case we were). */
	SCHANremove(cp);
	if (cp->Argument != NULL) {
	    DISPOSE(cp->Argument);
	    cp->Argument = NULL;
	}

	if (cp->State == CSgetauth) {
	    if (caseEQn(bp->Data, "mode", 4))
		NCmode(cp);
	    else
		NCauthinfo(cp);
	    break;
	}

	/* Loop through the command table. */
	for (p = bp->Data, dp = NCcommands; dp < ENDOF(NCcommands); dp++)
	    if (caseEQn(p, dp->Name, dp->Size)) {
		(*dp->Function)(cp);
		cp->BadCommands = 0;
		break;
	    }
	if (dp == ENDOF(NCcommands)) {
	    NCwritetext(cp, NCbadcommand);
	    if (++(cp->BadCommands) >= BAD_COMMAND_COUNT)
		cp->State = CSwritegoodbye;
	    for (i = 0; (p = NCquietlist[i]) != NULL; i++)
		if (caseEQ(p, dp->Name))
		    break;
	    if (p == NULL)
		syslog(L_NOTICE, "%s bad_command %s",
		    CHANname(cp), MaxLength(bp->Data, bp->Data));
	}
	break;

    case CSgetarticle:
    case CSgetrep:
	/* Reading an article; look for "\r\n.\r\n" terminator. */
	if (!ART_EOF(bp->Used, p)) {
	    /* Check for the null article. */
	    if (bp->Used == 3
	     && p[-3] == '.' && p[-2] == '\r' && p[-1] == '\n') {
		cp->Rejected++;
		NCwritetext(cp, NNTP_REJECTIT_EMPTY);
		cp->State = CSgetcmd;
		bp->Used = 0;

		/* Clear the work-in-progress entry. */
		NCclearwip(wp);
	    }

	    /* Check for big articles. */
	    if (LargestArticle > SAVE_AMT && bp->Used > LargestArticle) {
		/* Make some room, saving only the last few bytes. */
		for (p = bp->Data, i = 0; i < SAVE_AMT; p++, i++)
		    p[0] = p[bp->Used - SAVE_AMT];
		wp->Size += bp->Used - SAVE_AMT;
		bp->Used = SAVE_AMT;
		cp->State = CSeatarticle;
	    }
	    break;
	}

	/* Strip article terminator and post the article. */
	p[-3] = '\0';
	bp->Used -= 3;
	SCHANremove(cp);
	if (cp->Argument != NULL) {
	    DISPOSE(cp->Argument);
	    cp->Argument = NULL;
	}
	NCclean(bp);
	NCpostit(cp);
	break;

    case CSeatarticle:
	/* Eat the article and then complain that it was too large */
	if (ART_EOF(bp->Used, p)) {
	    /* Reached the end of the article. */
	    SCHANremove(cp);
	    if (cp->Argument != NULL) {
		DISPOSE(cp->Argument);
		cp->Argument = NULL;
	    }
	    p = wp->MessageID;
	    i = wp->Size + bp->Used;
	    syslog(L_ERROR, "%s internal rejecting huge article %s (%d > %d)",
		CHANname(cp), p ? p : "(null)", i, LargestArticle);
	    (void)sprintf(buff, "%d Article exceeds local limit of %ld bytes",
		    NNTP_REJECTIT_VAL, LargestArticle);
	    NCwritetext(cp, buff);
	    cp->State = CSgetcmd;
	    cp->Rejected++;

	    /* Write a local cancel entry so nobody else gives it to us. */
	    if (p) {
		av[0] = p;
		av[1] = NULL;
		if ((q = CCcancel(av)) != NULL)
		    syslog(L_ERROR, "%s cant cancel %s %s", LogName, av[0], q);
	    }

	    /* Clear the work-in-progress entry. */
	    NCclearwip(wp);

	    /* Reset input buffer to the default size; don't let realloc
	     * be lazy. */
	    DISPOSE(bp->Data);
	    bp->Size = START_BUFF_SIZE;
	    bp->Used = 0;
	    bp->Data = NEW(char, bp->Size);
	}
	else if (bp->Used > 8 * 1024) {
	    /* Make some room; save the last few bytes of the article */
	    for (p = bp->Data, i = 0; i < SAVE_AMT; p++, i++)
		p[0] = p[bp->Used - SAVE_AMT + 0];
	    wp->Size += bp->Used - SAVE_AMT;
	    bp->Used = SAVE_AMT;
	}
	break;
    }
}


/*
**  Set up the NNTP channel state.
*/
void
NCsetup(i)
    register int	i;
{
    register WIP	*wp;
    register NCDISPATCH	*dp;
    char		*p;
    char		buff[SMBUF];

    /* Set the greeting message. */
    if ((p = GetConfigValue(_CONF_PATHHOST)) == NULL)
	/* Worked in main, now it fails?  Curious. */
	p = Path.Data;
    (void)sprintf(buff, "%d %s InterNetNews server %s ready",
	    NNTP_POSTOK_VAL, p, Version);
    NCgreeting = COPY(buff);

    /* Set up the work-in-progress structure. */
    for (wp = NCwip = NEW(WIP, i), NCwipsize = i; --i >= 0; wp++)
	*wp = NCnullwip;

    /* Get the length of every command. */
    for (dp = NCcommands; dp < ENDOF(NCcommands); dp++)
	dp->Size = strlen(dp->Name);
}


/*
**  Tear down our state.
*/
void
NCclose()
{
    register WIP	*wp;
    register int	i;
    register CHANNEL	*cp;
    int			j;

    /* Close all incoming channels. */
    for (j = 0; (cp = CHANiter(&j, CTnntp)) != NULL; ) {
	if (NCcount > 0)
	    NCcount--;
	CHANclose(cp, CHANname(cp));
    }

    /* Free the WIP list. */
    for (wp = NCwip, i = NCwipsize; --i >= 0; wp++) {
	if (wp->MessageID)
	    DISPOSE(wp->MessageID);
	if (wp->Replic.Data)
	    DISPOSE(wp->Replic.Data);
    }
    DISPOSE(NCwip);
    for (i = SIZEOF(NCfreelist); --i >= 0; )
	if (NCfreelist[i] != NULL)
	    DISPOSE(NCfreelist[i]);
}


/*
**  Create an NNTP channel and print the greeting message.
*/
CHANNEL *
NCcreate(fd, MustAuthorize)
    int			fd;
    BOOL		MustAuthorize;
{
    register CHANNEL	*cp;
    int			i;

    /* Create the channel. */
    cp = CHANcreate(fd, CTnntp, MustAuthorize ? CSgetauth : CSgetcmd,
	    NCreader, NCwritedone);
    NCclearwip(&NCwip[cp->fd]);
#if	defined(SOL_SOCKET) && defined(SO_SNDBUF) && defined(SO_RCVBUF)
    i = 24 * 1024;
    if (setsockopt(fd, SOL_SOCKET, SO_SNDBUF, (char *)&i, sizeof i) < 0)
	syslog(L_ERROR, "%s cant setsockopt(SNDBUF) %m", CHANname(cp));
    if (setsockopt(fd, SOL_SOCKET, SO_RCVBUF, (char *)&i, sizeof i) < 0)
	syslog(L_ERROR, "%s cant setsockopt(RCVBUF) %m", CHANname(cp));
#endif	/* defined(SOL_SOCKET) && defined(SO_SNDBUF) && defined(SO_RCVBUF) */

    /* Now check our operating mode. */
    NCcount++;
    if (Mode == OMthrottled) {
	NCwriteshutdown(cp, ModeReason);
	return cp;
    }
    if (RejectReason) {
	NCwriteshutdown(cp, RejectReason);
	return cp;
    }

    /* See if we have too many channels. */
    if (MaxIncoming && NCcount >= MaxIncoming && !RCnolimit(cp)) {
	/* Recount, just in case we got out of sync. */
	for (NCcount = 0, i = 0; CHANiter(&i, CTnntp) != NULL; )
	    NCcount++;
	if (NCcount >= MaxIncoming) {
	    NCwriteshutdown(cp, "Too many connections");
	    return cp;
	}
    }
    cp->BadReads = 0;
    cp->BadCommands = 0;
    NCwritetext(cp, NCgreeting);
    return cp;
}
