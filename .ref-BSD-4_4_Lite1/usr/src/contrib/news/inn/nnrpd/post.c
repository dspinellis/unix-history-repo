/*  $Revision: 1.18 $
**
**  Check article, send it to the local server.
*/
#include "nnrpd.h"
#if	defined(DO_NEED_TIME)
#include <time.h>
#endif	/* defined(DO_NEED_TIME) */
#include <sys/time.h>

#define FLUSH_ERROR(F)		(fflush((F)) == EOF || ferror((F)))
#define HEADER_DELTA		20

typedef enum _HEADERTYPE {
    HTobs,
    HTreq,
    HTstd
} HEADERTYPE;

typedef struct _HEADER {
    STRING	Name;
    BOOL	CanSet;
    HEADERTYPE	Type;
    int		Size;
    char	*Value;
} HEADER;


STATIC char	Error[SMBUF];
STATIC char	NGSEPS[] = NG_SEPARATOR;
STATIC char	**OtherHeaders;
STATIC int	OtherCount;
STATIC int	OtherSize;
STATIC BOOL	WasMailed;
STATIC STRING	BadDistribs[] = {
    BAD_DISTRIBS
};

STATIC HEADER	Table[] = {
    /* 	Name			Canset	Type	*/
    {	"Path",			TRUE,	HTstd },
#define _path		 0
    {	"From",			TRUE,	HTreq },
#define _from		 1
    {	"Newsgroups",		TRUE,	HTreq },
#define _newsgroups	 2
    {	"Subject",		TRUE,	HTreq },
#define _subject	 3
    {	"Control",		TRUE,	HTstd },
#define _control	 4
    {	"Supersedes",		TRUE,	HTstd },
#define _supersedes	 5
    {	"Followup-To",		TRUE,	HTstd },
#define _followupto	 6
    {	"Date",			TRUE,	HTstd },
#define _date		 7
    {	"Organization",		TRUE,	HTstd },
#define _organization	 8
    {	"Lines",		TRUE,	HTstd },
#define _lines		 9
    {	"Sender",		TRUE,	HTstd },
#define _sender		10
    {	"Approved",		TRUE,	HTstd },
#define _approved	11
    {	"Distribution",		TRUE,	HTstd },
#define _distribution	12
    {	"Expires",		TRUE,	HTstd },
#define _expires	13
    {	"Message-ID",		TRUE,	HTstd },
#define _messageid	14
    {	"References",		TRUE,	HTstd },
#define _references	15
    {	"Reply-To",		TRUE,	HTstd },
#define _replyto	16
    {	"NNTP-Posting-Host",	FALSE,	HTstd },
#define _nntpposthost	17
    {	"Mime-Version",		TRUE,	HTstd },
#define _mimeversion	18
    {	"Content-Type",		TRUE,	HTstd },
#define _contenttype	19
    {	"Content-Transfer-Encoding", TRUE, HTstd },
#define _contenttransferencoding 20
    {	"Xref",			FALSE,	HTstd },
    {	"Summary",		TRUE,	HTstd },
    {	"Keywords",		TRUE,	HTstd },
    {	"Date-Received",	FALSE,	HTobs },
    {	"Received",		FALSE,	HTobs },
    {	"Posted",		FALSE,	HTobs },
    {	"Posting-Version",	FALSE,	HTobs },
    {	"Relay-Version",	FALSE,	HTobs },
};

#define HDR(_x)	(Table[(_x)].Value)



/*
**  Trim trailing spaces, return pointer to first non-space char.
*/
STATIC char *
TrimSpaces(p)
    register char	*p;
{
    register char	*start;

    for (start = p; ISWHITE(*start); start++)
	continue;
    for (p = start + strlen(start); p > start && CTYPE(isspace, p[-1]); )
	*--p = '\0';
    return start;
}


/*
**  Mark the end of the header starting at p, and return a pointer
**  to the start of the next one or NULL.  Handles continuations.
*/
STATIC char *
NextHeader(p)
    register char	*p;
{
    for ( ; (p = strchr(p, '\n')) != NULL; p++) {
	if (ISWHITE(p[1]))
	    continue;
	*p = '\0';
	return p + 1;
    }
    return NULL;
}


/*
**  Strip any headers off the article and dump them into the table.
**  On error, return NULL and fill in Error.
*/
STATIC char *
StripOffHeaders(article)
    char		*article;
{
    register char	*p;
    register char	*q;
    register HEADER	*hp;
    register char	c;

    /* Scan through buffer, a header at a time. */
    for (p = article; ; ) {

	/* See if it's a known header. */
	c = CTYPE(islower, *p) ? toupper(*p) : *p;
	for (hp = Table; hp < ENDOF(Table); hp++)
	    if (c == hp->Name[0]
	     && p[hp->Size] == ':'
	     && caseEQn(p, hp->Name, hp->Size)) {
		if (hp->Type == HTobs) {
		    (void)sprintf(Error, "Obsolete \"%s\" header", hp->Name);
		    return NULL;
		}
		if (hp->Value) {
		    (void)sprintf(Error, "Duplicate \"%s\" header", hp->Name);
		    return NULL;
		}
		for (q = &p[hp->Size + 1]; ISWHITE(*q); q++)
		    continue;
		hp->Value = q;
		break;
	    }

	/* No; add it to the set of other headers. */
	if (hp == ENDOF(Table)) {
	    if (OtherCount >= OtherSize - 1) {
		OtherSize += HEADER_DELTA;
		RENEW(OtherHeaders, char*, OtherSize);
	    }
	    OtherHeaders[OtherCount++] = p;
	}

	/* Get start of next header; if it's a blank line, we hit the end. */
	if ((p = NextHeader(p)) == NULL) {
	    (void)strcpy(Error, "Article has no body -- just headers");
	    return NULL;
	}
	if (*p == '\n')
	    break;
    }

    return p + 1;
}



/*
**  Check the control message, and see if it's legit.  Return pointer to
**  error message if not.
*/
STATIC STRING
CheckControl(ctrl)
    char	*ctrl;
{
    register char	*p;
    register char	*q;
    char		save;

    /* Snip off the first word. */
    for (p = ctrl; ISWHITE(*p); p++)
	continue;
    for (ctrl = p; *p && !ISWHITE(*p); p++)
	continue;
    if (p == ctrl)
	return "Empty control message";
    save = *p;
    *p = '\0';

    if (EQ(ctrl, "cancel")) {
	for (q = p + 1; ISWHITE(*q); q++)
	    continue;
	if (*q == '\0')
	    return "Message-ID missing in cancel";
    }
    else if (EQ(ctrl, "sendsys") || EQ(ctrl, "senduuname")
	  || EQ(ctrl, "version") || EQ(ctrl, "checkgroups")
	  || EQ(ctrl, "ihave") || EQ(ctrl, "sendme")
	  || EQ(ctrl, "newgroup") || EQ(ctrl, "rmgroup"))
	/* SUPPRESS 530 *//* Empty body for statement */
	;
    else {
	(void)sprintf(Error, "\"%s\" is not a valid control message",
		ctrl);
	return Error;
    }
    *p = save;
    return NULL;
}


/*
**  Check the Distribution header, and exit on error.
*/
STATIC STRING
CheckDistribution(p)
    register char	*p;
{
    static char		SEPS[] = " \t,";
    register STRING	*dp;

    if ((p = strtok(p, SEPS)) == NULL)
	return "Can't parse Distribution line.";
    do {
	for (dp = BadDistribs; *dp; dp++)
	    if (wildmat(p, *dp)) {
		(void)sprintf(Error, "Illegal distribution \"%s\"", p);
		return Error;
	    }
    } while ((p = strtok((char *)NULL, SEPS)) != NULL);
    return NULL;
}


/*
**  Process all the headers.  FYI, they're done in RFC-order.
**  Return NULL if okay, or an error message.
*/
STATIC STRING
ProcessHeaders(linecount)
    int			linecount;
{
    static char		MONTHS[] = "JanFebMarAprMayJunJulAugSepOctNovDec";
    static char		subjbuff[NNTP_STRLEN];
    static char		datebuff[40];
    static char		orgbuff[SMBUF];
    static char		linebuff[40];
    static char		mimeversion[SMBUF];
    static char		mimetype[SMBUF];
    static char		mimeencoding[SMBUF];
    register HEADER	*hp;
    register char	*p;
    time_t		t;
    struct tm		*gmt;
    TIMEINFO		Now;
    STRING		error;

    /* Do some preliminary fix-ups. */
    for (hp = Table; hp < ENDOF(Table); hp++) {
	if (!hp->CanSet && hp->Value) {
	    (void)sprintf(Error, "Can't set system \"%s\" header", hp->Name);
	    return Error;
	}
	if (hp->Value) {
	    hp->Value = TrimSpaces(hp->Value);
	    if (*hp->Value == '\0')
		hp->Value = NULL;
	}
    }

#if	defined(DO_NNRP_AUTH_SENDER)
    /* Zap the Sender? */
    if (!PERMauthorized)
	HDR(_sender) = NULL;
#endif	/* defined(DO_NNRP_AUTH_SENDER) */

    /* Set Date. */
    if (GetTimeInfo(&Now) < 0) {
	(void)sprintf(Error, "Can't get the time, %s", strerror(errno));
	return Error;
    }

    if (HDR(_date) == NULL) {
	if ((gmt = gmtime(&Now.time)) == NULL)
	    return "Can't get the time";
	(void)sprintf(datebuff, "%d %3.3s %d %02.2d:%02.2d:%02.2d GMT",
	    gmt->tm_mday, &MONTHS[3 * gmt->tm_mon], 1900 + gmt->tm_year,
	    gmt->tm_hour, gmt->tm_min, gmt->tm_sec);
	HDR(_date) = datebuff;
    }
    else {
	if ((t = parsedate(HDR(_date), &Now)) == -1)
	    return "Can't parse \"Date\" header";
	if (t > Now.time + DATE_FUZZ)
	    return "Article posted in the future";
    }

    /* Newsgroups are checked later. */

    /* Set Subject; Control overrides the subject. */
    if (HDR(_control)) {
	if ((error = CheckControl(HDR(_control))) != NULL)
	    return error;
	(void)sprintf(subjbuff, "cmsg %s", HDR(_control));
	HDR(_subject) = subjbuff;
    }
    else {
	p = HDR(_subject);
	if (p == NULL)
	    return "Required \"Subject\" header is missing";
	if (EQn(p, "cmsg ", 5)) {
	    HDR(_control) = p + 5;
	    if ((error = CheckControl(HDR(_control))) != NULL)
		return error;
	}
#if	0
	if (EQn(p, "Re: ", 4) && HDR(_references) == NULL)
	    return "Article starts with \"Re: \" but has no references";
#endif	/* 0 */
    }

    /* Set Message-ID */
    if (HDR(_messageid) == NULL) {
	if ((p = GenerateMessageID()) == NULL) {
	    (void)sprintf(Error, "Can't generate Message-ID, %s",
		    strerror(errno));
	    return Error;
	}
	HDR(_messageid) = p;
    }

    /* Set Path */
    if (HDR(_path) == NULL) {
	/* Note that innd will put host name here for us. */
	HDR(_path) = NEWSMASTER;
    }

    /* Reply-To; left alone. */
    /* Sender; set above. */

    /* Check Expires. */
    if (HDR(_expires) && parsedate(HDR(_expires), &Now) == -1)
	return "Can't parse \"Expires\" header";

    /* References; left alone. */
    /* Control; checked above. */

    /* Distribution. */
    if ((p = HDR(_distribution)) != NULL) {
	p = COPY(p);
	error = CheckDistribution(p);
	DISPOSE(p);
	if (error != NULL)
	    return error;
    }

    /* Set Organization */
    if (HDR(_organization) == NULL
     && (p = GetConfigValue(_CONF_ORGANIZATION)) != NULL) {
	(void)strcpy(orgbuff, p);
	HDR(_organization) = orgbuff;
    }

    /* Keywords; left alone. */
    /* Summary; left alone. */
    /* Approved; left alone. */

    /* MIME headers. */
    if (HDR(_mimeversion) == NULL
     && (p = GetConfigValue(_CONF_MIMEVERSION)) != NULL) {
	(void)strcpy(mimeversion, p);
	HDR(_mimeversion) = mimeversion;

	/* Set Content-Type */
	if (HDR(_contenttype) == NULL) {
	    if ((p = GetConfigValue(_CONF_CONTENTTYPE)) == NULL)
		return "Can't get \"Content-Type\" header";
	    (void)strcpy(mimetype, p);
	    HDR(_contenttype) = mimetype;
	}

	/* Set Content-Transfer-Encoding */
	if (HDR(_contenttransferencoding) == NULL) {
	    if ((p = GetConfigValue(_CONF_ENCODING)) == NULL)
		return "Can't get \"Content-Transfer-Encoding\" header";
	    (void)strcpy(mimeencoding, p);
	    HDR(_contenttransferencoding) = mimeencoding;
	}
    }

    /* Set Lines */
    (void)sprintf(linebuff, "%d", linecount);
    HDR(_lines) = linebuff;

    /* Supersedes; left alone. */

    /* NNTP-Posting host; set. */
    HDR(_nntpposthost) = ClientHost;

    /* Now make sure everything is there. */
    for (hp = Table; hp < ENDOF(Table); hp++)
	if (hp->Type == HTreq && hp->Value == NULL) {
	    (void)sprintf(Error, "Required \"%s\" header is missing",
		    hp->Name);
	    return Error;
	}

    return NULL;
}


#if	defined(DO_CHECK_INCLUDED_TEXT)
/*
**  See if the user has more included text than new text.  Simple-minded, but
**  reasonably effective for catching neophyte's mistakes.  A line starting
**  with > is included text.  Decrement the count on lines starting with <
**  so that we don't reject diff(1) output.
*/
STATIC STRING
CheckIncludedText(p, lines)
    register char	*p;
    register int	lines;
{
    register int	i;

    for (i = 0; ; p++) {
	switch (*p) {
	case '>':
	    i++;
	    break;
	case '<':
	    i--;
	    break;
	}
	if ((p = strchr(p + 1, '\n')) == NULL)
	    break;
    }
    if (i * 2 > lines)
	return "Article not posted -- more included text than new text";
    return NULL;
}
#endif	/* defined(DO_CHECK_INCLUDED_TEXT) */



/*
**  Try to mail an article to the moderator of the group.
*/
STATIC STRING
MailArticle(group, article)
    char		*group;
    char		*article;
{
    static char		CANTSEND[] = "Can't send text to mailer";
    register FILE	*F;
    register HEADER	*hp;
    register int	i;
    char		*address;
    char		buff[SMBUF];

    /* Try to get the address first. */
    if ((address = GetModeratorAddress(group)) == NULL) {
	(void)sprintf(Error, "No mailing address for \"%s\" -- %s",
		group, "ask your news administrator to fix this");
	return Error;
    }

    /* Now build up the command (ignore format/argument mismatch errors,
     * in case %s isn't in _PATH_SENDMAIL) and send the headers. */
    (void)sprintf(buff, _PATH_SENDMAIL, address);
    if ((F = popen(buff, "w")) == NULL)
	return "Can't start mailer";
    (void)fprintf(F, "To: %s\n", address);
    if (FLUSH_ERROR(F)) {
	(void)pclose(F);
	return CANTSEND;
    }

    /* Write the headers, a blank line, then the article. */
    for (hp = Table; hp < ENDOF(Table); hp++)
	if (hp->Value) {
	    (void)fprintf(F, "%s: %s\n", hp->Name, hp->Value);
	    if (FLUSH_ERROR(F)) {
		(void)pclose(F);
		return CANTSEND;
	    }
	}
    for (i = 0; i < OtherCount; i++) {
	(void)fprintf(F, "%s\n", OtherHeaders[i]);
	if (FLUSH_ERROR(F)) {
	    (void)pclose(F);
	    return CANTSEND;
	}
    }
    (void)fprintf(F, "\n");
    i = strlen(article);
    if (fwrite((POINTER)article, (SIZE_T)1, (SIZE_T)i, F) != i)
	return "Can't send article";
    if (FLUSH_ERROR(F)) {
	(void)pclose(F);
	return CANTSEND;
    }
    i = pclose(F);
    if (i) {
	(void)sprintf(Error, "Mailer exited with status %d -- %s",
		i, "Article might not have been mailed");
	return Error;
    }
    WasMailed = TRUE;
    return NULL;
}


/*
**  Check the newsgroups and make sure they're all valid, that none are
**  moderated, etc.
*/
STATIC STRING
ValidNewsgroups(hdr, article)
    char		*hdr;
    char		*article;
{
    static char		distbuff[SMBUF];
    register char	*groups;
    register char	*p;
    register GROUPENTRY	*gp;
    register BOOL	approved;
    struct _DDHANDLE	*h;
    BOOL		IsNewgroup;
    BOOL		FoundOne;

    p = HDR(_control);
    IsNewgroup = p && EQn(p, "newgroup", 8);
    groups = COPY(hdr);
    if ((p = strtok(groups, NGSEPS)) == NULL)
	return "Can't parse newsgroups line";

    /* Don't mail article if just checking Followup-To line. */
    approved = HDR(_approved) != NULL || article == NULL;

    Error[0] = '\0';
    FoundOne = FALSE;
    h = DDstart((FILE *)NULL, (FILE *)NULL);
    do {
#if	defined(DO_MERGE_TO_GROUPS)
	if (p[0] == 't' && p[1] == 'o' && p[2] == '.')
	    p = "to";
#endif	/* defined(DO_MERGE_TO_GROUPS) */
	if ((gp = GRPfind(p)) == NULL)
	    continue;
	FoundOne = TRUE;
	DDcheck(h, p);
	switch (gp->Flag) {
	case NF_FLAG_OK:
	    break;
	case NF_FLAG_MODERATED:
	    if (!approved) {
		DISPOSE(groups);
		DISPOSE(DDend(h));
		return MailArticle(gp->Name, article);
	    }
	    break;
	case NF_FLAG_IGNORE:
	case NF_FLAG_NOLOCAL:
	    (void)sprintf(Error, "Postings to \"%s\" are not allowed here.",
		    gp->Name);
	    break;
	case NF_FLAG_EXCLUDED:
	    /* Do NOT return an error. */
	    break;
	case NF_FLAG_ALIAS:
	    (void)sprintf(Error,
		    "The newsgroup \"%s\" has been renamed to \"%s\".\n",
		    p, gp->Alias);
	    break;
	}
    } while ((p = strtok((char *)NULL, NGSEPS)) != NULL);
    DISPOSE(groups);

    if (!FoundOne && !IsNewgroup)
	(void)sprintf(Error, "No such newsgroup as \"%s\"", p);
    if (Error[0]) {
	DISPOSE(DDend(h));
	return Error;
    }

    p = DDend(h);
    if (HDR(_distribution) == NULL && *p) {
	(void)strcpy(distbuff, p);
	HDR(_distribution) = distbuff;
    }
    DISPOSE(p);
    return NULL;
}


/*
**  Send a quit message to the server, eat its reply.
*/
STATIC void
SendQuit(FromServer, ToServer)
    FILE	*FromServer;
    FILE	*ToServer;
{
    char	buff[NNTP_STRLEN];

    (void)fprintf(ToServer, "quit\r\n");
    (void)fflush(ToServer);
    (void)fclose(ToServer);
    (void)fgets(buff, sizeof buff, FromServer);
    (void)fclose(FromServer);
}


/*
**  Offer the article to the server, return its reply.
*/
STATIC int
OfferArticle(buff, buffsize, FromServer, ToServer)
    char		*buff;
    int			buffsize;
    FILE		*FromServer;
    FILE		*ToServer;
{
    static char		CANTSEND[] = "Can't send %s to server, %s";

    (void)fprintf(ToServer, "ihave %s\r\n", HDR(_messageid));
    if (FLUSH_ERROR(ToServer)
     || fgets(buff, buffsize, FromServer) == NULL) {
	(void)sprintf(buff, CANTSEND, "IHAVE", strerror(errno));
	return -1;
    }
    return atoi(buff);
}


STRING
ARTpost(article, idbuff)
    char		*article;
    char		*idbuff;
{
    static char		CANTSEND[] = "Can't send %s to server, %s";
    register int	i;
    register char	*p;
    register char	*next;
    register HEADER	*hp;
    FILE		*ToServer;
    FILE		*FromServer;
    char		buff[NNTP_STRLEN + 2];
    STRING		error;

    /* Set up the other headers list. */
    if (OtherHeaders == NULL) {
	OtherSize = HEADER_DELTA;
	OtherHeaders = NEW(char*, OtherSize);
    }

    /* Basic processing. */
    OtherCount = 0;
    WasMailed = FALSE;
    for (hp = Table; hp < ENDOF(Table); hp++) {
	hp->Size = strlen(hp->Name);
	hp->Value = NULL;
    }
    if ((article = StripOffHeaders(article)) == NULL)
	return Error;
    for (i = 0, p = article; p; i++, p = next + 1)
	if ((next = strchr(p, '\n')) == NULL)
	    break;
#if	defined(DO_CHECK_INCLUDED_TEXT)
    if ((error = CheckIncludedText(article, i)) != NULL)
	return error;
#endif	/* defined(DO_CHECK_INCLUDED_TEXT) */
    if ((error = ProcessHeaders(i)) != NULL)
	return error;
    if (i == 0 && HDR(_control) == NULL)
	return "Article is empty";
    if ((error = ValidNewsgroups(HDR(_newsgroups), article)) != NULL
     || WasMailed)
	return error;
    if ((p = HDR(_followupto)) != NULL
     && !EQ(p, "poster")
     && (error = ValidNewsgroups(p, (char *)NULL)) != NULL)
	return error;
#if	LOCAL_MAX_ARTSIZE > 0
    if (strlen(article) > LOCAL_MAX_ARTSIZE) {
	(void)sprintf(Error,
		"Article is bigger then local limit of %ld bytes\n",
		LOCAL_MAX_ARTSIZE);
	return Error;
    }
#endif	/* LOCAL_MAX_ARTSIZE > 0 */

    /* Open a local connection to the server. */
    if (RemoteMaster)
	i = NNTPconnect(RemoteMaster, &FromServer, &ToServer, buff);
    else {
#if	defined(DO_HAVE_UNIX_DOMAIN)
	i = NNTPlocalopen(&FromServer, &ToServer, buff);
#else
	i = NNTPremoteopen(&FromServer, &ToServer, buff);
#endif	/* defined(DO_HAVE_UNIX_DOMAIN) */
    }
    if (i < 0) {
	if (buff[0])
	    (void)strcpy(Error, buff);
	else
	    (void)sprintf(Error, CANTSEND, "connect request", strerror(errno));
	return Error;
    }
    if (Tracing)
	syslog(L_TRACE, "%s post_connect %s", ClientHost, RemoteMaster);

    /* The code below has too many (void) casts for my tastes.  At least
     * they are all inside cases that are most likely never going to
     * happen -- for example, if the server crashes. */

    /* Offer article to server. */
    i = OfferArticle(buff, sizeof buff, FromServer, ToServer);
    if (i == NNTP_AUTH_NEEDED_VAL) {
	/* Send authorization. */
	if (NNTPsendpassword(RemoteMaster, FromServer, ToServer) < 0) {
	    (void)sprintf(Error, "Can't authorize with %s",
		RemoteMaster ? RemoteMaster : "innd");
	    return Error;
	}
	i = OfferArticle(buff, sizeof buff, FromServer, ToServer);
    }
    if (i != NNTP_SENDIT_VAL) {
	(void)strcpy(Error, buff);
	SendQuit(FromServer, ToServer);
	return Error;
    }
    if (Tracing)
	syslog(L_TRACE, "%s post starting", ClientHost);

    /* Write the headers and a blank line. */
    for (hp = Table; hp < ENDOF(Table); hp++)
	if (hp->Value)
	    (void)fprintf(ToServer, "%s: %s\r\n", hp->Name, hp->Value);
    for (i = 0; i < OtherCount; i++)
	(void)fprintf(ToServer, "%s\r\n", OtherHeaders[i]);
    (void)fprintf(ToServer, "\r\n");
    if (FLUSH_ERROR(ToServer)) {
	(void)sprintf(Error, CANTSEND, "headers", strerror(errno));
	(void)fclose(FromServer);
	(void)fclose(ToServer);
	return Error;
    }

    /* Send the article, get the server's reply. */
    if (NNTPsendarticle(article, ToServer, TRUE) < 0
     || fgets(buff, sizeof buff, FromServer) == NULL) {
	(void)sprintf(Error, CANTSEND, "article", strerror(errno));
	(void)fclose(FromServer);
	(void)fclose(ToServer);
	return Error;
    }

    /* Did the server want the article? */
    if (atoi(buff) != NNTP_TOOKIT_VAL) {
	(void)strcpy(Error, buff);
	SendQuit(FromServer, ToServer);
	return Error;
    }

    /* Send a quit and close down */
    SendQuit(FromServer, ToServer);
    (void)fclose(FromServer);
    (void)fclose(ToServer);
    if (idbuff)
	(void)strcpy(idbuff, HDR(_messageid));
    return NULL;
}
