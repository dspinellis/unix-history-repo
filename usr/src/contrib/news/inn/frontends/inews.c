/*  $Revision: 1.33 $
**
**  Send an article (prepared by someone on the local site) to the
**  master news server.
*/
#include "configdata.h"
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <pwd.h>
#include <grp.h>
#if	defined(DO_NEED_TIME)
#include <time.h>
#endif	/* defined(DO_NEED_TIME) */
#include <sys/time.h>
#include <fcntl.h>
#include "nntp.h"
#include "paths.h"
#include "libinn.h"
#include "clibrary.h"
#include "macros.h"


#define FLUSH_ERROR(F)		(fflush((F)) == EOF || ferror((F)))
#define LPAREN			'('	/* For vi :-) */
#define HEADER_DELTA		20
#define GECOSTERM(c)		\
	    ((c) == ',' || (c) == ';' || (c) == ':' || (c) == LPAREN)


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


STATIC BOOL	Dump;
STATIC BOOL	Revoked;
STATIC BOOL	Spooling;
STATIC char	SPOOLNEWS[] = _PATH_SPOOLNEWS;
STATIC char	**OtherHeaders;
STATIC char	NGSEPS[] = NG_SEPARATOR;
STATIC char	SIGSEP[] = SIG_SEPARATOR;
STATIC FILE	*FromServer;
STATIC FILE	*ToServer;
STATIC int	OtherCount;
STATIC int	OtherSize;
STATIC char	*Exclusions = "";
STATIC STRING	BadDistribs[] = {
    BAD_DISTRIBS
};

STATIC HEADER	Table[] = {
    /* 	Name			Canset	Type	*/
    {	"Path",			TRUE,	HTstd },
#define _path		 0
    {	"From",			TRUE,	HTstd },
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
    {	"Also-Control",		TRUE,	HTstd },
#define _alsocontrol	17
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
**  Send the server a quit message, wait for a reply.
*/
STATIC NORETURN
QuitServer(x)
    int		x;
{
    char	buff[NNTP_STRLEN + 2];
    char	*p;

    if (Spooling)
	exit(x);
    if (x)
	(void)fprintf(stderr, "(Article not posted.)\n");
    (void)fprintf(ToServer, "quit\r\n");
    if (FLUSH_ERROR(ToServer)) {
	(void)fprintf(stderr, "Can't send quit to server, %s",
		strerror(errno));
	exit(1);
    }
    if (fgets(buff, sizeof buff, FromServer) == NULL) {
	(void)fprintf(stderr, "Warning -- server did not reply to quit, %s",
		strerror(errno));
	exit(1);
    }
    if ((p = strchr(buff, '\r')) != NULL)
	*p = '\0';
    if ((p = strchr(buff, '\n')) != NULL)
	*p = '\0';
    if (atoi(buff) != NNTP_GOODBYE_ACK_VAL) {
	(void)fprintf(stderr, "Server didn't reply to quit properly:\n\t%s\n",
		buff);
	exit(1);
    }
    (void)fclose(FromServer);
    (void)fclose(ToServer);
    exit(x);
}


/*
**  Print and error message (with errno) and exit with an error code.
*/
STATIC NORETURN
PerrorExit(ShouldQuit, s)
    BOOL	ShouldQuit;
    char	*s;
{
    (void)fprintf(stderr, "%s, %s.\n", s, strerror(errno));
    if (ShouldQuit)
	QuitServer(1);
    exit(1);
}



/*
**  Flush a stdio FILE; exit if there are any errors.
*/
STATIC void
SafeFlush(F)
    FILE	*F;
{
    if (FLUSH_ERROR(F))
	PerrorExit(TRUE, "Can't send text to server");
}


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
**  to the start of the next one.  Handles continuations.
*/
STATIC char *
NextHeader(p)
    register char	*p;
{
    for ( ; ; p++) {
	if ((p = strchr(p, '\n')) == NULL) {
	    (void)fprintf(stderr, "Article is all headers.\n");
	    QuitServer(1);
	}
	if (!ISWHITE(p[1])) {
	    *p = '\0';
	    return p + 1;
	}
    }
}


/*
**  Strip any headers off the article and dump them into the table.
*/
STATIC char *
StripOffHeaders(article)
    char		*article;
{
    register char	*p;
    register char	*q;
    register HEADER	*hp;
    register char	c;
    register int	i;

    /* Set up the other headers list. */
    OtherSize = HEADER_DELTA;
    OtherHeaders = NEW(char*, OtherSize);
    OtherCount = 0;

    /* Scan through buffer, a header at a time. */
    for (i = 0, p = article; ; i++) {

	if ((q = strchr(p, ':')) == NULL) {
	    (void)fprintf(stderr, "No colon in header line \"%.20s...\"\n",
		    p);
	    QuitServer(1);
	}
	if (q[1] == '\n' && !ISWHITE(q[2])) {
	    /* Empty header; ignore this one, get next line. */
	    p = NextHeader(p);
	    if (*p == '\n')
		break;
	}

	if (q[1] != '\0' && !ISWHITE(q[1])) {
	    if ((q = strchr(q, '\n')) != NULL)
		*q = '\0';
	    (void)fprintf(stderr, "No space after colon in \"%.20s...\"\n", p);
	    QuitServer(1);
	}

	/* See if it's a known header. */
	c = CTYPE(islower, *p) ? toupper(*p) : *p;
	for (hp = Table; hp < ENDOF(Table); hp++)
	    if (c == hp->Name[0]
	     && p[hp->Size] == ':'
	     && ISWHITE(p[hp->Size + 1])
	     && caseEQn(p, hp->Name, hp->Size)) {
		if (hp->Type == HTobs) {
		    (void)fprintf(stderr, "Obsolete \"%s\" header.\n",
			    hp->Name);
		    QuitServer(1);
		}
		if (hp->Value) {
		    (void)fprintf(stderr, "Duplicate \"%s\" header.\n",
			    hp->Name);
		    QuitServer(1);
		}
		for (q = &p[hp->Size + 1]; ISWHITE(*q); q++)
		    continue;
		hp->Value = q;
		break;
	    }

	/* Too many headers? */
	if (++i > 5 * HEADER_DELTA) {
	    (void)fprintf(stderr, "More than %d lines of header.\n", i);
	    QuitServer(1);
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
	p = NextHeader(p);
	if (*p == '\n')
	    break;
    }

    return p + 1;
}



/*
**  See if the user is allowed to cancel the indicated message.  Assumes
**  that the Sender or From line has already been filled in.
*/
STATIC void
CheckCancel(msgid, JustReturn)
    char		*msgid;
    BOOL		JustReturn;
{
    char		localfrom[SMBUF];
    register char	*p;
    char		buff[BUFSIZ];
    char		remotefrom[SMBUF];

    /* Ask the server for the article. */
    (void)fprintf(ToServer, "head %s\r\n", msgid);
    SafeFlush(ToServer);
    if (fgets(buff, sizeof buff, FromServer) == NULL
     || atoi(buff) != NNTP_HEAD_FOLLOWS_VAL) {
	if (JustReturn)
	    return;
	(void)fprintf(stderr, "Server has no such article.\n");
	QuitServer(1);
    }

    /* Read the headers, looking for the From or Sender. */
    remotefrom[0] = '\0';
    while (fgets(buff, sizeof buff, FromServer) != NULL) {
	if ((p = strchr(buff, '\r')) != NULL)
	    *p = '\0';
	if ((p = strchr(buff, '\n')) != NULL)
	    *p = '\0';
	if (buff[0] == '.' && buff[1] == '\0')
	    break;
	if (EQn(buff, "Sender:", 7))
	    (void)strcpy(remotefrom, TrimSpaces(&buff[7]));
	else if (remotefrom[0] == '\0' && EQn(buff, "From:", 5))
	    (void)strcpy(remotefrom, TrimSpaces(&buff[5]));
    }
    if (remotefrom[0] == '\0') {
	if (JustReturn)
	    return;
	(void)fprintf(stderr, "Article is garbled.\n");
	QuitServer(1);
    }
    HeaderCleanFrom(remotefrom);

    /* Get the local user. */
    (void)strcpy(localfrom, HDR(_sender) ? HDR(_sender) : HDR(_from));
    HeaderCleanFrom(localfrom);

    /* Is the right person cancelling? */
    if (!EQ(localfrom, remotefrom)) {
	(void)fprintf(stderr,
		"Article was posted by \"%s\" and you are \"%s\".\n",
		remotefrom, localfrom);
	QuitServer(1);
    }
}


/*
**  See if the user is the news administrator.
*/
STATIC BOOL
AnAdministrator(name, group)
    char		*name;
    int			group;
{
    struct passwd	*pwp;
    struct group	*grp;
    char		**mem;
    char		*p;

    if (Revoked)
	return FALSE;

    /* Find out who we are. */
    if ((pwp = getpwnam(NEWSUSER)) == NULL)
	/* Silent falure; clients might not have the group. */
	return FALSE;
    if (getuid() == pwp->pw_uid)
	return TRUE;

    /* See if the we're in the right group. */
    if ((grp = getgrnam(NEWSGID)) == NULL || (mem = grp->gr_mem) == NULL)
	/* Silent falure; clients might not have the group. */
	return FALSE;
    if (group == grp->gr_gid)
	return TRUE;
    while ((p = *mem++) != NULL)
	if (EQ(name, p))
	    return TRUE;
    return FALSE;
}


/*
**  Check the control message, and see if it's legit.
*/
STATIC void
CheckControl(ctrl, pwp)
    char		*ctrl;
    struct passwd	*pwp;
{
    register char	*p;
    register char	*q;
    char		save;
    char		name[SMBUF];

    /* Snip off the first word. */
    for (p = ctrl; ISWHITE(*p); p++)
	continue;
    for (ctrl = p; *p && !ISWHITE(*p); p++)
	continue;
    if (p == ctrl) {
	(void)fprintf(stderr, "Empty control message.\n");
	QuitServer(1);
    }
    save = *p;
    *p = '\0';

    if (EQ(ctrl, "cancel")) {
	for (q = p + 1; ISWHITE(*q); q++)
	    continue;
	if (*q == '\0') {
	    (void)fprintf(stderr, "Message-ID missing in cancel.\n");
	    QuitServer(1);
	}
	if (!Spooling)
	    CheckCancel(q, FALSE);
    }
    else if (EQ(ctrl, "checkgroups")
	  || EQ(ctrl, "ihave")
	  || EQ(ctrl, "sendme")
	  || EQ(ctrl, "newgroup")
	  || EQ(ctrl, "rmgroup")
	  || EQ(ctrl, "sendsys")
	  || EQ(ctrl, "senduuname")
	  || EQ(ctrl, "version")) {
	(void)strcpy(name, pwp->pw_name);
	if (!AnAdministrator(name, (int)pwp->pw_gid)) {
	    (void)fprintf(stderr,
		    "Ask your news administrator to do the \"%s\" for you.\n",
		    ctrl);
	    QuitServer(1);
	}
    }
    else {
	(void)fprintf(stderr, "\"%s\" is not a valid control message.\n",
		ctrl);
	QuitServer(1);
    }
    *p = save;
}



/*
**  Parse the GECOS field to get the user's full name.  This comes Sendmail's
**  buildfname routine.  Ignore leading stuff like "23-" "stuff]-" or
**  "stuff -" as well as trailing whitespace, or anything that comes after
**  a comma, semicolon, or in parentheses.  This seems to strip off most of
**  the UCB or ATT stuff people fill out the entries with.  Also, turn &
**  into the login name, with perhaps an initial capital.  (Everyone seems
**  to hate that, but everyone also supports it.)
*/
STATIC char *
FormatUserName(pwp, node)
    struct passwd	*pwp;
    char		*node;
{
    char	buff[BUFSIZ];
    char	outbuff[SMBUF];
    char	*out;
    char	*p;

#if	defined(DONT_MUNGE_GECOS)
    (void)strcpy(outbuff, pwp->pw_gecos);
#else
    p = pwp->pw_gecos;
    if (*p == '*')
	p++;
    for (out = outbuff; *p && !GECOSTERM(*p); p++) {
	if (*p == '&') {
	    (void)strcpy(out, pwp->pw_name);
	    if (CTYPE(islower, *out)
	     && (out == outbuff || !isalpha(out[-1])))
		*out = toupper(*out);
	    while (*out)
		out++;
	}
	else if (*p == '-'
	      && p > pwp->pw_gecos
	      && (isdigit(p[-1]) || isspace(p[-1]) || p[-1] == ']'))
	    out = outbuff;
	else
	    *out++ = *p;
    }
    *out = '\0';
#endif	/* defined(DONT_MINGE_GECOS) */

    out = TrimSpaces(outbuff);
    if (out[0])
	(void)sprintf(buff, "%s@%s (%s)", pwp->pw_name, node, out);
    else
	(void)sprintf(buff, "%s@%s", pwp->pw_name, node);
    return COPY(buff);
}


/*
**  Check the Distribution header, and exit on error.
*/
STATIC void
CheckDistribution(p)
    register char	*p;
{
    static char		SEPS[] = " \t,";
    register STRING	*dp;

    if ((p = strtok(p, SEPS)) == NULL) {
	(void)fprintf(stderr, "Can't parse Distribution line.\n");
	QuitServer(1);
    }
    do {
	for (dp = BadDistribs; *dp; dp++)
	    if (wildmat(p, *dp)) {
		(void)fprintf(stderr, "Illegal distribution \"%s\"\n", p);
		QuitServer(1);
	    }
    } while ((p = strtok((char *)NULL, SEPS)) != NULL);
}


/*
**  Process all the headers.  FYI, they're done in RFC-order.
*/
STATIC void
ProcessHeaders(AddOrg, linecount, pwp)
    BOOL		AddOrg;
    int			linecount;
    struct passwd	*pwp;
{
    static char		MONTHS[] = "JanFebMarAprMayJunJulAugSepOctNovDec";
    static char		PATHFLUFF[] = PATHMASTER;
    static char		SIGNS[] = "+-";
    register HEADER	*hp;
    register char	*p;
    TIMEINFO		Now;
    struct tm		*tm;
    char		buff[SMBUF];
    char		from[SMBUF];
    int			i;
    long		zone;

    /* Do some preliminary fix-ups. */
    for (hp = Table; hp < ENDOF(Table); hp++) {
	if (!hp->CanSet && hp->Value) {
	    (void)fprintf(stderr, "Can't set system \"%s\" header.\n",
		    hp->Name);
	    QuitServer(1);
	}
	if (hp->Value) {
	    hp->Value = TrimSpaces(hp->Value);
	    if (*hp->Value == '\0')
		hp->Value = NULL;
	}
    }

    /* Set From or Sender. */
    if ((p = GetConfigValue(_CONF_FROMHOST)) == NULL)
	PerrorExit(TRUE, "Can't get host name");
    if (HDR(_from) == NULL)
	HDR(_from) = FormatUserName(pwp, p);
    else {
	(void)sprintf(buff, "%s@%s", pwp->pw_name, p);
	(void)strcpy(from, HDR(_from));
	HeaderCleanFrom(from);
	if (!EQ(from, buff))
	    HDR(_sender) = COPY(buff);
    }

    /* Set Date. */
    if (GetTimeInfo(&Now) < 0)
	PerrorExit(TRUE, "Can't get the time");
    if ((tm = localtime(&Now.time)) == NULL)
	PerrorExit(TRUE, "Can't convert to local time");

    /* The %0n.nd contruct from <kre@munnari.oz.au> is clever.  Modern
     * printf's treat it %02 (two digits wide) .2 (zero-fill to at least
     * two digits), while old versions treat it as %02 (zero-fill two
     * digits wide) .2 (noise).  You might want to check this on your
     * system. */
    if (Now.tzone < 0) {
	p = &SIGNS[0];
	zone = -Now.tzone;
    }
    else {
	p = &SIGNS[1];
	zone = Now.tzone;
    }
    (void)sprintf(buff, "%d %3.3s %d %02.2d:%02.2d:%02.2d %c%04.4d",
	tm->tm_mday, &MONTHS[3 * tm->tm_mon], 1900 + tm->tm_year,
	tm->tm_hour, tm->tm_min, tm->tm_sec,
	*p, (int)((zone / 60) * 100 + (zone % 60)));
    HDR(_date) = COPY(buff);

    /* Newsgroups are checked later. */

    /* Set Subject; Control overrides the subject. */
    if (HDR(_control)) {
	CheckControl(HDR(_control), pwp);
	HDR(_subject) = NEW(char, 5 + strlen(HDR(_control)) + 1);
	(void)sprintf(HDR(_subject), "cmsg %s", HDR(_control));
    }
    else {
	p = HDR(_subject);
	if (p == NULL) {
	    (void)fprintf(stderr,
		    "Required \"Subject\" header is missing or empty.\n");
	    QuitServer(1);
	}
	if (EQn(p, "cmsg ", 5)) {
	    HDR(_control) = p + 5;
	    CheckControl(HDR(_control), pwp);
	}
	else if (HDR(_alsocontrol))
	    CheckControl(HDR(_alsocontrol), pwp);
#if	0
	if (EQn(p, "Re: ", 4) && HDR(_references) == NULL) {
	    (void)fprintf(stderr,
		    "Article starts with \"Re: \" but has no references.\n");
	    QuitServer(1);
	}
#endif	/* 0 */
    }

    /* Set Message-ID */
    if (HDR(_messageid) == NULL) {
	if ((p = GenerateMessageID()) == NULL) {
	    (void)fprintf(stderr, "Can't generate Message-ID, %s\n",
		    strerror(errno));
	    QuitServer(1);
	}
	HDR(_messageid) = COPY(p);
    }
    else if ((p = strchr(HDR(_messageid), '@')) == NULL
	   || strchr(++p, '@') != NULL) {
	(void)fprintf(stderr, "Message-ID must have exactly one '@'\n");
	QuitServer(1);
    }

    /* Set Path */
    if (HDR(_path) == NULL) {
	i = strlen(Exclusions) + STRLEN(PATHFLUFF);
#if	defined(DO_INEWS_PATH)
	if ((p = GetFileConfigValue(_CONF_PATHHOST)) != NULL) {
	    i += strlen(p) + 1;
	    HDR(_path) = NEW(char, i + 1);
	    if (*p)
		(void)sprintf(HDR(_path), "%s%s!%s", Exclusions, p, PATHFLUFF);
	    else
		(void)sprintf(HDR(_path), "%s%s", Exclusions, PATHFLUFF);
	}
	else if (GetFileConfigValue(_CONF_SERVER) != NULL) {
	    if ((p = GetFQDN()) == NULL) {
		(void)fprintf(stderr, "Can't get host name, %s\n",
			strerror(errno));
		QuitServer(1);
	    }
	    i += strlen(p) + 1;
	    HDR(_path) = NEW(char, i + 1);
	    (void)sprintf(HDR(_path), "%s%s!%s", Exclusions, p, PATHFLUFF);
	}
	else {
	    HDR(_path) = NEW(char, i + 1);
	    (void)sprintf(HDR(_path), "%s%s", Exclusions, PATHFLUFF);
	}
#else
	HDR(_path) = NEW(char, i + 1);
	(void)sprintf(HDR(_path), "%s%s", Exclusions, PATHFLUFF);
#endif	/* defined(DO_INEWS_PATH) */
    }

    /* Reply-To; left alone. */
    /* Sender; set above. */
    /* Followup-To; checked with Newsgroups. */

    /* Check Expires. */
    if (HDR(_expires) && parsedate(HDR(_expires), &Now) == -1) {
	(void)fprintf(stderr, "Can't parse \"%s\" as an expiration date.\n",
		HDR(_expires));
	QuitServer(1);
    }

    /* References; left alone. */
    /* Control; checked above. */

    /* Distribution. */
    if ((p = HDR(_distribution)) != NULL) {
	p = COPY(p);
	CheckDistribution(p);
	DISPOSE(p);
    }

    /* Set Organization. */
    if (AddOrg
     && HDR(_organization) == NULL
     && (p = GetConfigValue(_CONF_ORGANIZATION)) != NULL) {
	HDR(_organization) = COPY(p);
    }

    /* Keywords; left alone. */
    /* Summary; left alone. */
    /* Approved; left alone. */

    /* Set Lines */
    (void)sprintf(buff, "%d", linecount);
    HDR(_lines) = COPY(buff);

    /* Check Supersedes. */
    if (HDR(_supersedes))
	CheckCancel(HDR(_supersedes), TRUE);

    /* Now make sure everything is there. */
    for (hp = Table; hp < ENDOF(Table); hp++)
	if (hp->Type == HTreq && hp->Value == NULL) {
	    (void)fprintf(stderr,
		    "Required \"%s\" header is missing or empty.\n",
		    hp->Name);
	    QuitServer(1);
	}
}


/*
**  Try to append $HOME/.signature to the article.  When in doubt, exit
**  out in order to avoid postings like "Sorry, I forgot my .signature
**  -- here's the article again."
*/
STATIC char *
AppendSignature(UseMalloc, article, homedir, linesp)
    BOOL	UseMalloc;
    char	*article;
    char	*homedir;
    int		*linesp;
{
    static char	NOSIG[] = "Can't add your .signature (%s), article not posted";
    int		i;
    int		length;
    char	*p;
    char	buff[BUFSIZ];
    FILE	*F;

    /* Open the file. */
    *linesp = 0;
    (void)sprintf(buff, "%s/.signature", homedir);
    if ((F = fopen(buff, "r")) == NULL) {
	if (errno == ENOENT)
	    return article;
	(void)fprintf(stderr, NOSIG, strerror(errno));
	QuitServer(1);
    }

    /* Read it in. */
    length = fread((POINTER)buff, (SIZE_T)1, (SIZE_T)sizeof buff - 2, F);
    i = feof(F);
    (void)fclose(F);
    if (length == 0) {
	(void)fprintf(stderr, NOSIG, "empty file");
	QuitServer(1);
    }
    if (length < 0) {
	(void)fprintf(stderr, NOSIG, strerror(errno));
	QuitServer(1);
    }
    if (length == sizeof buff - 2 && !i) {
	(void)fprintf(stderr, NOSIG, "too big");
	QuitServer(1);
    }

    /* Make sure the buffer ends with \n\0. */
    if (buff[length - 1] != '\n')
	buff[length++] = '\n';
    buff[length] = '\0';

    /* Count the lines. */
    for (i = 0, p = buff; (p = strchr(p, '\n')) != NULL; p++)
	if (++i > SIG_MAXLINES) {
	    (void)fprintf(stderr, NOSIG, "too many lines");
	    QuitServer(1);
	}
    *linesp = 1 + i;

    /* Grow the article to have the signature. */
    i = strlen(article);
    if (UseMalloc) {
	p = NEW(char, i + (sizeof SIGSEP - 1) + length + 1);
	(void)strcpy(p, article);
	article = p;
    }
    else
	RENEW(article, char, i + (sizeof SIGSEP - 1) + length + 1);
    (void)strcpy(&article[i], SIGSEP);
    (void)strcpy(&article[i + sizeof SIGSEP - 1], buff);
    return article;
}


#if	defined(DO_CHECK_INCLUDED_TEXT)
/*
**  See if the user has more included text than new text.  Simple-minded, but
**  reasonably effective for catching neophyte's mistakes.  A line starting
**  with > is included text.  Decrement the count on lines starting with <
**  so that we don't reject diff(1) output.
*/
STATIC void
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
    if (i * 2 > lines) {
	(void)fprintf(stderr,
		"Article not posted -- more included text than new text\n");
	QuitServer(1);
    }
}
#endif	/* defined(DO_CHECK_INCLUDED_TEXT) */



/*
**  Try to mail an article to the moderator of the group.
*/
STATIC BOOL
MailArticle(group, article)
    char		*group;
    char		*article;
{
    register FILE	*F;
    register HEADER	*hp;
    register int	i;
    char		*address;
    char		buff[SMBUF];

    /* Try to get the address first. */
    if ((address = GetModeratorAddress(group)) == NULL) {
	(void)fprintf(stderr,
		"The \"%s\" newsgroup is moderated, but has no address;\n",
		group);
	(void)fprintf(stderr, "ask your news administrator to fix this.\n");
	return FALSE;
    }

    /* Say what we're going to do. */
    (void)printf(
	"The \"%s\" newsgroup is moderated.  Your article will not be\n",
	group);
    (void)printf("posted, but mailed to the moderator for approval.\n");

    /* Now build up the command (ignore format/argument mismatch errors,
     * in case %s isn't in _PATH_SENDMAIL) and send the headers. */
    (void)sprintf(buff, _PATH_SENDMAIL, address);
    if ((F = popen(buff, "w")) == NULL)
	PerrorExit(TRUE, "Can't start mailer");
    (void)fprintf(F, "To: %s\n", address);
    SafeFlush(F);

    /* Write the headers, a blank line, then the article. */
    for (hp = Table; hp < ENDOF(Table); hp++)
	if (hp->Value) {
	    (void)fprintf(F, "%s: %s\n", hp->Name, hp->Value);
	    SafeFlush(F);
	}
    for (i = 0; i < OtherCount; i++) {
	(void)fprintf(F, "%s\n", OtherHeaders[i]);
	SafeFlush(F);
    }
    (void)fprintf(F, "\n");
    i = strlen(article);
    if (fwrite((POINTER)article, (SIZE_T)1, (SIZE_T)i, F) != i)
	PerrorExit(TRUE, "Can't send article");
    SafeFlush(F);
    i = pclose(F);
    if (i) {
	(void)fprintf(stderr, "Mailer exited with status %d;\n", i);
	(void)fprintf(stderr, "Article might not have been mailed.\n");
	return FALSE;
    }
    return TRUE;
}


/*
**  Check the newsgroups, make sure they're all valid, that none are
**  moderated, etc.
*/
STATIC BOOL
ValidNewsgroups(hdr, F, article)
    char		*hdr;
    FILE		*F;
    char		*article;
{
    register char	*groups;
    register char	*p;
    register int	i;
    BOOL		approved;
    BOOL		mailed;
    BOOL		FoundOne;
    char		*group;
    char		*q;
    char		buff[SMBUF];
    struct _DDHANDLE	*h;
    BOOL		IsNewgroup;

    p = HDR(_control);
    IsNewgroup = p && EQn(p, "newgroup", 8);

    groups = COPY(hdr);
    if ((p = strtok(groups, NGSEPS)) == NULL) {
	(void)fprintf(stderr, "Can't parse newsgroups line.\n");
	return FALSE;
    }

    /* Don't mail article if just checking Followup-To line. */
    approved = HDR(_approved) != NULL || article == NULL;
    FoundOne = FALSE;

    h = DDstart(FromServer, ToServer);
    do {
#if	defined(DO_MERGE_TO_GROUPS)
	if (p[0] == 't' && p[1] == 'o' && p[2] == '.')
	    p = "to";
#endif	/* defined(DO_MERGE_TO_GROUPS) */
	i = strlen(p);
	(void)fseek(F, (OFFSET_T)0, SEEK_SET);
	while (fgets(buff, sizeof buff, F) != NULL)
	    if (buff[0] == *p && EQn(buff, p, i) && buff[i] == ' ')
		break;
	if (feof(F))
	    continue;
	FoundOne = TRUE;
	DDcheck(h, p);

	/* Skip past the newsgroup name, the high and low counts, to find
	 * the flags. */
	for (group = p, p = &buff[i]; *p == ' ' || CTYPE(isdigit, *p); p++)
	    continue;

	switch (*p) {
	case NF_FLAG_OK:
	    break;
	case NF_FLAG_MODERATED:
	    if (Dump)
		(void)fprintf(stderr,
			"%s is moderated -- article would be mailed\n",
			group);
	    else if (!approved) {
		mailed = MailArticle(group, article);
		DISPOSE(DDend(h));
		QuitServer(mailed ? 0 : 1);
	    }
	    break;
	case NF_FLAG_IGNORE:
	case NF_FLAG_NOLOCAL:
	    (void)fprintf(stderr, "Postings to \"%s\" are not allowed here.\n",
		    group);
	    DISPOSE(DDend(h));
	    return FALSE;
	case NF_FLAG_EXCLUDED:
	    (void)fprintf(stderr, "Warning:  \"%s\" is rejected here.\n",
		    group);
	    /* Do NOT return false. */
	    break;
	case NF_FLAG_ALIAS:
	    if ((q = strchr(p, '\n')) != NULL)
		*q = '\0';
	    (void)fprintf(stderr,
		    "The newsgroup \"%s\" has been renamed to \"%s\".\n",
		    group, p + 1);
	    DISPOSE(DDend(h));
	    return FALSE;
	}

    } while ((p = strtok((char *)NULL, NGSEPS)) != NULL);

    if (!FoundOne && !IsNewgroup) {
	(void)fprintf(stderr, "No such newsgroups as \"%s\".\n", hdr);
	DISPOSE(DDend(h));
	return FALSE;
    }

    /* Set default distribution. */
    p = DDend(h);
    if (HDR(_distribution) == NULL && *p)
	HDR(_distribution) = p;

    return TRUE;
}



/*
**  Read stdin into a string and return it.  Can't use ReadInDescriptor
**  since that will fail if stdin is a tty.
*/
STATIC char *
ReadStdin()
{
    register int	size;
    register char	*p;
    char		*article;
    register char	*end;
    register int	i;

    size = BUFSIZ;
    article = NEW(char, size);
    end = &article[size - 3];
    for (p = article; (i = getchar()) != EOF; *p++ = (char)i)
	if (p == end) {
	    RENEW(article, char, size + BUFSIZ);
	    p = &article[size - 3];
	    size += BUFSIZ;
	    end = &article[size - 3];
	}

    /* Force a \n terminator. */
    if (p > article && p[-1] != '\n')
	*p++ = '\n';
    *p = '\0';
    return article;
}



/*
**  Offer the article to the server, return its reply.
*/
STATIC int
OfferArticle(buff, Authorized)
    char		*buff;
    BOOL		Authorized;
{
    (void)fprintf(ToServer, "post\r\n");
    SafeFlush(ToServer);
    if (fgets(buff, NNTP_STRLEN, FromServer) == NULL)
	PerrorExit(TRUE,
	    Authorized ? "Can't offer article to server (authorized)"
		       : "Can't offer article to server");
    return atoi(buff);
}


/*
**  Make a temporary filename that is unlikely to collide with mktemp
**  or cause problems for sites with 14-character filename limits.
*/
STATIC void
TempName(dir, buff)
    char	*dir;
    char	*buff;
{
    time_t	now;

    (void)time(&now);
    now &= 0xFFFFFFFF;
    (void)sprintf(buff, "%s/%08.8lxXXXXXX", dir, (long)now);
    (void)mktemp(buff);
}


/*
**  Spool article to temp file.
*/
STATIC void
Spoolit(article, Length, deadfile)
    char		*article;
    SIZE_T		Length;
    char		*deadfile;
{
    static char		SPOOLTEMP[] = _PATH_SPOOLTEMP;
    register HEADER	*hp;
    register FILE	*F;
    register int	i;
    char		temp[BUFSIZ];
    char		buff[BUFSIZ];
    struct stat		Sb;

    /* Try to write to the spool dir, else the deadfile. */
    if ((stat(SPOOLNEWS, &Sb) >= 0 && S_ISDIR(Sb.st_mode))
     || (F = xfopena(deadfile)) == NULL) {
	TempName(SPOOLTEMP, temp);
	(void)umask(0);
	if ((i = open(temp, O_WRONLY | O_CREAT, BATCHFILE_MODE)) < 0
	 || (F = fdopen(i, "w")) == NULL)
	    PerrorExit(FALSE, "Can't create spool file");
	deadfile = NULL;
    }

    /* Write the headers and a blank line. */
    for (hp = Table; hp < ENDOF(Table); hp++)
	if (hp->Value)
	    (void)fprintf(F, "%s: %s\n", hp->Name, hp->Value);
    for (i = 0; i < OtherCount; i++)
	(void)fprintf(F, "%s\n", OtherHeaders[i]);
    (void)fprintf(F, "\n");
    if (FLUSH_ERROR(stdout))
	PerrorExit(FALSE, "Can't write headers");

    /* Write the article and exit. */
    if (fwrite((POINTER)article, (SIZE_T)1, Length, F) != Length)
	PerrorExit(FALSE, "Can't write article");
    SafeFlush(F);
    if (fclose(F) == EOF)
	PerrorExit(FALSE, "Can't close spool file");

    if (deadfile == NULL) {
	/* Put the file in a good place. */
	TempName(SPOOLNEWS, buff);
	if (rename(temp, buff) < 0)
	    PerrorExit(FALSE, "Can't rename spool file");
    }
}


/*
**  Print usage message and exit.
*/
STATIC NORETURN
Usage()
{
    (void)fprintf(stderr, "Usage: inews [-D] [-h] [header_flags] [article]\n");
    /* Don't call QuitServer here -- connection isn't open yet. */
    exit(1);
}


int
main(ac, av)
    int			ac;
    char		*av[];
{
    static char		NOCONNECT[] = "Can't connect to server";
    register int	i;
    register char	*p;
    register HEADER	*hp;
    int			j;
    int			Mode;
    int			SigLines;
    FILE		*F;
    struct passwd	*pwp;
    char		*article;
    char		*deadfile;
    char		buff[NNTP_STRLEN + 2];
    char		SpoolMessage[NNTP_STRLEN + 2];
    BOOL		DoSignature;
    BOOL		AddOrg;
    SIZE_T		Length;

    /* Find out who we are. */
    if ((i = geteuid()) < 0)
	PerrorExit(TRUE, "Can't get your user ID");
    if ((pwp = getpwuid((UID_T)i)) == NULL)
	PerrorExit(TRUE, "Can't get your password entry");

    /* Set defaults. */
    Mode = '\0';
    Dump = FALSE;
    DoSignature = TRUE;
    AddOrg = TRUE;
    (void)umask(NEWSUMASK);

    /* Parse JCL. */
    while ((i = getopt(ac, av, "DNAVWORShx:a:c:d:e:f:n:r:t:F:o:w:")) != EOF)
	switch (i) {
	default:
	    Usage();
	    /* NOTREACHED */
	case 'D':
	case 'N':
	    Dump = TRUE;
	    break;
	case 'A':
	case 'V':
	case 'W':
	    /* Ignore C News options. */
	    break;
	case 'O':
	    AddOrg = FALSE;
	    break;
	case 'R':
	    Revoked = TRUE;
	    break;
	case 'S':
	    DoSignature = FALSE;
	    break;
	case 'h':
	    Mode = i;
	    break;
	case 'x':
	    Exclusions = NEW(char, strlen(optarg) + 1 + 1);
	    (void)sprintf(Exclusions, "%s!", optarg);
	    break;
	/* Header lines that can be specified on the command line. */
	case 'a':	HDR(_approved) = optarg;		break;
	case 'c':	HDR(_control) = optarg;			break;
	case 'd':	HDR(_distribution) = optarg;		break;
	case 'e':	HDR(_expires) = optarg;			break;
	case 'f':	HDR(_from) = optarg;			break;
	case 'n':	HDR(_newsgroups) = optarg;		break;
	case 'r':	HDR(_replyto) = optarg;			break;
	case 't':	HDR(_subject) = optarg;			break;
	case 'F':	HDR(_references) = optarg;		break;
	case 'o':	HDR(_organization) = optarg;		break;
	case 'w':	HDR(_followupto) = optarg;		break;
	}
    ac -= optind;
    av += optind;

    /* Parse positional arguments; at most one, the input file. */
    switch (ac) {
    default:
	Usage();
	/* NOTREACHED */
    case 0:
	/* Read stdin. */
	article = ReadStdin();
	break;
    case 1:
	/* Read named file. */
	article = ReadInFile(av[0], (struct stat *)NULL);
	if (article == NULL)
	    PerrorExit(FALSE, "Can't read input file");
	break;
    }

    /* Try to open a connection to the server. */
    if (NNTPremoteopen(&FromServer, &ToServer, buff) < 0) {
	Spooling = TRUE;
	if ((p = strchr(buff, '\n')) != NULL)
	    *p = '\0';
	if ((p = strchr(buff, '\r')) != NULL)
	    *p = '\0';
	(void)strcpy(SpoolMessage, buff[0] ? buff : NOCONNECT);
	(void)sprintf(buff, "%s/dead.article", pwp->pw_dir);
	deadfile = COPY(buff);
    }
    else {
	/* See if we can post. */
	i = atoi(buff);

	/* Tell the server we're posting. */
	setbuf(FromServer, NEW(char, BUFSIZ));
	setbuf(ToServer, NEW(char, BUFSIZ));
	(void)fprintf(ToServer, "mode reader\r\n");
	SafeFlush(ToServer);
	if (fgets(buff, NNTP_STRLEN, FromServer) == NULL)
	    PerrorExit(TRUE, "Can't tell server we're reading");
	if ((j = atoi(buff)) != NNTP_BAD_COMMAND_VAL)
	    i = j;

	if (i != NNTP_POSTOK_VAL) {
	    (void)fprintf(stderr, "You do not have permission to post.\n");
	    QuitServer(1);
	    exit(1);
	}
    }

    /* Basic processing. */
    for (hp = Table; hp < ENDOF(Table); hp++)
	hp->Size = strlen(hp->Name);
    if (Mode == 'h')
	article = StripOffHeaders(article);
    for (i = 0, p = article; (p = strchr(p, '\n')) != NULL; i++, p++)
	continue;
#if	defined(DO_CHECK_INCLUDED_TEXT)
    CheckIncludedText(article, i);
#endif	/* defined(DO_CHECK_INCLUDED_TEXT) */
    if (DoSignature)
	article = AppendSignature(Mode == 'h', article, pwp->pw_dir, &SigLines);
    else
	SigLines = 0;
    ProcessHeaders(AddOrg, i + SigLines, pwp);
    Length = strlen(article);
#if	LOCAL_MAX_ARTSIZE > 0
    if (Length > LOCAL_MAX_ARTSIZE) {
	(void)fprintf(stderr,
		"Article is bigger then local limit of %ld bytes\n",
		LOCAL_MAX_ARTSIZE);
	QuitServer(1);
    }
#endif	/* LOCAL_MAX_ARTSIZE > 0 */

    /* Do final checks. */
    if (i == 0 && HDR(_control) == NULL) {
	(void)fprintf(stderr, "Article is empty.\n");
	QuitServer(1);
    }
    for (hp = Table; hp < ENDOF(Table); hp++)
	if (hp->Value && (int)strlen(hp->Value) + hp->Size > NNTP_STRLEN) {
	    (void)fprintf(stderr, "\"%s\" header is too long.\n", hp->Name);
	    QuitServer(1);
	}
    for (i = 0; i < OtherCount; i++)
	if ((int)strlen(OtherHeaders[i]) > NNTP_STRLEN) {
	    (void)fprintf(stderr,
		    "Header too long (%d characters max):\n\t%40.40s...\n",
		    NNTP_STRLEN, OtherHeaders[i]);
	    QuitServer(1);
	}

    /* Check the newsgroups. */
    if ((F = CAlistopen(FromServer, ToServer, (char *)NULL)) != NULL) {
	if (!ValidNewsgroups(HDR(_newsgroups), F, article)) {
	    CAclose();
	    QuitServer(1);
	}
	if ((p = HDR(_followupto)) != NULL
	 && !EQ(p, "poster")
	 && !ValidNewsgroups(p, F, (char *)NULL)) {
	    CAclose();
	    QuitServer(1);
	}
	CAclose();
    }
    else if (!Spooling)
	PerrorExit(TRUE, "Can't get list of newsgroups");

    if (Dump) {
	/* Write the headers and a blank line. */
	for (hp = Table; hp < ENDOF(Table); hp++)
	    if (hp->Value)
		(void)printf("%s: %s\n", hp->Name, hp->Value);
	for (i = 0; i < OtherCount; i++)
	    (void)printf("%s\n", OtherHeaders[i]);
	(void)printf("\n");
	if (FLUSH_ERROR(stdout))
	    PerrorExit(TRUE, "Can't write headers");

	/* Write the article and exit. */
	if (fwrite((POINTER)article, (SIZE_T)1, Length, stdout) != Length)
	    PerrorExit(TRUE, "Can't write article");
	SafeFlush(stdout);
	QuitServer(0);
    }

    if (Spooling) {
	(void)fprintf(stderr, "Warning %s -- Article will be spooled.\n",
		SpoolMessage);
	Spoolit(article, Length, deadfile);
	exit(0);
    }

    /* Article is prepared, offer it to the server. */
    i = OfferArticle(buff, FALSE);
    if (i == NNTP_AUTH_NEEDED_VAL) {
	/* Posting not allowed, try to authorize. */
	if (NNTPsendpassword((char *)NULL, FromServer, ToServer) < 0)
	    PerrorExit(TRUE, "Authorization error");
	i = OfferArticle(buff, TRUE);
    }
    if (i != NNTP_START_POST_VAL) {
	(void)fprintf(stderr, "Server doesn't want the article:\n\t%s\n",
		buff);
	QuitServer(1);
    }

    /* Write the headers, a blank line, then the article. */
    for (hp = Table; hp < ENDOF(Table); hp++)
	if (hp->Value)
	    (void)fprintf(ToServer, "%s: %s\r\n", hp->Name, hp->Value);
    for (i = 0; i < OtherCount; i++)
	(void)fprintf(ToServer, "%s\r\n", OtherHeaders[i]);
    (void)fprintf(ToServer, "\r\n");
    if (NNTPsendarticle(article, ToServer, TRUE) < 0)
	PerrorExit(TRUE, "Can't send article to server");
    SafeFlush(ToServer);

    if (fgets(buff, sizeof buff, FromServer) == NULL)
	PerrorExit(TRUE, "No reply from server after sending the article");
    if ((p = strchr(buff, '\r')) != NULL)
	*p = '\0';
    if ((p = strchr(buff, '\n')) != NULL)
	*p = '\0';
    if (atoi(buff) != NNTP_POSTEDOK_VAL) {
	(void)fprintf(stderr, "Can't send article to the server:\n\t%s\n",
		buff);
	QuitServer(1);
    }

    /* Close up. */
    QuitServer(0);
    /* NOTREACHED */
}
