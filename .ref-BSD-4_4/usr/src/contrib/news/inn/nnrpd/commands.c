/*  $Revision: 1.14 $
**
**  Miscellaneous commands.
*/
#include "nnrpd.h"
#include <time.h>


typedef struct _LISTINFO {
    STRING	File;
    STRING	Items;
    STRING	Format;
} LISTINFO;


STATIC LISTINFO		INFOactive = {
    ACTIVE, "active newsgroups",
    "Newsgroups in form \"group high low flags\""
};
STATIC LISTINFO		INFOactivetimes = {
    ACTIVETIMES, "creation times",
    "Group creations in form \"name time who\""
};
STATIC LISTINFO		INFOdistribs = {
    _PATH_NNRPDIST, "newsgroup distributions",
    "Distributions in form \"area description\""
};
STATIC LISTINFO		INFOdistribpats = {
    _PATH_DISTPATS, "distribution patterns",
    "Default distributions in form \"weight:pattern:value\""
};
STATIC LISTINFO		INFOgroups = {
    NEWSGROUPS, "newsgroup descriptions",
    "Descriptions in form \"group description\""
};
STATIC LISTINFO		INFOschema = {
    _PATH_SCHEMA, "overview schema",
    "Order of fields in overview database"
};


/* ARGSUSED */
FUNCTYPE
CMDauthinfo(ac, av)
    int		ac;
    char	*av[];
{
    static char	User[30];
    static char	Password[30];
    char	accesslist[BIG_BUFFER];

    if (caseEQ(av[1], "user")) {
	(void)strncpy(User, av[2], sizeof User - 1);
	User[sizeof User - 1] = 0;
	Reply("%d PASS required\r\n", NNTP_AUTH_NEXT_VAL);
	return;
    }

    if (!caseEQ(av[1], "pass")) {
	Reply("%d bad authinfo param\r\n", NNTP_BAD_COMMAND_VAL);
	return;
    }
    if (User[0] == '\0') {
	Reply("%d USER required\r\n", NNTP_AUTH_REJECT_VAL);
	return;
    }

    (void)strncpy(Password, av[2], sizeof Password - 1);
    Password[sizeof Password - 1] = 0;

    if (EQ(User, PERMuser) && EQ(Password, PERMpass)) {
	syslog(L_NOTICE, "%s user %s", ClientHost, User);
	Reply("%d Ok\r\n", NNTP_AUTH_OK_VAL);
	PERMneedauth = FALSE;
	PERMauthorized = TRUE;
	return;
    }
    if (PERMinfile((char *)NULL, (char *)NULL, User, Password, accesslist)) {
	PERMspecified = NGgetlist(&PERMlist, accesslist);
	syslog(L_NOTICE, "%s user %s", ClientHost, User);
	Reply("%d Ok\r\n", NNTP_AUTH_OK_VAL);
	PERMneedauth = FALSE;
	PERMauthorized = TRUE;
	return;
    }

    syslog(L_FATAL, "%s bad_auth", ClientHost);
    Reply("%d Authentication error\r\n", NNTP_ACCESS_VAL);
    ExitWithStats(1);
}


/*
**  The "DATE" command.  Part of NNTPv2.
*/
/* ARGSUSED0 */
FUNCTYPE
CMDdate(ac, av)
    int		ac;
    char	*av[];
{
    TIMEINFO	t;
    struct tm	*gmt;

    if (GetTimeInfo(&t) < 0 || (gmt = gmtime(&t.time)) == NULL) {
	Reply("%d Can't get time, %s\r\n", NNTP_TEMPERR_VAL, strerror(errno));
	return;
    }
    Reply("%d %04.4d%02.2d%02.2d%02.2d%02.2d%02.2d\r\n",
	NNTP_DATE_FOLLOWS_VAL,
	gmt->tm_year + 1900, gmt->tm_mon + 1, gmt->tm_mday,
	gmt->tm_hour, gmt->tm_min, gmt->tm_sec);
}


/*
**  List active newsgroups, newsgroup descriptions, and distributions.
*/
/* ARGSUSED0 */
FUNCTYPE
CMDlist(ac, av)
    int			ac;
    char		*av[];
{
    register QIOSTATE	*qp;
    register char	*p;
    register char	*save;
    char		*grplist[2];
    LISTINFO		*lp;

    p = av[1];
    if (p == NULL || caseEQ(p, "active")) {
	if (!GetGroupList()) {
	    syslog(L_NOTICE, "%s cant getgroupslist for list %m", ClientHost);
	    Reply("%d Group update failed. Try later.\r\n", NNTP_TEMPERR_VAL);
	    ExitWithStats(1);
	}
	lp = &INFOactive;
    }
    else if (caseEQ(p, "active.times"))
	lp = &INFOactivetimes;
    else if (caseEQ(p, "distributions"))
	lp = &INFOdistribs;
    else if (caseEQ(p, "distrib.pats"))
	lp = &INFOdistribpats;
    else if (caseEQ(p, "newsgroups"))
	lp = &INFOgroups;
    else if (caseEQ(p, "overview.fmt"))
	lp = &INFOschema;
    else {
	Reply("%s\r\n", NNTP_SYNTAX_USE);
	return;
    }

    if ((qp = QIOopen(lp->File, QIO_BUFFER)) == NULL) {
	syslog(L_ERROR, "%s cant fopen %s %m", ClientHost, lp->File);
	Reply("%d No list of %s available.\r\n", NNTP_TEMPERR_VAL, lp->Items);
	return;
    }

    Reply("%d %s.\r\n", NNTP_LIST_FOLLOWS_VAL, lp->Format);
    if (!PERMspecified && !PERMdefault) {
	/* Optmize for unlikely case of no permissions and FALSE default. */
	(void)QIOclose(qp);
	Printf(".\r\n");
    }

    /* Set up group list terminator. */
    grplist[1] = NULL;

    /* Read lines, ignore long ones. */
    while ((p = QIOread(qp)) != NULL) {
	if (lp == &INFOdistribs || lp == &INFOdistribpats) {
	    Printf("%s\r\n", p);
	    continue;
	}
	if (lp == &INFOschema) {
	    if (*p != '\0' && *p != '#')
		Printf("%s\r\n", p);
	    continue;
	}
	if ((save = strchr(p, ' ')) != NULL)
	    *save = '\0';
	if (PERMspecified) {
	    grplist[0] = p;
	    if (!PERMmatch(PERMdefault, PERMlist, grplist))
		continue;
	}
	if (save != NULL)
	    *save = ' ';
	Printf("%s\r\n", p);
    }
    QIOclose(qp);

    Printf(".\r\n");
}


/*
**  Handle the "mode" command.
*/
/* ARGSUSED */
FUNCTYPE
CMDmode(ac, av)
    int		ac;
    char	*av[];
{
    if (caseEQ(av[1], "reader"))
	Reply("%d %s InterNetNews NNRP server %s ready (%s).\r\n",
	       PERMcanpost ? NNTP_POSTOK_VAL : NNTP_NOPOSTOK_VAL,
	       MyHostName, INNVersion(),
	       PERMcanpost ? "posting ok" : "no posting");
    else
	Reply("%d What?\r\n", NNTP_BAD_COMMAND_VAL);
}


/*
**  Display new newsgroups since a given date and time for specified
**  <distributions>.
*/
FUNCTYPE
CMDnewgroups(ac, av)
    int			ac;
    char		*av[];
{
    static char		USAGE[] =
	"NEWGROUPS yymmdd hhmmss [\"GMT\"] [<distributions>]";
    static char		**distlist;
    register char	*p;
    register char	*q;
    register char	**dp;
    register QIOSTATE	*qp;
    register GROUPENTRY	*gp;
    BOOL		All;
    long		date;
    char		*grplist[2];

    /* Parse the date. */
    date = NNTPtoGMT(av[1], av[2]);
    if (date < 0) {
	Reply("%d Usage: %s\r\n", NNTP_SYNTAX_VAL, USAGE);
	return;
    }
    ac -= 3;
    av += 3;
    if (ac > 0 && caseEQ(*av, "GMT")) {
	av++;
	ac--;
    }
    else
	date = LOCALtoGMT(date);

    if (ac == 0)
	All = TRUE;
    else {
	if (!ParseDistlist(&distlist, *av) < 0) {
	    Reply("%d Bad distribution list %s:\r\n", NNTP_SYNTAX_VAL, *av);
	    return;
	}
	All = FALSE;
    }

    if (!GetGroupList()) {
	syslog(L_NOTICE, "%s cant getgroupslist for list %m", ClientHost);
	Reply("%d Group update failed. Try later.\r\n", NNTP_TEMPERR_VAL);
	ExitWithStats(1);
    }

    if ((qp = QIOopen(ACTIVETIMES, QIO_BUFFER)) == NULL) {
	syslog(L_ERROR, "%s cant fopen %s %m",
	    ClientHost, ACTIVETIMES);
	Reply("%d Cannot open newsgroup date file.\r\n", NNTP_TEMPERR_VAL);
	return;
    }
    Reply("%d New newsgroups follow.\r\n", NNTP_NEWGROUPS_FOLLOWS_VAL);

    /* Read the file, ignoring long lines. */
    while ((p = QIOread(qp)) != NULL) {
	if ((q = strchr(p, ' ')) == NULL)
	    continue;
	*q++ = '\0';
	if (atol(q) < date || (gp = GRPfind(p)) == NULL)
	    continue;

	if (PERMspecified) {
	    grplist[0] = p;
	    grplist[1] = NULL;
	    if (!PERMmatch(PERMdefault, PERMlist, grplist))
		continue;
	}
	else if (!PERMdefault)
	    continue;

	if (!All) {
	    if ((q = strchr(p, '.')) == NULL)
		continue;
	    for (*q = '\0', dp = distlist; *dp; dp++)
		if (EQ(p, *dp)) {
		    *q = '.';
		    break;
		}
	    if (*dp == NULL)
		continue;
	}
	Printf("%s %ld %ld %c%s\r\n",
	    p, (long)gp->High, (long)gp->Low,
	    gp->Flag, gp->Alias ? gp->Alias : "");
    }
    QIOclose(qp);
    Printf(".\r\n");
}


/*
**  Post an article.
*/
/* ARGSUSED */
FUNCTYPE
CMDpost(ac, av)
    int		ac;
    char	*av[];
{
    static char		*article;
    static int		size;
    register char	*p;
    register char	*end;
    register int	longline;
    register READTYPE	r;
    int			i;
    long		l;
    STRING		response;
    char		idbuff[SMBUF];

    if (!PERMcanpost) {
	syslog(L_NOTICE, "%s noperm post without permission", ClientHost);
	Reply("%s\r\n", NNTP_CANTPOST);
	return;
    }

    /* Start at beginning of buffer. */
    if (article == NULL) {
	size = 4096;
	article = NEW(char, size);
    }
    p = article;
    end = &article[size];

    Reply("%d Ok\r\n", NNTP_START_POST_VAL);
    (void)fflush(stdout);

    for (l = 0, longline = 0; ; l++) {
	/* Need more room? */
	if (end - p < ART_LINE_MALLOC) {
	    i = p - article;
	    size += ART_LINE_MALLOC;
	    RENEW(article, char, size);
	    end = &article[size];
	    p = i + article;
	}

	/* Read line, process bad cases. */
	switch (r = READline(p, ART_LINE_LENGTH, DEFAULT_TIMEOUT)) {
	default:
	    syslog(L_ERROR, "%s internal %d in post", ClientHost, r);
	    /* FALLTHROUGH */
	case RTtimeout:
	    syslog(L_ERROR, "%s timeout in post", ClientHost);
	    Printf("%d timeout after %d seconds, closing connection\r\n",
		   NNTP_TEMPERR_VAL, DEFAULT_TIMEOUT);
	    ExitWithStats(1);
	    /* NOTREACHED */
	case RTeof:
	    syslog(L_ERROR, "%s eof in post", ClientHost);
	    ExitWithStats(1);
	    /* NOTREACHED */
	case RTlong:
	    if (longline == 0)
		longline = l + 1;
	    continue;
	case RTok:
	    break;
	}

	/* Process normal text. */
	if (*p != '.') {
	    p += strlen(p);
	    *p++ = '\n';
	    *p = '\0';
	    continue;
	}

	/* Got a leading period; see if it's the terminator. */
	if (p[1] == '\0') {
	    *p = '\0';
	    break;
	}

	/* "Arnold, please copy down over the period for me." */
	while ((p[0] = p[1]) != '\0')
	    p++;
	*p++ = '\n';
	*p = '\0';
    }

    if (longline) {
	syslog(L_NOTICE, "%s toolong in post", ClientHost);
	Printf("%d Line %d too long\r\n", NNTP_POSTFAIL_VAL, longline);
	POSTrejected++;
	return;
    }

    /* Send the article to the server. */
    response = ARTpost(article, idbuff);
    if (response == NULL) {
	syslog(L_NOTICE, "%s post ok %s", ClientHost, idbuff);
	Reply("%s\r\n", NNTP_POSTEDOK);
	POSTreceived++;
    }
    else {
	if ((p = strchr(response, '\r')) != NULL)
	    *p = '\0';
	if ((p = strchr(response, '\n')) != NULL)
	    *p = '\0';
	syslog(L_NOTICE, "%s post failed %s", ClientHost, response);
	Reply("%d %s\r\n", NNTP_POSTFAIL_VAL, response);
	POSTrejected++;
    }
}


/*
**  The "xpath" command.  An uncommon extension.
*/
/* ARGSUSED */
FUNCTYPE
CMDxpath(ac, av)
    int		ac;
    char	*av[];
{
    char	*p;

    if ((p = HISgetent(av[1], TRUE)) == NULL)
	Reply("%d Don't have it\r\n", NNTP_DONTHAVEIT_VAL);
    else
	Reply("%d %s\r\n", NNTP_NOTHING_FOLLOWS_VAL, p);
}
