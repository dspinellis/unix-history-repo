/*  $Revision: 1.13 $
**
**  Article-related routines.
*/
#include "nnrpd.h"


/*
**  Data structures for use in ARTICLE/HEAD/BODY/STAT common code.
*/
typedef enum _SENDTYPE {
    STarticle,
    SThead,
    STbody,
    STstat
} SENDTYPE;

typedef struct _SENDDATA {
    SENDTYPE	Type;
    int		ReplyCode;
    STRING	Item;
} SENDDATA;


/*
**  Information about the schema of the news overview files.
*/
typedef struct _ARTOVERFIELD {
    char	*Header;
    int		Length;
    BOOL	HasHeader;
} ARTOVERFIELD;


STATIC char		ARTnotingroup[] = NNTP_NOTINGROUP;
STATIC char		ARTnoartingroup[] = NNTP_NOARTINGRP;
STATIC char		ARTnocurrart[] = NNTP_NOCURRART;
STATIC QIOSTATE		*ARTqp;
STATIC ARTOVERFIELD	*ARTfields;
STATIC int		ARTfieldsize;
STATIC SENDDATA		SENDbody = {
    STbody,	NNTP_BODY_FOLLOWS_VAL,		"body"
};
STATIC SENDDATA		SENDarticle = {
    STarticle,	NNTP_ARTICLE_FOLLOWS_VAL,	"article"
};
STATIC SENDDATA		SENDstat = {
    STstat,	NNTP_NOTHING_FOLLOWS_VAL,	"status"
};
STATIC SENDDATA		SENDhead = {
    SThead,	NNTP_HEAD_FOLLOWS_VAL,		"head"
};


/*
**  Overview state information.
*/
STATIC QIOSTATE		*OVERqp;		/* Open overview file	*/
STATIC char		*OVERline;		/* Current line		*/
STATIC ARTNUM		OVERarticle;		/* Current article	*/
STATIC int		OVERopens;		/* Number of opens done	*/


/*
**  Read the overview schema.
*/
void
ARTreadschema()
{
    static char			SCHEMA[] = _PATH_SCHEMA;
    register FILE		*F;
    register char		*p;
    register ARTOVERFIELD	*fp;
    register int		i;
    char			buff[SMBUF];

    /* Open file, count lines. */
    if ((F = fopen(SCHEMA, "r")) == NULL)
	return;
    for (i = 0; fgets(buff, sizeof buff, F) != NULL; i++)
	continue;
    (void)fseek(F, (OFFSET_T)0, SEEK_SET);
    ARTfields = NEW(ARTOVERFIELD, i + 1);

    /* Parse each field. */
    for (fp = ARTfields; fgets(buff, sizeof buff, F) != NULL; ) {
	/* Ignore blank and comment lines. */
	if ((p = strchr(buff, '\n')) != NULL)
	    *p = '\0';
	if ((p = strchr(buff, COMMENT_CHAR)) != NULL)
	    *p = '\0';
	if (buff[0] == '\0')
	    continue;
	if ((p = strchr(buff, ':')) != NULL) {
	    *p++ = '\0';
	    fp->HasHeader = EQ(p, "full");
	}
	else
	    fp->HasHeader = FALSE;
	fp->Header = COPY(buff);
	fp->Length = strlen(buff);
	fp++;
    }
    ARTfieldsize = fp - ARTfields;
    (void)fclose(F);
}


/*
**  If we have an article open, close it.
*/
void
ARTclose()
{
    if (ARTqp) {
	QIOclose(ARTqp);
	ARTqp = NULL;
    }
}


/*
**  Get the Message-ID from a file.
*/
STATIC void
ARTgetmsgid(qp, id)
    register QIOSTATE	*qp;
    char		*id;
{
    register char	*p;
    register char	*q;

    for (*id = '\0'; (p = QIOread(qp)) != NULL && *p != '\0'; ) {
	if (*p != 'M' && *p != 'm')
	    continue;
	if ((q = strchr(p, ' ')) == NULL)
	    continue;
	*q++ = '\0';
	if (caseEQ(p, "Message-ID:")) {
	    (void)strcpy(id, q);
	    break;
	}
    }
    (void)QIOrewind(qp);
}


/*
**  If the article name is valid, open it and stuff in the ID.
*/
STATIC BOOL
ARTopen(name, id)
    char		*name;
    char		*id;
{
    static ARTNUM	save_artnum;
    static char		save_artid[BIG_BUFFER];
    struct stat		Sb;

    /* Re-use article if it's the same one. */
    if (ARTqp != NULL) {
	if (save_artnum == atol(name) && QIOrewind(ARTqp) != -1) {
	    if (id)
		(void)strcpy(id, save_artid);
	    return TRUE;
	}
	QIOclose(ARTqp);
    }

    /* Open it, make sure it's a regular file. */
    if ((ARTqp = QIOopen(name, QIO_BUFFER)) == NULL)
	return FALSE;
    if (fstat(QIOfileno(ARTqp), &Sb) < 0 || !S_ISREG(Sb.st_mode)) {
	QIOclose(ARTqp);
	ARTqp = NULL;
	return FALSE;
    }
    CloseOnExec(QIOfileno(ARTqp), TRUE);

    save_artnum = atol(name);
    ARTgetmsgid(ARTqp, save_artid);
    (void)strcpy(id, save_artid);
    return TRUE;
}


/*
**  Open the article for a given Message-ID.
*/
STATIC QIOSTATE *
ARTopenbyid(msg_id, ap)
    char	*msg_id;
    ARTNUM	*ap;
{
    QIOSTATE	*qp;
    char	*p;
    char	*q;

    *ap = 0;
    if ((p = HISgetent(msg_id, FALSE)) == NULL)
	return NULL;
    if ((qp = QIOopen(p, QIO_BUFFER)) == NULL)
	return NULL;
    CloseOnExec(QIOfileno(qp), TRUE);
    if ((q = strrchr(p, '/')) != NULL)
	*q++ = '\0';
    if (GRPlast[0] && EQ(p, GRPlast))
	*ap = atol(q);
    return qp;
}


/*
**  Send a (part of) a file to stdout, doing newline and dot conversion.
*/
STATIC void
ARTsend(qp, what)
    register QIOSTATE	*qp;
    SENDTYPE		what;
{
    register char	*p;

    ARTcount++;
    GRParticles++;

    /* Get the headers. */
    for ( ; ; ) {
	p = QIOread(qp);
	if (p == NULL) {
	    if (QIOtoolong(qp))
		continue;
	    break;
	}
	if (*p == '\0')
	    break;
	if (what == STbody)
	    continue;
	Printf("%s%s\r\n", *p == '.' ? "." : "", p);
    }

    if (what == SThead) {
	Printf(".\r\n");
	return;
    }

    if (what == STarticle)
	Printf("\r\n");
    for ( ; ; ) {
	p = QIOread(qp);
	if (p == NULL) {
	    if (QIOtoolong(qp))
		continue;
	    break;
	}
	Printf("%s%s\r\n", *p == '.' ? "." : "", p);
    }
    Printf(".\r\n");
}


/*
**  Find an article number in the article array via a binary search;
**  return -1 if not found.  Cache last hit to make linear lookups
**  faster.
*/
STATIC int
ARTfind(i)
    register ARTNUM	i;
{
    register ARTNUM	*bottom;
    register ARTNUM	*middle;
    register ARTNUM	*top;

    if (ARTsize == 0)
	return -1;

    top = &ARTnumbers[ARTsize - 1];
    if (ARTcache && ++ARTcache <= top && *ARTcache <= i) {
	if (*ARTcache == i)
	    return ARTcache - ARTnumbers;
	bottom = ARTcache;
    }
    else {
	ARTcache = NULL;
	bottom = ARTnumbers;
    }

    for ( ; ; ) {
	if (i < *bottom || i > *top)
	    break;

	middle = bottom + (top - bottom) / 2;
	if (i == *middle) {
	    /* Found it; update cache. */
	    ARTcache = middle;
	    return middle - ARTnumbers;
	}

	if (i > *middle)
	    bottom = middle + 1;
	else
	    top = middle;
    }
    return -1;
}


/*
**  Ask the innd server for the article.  Only called from CMDfetch,
**  and only if history file is buffered.  Common case:  "oops, cancel
**  that article I just posted."
*/
STATIC QIOSTATE *
ARTfromboss(what, id)
    SENDDATA		*what;
    char		*id;
{
    FILE		*FromServer;
    FILE		*ToServer;
    QIOSTATE		*qp;
    char		buff[NNTP_STRLEN + 2];
    char		*name;
    char		*p;
    BOOL		more;

    /* If we can, open the connection. */
    if (NNTPlocalopen(&FromServer, &ToServer, (char *)NULL) < 0)
	return NULL;

    /* Send the query to the server. */
    qp = NULL;
    (void)fprintf(ToServer, "XPATH %s\r\n", id);
    (void)fflush(ToServer);
    if (ferror(ToServer))
	goto QuitClose;

    /* Get the reply; article exist? */
    if (fgets(buff, sizeof buff, FromServer) == NULL
     || atoi(buff) == NNTP_DONTHAVEIT_VAL)
	goto QuitClose;

    /* Yes.  Be quick if just doing a stat. */
    if (what == &SENDstat) {
	qp = QIOopen("/dev/null", 0);
	goto QuitClose;
    }

    /* Clean up response. */
    if ((p = strchr(buff, '\r')) != NULL)
	*p = '\0';
    if ((p = strchr(buff, '\n')) != NULL)
	*p = '\0';

    /* Loop over all filenames until we can open one. */
    for (name = buff; *name; name = p + 1) {
	/* Snip off next name, turn dots to slashes. */
	for (p = name; ISWHITE(*p); p++)
	    continue;
	for (name = p; *p && *p != ' '; p++)
	    if (*p == '.')
		*p = '/';
	more = *p == ' ';
	if (more)
	    *p = '\0';
	if ((qp = QIOopen(name, QIO_BUFFER)) != NULL || !more)
	    break;
    }

    /* Send quit, read server's reply, close up and return. */
  QuitClose:
    (void)fprintf(ToServer, "quit\r\n");
    (void)fclose(ToServer);
    (void)fgets(buff, sizeof buff, FromServer);
    (void)fclose(FromServer);
    return qp;
}


/*
**  Fetch part or all of an article and send it to the client.
*/
FUNCTYPE
CMDfetch(ac, av)
    int			ac;
    char		*av[];
{
    char		buff[SMBUF];
    char		idbuff[BIG_BUFFER];
    SENDDATA		*what;
    register QIOSTATE	*qp;
    register BOOL	ok;
    ARTNUM		art;

    /* Find what to send; get permissions. */
    ok = PERMcanread;
    switch (*av[0]) {
    default:
	what = &SENDbody;
	break;
    case 'a': case 'A':
	what = &SENDarticle;
	break;
    case 's': case 'S':
	what = &SENDstat;
	break;
    case 'h': case 'H':
	what = &SENDhead;
	/* Poster might do a "head" command to verify the article. */
	ok = PERMcanread || PERMcanpost;
	break;
    }

    if (!ok) {
	Reply("%s\r\n", NOACCESS);
	return;
    }

    /* Requesting by Message-ID? */
    if (ac == 2 && av[1][0] == '<') {
	if ((qp = ARTopenbyid(av[1], &art)) == NULL
	 && (qp = ARTfromboss(what, av[1])) == NULL) {
	    Reply("%d No such article\r\n", NNTP_DONTHAVEIT_VAL);
	    return;
	}
	if (!PERMartok(qp)) {
	    QIOclose(qp);
	    Reply("%s\r\n", NOACCESS);
	    return;
	}
	Reply("%d %ld %s %s\r\n", what->ReplyCode, art, what->Item, av[1]);
	if (what->Type != STstat)
	    ARTsend(qp, what->Type);
	QIOclose(qp);
	return;
    }

    /* Trying to read. */
    if (GRPcount == 0) {
	Reply("%s\r\n", ARTnotingroup);
	return;
    }

    /* Default is to get current article, or specified article. */
    if (ac == 1) {
	if (ARTindex < 0 || ARTindex >= ARTsize) {
	    Reply("%s\r\n", ARTnocurrart);
	    return;
	}
	(void)sprintf(buff, "%ld", ARTnumbers[ARTindex]);
    }
    else {
	if (strspn(av[1], "0123456789") != strlen(av[1])) {
	    Reply("%s\r\n", ARTnoartingroup);
	    return;
	}
	(void)strcpy(buff, av[1]);
    }

    /* Move forward until we can find one. */
    while (!ARTopen(buff, idbuff)) {
	if (ac > 1 || ++ARTindex >= ARTsize) {
	    Reply("%s\r\n", ARTnoartingroup);
	    return;
	}
	(void)sprintf(buff, "%ld", ARTnumbers[ARTindex]);
    }

    Reply("%d %s %s %s\r\n", what->ReplyCode, buff, idbuff, what->Item);
    if (what->Type != STstat)
	ARTsend(ARTqp, what->Type);
    if (ac > 1)
	ARTindex = ARTfind((ARTNUM)atol(buff));
}


/*
**  Go to the next or last (really previous) article in the group.
*/
FUNCTYPE
CMDnextlast(ac, av)
    int		ac;
    char	*av[];
{
    char	buff[SPOOLNAMEBUFF];
    char	idbuff[SMBUF];
    int		save;
    BOOL	next;
    int		delta;
    int		errcode;
    STRING	message;

    if (!PERMcanread) {
	Reply("%s\r\n", NOACCESS);
	return;
    }
    if (GRPcount == 0) {
	Reply("%s\r\n", ARTnotingroup);
	return;
    }
    if (ARTindex < 0 || ARTindex >= ARTsize) {
	Reply("%s\r\n", ARTnocurrart);
	return;
    }

    next = (av[0][0] == 'n' || av[0][0] == 'N');
    if (next) {
	delta = 1;
	errcode = NNTP_NONEXT_VAL;
	message = "next";
    }
    else {
	delta = -1;
	errcode = NNTP_NOPREV_VAL;
	message = "previous";
    }

    save = ARTindex;
    ARTindex += delta;
    if (ARTindex < 0 || ARTindex >= ARTsize) {
	Reply("%d No %s to retrieve.\r\n", errcode, message);
	ARTindex = save;
	return;
    }

    (void)sprintf(buff, "%ld", ARTnumbers[ARTindex]);
    while (!ARTopen(buff, idbuff)) {
	ARTindex += delta;
	if (ARTindex < 0 || ARTindex >= ARTsize) {
	    Reply("%d No %s article to retrieve.\r\n", errcode, message);
	    ARTindex = save;
	    return;
	}
	(void)sprintf(buff, "%ld", ARTnumbers[ARTindex]);
    }

    Reply("%d %s %s Article retrieved; request text separately.\r\n",
	   NNTP_NOTHING_FOLLOWS_VAL, buff, idbuff);

    if (ac > 1)
	ARTindex = ARTfind((ARTNUM)atol(buff));
}


/*
**  Return the header from the specified file, or NULL if not found.
**  We can estimate the Lines header, if that's what's wanted.
*/
STATIC char *
GetHeader(qp, header, IsLines)
    register QIOSTATE	*qp;
    register char	*header;
    BOOL		IsLines;
{
    static char		buff[40];
    register char	*p;
    register char	*q;
    struct stat		Sb;

    for ( ; ; ) {
	if ((p = QIOread(qp)) == NULL) {
	    if (QIOtoolong(qp))
		continue;
	    break;
	}
	if (*p == '\0')
	    /* End of headers. */
	    break;
	if (ISWHITE(*p) || (q = strchr(p, ':')) == NULL)
	    /* Continuation or bogus (shouldn't happen) line; ignore. */
	    continue;
	*q = '\0';
	if (caseEQ(header, p))
	    return *++q ? q + 1 : NULL;
    }

    if (IsLines && fstat(QIOfileno(qp), &Sb) >= 0) {
	/* Lines estimation taken from Tor Lillqvist <tml@tik.vtt.fi>'s
	 * posting <TML.92Jul10031233@hemuli.tik.vtt.fi> in
	 * news.sysadmin. */
	(void)sprintf(buff, "%d",
	    (int)(6.4e-8 * Sb.st_size * Sb.st_size + 0.023 * Sb.st_size - 12));
	return buff;
    }
    return NULL;
}


STATIC BOOL
CMDgetrange(ac, av, rp)
    int			ac;
    char		*av[];
    register ARTRANGE	*rp;
{
    register char	*p;

    if (GRPcount == 0) {
	Reply("%s\r\n", ARTnotingroup);
	return FALSE;
    }

    if (ac == 1) {
	/* No argument, do only current article. */
	if (ARTindex < 0 || ARTindex >= ARTsize) {
	    Reply("%s\r\n", ARTnocurrart);
	    return FALSE;
	}
	rp->High = rp->Low = ARTnumbers[ARTindex];
	return TRUE;
    }

    /* Got just a single number? */
    if ((p = strchr(av[1], '-')) == NULL) {
	rp->Low = rp->High = atol(av[1]);
	return TRUE;
    }

    /* Parse range. */
    *p++ = '\0';
    rp->Low = atol(av[1]);
    if (ARTsize) {
	if (*p == '\0' || (rp->High = atol(p)) < rp->Low)
	    /* "XHDR 234-0 header" gives everything to the end. */
	    rp->High = ARTnumbers[ARTsize - 1];
	else if (rp->High > ARTnumbers[ARTsize - 1])
	    rp->High = ARTnumbers[ARTsize - 1];
	if (rp->Low < ARTnumbers[0])
	    rp->Low = ARTnumbers[0];
    }
    else
	/* No articles; make sure loops don't run. */
	rp->High = rp->Low ? rp->Low - 1 : 0;
    return TRUE;
}


/*
**  Return a field from the overview line or NULL on error.  Return a copy
**  since we might be re-using the line later.
*/
STATIC char *
OVERGetHeader(p, field)
    register char	*p;
    int			field;
{
    static char		*buff;
    static int		buffsize;
    register int	i;
    ARTOVERFIELD	*fp;
    char		*next;

    /* Skip leading headers. */
    for (fp = &ARTfields[field - 1]; --field >= 0 && *p; p++)
	if ((p = strchr(p, '\t')) == NULL)
	    return NULL;
    if (*p == '\0')
	return NULL;

    if (fp->HasHeader)
	p += fp->Length + 2;

    /* Figure out length; get space. */
    if ((next = strchr(p, '\t')) != NULL)
	i = next - p;
    else
	i = strlen(p);
    if (buffsize == 0) {
	buffsize = i;
	buff = NEW(char, buffsize + 1);
    }
    else if (buffsize < i) {
	buffsize = i;
	RENEW(buff, char, buffsize + 1);
    }

    (void)strncpy(buff, p, i);
    buff[i] = '\0';
    return buff;
}


/*
**  Open an OVERVIEW file.
*/
STATIC BOOL
OVERopen()
{
    char	name[SPOOLNAMEBUFF];

    /* Already open? */
    if (OVERqp != NULL)
	/* Don't rewind -- we are probably going forward via repeated
	 * NNTP commands. */
	return TRUE;

    /* Failed here before? */
    if (OVERopens++)
	return FALSE;

    OVERline = NULL;
    OVERarticle = 0;
    (void)sprintf(name, "%s/%s/%s", _PATH_OVERVIEWDIR, GRPlast, _PATH_OVERVIEW);
    OVERqp = QIOopen(name, QIO_BUFFER);
    return OVERqp != NULL;
}


/*
**  Close the OVERVIEW file.
*/
void
OVERclose()
{
    if (OVERqp != NULL) {
	QIOclose(OVERqp);
	OVERqp = NULL;
	OVERopens = 0;
    }
}


/*
**  Return the overview data for an article or NULL on failure.
**  Assumes that what we return is never modified.
*/
STATIC char *
OVERfind(artnum)
    ARTNUM	artnum;
{
    if (OVERqp == NULL)
	return NULL;

    if (OVERarticle > artnum) {
	(void)QIOrewind(OVERqp);
	OVERarticle = 0;
	OVERline = NULL;
    }

    for ( ; OVERarticle < artnum; OVERarticle = atol(OVERline))
	if ((OVERline = QIOread(OVERqp)) == NULL) {
	    if (QIOtoolong(OVERqp))
		continue;
	    /* Don't close file; we may rewind. */
	    return NULL;
	}

    return OVERarticle == artnum ? OVERline : NULL;
}


/*
**  Read an article and create an overview line without the trailing
**  newline.  Returns pointer to static space or NULL on error.
*/
STATIC char *
OVERgen(name)
    char			*name;
{
    static ARTOVERFIELD		*Headers;
    static char			*buff;
    static int			buffsize;
    register ARTOVERFIELD	*fp;
    register ARTOVERFIELD	*hp;
    register QIOSTATE		*qp;
    register char		*colon;
    register char		*line;
    register char		*p;
    register int		i;
    register int		size;
    register int		ov_size;
    register long		lines;
    struct stat			Sb;
    long			t;
    char			value[10];

    /* Open article. */
    if ((qp = QIOopen(name, QIO_BUFFER)) == NULL)
	return NULL;
    if ((p = strrchr(name, '/')) != NULL)
	name = p + 1;

    /* Set up place to store headers. */
    if (Headers == NULL) {
	Headers = NEW(ARTOVERFIELD, ARTfieldsize);
	for (hp = Headers, i = ARTfieldsize; --i >= 0; hp++)
	    hp->Length = 0;
    }
    for (hp = Headers, i = ARTfieldsize; --i >= 0; hp++)
	hp->HasHeader = FALSE;

    for ( ; ; ) {
	/* Read next line. */
	if ((line = QIOread(qp)) == NULL) {
	    if (QIOtoolong(qp))
		continue;
	    /* Error or EOF (in headers!?); shouldn't happen. */
	    QIOclose(qp);
	    return NULL;
	}

	/* End of headers? */
	if (*line == '\0')
	    break;

	/* See if we want this header. */
	fp = ARTfields;
	for (hp = Headers, i = ARTfieldsize; --i >= 0; hp++, fp++) {
	    colon = &line[fp->Length];
	    if (*colon != ':')
		continue;
	    *colon = '\0';
	    if (!caseEQ(line, fp->Header)) {
		*colon = ':';
		continue;
	    }
	    *colon = ':';
	    if (fp->HasHeader)
		p = line;
	    else
		/* Skip colon and whitespace, store value. */
		for (p = colon; *++p && ISWHITE(*p); )
		    continue;
	    size = strlen(p);
	    if (hp->Length == 0) {
		hp->Length = size;
		hp->Header = NEW(char, hp->Length + 1);
	    }
	    else if (hp->Length < size) {
		hp->Length = size;
		RENEW(hp->Header, char, hp->Length + 1);
	    }
	    (void)strcpy(hp->Header, p);
	    for (p = hp->Header; *p; p++)
		if (*p == '\t' || *p == '\n')
		    *p = ' ';
	    hp->HasHeader = TRUE;
	}
    }

    /* Read body of article, just to get lines. */
    for (lines = 0; ; lines++)
	if ((p = QIOread(qp)) == NULL) {
	    if (QIOtoolong(qp))
		continue;
	    if (QIOerror(qp)) {
		QIOclose(qp);
		return NULL;
	    }
	    break;
	}

    /* Calculate total size, fix hardwired headers. */
    ov_size = strlen(name) + ARTfieldsize + 2;
    for (hp = Headers, fp = ARTfields, i = ARTfieldsize; --i >= 0; hp++, fp++) {
	if (caseEQ(fp->Header, "Bytes") || caseEQ(fp->Header, "Lines")) {
	    if (fp->Header[0] == 'B' || fp->Header[0] == 'b')
		t = fstat(QIOfileno(qp), &Sb) >= 0 ? (long)Sb.st_size : 0L;
	    else
		t = lines;

	    (void)sprintf(value, "%ld", t);
	    size = strlen(value);
	    if (hp->Length == 0) {
		 hp->Length = size;
		hp->Header = NEW(char, hp->Length + 1);
	    }
	    else if (hp->Length < size) {
		hp->Length = size;
		RENEW(hp->Header, char, hp->Length + 1);
	    }
	    (void)strcpy(hp->Header, value);
	    hp->HasHeader = TRUE;
	}
	if (hp->HasHeader)
	    ov_size += strlen(hp->Header);
    }

    /* Get space. */
    if (buffsize == 0) {
	buffsize = ov_size;
	buff = NEW(char, buffsize + 1);
    }
    else if (buffsize < ov_size) {
	buffsize = ov_size;
	RENEW(buff, char, buffsize + 1);
    }

    /* Glue all the fields together. */
    p = buff + strlen(strcpy(buff, name));
    for (hp = Headers, i = ARTfieldsize; --i >= 0; hp++) {
	*p++ = '\t';
	if (hp->HasHeader)
	    p += strlen(strcpy(p, hp->Header));
    }
    *p = '\0';

    QIOclose(qp);
    return buff;
}


/*
**  XHDR, a common extension.  Retrieve specified header from a
**  Message-ID or article range.
*/
FUNCTYPE
CMDxhdr(ac, av)
    int			ac;
    char		*av[];
{
    register QIOSTATE	*qp;
    register ARTNUM	i;
    register char	*p;
    int			Overview;
    BOOL		IsLines;
    ARTRANGE		range;
    char		buff[SPOOLNAMEBUFF];
    ARTNUM		art;

    if (!PERMcanread) {
	Reply("%s\r\n", NOACCESS);
	return;
    }
    IsLines = caseEQ(av[1], "lines");

    /* Message-ID specified? */
    if (ac == 3 && av[2][0] == '<') {
	if ((qp = ARTopenbyid(av[2], &art)) == NULL) {
	    Reply("%d No such article\r\n", NNTP_DONTHAVEIT_VAL);
	    return;
	}
	Reply("%d %ld %s header of article %s.\r\n",
	   NNTP_HEAD_FOLLOWS_VAL, art, av[1], av[2]);
	p = GetHeader(qp, av[1], IsLines);
	Printf("%s %s\r\n", av[2], p ? p : "(none)");
	QIOclose(qp);
	Printf(".\r\n");
	return;
    }

    /* Range specified. */
    if (!CMDgetrange(ac - 1, av + 1, &range))
	return;

    /* Is this a header in our overview? */
    for (Overview = 0, i = 0; i < ARTfieldsize; i++)
	if (caseEQ(ARTfields[i].Header, av[1])) {
	    if (OVERopen())
		Overview = i + 1;
	    break;
	}

    Reply("%d %s fields follow\r\n", NNTP_HEAD_FOLLOWS_VAL, av[1]);
    for (i = range.Low; i <= range.High; i++) {
	if (ARTfind(i) < 0)
	    continue;

	/* Get it from the overview? */
	if (Overview && (p = OVERfind(i)) != NULL) {
	    p = OVERGetHeader(p, Overview);
	    Printf("%d %s\r\n", i, p && *p ? p : "(none)");
	    continue;
	}

	(void)sprintf(buff, "%ld", i);
	if ((qp = QIOopen(buff, QIO_BUFFER)) == NULL)
	    continue;
	p = GetHeader(qp, av[1], IsLines);
	Printf("%d %s\r\n", i, p ? p : "(none)");
	QIOclose(qp);
    }
    Printf(".\r\n");
}


/*
**  XOVER another extension.  Dump parts of the overview database.
*/
FUNCTYPE
CMDxover(ac, av)
    int			ac;
    char		*av[];
{
    register char	*p;
    register ARTNUM	i;
    register BOOL	Opened;
    ARTRANGE		range;
    char		buff[SPOOLNAMEBUFF];

    if (!PERMcanread) {
	Printf("%s\r\n", NOACCESS);
	return;
    }

    /* Trying to read. */
    if (GRPcount == 0) {
	Reply("%s\r\n", ARTnotingroup);
	return;
    }

    /* Parse range. */
    if (!CMDgetrange(ac, av, &range))
	return;

    Reply("%d data follows\r\n", NNTP_OVERVIEW_FOLLOWS_VAL);
    for (Opened = OVERopen(), i = range.Low; i <= range.High; i++) {
	if (ARTfind(i) < 0)
	    continue;

	if (Opened && (p = OVERfind(i)) != NULL) {
	    Printf("%s\r\n", p);
	    continue;
	}

	(void)sprintf(buff, "%ld", i);
	if ((p = OVERgen(buff)) != NULL)
	    Printf("%s\r\n", p);
    }
    Printf(".\r\n");
}


/*
**  XPAT, an uncommon extension.  Print only headers that match the pattern.
*/
/* ARGSUSED */
FUNCTYPE
CMDxpat(ac, av)
    int			ac;
    char		*av[];
{
    register char	*p;
    register QIOSTATE	*qp;
    register ARTNUM	i;
    ARTRANGE		range;
    char		*header;
    char		*pattern;
    char		*text;
    int			Overview;
    char		buff[SPOOLNAMEBUFF];
    ARTNUM		art;

    if (!PERMcanread) {
	Printf("%s\r\n", NOACCESS);
	return;
    }

    header = av[1];

    /* Message-ID specified? */
    if (av[2][0] == '<') {
	p = av[2];
	qp = ARTopenbyid(p, &art);
	if (qp == NULL) {
	    Printf("%d No such article.\r\n", NNTP_DONTHAVEIT_VAL);
	    return;
	}

	Printf("%d %s matches follow.\r\n", NNTP_HEAD_FOLLOWS_VAL, header);
	pattern = Glom(&av[3]);
	if ((text = GetHeader(qp, header, FALSE)) != NULL
	 && wildmat(text, pattern))
	    Printf("%s %s\r\n", p, text);

	QIOclose(qp);
	Printf(".\r\n");
	DISPOSE(pattern);
	return;
    }

    /* Range specified. */
    if (!CMDgetrange(ac - 1, av + 1, &range))
	return;

    /* In overview? */
    for (Overview = 0, i = 0; i < ARTfieldsize; i++)
	if (caseEQ(ARTfields[i].Header, av[1])) {
	    if (OVERopen())
		Overview = i + 1;
	    break;
	}

    Printf("%d %s matches follow.\r\n", NNTP_HEAD_FOLLOWS_VAL, header);
    for (pattern = Glom(&av[3]), i = range.Low; i < range.High; i++) {
	if (ARTfind(i) < 0)
	    continue;

	/* Get it from the Overview? */
	if (Overview
	 && (p = OVERfind(i)) != NULL
	 && (p = OVERGetHeader(p, Overview)) != NULL) {
	    if (wildmat(p, pattern))
		Printf("%ld %s\r\n", i, p);
	    continue;
	}

	(void)sprintf(buff, "%ld", i);
	if ((qp = QIOopen(buff, QIO_BUFFER)) == NULL)
	    continue;
	if ((p = GetHeader(qp, av[1], FALSE)) == NULL)
	    p = "(none)";
	if (wildmat(p, pattern))
	    Printf("%ld %s\r\n", i, p);
	QIOclose(qp);
    }

    Printf(".\r\n");
    DISPOSE(pattern);
}
