/*  $Revision: 1.14 $
**
**  Transmit articles to remote site.
*/
#include "configdata.h"
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <signal.h>
#include <setjmp.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <fcntl.h>
#if	defined(DO_NEED_TIME)
#include <time.h>
#endif	/* defined(DO_NEED_TIME) */
#include <sys/time.h>
#include <sys/uio.h>
#include "nntp.h"
#include "paths.h"
#include "logging.h"
#include "libinn.h"
#include "clibrary.h"
#include "qio.h"
#include "dbz.h"
#include "macros.h"


/*
**  Supported encoding schemes.
*/
typedef enum _MIMEXFERTYPE {
    MTnotmime,
    MTquotedprintable,
    MTbase64
} MIMEXFERTYPE;


#define OUTPUT_BUFFER_SIZE	(16 * 1024)


/*
** Syslog formats - collected together so they remain consistent
*/
STATIC char	STAT1[] =
	"%s stats offered %lu accepted %lu refused %lu rejected %lu";
STATIC char	STAT2[] = "%s times user %.3f system %.3f elapsed %.3f";
STATIC char	GOT_RESENDIT[] = "%s requeued %s %s";
STATIC char	CANT_CONNECT[] = "%s connect failed %s";
STATIC char	CANT_AUTHENTICATE[] = "%s authenticate failed %s";
STATIC char	IHAVE_FAIL[] = "%s ihave failed %s";


/*
**  Global variables.
*/
STATIC BOOL		AlwaysRewrite;
STATIC BOOL		Debug;
STATIC BOOL		DoRequeue = TRUE;
STATIC BOOL		Purging;
STATIC BOOL		Slavish;
STATIC BOOL		STATprint;
STATIC BOOL		Mime;
STATIC MIMEXFERTYPE	MimeArticle = MTnotmime;
STATIC char		*BATCHname;
STATIC char		*BATCHtemp;
STATIC char		*REMhost;
STATIC double		STATbegin;
STATIC double		STATend;
STATIC FILE		*BATCHfp;
STATIC int		FromServer;
STATIC int		ToServer;
STATIC QIOSTATE		*BATCHqp;
STATIC SIGVAR		GotAlarm;
STATIC SIGVAR		GotInterrupt;
STATIC SIGVAR		JMPyes;
STATIC jmp_buf		JMPwhere;
STATIC char		*REMbuffer;
STATIC char		*REMbuffptr;
STATIC char		*REMbuffend;
STATIC unsigned long	STATaccepted;
STATIC unsigned long	STAToffered;
STATIC unsigned long	STATrefused;
STATIC unsigned long	STATrejected;


/*
**  Find the history file entry for the Message-ID and return a file
**  positioned at the third field.
*/
STATIC FILE *
HistorySeek(MessageID)
    char		*MessageID;
{
    static char		History[] = _PATH_HISTORY;
    static FILE		*F;
    register char	*p;
    register char	*q;
    register int	i;
    datum		key;
    datum		val;
    OFFSET_T		offset;

    /* Open the history file. */
    if (F == NULL) {
	if (dbminit(History) < 0) {
	    (void)fprintf(stderr, "Can't set up \"%s\" database, %s\n",
		    History, strerror(errno));
	    exit(1);
	}
	if ((F = fopen(History, "r")) == NULL) {
	    (void)fprintf(stderr, "Can't open \"%s\" for reading, %s\n",
		    History, strerror(errno));
	    exit(1);
	}
    }

    /* Do the lookup. */
    key.dsize = strlen(MessageID) + 1;
    key.dptr = MessageID;
    val = dbzfetch(key);
    if (val.dptr == NULL || val.dsize != sizeof offset)
	return NULL;

    /* Get the seek offset, and seek. */
    for (p = val.dptr, q = (char *)&offset, i = sizeof offset; --i >= 0; )
	*q++ = *p++;
    if (fseek(F, offset, SEEK_SET) == -1)
	return NULL;
    return F;
}


/*
**  Return TRUE if the history file has the article expired.
*/
STATIC BOOL
Expired(MessageID)
    char		*MessageID;
{
    register int	c;
    register int	i;
    register FILE	*F;

    if ((F = HistorySeek(MessageID)) == NULL)
	/* Assume the worst. */
	return TRUE;

    /* Move to the filename fields. */
    for (i = 2; (c = getc(F)) != EOF && c != '\n'; )
	if (c == HIS_FIELDSEP && --i == 0)
	    break;
    if (c != HIS_FIELDSEP)
	return TRUE;

    /* See if we get any filename before the end of the line. */
    while ((c = getc(F)) != EOF && c != '\n')
	if (!ISWHITE(c))
	    /* Found non-whitespace; assume it's a filename. */
	    return FALSE;
    return TRUE;
}


/*
**  Flush and reset the site's output buffer.  Return FALSE on error.
*/
STATIC BOOL
REMflush()
{
    int		i;

    i = xwrite(ToServer, REMbuffer, (int)(REMbuffptr - REMbuffer));
    REMbuffptr = REMbuffer;
    return i < 0 ? FALSE : TRUE;
}


/*
**  Send a line to the server, adding the dot escape and \r\n.
*/
STATIC BOOL
REMwrite(p, i)
    register char	*p;
    register int	i;
{
    static char		HDR[] = "Content-Transfer-Encoding:";
    static char		COD[] =
		"Content-Transfer-Encoding: quoted-printable\r\n";
    register char	*dest;
    int			size;

    /* Buffer too full? */
    if (REMbuffend - REMbuffptr < i + 3) {
	if (!REMflush())
	    return FALSE;
	if (REMbuffend - REMbuffer < i + 3) {
	    /* Line too long -- grow buffer. */
	    size = i * 2;
	    RENEW(REMbuffer, char, size);
	    REMbuffend = &REMbuffer[size];
	}
    }

    if (MimeArticle != MTnotmime)
        if ((*p == 'C' && EQn(p, HDR, STRLEN(HDR)))
         || ((*p == 'C' || *p == 'c') && caseEQn(p, HDR, STRLEN(HDR)))) {
	    (void)memcpy((POINTER)REMbuffptr, (POINTER)COD, STRLEN(COD));
	    REMbuffptr += STRLEN(COD);
	    return TRUE;
        }

    /* Dot escape, text of the line, line terminator. */
    if (*p == '.')
	*REMbuffptr++ = '.';
    if (i > MEMCPY_THRESHOLD) {
	(void)memcpy((POINTER)REMbuffptr, (POINTER)p, (SIZE_T)i);
	REMbuffptr += i;
    }
    else {
	for (dest = REMbuffptr, i++; --i > 0; )
	    *dest++ = *p++;
	REMbuffptr = dest;
    }
    *REMbuffptr++ = '\r';
    *REMbuffptr++ = '\n';

    return TRUE;
}


/*
**  Send a line to the server, adding the dot escape and \r\n.
*/
STATIC BOOL
REMwriteQuoted(p, i)
    register char	*p;
    register int	i;
{
    static char		HEXDIGITS[] = "0123456789ABCDEF";
    register char	*dest;
    register int	size;
    register int	count;
    register int	prev;

    /* Buffer too full? */
    if (REMbuffend - REMbuffptr < i + 3) {
	if (!REMflush())
	    return FALSE;
	if (REMbuffend - REMbuffer < i + 3) {
	    /* Line too long -- grow buffer. */
	    size = i * 2;
	    RENEW(REMbuffer, char, size);
	    REMbuffend = &REMbuffer[size];
	}
    }

    for (count = 0, prev = 255, dest = REMbuffptr, i++; --i > 0; ) {
	if ((*p < 32 && *p != '\t')
	 || *p == '='
	 || *p >= 127
	 || (count == 0 && *p =='.')) {
	    *dest++ = '=';
	    *dest++ = HEXDIGITS[*p >> 4];
	    *dest++ = HEXDIGITS[*p & 0x0F];
	    p++;
	    count += 3;
	    prev = 'A';
	}
	else {
	    prev = *dest++ = *p++;
	    count++;
	}
        if (count > 72) {
	    *dest++ = '=';
	    *dest++ = '\r';
	    *dest++ = '\n';
	    count = 0;
	    prev = '\n';
        }
    }
    if (prev == ' ' || prev == '\t')
	*dest++ = '=';

    REMbuffptr = dest;
    *REMbuffptr++ = '\r';
    *REMbuffptr++ = '\n';

    return TRUE;
}


/*
**  Print transfer statistics, clean up, and exit.
*/
STATIC NORETURN
ExitWithStats(x)
    int			x;
{
    static char		QUIT[] = "quit";
    TIMEINFO		Now;
    double		usertime;
    double		systime;

    if (!Purging) {
	(void)REMwrite(QUIT, STRLEN(QUIT));
	(void)REMflush();
    }
    (void)GetTimeInfo(&Now);
    STATend = TIMEINFOasDOUBLE(Now);
    if (GetResourceUsage(&usertime, &systime) < 0) {
	usertime = 0;
	systime = 0;
    }

    if (STATprint) {
	(void)printf(STAT1,
	    REMhost, STAToffered, STATaccepted, STATrefused, STATrejected);
	(void)printf("\n");
	(void)printf(STAT2, REMhost, usertime, systime, STATend - STATbegin);
	(void)printf("\n");
    }

    syslog(L_NOTICE, STAT1,
	REMhost, STAToffered, STATaccepted, STATrefused, STATrejected);
    syslog(L_NOTICE, STAT2, REMhost, usertime, systime, STATend - STATbegin);

    if (BATCHfp != NULL && unlink(BATCHtemp) < 0 && errno != ENOENT)
	(void)fprintf(stderr, "Can't remove \"%s\", %s\n",
		BATCHtemp, strerror(errno));
    exit(x);
    /* NOTREACHED */
}


/*
**  Close the batchfile and the temporary file, and rename the temporary
**  to be the batchfile.
*/
STATIC void
CloseAndRename()
{
    /* Close the files, rename the temporary. */
    QIOclose(BATCHqp);
    if (ferror(BATCHfp)
     || fflush(BATCHfp) == EOF
     || fclose(BATCHfp) == EOF) {
	(void)unlink(BATCHtemp);
	(void)fprintf(stderr, "Can't close \"%s\", %s\n",
		BATCHtemp, strerror(errno));
	ExitWithStats(1);
    }
    if (rename(BATCHtemp, BATCHname) < 0) {
	(void)fprintf(stderr, "Can't rename \"%s\", %s\n",
		BATCHtemp, strerror(errno));
	ExitWithStats(1);
    }
}


/*
**  Requeue an article, opening the temp file if we have to.  If we get
**  a file write error, exit so that the original input is left alone.
*/
STATIC void
Requeue(Article, MessageID)
    char	*Article;
    char	*MessageID;
{
    /* Temp file already open? */
    if (BATCHfp == NULL) {
	(void)mktemp(BATCHtemp);
	if ((BATCHfp = fopen(BATCHtemp, "w")) == NULL) {
	    (void)fprintf(stderr, "Can't open \"%s\", %s\n",
		    BATCHtemp, strerror(errno));
	    ExitWithStats(1);
	}
    }

    /* Called only to get the file open? */
    if (Article == NULL)
	return;

    if (MessageID != NULL)
	(void)fprintf(BATCHfp, "%s %s\n", Article, MessageID);
    else
	(void)fprintf(BATCHfp, "%s\n", Article);
    if (fflush(BATCHfp) == EOF || ferror(BATCHfp)) {
	(void)fprintf(stderr, "Can't requeue \"%s\", %s\n",
		Article, strerror(errno));
	ExitWithStats(1);
    }
}


/*
**  Requeue an article then copy the rest of the batch file out.
*/
STATIC void
RequeueRestAndExit(Article, MessageID)
    char		*Article;
    char		*MessageID;
{
    register char	*p;

    if (!AlwaysRewrite
     && STATaccepted == 0 && STATrejected == 0 && STATrefused == 0) {
	(void)fprintf(stderr, "Nothing sent -- leaving batchfile alone.\n");
	ExitWithStats(1);
    }

    (void)fprintf(stderr, "Rewriting batch file and exiting.\n");
    Requeue(Article, MessageID);

    for ( ; ; ) {
	if ((p = QIOread(BATCHqp)) == NULL) {
	    if (QIOerror(BATCHqp)) {
		(void)fprintf(stderr, "Can't read \"%s\", %s\n",
			BATCHname, strerror(errno));
		ExitWithStats(1);
	    }
	    if (QIOtoolong(BATCHqp)) {
		(void)fprintf(stderr, "Skipping long line in \"%s\".\n",
			BATCHname);
		(void)QIOread(BATCHqp);
		continue;
	    }

	    /* Normal EOF. */
	    break;
	}

	if (fprintf(BATCHfp, "%s\n", p) == EOF
	 || ferror(BATCHfp)) {
	    (void)fprintf(stderr, "Can't requeue \"%s\", %s\n",
		    p, strerror(errno));
	    ExitWithStats(1);
	}
    }

    CloseAndRename();
    ExitWithStats(1);
}


/*
**  Clean up the NNTP escapes from a line.
*/
STATIC char *
REMclean(buff)
    char	*buff;
{
    char	*p;

    if ((p = strchr(buff, '\r')) != NULL)
	*p = '\0';
    if ((p = strchr(buff, '\n')) != NULL)
	*p = '\0';

    /* The dot-escape is only in text, not command responses. */
    return buff;
}


/*
**  Read a line of input, with timeout.  Also handle \r\n-->\n mapping
**  and the dot escape.  Return TRUE if okay, *or we got interrupted.*
*/
STATIC BOOL
REMread(start, size)
    char		*start;
    int			size;
{
    static int		count;
    static char		buffer[BUFSIZ];
    static char		*bp;
    register char	*p;
    register char	*q;
    register char	*end;
    struct timeval	t;
    FDSET		rmask;
    int			i;
    char		c;

    if (!REMflush())
	return FALSE;

    for (p = start, end = &start[size - 1]; ; ) {
	if (count == 0) {
	    /* Fill the buffer. */
    Again:
	    FD_ZERO(&rmask);
	    FD_SET(FromServer, &rmask);
	    t.tv_sec = 10 * 60;
	    t.tv_usec = 0;
	    i = select(FromServer + 1, &rmask, (FDSET *)NULL,
			(FDSET *)NULL, &t);
	    if (GotInterrupt)
		return TRUE;
	    if (i < 0) {
		if (errno == EINTR)
		    goto Again;
		return FALSE;
	    }
	    if (i == 0 || !FD_ISSET(FromServer, &rmask))
		return FALSE;
	    count = read(FromServer, buffer, sizeof buffer);
	    if (GotInterrupt)
		return TRUE;
	    if (count <= 0)
		return FALSE;
	    bp = buffer;
	}

	/* Process next character. */
	count--;
	c = *bp++;
	if (c == '\n')
	    break;
	if (p < end)
	    *p++ = c;
    }

    /* We know we got \n; if previous char was \r, turn it into \n. */
    if (p > start && p < end && p[-1] == '\r')
	p[-1] = '\n';
    *p = '\0';

    /* Handle the dot escape. */
    if (*p == '.') {
	if (p[1] == '\n' && p[2] == '\0')
	    /* EOF. */
	    return FALSE;
	for (q = &start[1]; (*p++ = *q++) != '\0'; )
	    continue;
    }
    return TRUE;
}


/*
**  Handle the interrupt.
*/
static void
Interrupted(Article, MessageID)
    char	*Article;
    char	*MessageID;
{
    (void)fprintf(stderr, "Interrupted\n");
    RequeueRestAndExit(Article, MessageID);
}


/*
**  Send a whole article to the server.
*/
STATIC BOOL
REMsendarticle(Article, MessageID, qp)
    char		*Article;
    char		*MessageID;
    register QIOSTATE	*qp;
{
    static char		TERM[] = ".\r\n";
    register char	*p;
    register BOOL	ok;
    register BOOL	InHeaders;
    char		buff[NNTP_STRLEN];

    for (InHeaders = TRUE; ; ) {
	if ((p = QIOread(qp)) == NULL) {
	    if (QIOerror(qp)) {
		(void)fprintf(stderr, "Can't read \"%s\", %s\n",
			Article, strerror(errno));
		return FALSE;
	    }
	    if (QIOtoolong(qp)) {
		(void)fprintf(stderr, "Line too long in \"%s\"\n", Article);
		(void)QIOread(BATCHqp);
		continue;
	    }

	    /* Normal EOF. */
	    break;
	}
	if (*p == '\0')
	    InHeaders = FALSE;

	if (InHeaders || MimeArticle == MTnotmime) {
	    if (!REMwrite(p, QIOlength(qp))) {
	        (void)fprintf(stderr, "Can't send \"%s\", %s\n",
		        Article, strerror(errno));
	        return FALSE;
	    }
	}
	else {
	    switch (MimeArticle) {
	    default:
	    case MTbase64:
		ok = FALSE;
		break;
	    case MTquotedprintable:
		ok = REMwriteQuoted(p, QIOlength(qp));
		break;
	    }
	    if (!ok) {
		(void)fprintf(stderr, "Can't send \"%s\", %s\n",
			Article, strerror(errno));
		return FALSE;
	    }
	}
	if (GotInterrupt)
	    Interrupted(Article, MessageID);
    }
    if (!REMflush()) {
	(void)fprintf(stderr, "Can't end \"%s\", %s\n",
		Article, strerror(errno));
	return FALSE;
    }
    if (Debug)
	(void)fprintf(stderr, "> [ article ]%s\n",
	     MimeArticle == MTnotmime ? "" : " (Mime: quoted-printable)");

    /* Write the terminator. */
    if (write(ToServer, TERM, STRLEN(TERM)) != STRLEN(TERM)) {
	(void)fprintf(stderr, "Can't end \"%s\", %s\n",
		Article, strerror(errno));
	return FALSE;
    }
    if (GotInterrupt)
	Interrupted(Article, MessageID);
    if (Debug)
	(void)fprintf(stderr, "> .\n");

    /* What did the remote site say? */
    if (!REMread(buff, (int)sizeof buff)) {
	(void)fprintf(stderr, "No reply after sending \"%s\", %s\n",
		Article, strerror(errno));
	return FALSE;
    }
    if (GotInterrupt)
	Interrupted(Article, MessageID);
    if (Debug)
	(void)fprintf(stderr, "< %s", buff);

    /* Parse the reply. */
    switch (atoi(buff)) {
    default:
	(void)fprintf(stderr, "Unknown reply after \"%s\" -- %s",
		Article, buff);
	if (DoRequeue)
	    Requeue(Article, MessageID);
	break;
    case NNTP_RESENDIT_VAL:
    case NNTP_GOODBYE_VAL:
	syslog(L_NOTICE, GOT_RESENDIT, REMhost, MessageID, REMclean(buff));
	Requeue(Article, MessageID);
	break;
    case NNTP_TOOKIT_VAL:
	STATaccepted++;
	break;
    case NNTP_REJECTIT_VAL:
	STATrejected++;
	break;
    }

    /* Article sent, or we requeued it. */
    return TRUE;
}


/*
**  Get the Message-ID header from an open article.
*/
STATIC char *
GetMessageID(qp)
    register QIOSTATE	*qp;
{
    static char		HDR[] = "Message-ID:";
    static char		buff[DBZMAXKEY + 1];
    register char	*p;

    while ((p = QIOread(qp)) != NULL)
	if ((*p == 'M' && EQn(p, HDR, STRLEN(HDR)))
	 || ((*p == 'M' || *p == 'm') && caseEQn(p, HDR, STRLEN(HDR)))) {
	    /* Found the header -- skip whitespace. */
	    for (p += STRLEN(HDR); ISWHITE(*p); p++)
		continue;
	    if (*p == '\0' || (int)strlen(p) > DBZMAXKEY)
		/* Header is empty or too long. */
		break;
	    (void)strcpy(buff, p);
	    return buff;
	}
    return NULL;
}


/*
**  Get the MIME Content headers from an open article.
*/
STATIC void
GetMimeHeaders(qp, Encodingp, Typep)
    register QIOSTATE	*qp;
    register char	**Encodingp;
    register char	**Typep;
{
    static char		ENC_HDR[] = "Content-Transfer-Encoding:";
    static char		TYPE_HDR[] = "Content-Type:";
    static char		Encoding[SMBUF + 1];
    static char		ContentType[SMBUF + 1];
    register char	*p;

    for (*Encodingp = *Typep = NULL; (p = QIOread(qp)) != NULL && *p; ) {
	if (*p != 'C' && *p != 'c')
	    continue;
	if (caseEQn(p, ENC_HDR, STRLEN(ENC_HDR))) {
	    for (p += STRLEN(ENC_HDR); ISWHITE(*p); p++)
		continue;
	    if (*p == '\0' || (int)strlen(p) > sizeof Encoding)
		/* Header is empty or too long. */
		continue;
	    (void)strcpy(Encoding, p);
	    *Encodingp = Encoding;
	    if (*Typep)
		break;
	}
	else if (caseEQn(p, TYPE_HDR, STRLEN(TYPE_HDR))) {
	    for (p += STRLEN(TYPE_HDR); ISWHITE(*p); p++)
		continue;
	    if (*p == '\0' || (int)strlen(p) > sizeof ContentType)
		/* Header is empty or too long. */
		break;
	    (void)strcpy(ContentType, p);
	    /* Strip off any subtype part. */
	    for (p = ContentType; *p; p++)
		if (*p == '/' || *p == ';') {
		    *p = '\0';
		    break;
		}
	    *Typep = ContentType;
	    if (*Encodingp)
		break;
	}
    }
}



/*
**  Mark that we got interrupted.
*/
STATIC SIGHANDLER
CATCHinterrupt(s)
    int		s;
{
    GotInterrupt = TRUE;
    /* Let two interrupts kill us. */
    (void)signal(s, SIG_DFL);
}


/*
**  Mark that the alarm went off.
*/
/* ARGSUSED0 */
STATIC SIGHANDLER
CATCHalarm(s)
    int		s;
{
    GotAlarm = TRUE;
    if (JMPyes)
	longjmp(JMPwhere, 1);
}


/*
**  Print a usage message and exit.
*/
STATIC NORETURN
Usage()
{
    (void)fprintf(stderr,
	"Usage: innxmit [-a] [-d] [-M] [-p] [-r] [-S] [-t#] [-T#] host file\n");
    exit(1);
}


int
main(ac, av)
    int			ac;
    char		*av[];
{
    static char		SPOOL[] = _PATH_SPOOL;
    static char		BATCHDIR[] = _PATH_BATCHDIR;
    static char		SKIPPING[] = "Skipping \"%s\" --%s?\n";
    register int	i;
    register char	*p;
    register QIOSTATE	*qp;
    TIMEINFO		Now;
    FILE		*From;
    FILE		*To;
    char		buff[NNTP_STRLEN];
    char		*AltSpool;
    char		*Article;
    char		*ContentEncoding;
    char		*ContentType;
    char		*MessageID;
    char		*AltPath;
    SIGHANDLER		(*old)();
    unsigned int	ConnectTimeout;
    unsigned int	TotalTimeout;

    /* Set defaults. */
    ConnectTimeout = 0;
    TotalTimeout = 0;
    AltSpool = NULL;
    (void)umask(NEWSUMASK);

    /* Parse JCL. */
    while ((i = getopt(ac, av, "A:adMprSt:T:v")) != EOF)
	switch (i) {
	default:
	    Usage();
	    /* NOTREACHED */
	case 'A':
	    AltSpool = optarg;
	    AltPath = NEW(char, SPOOLNAMEBUFF + strlen(AltSpool));
	    break;
	case 'a':
	    AlwaysRewrite = TRUE;
	    break;
	case 'd':
	    Debug = TRUE;
	    break;
	case 'M':
	    Mime = TRUE;
	    break;
	case 'p':
	    AlwaysRewrite = TRUE;
	    Purging = TRUE;
	    break;
	case 'r':
	    DoRequeue = FALSE;
	    break;
	case 'S':
	    Slavish = TRUE;
	    break;
	case 't':
	    ConnectTimeout = atoi(optarg);
	    break;
	case 'T':
	    TotalTimeout = atoi(optarg);
	    break;
	case 'v':
	    STATprint = TRUE;
	    break;
	}
    ac -= optind;
    av += optind;

    /* Parse arguments; host and filename. */
    if (ac != 2)
	Usage();
    REMhost = av[0];
    BATCHname = av[1];

    if (chdir(SPOOL) < 0) {
	(void)fprintf(stderr, "Can't cd to \"%s\", %s\n",
		SPOOL, strerror(errno));
	exit(1);
    }

    (void)openlog("innxmit", L_OPENLOG_FLAGS | LOG_PID, LOG_INN_PROG);

    /* Open the batch file and lock others out. */
    if (BATCHname[0] != '/') {
	BATCHname = NEW(char, STRLEN(BATCHDIR) + 1 + strlen(av[1]) + 1);
	(void)sprintf(BATCHname, "%s/%s", BATCHDIR, av[1]);
    }
    if ((i = open(BATCHname, O_RDWR)) < 0
     || (BATCHqp = QIOfdopen(i, QIO_BUFFER)) == NULL) {
	(void)fprintf(stderr, "Can't open \"%s\", %s\n",
		BATCHname, strerror(errno));
	exit(1);
    }
    if (LockFile(QIOfileno(BATCHqp), TRUE) < 0) {
#if	defined(EWOULDBLOCK)
	if (errno == EWOULDBLOCK)
	    exit(0);
#endif	/* defined(EWOULDBLOCK) */
	(void)fprintf(stderr, "Can't lock \"%s\", %s\n",
		BATCHname, strerror(errno));
	exit(1);
    }

    /* Get a temporary name in the same directory as the batch file. */
    p = strrchr(BATCHname, '/');
    BATCHtemp = NEW(char, strlen(BATCHname) + STRLEN("/bchXXXXXX") + 1);
    *p = '\0';
    (void)sprintf(BATCHtemp, "%s/bchXXXXXX", BATCHname);
    *p = '/';

    /* Set up buffer used by REMwrite. */
    REMbuffer = NEW(char, OUTPUT_BUFFER_SIZE);
    REMbuffend = &REMbuffer[OUTPUT_BUFFER_SIZE];
    REMbuffptr = REMbuffer;

    /* Start timing. */
    if (GetTimeInfo(&Now) < 0) {
	(void)fprintf(stderr, "Can't get time, %s\n", strerror(errno));
	exit(1);
    }
    STATbegin = TIMEINFOasDOUBLE(Now);

    if (!Purging) {
	/* Open a connection to the remote server. */
	if (ConnectTimeout) {
	    GotAlarm = FALSE;
	    old = signal(SIGALRM, CATCHalarm);
	    (void)alarm(ConnectTimeout);
	    JMPyes = TRUE;
	    if (setjmp(JMPwhere)) {
		(void)fprintf(stderr, "Can't connect to %s, timed out\n",
			REMhost);
		exit(1);
	    }
	}
	if (NNTPconnect(REMhost, &From, &To, buff) < 0 || GotAlarm) {
	    i = errno;
	    (void)fprintf(stderr, "Can't connect to %s, %s\n",
		    REMhost, buff[0] ? REMclean(buff) : strerror(errno));
	    if (GotAlarm)
		syslog(L_NOTICE, CANT_CONNECT, REMhost, "timeout");
	    else
		syslog(L_NOTICE, CANT_CONNECT, REMhost,
		    buff[0] ? REMclean(buff) : strerror(i));
	    exit(1);
	}
	if (Debug)
	    (void)fprintf(stderr, "< %s\n", REMclean(buff));
	if (NNTPsendpassword(REMhost, From, To) < 0 || GotAlarm) {
	    i = errno;
	    (void)fprintf(stderr, "Can't authenticate with %s, %s\n",
		    REMhost, strerror(errno));
	    syslog(L_ERROR, CANT_AUTHENTICATE,
		REMhost, GotAlarm ? "timeout" : strerror(i));
	    /* Don't send quit; we want the remote to print a message. */
	    exit(1);
	}
	if (ConnectTimeout) {
	    (void)alarm(0);
	    (void)signal(SIGALRM, old);
	    JMPyes = FALSE;
	}

	/* We no longer need standard I/O. */
	FromServer = fileno(From);
	ToServer = fileno(To);
    }

    /* Set up signal handlers. */
    (void)signal(SIGHUP, CATCHinterrupt);
    (void)signal(SIGINT, CATCHinterrupt);
    (void)signal(SIGTERM, CATCHinterrupt);
    (void)signal(SIGPIPE, SIG_IGN);
    if (TotalTimeout) {
	(void)alarm(TotalTimeout);
	(void)signal(SIGALRM, CATCHalarm);
    }

    /* Main processing loop. */
    GotInterrupt = FALSE;
    GotAlarm = FALSE;
    for (Article = NULL, MessageID = NULL; ; ) {
	if (GotAlarm) {
	    (void)fprintf(stderr, "Timed out\n");
	    /* Don't resend the current article. */
	    RequeueRestAndExit((char *)NULL, (char *)NULL);
	}
	if (GotInterrupt)
	    Interrupted(Article, MessageID);

	if ((Article = QIOread(BATCHqp)) == NULL) {
	    if (QIOerror(BATCHqp)) {
		(void)fprintf(stderr, "Can't read \"%s\", %s\n",
			BATCHname, strerror(errno));
		ExitWithStats(1);
	    }
	    if (QIOtoolong(BATCHqp)) {
		(void)fprintf(stderr, "Skipping long line in \"%s\"\n",
			BATCHname);
		(void)QIOread(BATCHqp);
		continue;
	    }

	    /* Normal EOF -- we're done. */
	    QIOclose(BATCHqp);
	    break;
	}

	/* Split the line into possibly two fields. */
	if (Article[0] == '/'
	 && Article[STRLEN(SPOOL)] == '/'
	 && EQn(Article, SPOOL, STRLEN(SPOOL)))
	    Article += STRLEN(SPOOL) + 1;
	if ((MessageID = strchr(Article, ' ')) != NULL) {
	    *MessageID++ = '\0';
	    if (!Slavish) {
		if (*MessageID != '<'
		 || (p = strrchr(MessageID, '>')) == NULL
		 || *++p != '\0') {
		    (void)fprintf(stderr, "Ignoring line \"%s %s...\"\n",
			Article, MessageID);
		    continue;
		}
		*p = '\0';
	    }
	}

	if (*Article == '\0') {
	    (void)fprintf(stderr, "Empty filename for \"%s\" in \"%s\"\n",
		    MessageID, BATCHname);
	    /* We could do a history lookup. */
	    continue;
	}

	if (Purging && MessageID != NULL && !Expired(MessageID)) {
	    Requeue(Article, MessageID);
	    continue;
	}

	/* Open the article. */
	if ((qp = QIOopen(Article, QIO_BUFFER)) == NULL
	 && AltSpool
	 && *Article != '/') {
	    (void)sprintf(AltPath, "%s/%s", AltSpool, Article);
	    qp = QIOopen(AltPath, QIO_BUFFER);
	}

	if (qp == NULL) {
	    switch (errno) {
	    default:
		(void)fprintf(stderr, "Requeue \"%s\", %s\n",
			Article, strerror(errno));
		Requeue(Article, MessageID);
		break;
	    case ENOENT:
		/* Cancelled or expired.  We could look the file up
		 * in the history database and see if it does exist. */
		break;
	    case ENOTDIR:
		(void)fprintf(stderr, SKIPPING, Article, "mangled");
		break;
	    }
	    continue;
	}

	if (Purging) {
	    QIOclose(qp);
	    Requeue(Article, MessageID);
	    continue;
	}

	/* Get the Message-ID from the article if we need to. */
	if (MessageID == NULL) {
	    if ((MessageID = GetMessageID(qp)) == NULL) {
		(void)fprintf(stderr, SKIPPING, Article, "no Message-ID");
		QIOclose(qp);
		continue;
	    }
	    if (QIOrewind(qp) < 0) {
		(void)fprintf(stderr, "Can't rewind \"%s\", %s -- requeue\n",
			Article, strerror(errno));
		QIOclose(qp);
		Requeue(Article, (char *)NULL);
		continue;
	    }
	}
	if (Mime == TRUE) {
	    MimeArticle = MTnotmime;
	    GetMimeHeaders(qp, &ContentEncoding, &ContentType);
	    if (QIOrewind(qp) < 0) {
		(void)fprintf(stderr, "Can't rewind \"%s\", %s -- requeue\n",
			Article, strerror(errno));
		QIOclose(qp);
		Requeue(Article, (char *)NULL);
		continue;
	    }
	    if (ContentEncoding 
	     && (caseEQ(ContentEncoding, "binary")
	      || caseEQ(ContentEncoding, "8bit")))
		if (ContentType == NULL || caseEQ(ContentType, "text"))
		    MimeArticle = MTquotedprintable;
		else
		    /* Shouldbe MTbase64, but not implemented yet. */
		    MimeArticle = MTnotmime;
	}
	if (GotInterrupt)
	    Interrupted(Article, MessageID);

	/* Offer the article. */
	(void)sprintf(buff, "%s %s", Slavish ? "xreplic" : "ihave", MessageID);
	if (!REMwrite(buff, (int)strlen(buff))) {
	    (void)fprintf(stderr, "Can't offer article, %s\n",
		    strerror(errno));
	    QIOclose(qp);
	    RequeueRestAndExit(Article, MessageID);
	}
	STAToffered++;
	if (Debug)
	    (void)fprintf(stderr, "> %s\n", buff);
	if (GotInterrupt)
	    Interrupted(Article, MessageID);

	/* Does he want it? */
	if (!REMread(buff, (int)sizeof buff)) {
	    (void)fprintf(stderr, "No reply to ihave, %s\n", strerror(errno));
	    QIOclose(qp);
	    RequeueRestAndExit(Article, MessageID);
	}
	if (GotInterrupt)
	    Interrupted(Article, MessageID);
	if (Debug)
	    (void)fprintf(stderr, "< %s", buff);

	/* Parse the reply. */
	switch (atoi(buff)) {
	default:
	    (void)fprintf(stderr, "Unknown reply to \"%s\" -- %s",
		    Article, buff);
	    if (DoRequeue)
		Requeue(Article, MessageID);
	    break;
	case NNTP_RESENDIT_VAL:
	case NNTP_GOODBYE_VAL:
	    /* Most likely out of space -- no point in continuing. */
	    syslog(L_NOTICE, IHAVE_FAIL, REMhost, REMclean(buff));
	    RequeueRestAndExit(Article, MessageID);
	    /* NOTREACHED */
	case NNTP_SENDIT_VAL:
	    if (!REMsendarticle(Article, MessageID, qp))
		RequeueRestAndExit(Article, MessageID);
	    break;
	case NNTP_SYNTAX_VAL:
	case NNTP_HAVEIT_VAL:
	    STATrefused++;
	    break;
#if	defined(NNTP_SENDIT_LATER)
	case NNTP_SENDIT_LATER_VAL:
	    Requeue(Article, MessageID);
	    break;
#endif	/* defined(NNTP_SENDIT_LATER) */
	}

	QIOclose(qp);
    }

    if (BATCHfp != NULL)
	/* We requeued something, so close the temp file. */
	CloseAndRename();
    else if (unlink(BATCHname) < 0 && errno != ENOENT)
	(void)fprintf(stderr, "Can't remove \"%s\", %s\n",
		BATCHtemp, strerror(errno));
    ExitWithStats(0);
    /* NOTREACHED */
}
