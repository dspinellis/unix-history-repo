/*  $Revision: 1.14 $
**
**  Miscellaneous support routines.
*/
#include "nnrpd.h"
#include "dbz.h"
#if	defined(DO_NEED_TIME)
#include <time.h>
#endif	/* defined(DO_NEED_TIME) */
#include <sys/time.h>


#define ASCtoNUM(c)		((c) - '0')
#define CHARStoINT(c1, c2)	(ASCtoNUM((c1)) * 10 + ASCtoNUM((c2)))
#define DaysInYear(y)		((y % 4 ? 365 : 366))


/*
**  Parse a string into a NULL-terminated array of words; return number
**  of words.  If argvp isn't NULL, it and what it points to will be
**  DISPOSE'd.
*/
int
Argify(line, argvp)
    char		*line;
    char		***argvp;
{
    register char	**argv;
    register char	*p;
    register int	i;

    if (*argvp != NULL) {
	DISPOSE(*argvp[0]);
	DISPOSE(*argvp);
    }

    /*  Copy the line, which we will split up. */
    while (ISWHITE(*line))
	line++;
    i = strlen(line);
    p = NEW(char, i + 1);
    (void)strcpy(p, line);

    /* Allocate worst-case amount of space. */
    for (*argvp = argv = NEW(char*, i + 2); *p; ) {
	/* Mark start of this word, find its end. */
	for (*argv++ = p; *p && !ISWHITE(*p); )
	    p++;
	if (*p == '\0')
	    break;

	/* Nip off word, skip whitespace. */
	for (*p++ = '\0'; ISWHITE(*p); )
	    p++;
    }
    *argv = NULL;
    return argv - *argvp;
}


/*
**  Take a vector which Argify made and glue it back together with
**  spaces between each element.  Returns a pointer to dynamic space.
*/
char *
Glom(av)
    char		**av;
{
    register char	**v;
    register char	*p;
    register int	i;
    char		*save;

    /* Get space. */
    for (i = 0, v = av; *v; v++)
	i += strlen(*v) + 1;

    for (save = p = NEW(char, i + 1), v = av; *v; v++) {
	if (p > save)
	    *p++ = ' ';
	p += strlen(strcpy(p, *v));
    }

    return save;
}


/*
**  Match a list of newsgroup specifiers against a list of newsgroups.
**  func is called to see if there is a match.
*/
BOOL
PERMmatch(match, Pats, list)
    register BOOL	match;
    char		**Pats;
    char		**list;
{
    register int	i;
    register char	*p;

    if (Pats[0] == NULL)
	return TRUE;

    for ( ; *list; list++)
	for (i = 0; (p = Pats[i]) != NULL; i++) {
	    if (p[0] == '!') {
		if (wildmat(*list, ++p))
		    match = FALSE;
	    }
	    else if (wildmat(*list, p))
		match = TRUE;
	}

    return match;
}


/*
**  Check to see if user is allowed to see this article by matching
**  Newsgroups line.
*/
BOOL
PERMartok(qp)
    register QIOSTATE	*qp;
{
    static char		**grplist;
    register char	*p;
    register char	*q;
    BOOL		found;

    if (!PERMspecified)
	return PERMdefault;

    for (found = FALSE; ; ) {
	p = QIOread(qp);
	if (p == NULL) {
	    if (QIOtoolong(qp))
		continue;
	    break;
	}

	if (*p == '\n')
	    /* End of header */
	    break;
	if (*p != 'N' && *p != 'n')
	    continue;
	if ((q = strchr(p, ':')) == NULL)
	    continue;
	*q = '\0';
	if (caseEQ(p, "newsgroups")) {
	    found = NGgetlist(&grplist, q + 2);
	    break;
	}
    }
    (void)QIOrewind(qp);

    if (!found)
	/* No newgroups or null entry. */
	return 1;

    return PERMmatch(PERMdefault, PERMlist, grplist);
}


/*
**  Parse a date like yymmddhhmmss into a long.  Return -1 on error.
*/
long
NNTPtoGMT(av1, av2)
    char		*av1;
    char		*av2;
{
    /* Note that this is origin-one! */
    static int		DaysInMonth[12] = {
	0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30
    };
    register char	*p;
    int			year;
    int			month;
    int			day;
    int			hour;
    int			mins;
    int			secs;
    register int	i;
    long		seconds;
    char		buff[6 + 6 + 1];

    if (strlen(av1) != 6 || strlen(av2) != 6)
	return -1;
    (void)sprintf(buff, "%s%s", av1, av2);
    for (p = buff; *p; p++)
	if (!CTYPE(isdigit, *p))
	    return -1;

    year  = CHARStoINT(buff[ 0], buff[ 1]);
    month = CHARStoINT(buff[ 2], buff[ 3]);
    day   = CHARStoINT(buff[ 4], buff[ 5]);
    hour  = CHARStoINT(buff[ 6], buff[ 7]);
    mins  = CHARStoINT(buff[ 8], buff[ 9]);
    secs  = CHARStoINT(buff[10], buff[11]);

    if (month < 1 || month > 12
     || day < 1 || day > 31
     || mins < 0 || mins > 59
     || secs < 0 || secs > 59)
	return -1;
    if (hour == 24) {
	hour = 0;
	day++;
    }
    else if (hour < 0 || hour > 23)
	return -1;

    for (seconds = 0, year += 1900, i = 1970; i < year; i++)
	seconds += DaysInYear(i);
    if (DaysInYear(year) == 366 && month > 2)
	seconds++;
    while (--month > 0)
	seconds += DaysInMonth[month];
    seconds += day - 1;
    seconds = 24 * seconds + hour;
    seconds = 60 * seconds + mins;
    seconds = 60 * seconds + secs;

    return seconds;
}


/*
**  Convert local time (seconds since epoch) to GMT.
*/
long
LOCALtoGMT(t)
    long	t;
{
    TIMEINFO	Now;

    (void)GetTimeInfo(&Now);
    t += Now.tzone * 60;
    return t;
}


/*
**  Return the path name of an article if it is in the history file.
**  Return a pointer to static data.
*/
char *
HISgetent(msg_id, fulldata)
    char		*msg_id;
    BOOL		fulldata;
{
    static BOOL		setup;
#if	NNRP_DBZINCORE_DELAY > 0
    static int		count = NNRP_DBZINCORE_DELAY;
#endif	/* NNRP_DBZINCORE_DELAY > 0 */
    static FILE		*hfp;
    static char		path[BIG_BUFFER];
    register char	*p;
    register char	*q;
    register int	i;
    char		*save;
    char		buff[BIG_BUFFER];
    OFFSET_T		l;
    datum		key;
    datum		value;
    struct stat		Sb;

#if	NNRP_DBZINCORE_DELAY > 0
    if (count && --count == 0) {
	if (setup) {
	    (void)dbmclose();
	    setup = FALSE;
	}
	(void)dbzincore(1);
    }
#endif	/* NNRP_DBZINCORE_DELAY > 0 */
    if (!setup) {
	if (dbminit(HISTORY) < 0) {
	    syslog(L_ERROR, "%s cant dbminit %s %m", ClientHost, HISTORY);
	    return NULL;
	}
	setup = TRUE;
    }

    /* Set the key value, fetch the entry. */
    for (p = key.dptr = msg_id; *p; p++)
	if (*p == HIS_FIELDSEP || *p == '\n')
	    *p = HIS_BADCHAR;
    key.dsize = p - key.dptr + 1;
    value = dbzfetch(key);
    if (value.dptr == NULL)
	return NULL;
    for (q = (char *)&l, p = value.dptr, i = sizeof l; --i >= 0; )
	*q++ = *p++;

    /* Open history file if we need to. */
    if (hfp == NULL) {
	if ((hfp = fopen(HISTORY, "r")) == NULL) {
	    syslog(L_ERROR, "%s cant fopen %s %m", ClientHost, HISTORY);
	    return NULL;
	}
	CloseOnExec((int)fileno(hfp), TRUE);
    }

    /* Seek and read. */
    if (fseek(hfp, l, SEEK_SET) == -1) {
	syslog(L_ERROR, "%s cant fseek to %ld %m", ClientHost, l);
	return NULL;
    }
    if (fgets(buff, sizeof buff, hfp) == NULL) {
	syslog(L_ERROR, "%s cant fgets from %ld %m", ClientHost, l);
	return NULL;
    }
    if ((p = strchr(buff, '\n')) != NULL)
	*p = '\0';

    /* Skip first two fields. */
    if ((p = strchr(buff, '\t')) == NULL) {
	syslog(L_ERROR, "%s bad_history at %ld for %s", ClientHost, l, msg_id);
	return NULL;
    }
    if ((p = strchr(p + 1, '\t')) == NULL)
	/* Article has expired. */
	return NULL;
    save = p + 1;

    /* Want the full data? */
    if (fulldata) {
	(void)strcpy(path, save);
	for (p = path; *p; p++)
	    if (*p == '.')
		*p = '/';
	return path;
    }

    /* Want something we can open; loop over all entries. */
    for ( ; ; save = q + 1) {
	if ((q = strchr(save, ' ')) != NULL)
	    *q = '\0';
	for (p = save; *p; p++)
	    if (*p == '.')
		*p = '/';
	(void)sprintf(path, "%s/%s", _PATH_SPOOL, save);
	if (stat(path, &Sb) >= 0)
	    return path;
	if (q == NULL)
	    break;
    }

    return NULL;
}


/*
**  Parse a newsgroups line, return TRUE if there were any.
*/
BOOL
NGgetlist(argvp, list)
    char		***argvp;
    char		*list;
{
    register char	*p;

    for (p = list; *p; p++)
	if (*p == ',')
	    *p = ' ';

    return Argify(list, argvp) != 0;
}


/*
**  Take an NNTP distribution list <d1,d2,...> and turn it into an array.
*/
BOOL
ParseDistlist(argvp, list)
    char		***argvp;
    char		*list;
{
    static char		**argv;
    register char	*p;

    if (list[0] != '<' || (p = strchr(&list[1], '>')) == NULL)
	return FALSE;
    *p = '\0';

    for (p = list + 1; *p; p++)
	if (*p == ',')
	    *p = ' ';
    (void)Argify(list + 1, &argv);
    *argvp = argv;
    return TRUE;
}


/*
**  Read a line of input, with timeout.
*/
READTYPE
READline(start, size, timeout)
    char		*start;
    int			size;
    int			timeout;
{
    static int		count;
    static char		buffer[BUFSIZ];
    static char		*bp;
    register char	*p;
    register char	*end;
    struct timeval	t;
    FDSET		rmask;
    int			i;
    char		c;

    for (p = start, end = &start[size - 1]; ; ) {
	if (count == 0) {
	    /* Fill the buffer. */
    Again:
	    FD_ZERO(&rmask);
	    FD_SET(STDIN, &rmask);
	    t.tv_sec = timeout;
	    t.tv_usec = 0;
	    i = select(STDIN + 1, &rmask, (FDSET *)NULL, (FDSET *)NULL, &t);
	    if (i < 0) {
		if (errno == EINTR)
		    goto Again;
		syslog(L_ERROR, "%s cant select %m", ClientHost);
		return RTtimeout;
	    }
	    if (i == 0 || !FD_ISSET(STDIN, &rmask))
		return RTtimeout;
	    count = read(STDIN, buffer, sizeof buffer);
	    if (count < 0) {
		syslog(L_ERROR, "%s cant read %m", ClientHost);
		return RTtimeout;
	    }
	    if (count == 0)
		return RTeof;
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

    /* If last two characters are \r\n, kill the \r as well as the \n. */
    if (p > start && p < end && p[-1] == '\r')
	p--;
    *p = '\0';
    return p == end ? RTlong : RTok;
}
