/*  $Revision: 1.11 $
**
**  The newnews command.
*/
#include "nnrpd.h"


#define FILE_LIST_DELTA		10
#define GET_DATE(p, line)	\
	((p = strchr(line, HIS_FIELDSEP)) == NULL ? 0 : atol(++p))


/*
**  Open an article and see if its distribution is in the list.
*/
STATIC BOOL
DistMatches(distribs, files)
    char		**distribs;
    char		**files;
{
    register char	**dp;
    register QIOSTATE	*qp;
    register char	*p;
    register char	*q;
    char		buff[BIG_BUFFER];
    char		*save;

    /* Loop through the file list, trying to open one of them.. */
    for (save = files[0]; *files; files++) {
	(void)sprintf(buff, "%s/%s", _PATH_SPOOL, files[0]);
	for (p = &buff[STRLEN(_PATH_SPOOL)]; *p; p++)
	    if (*p == '.')
		*p = '/';
	if ((qp = QIOopen(buff, QIO_BUFFER)) != NULL)
	    break;
    }
    if (*files == NULL) {
	syslog(L_ERROR, "%s cant fopen %s %m", ClientHost, save);
	return FALSE;
    }

    /* Scan the article for the Distribution header. */
    while ((p = QIOread(qp)) != NULL) {
	if (*p == '\n')
	    /* End of headers. */
	    break;
	if (*p != 'd' && *p != 'D')
	    continue;
	if ((q = strchr(p, '\n')) != NULL)
	    *q = '\0';
	if ((q = strchr(p, ':')) == NULL)
	    continue;
	*q = '\0';
	if (caseEQ(buff, "distribution")) {
	    for (q += 2, dp = distribs; *dp; dp++)
		if (caseEQ(q, *dp)) {
		    QIOclose(qp);
		    return TRUE;
		}
	    break;
	}
    }
    QIOclose(qp);
    return FALSE;
}


/*
**  Split file list into array of newsgroups.  Return static pointer,
**  or NULL if there are no filenames.
*/
STATIC char **
GetFiles(p)
    register char	*p;
{
    static int		size;
    static char		**list;
    register int	i;

    if (size == 0) {
	size = FILE_LIST_DELTA;
	list = NEW(char*, size + 1);
    }

    for (i = 0 ; ; ) {
	while (ISWHITE(*p))
	    p++;
	if (*p == '\0' || *p == '\n')
	    break;

	if (i >= size - 1) {
	    size += FILE_LIST_DELTA;
	    RENEW(list, char *, size + 1);
	}
	for (list[i++] = p; *p && *p != '\n' && !ISWHITE(*p); p++)
	    if (*p == '/')
		*p = '\0';
    }
    list[i] = NULL;
    return i ? list : NULL;
}

/*
**  Seek to first line in the history file where the date is after the
**  desired one.   Returns FALSE on failure.
*/
STATIC BOOL
FindLinesAfter(date, line, linesize, F)
    long	date;
    char	*line;
    int		linesize;
    FILE	*F;
{
    char	*p;
    long	upper;
    long	lower;
    long	middle;

    /* Read first line -- is it in our range? */
    (void)fseek(F, 0L, SEEK_SET);
    if (fgets(line, linesize, F) == NULL)
	return FALSE;
    if (GET_DATE(p, line) >= date)
	return TRUE;

    /* Set search ranges and go. */
    lower = 0;
    (void)fseek(F, 0L, SEEK_END);
    upper = ftell(F);
    for ( ; ; ) {
	/* Seek to middle line. */
	middle = (upper + lower) / 2;
	(void)fseek(F, middle, SEEK_SET);
	while (getc(F) != '\n' && ++middle <= upper)
	    continue;

	if (middle >= upper)
	    break;

	if (fgets(line, linesize, F) != NULL && GET_DATE(p, line) > date)
	    upper = middle;
	else if (lower == middle)
	    break;
	else
	    lower = middle;
    }

    /* Move to lower bound; we know this will always be the start of a line. */
    (void)fseek(F, lower, SEEK_SET);
    while (fgets(line, linesize, F) != NULL)
	if (GET_DATE(p, line) >= date)
	    return TRUE;

    return FALSE;
}


/*
**  NEWNEWS newsgroups date time ["GMT"] [<distributions>]
**  Return the Message-ID of any articles after the specified date,
**  and within the specified distributions.
*/
FUNCTYPE
CMDnewnews(ac, av)
    register int	ac;
    char		*av[];
{
    static char		**groups;
    register char	*start;
    register char	*p;
    register FILE	*F;
    register BOOL	AllDists;
    register BOOL	AllGroups;
    char		**distribs;
    char		**files;
    char		line[BIG_BUFFER];
    long		date;

    if (!PERMcanread) {
	Reply("%s\r\n", NNTP_ACCESS);
	return;
    }

    (void)sprintf(line, "%s %s %s %s %s",
	    av[1], av[2], av[3],
	    (ac >= 5 && *av[4] == 'G') ? "GMT" : "local",
	    (ac >= 5 && *av[ac - 1] == '<') ? av[ac - 1] : "none");
    syslog(L_NOTICE, "%s newnews %s", ClientHost, line);

    /* Parse the newsgroups. */
    AllGroups = EQ(av[1], "*");
    if (!AllGroups && !NGgetlist(&groups, av[1])) {
	Reply("%d Bad newsgroup specifier %s\r\n", NNTP_SYNTAX_VAL, av[1]);
	return;
    }

    /* Parse the date. */
    date = NNTPtoGMT(av[2], av[3]);
    if (date < 0) {
	Reply("%d Bad date\r\n", NNTP_SYNTAX_VAL);
	return;
    }
    ac -= 4;
    av += 4;
    if (ac > 0 && caseEQ(*av, "GMT")) {
	ac--;
	av++;
    }
    else
	date = LOCALtoGMT(date);

    /* Parse the distributions. */
    if (ac == 0)
	AllDists = TRUE;
    else {
	if (!ParseDistlist(&distribs, *av)) {
	    Reply("%d Bad distribution %s\r\n", NNTP_SYNTAX_VAL, *av);
	    return;
	}
	AllDists = FALSE;
    }

    if ((F = fopen(HISTORY, "r")) == NULL) {
	syslog(L_ERROR, "%s cant fopen %s %m",
	    ClientHost, HISTORY);
	Reply("%d Can't open history\r\n", NNTP_TEMPERR_VAL);
	return;
    }

    Reply("%s\r\n", NNTP_NEWNEWSOK);

    files = NULL;
    if (FindLinesAfter(date, line, sizeof line, F))
	do {
	    /* Skip two tab-separated fields. */
	    if ((p = strchr(line, HIS_FIELDSEP)) == NULL
	     || (start = strchr(p + 1, HIS_FIELDSEP)) == NULL)
		continue;

	    /* Get the file list. */
	    if (*++start == '\n' || (files = GetFiles(start)) == NULL)
		continue;

	    /* Check permissions. */
	    if (!AllGroups && !PERMmatch(FALSE, groups, files))
		continue;
	    if (!AllDists && !DistMatches(distribs, files))
		continue;
	    *p = '\0';
	    Printf("%s\r\n", line);
	} while (fgets(line, sizeof line, F) != NULL);

    (void)fclose(F);
    Printf(".\r\n");
}
