/*  $Revision: 1.6 $
**
**  Get data from history database.
*/
#include "configdata.h"
#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#if	defined(DO_NEED_TIME)
#include <time.h>
#endif	/* defined(DO_NEED_TIME) */
#include <sys/time.h>
#include <errno.h>
#include "paths.h"
#include "libinn.h"
#include "clibrary.h"
#include "dbz.h"
#include "macros.h"


/*
**  Get the next filename from the history file.
*/
STATIC BOOL
GetName(F, buff, Againp)
    register FILE	*F;
    register char	*buff;
    BOOL		*Againp;
{
    static char		SPOOL[] = _PATH_SPOOL;
    register int	c;
    register char	*p;

    /* Skip whitespace before filename. */
    while ((c = getc(F)) == ' ')
	continue;
    if (c == EOF || c == '\n')
	return FALSE;

    (void)strcpy(buff, SPOOL);
    p = &buff[STRLEN(SPOOL)];
    *p++ = '/';
    *p++ = (char)c;
    while ((c = getc(F)) != EOF && c != ' ' && c != '\n')
	*p++ = (char)(c == '.' ? '/' : c);
    *p = '\0';
    *Againp = c != EOF && c != '\n';
    return TRUE;
}

/*
**  Given a DBZ value, seek to the right spot.
*/
STATIC BOOL
HistorySeek(F, p)
    register FILE	*F;
    register char	*p;
{
    register char	*dest;
    OFFSET_T		l;
    register int	c;
    register int	i;

    for (dest = (char *)&l, i = sizeof l; --i >= 0; )
	*dest++ = *p++;
    if (fseek(F, l, SEEK_SET) == -1) {
	(void)fprintf(stderr, "Can't seek to %ld, %s\n", l, strerror(errno));
	return FALSE;
    }

    /* Move to the filename fields. */
    for (i = 2; (c = getc(F)) != EOF && c != '\n'; )
	if (c == HIS_FIELDSEP && --i == 0)
	    break;
    if (c != HIS_FIELDSEP)
	/* Could have only two fields (if expired) so don't complain now.
	 * (void)fprintf(stderr, "Bad text line for \"%s\", %s\n",
	 *	key, strerror(errno));
	 */
	return FALSE;

    return TRUE;
}


/*
**  Print the full line from the history file.
*/
STATIC void
FullLine(F, p)
    register FILE	*F;
    register char	*p;
{
    register char	*dest;
    OFFSET_T		l;
    register int	c;
    register int	i;

    for (dest = (char *)&l, i = sizeof l; --i >= 0; )
	*dest++ = *p++;
    if (fseek(F, l, SEEK_SET) == -1) {
	(void)fprintf(stderr, "Can't seek to %ld, %s\n", l, strerror(errno));
	exit(1);
    }

    while ((c = getc(F)) != EOF && c != '\n')
	(void)putchar(c);
    (void)putchar('\n');
}


/*
**  Read stdin for list of Message-ID's, output list of ones we
**  don't have.  Or, output list of files for ones we DO have.
*/
STATIC void
IhaveSendme(History, What)
    STRING		History;
    register char	What;
{
    register FILE	*F;
    register char	*p;
    register char	*q;
    datum		key;
    datum		value;
    struct stat		Sb;
    BOOL		More;
    char		buff[BUFSIZ];
    char		Name[SPOOLNAMEBUFF];

    /* Open history. */
    if (dbminit(History) < 0) {
	(void)fprintf(stderr, "Can't open history database, %s\n",
		strerror(errno));
	exit(1);
    }
    if ((F = fopen(History, "r")) == NULL) {
	(void)fprintf(stderr, "Can't open \"%s\", %s\n",
		History, strerror(errno));
	exit(1);
    }

    while (fgets(buff, sizeof buff, stdin) != NULL) {
	for (p = buff; ISWHITE(*p); p++)
	    continue;
	if (*p != '<')
	    continue;
	for (q = p; *q && *q != '>' && !ISWHITE(*q); q++)
	    continue;
	if (*q != '>')
	    continue;
	*++q = '\0';
	key.dptr = p;
	key.dsize = q - key.dptr + 1;
	value = dbzfetch(key);

	/* Ihave -- say if we want it, and continue. */
	if (What == 'i') {
	    if (value.dptr == NULL)
		(void)printf("%s\n", p);
	    continue;
	}

	/* Sendme -- print a filename for the message. */
	if (value.dptr == NULL)
	    /* Doesn't exist. */
	    continue;
	if (HistorySeek(F, value.dptr))
	    while (GetName(F, Name, &More)) {
		if (stat(Name, &Sb) >= 0) {
		    (void)printf("%s\n", Name);
		    break;
		}
		if (!More)
		    break;
	    }
    }
}


/*
**  Print a usage message and exit.
*/
STATIC NORETURN
Usage()
{
    (void)fprintf(stderr, "Usage: grephistory [flags] MessageID\n");
    exit(1);
}


int
main(ac, av)
    int			ac;
    char		*av[];
{
    register int	i;
    register char	*p;
    register FILE	*F;
    STRING		History;
    datum		key;
    datum		value;
    struct stat		Sb;
    BOOL		More;
    char		What;
    char		Name[SPOOLNAMEBUFF];

    /* Set defaults. */
    History = _PATH_HISTORY;
    What = '?';

    /* Parse JCL. */
    while ((i = getopt(ac, av, "f:eilnqs")) != EOF)
	switch (i) {
	default:
	    Usage();
	    /* NOTREACHED */
	case 'f':
	    History = optarg;
	    break;
	case 'e':
	case 'i':
	case 'l':
	case 'n':
	case 'q':
	case 's':
	    if (What != '?') {
		(void)fprintf(stderr, "Only one [eilnqs] flag allowed.\n");
		exit(1);
	    }
	    What = (char)i;
	    break;
	}
    ac -= optind;
    av += optind;

    /* Set operating mode. */
    switch (What) {
    case '?':
	What = 'n';
	break;
    case 'i':
    case 's':
	IhaveSendme(History, What);
	exit(0);
	/* NOTREACHED */
    }

    /* All modes other than -i -l want a Message-ID. */
    if (ac != 1)
	Usage();

    /* Open the history file, do the lookup. */
    if (dbminit(History) < 0) {
	(void)fprintf(stderr, "Can't open history database, %s\n",
		strerror(errno));
	exit(1);
    }
    key.dptr = av[0];
    if (*key.dptr != '<') {
	/* Add optional braces. */
	key.dptr = NEW(char, 1 + strlen(av[0]) + 1);
	(void)sprintf(key.dptr, "<%s>", av[0]);
    }
    for (p = key.dptr; *p; p++)
	if (*p == HIS_FIELDSEP || *p == '\n')
	    *p = HIS_BADCHAR;
    key.dsize = p - key.dptr + 1;
    value = dbzfetch(key);

    /* Not found. */
    if (value.dptr == NULL) {
	if (What == 'n')
	    (void)fprintf(stderr, "Not found.\n");
	exit(1);
    }

    /* Simple case? */
    if (What == 'q')
	exit(0);

    /* Open the text file, go to the entry. */
    if ((F = fopen(History, "r")) == NULL) {
	(void)fprintf(stderr, "Can't open \"%s\", %s\n",
		History, strerror(errno));
	exit(1);
    }
    if (What == 'l') {
	FullLine(F, value.dptr);
	exit(0);
    }

    /* Loop until we find an existing file. */
    if (HistorySeek(F, value.dptr))
	while (GetName(F, Name, &More)) {
	    if (stat(Name, &Sb) >= 0) {
		(void)printf("%s\n", Name);
		exit(0);
	    }
	    if (!More)
		break;
	}

    if (What == 'n')
	(void)printf("/dev/null\n");
    exit(0);
    /* NOTREACHED */
}
