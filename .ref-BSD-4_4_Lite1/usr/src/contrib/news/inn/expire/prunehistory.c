/*  $Revision: 1.7 $
**
**  Prune file names from history file.
*/
#include "configdata.h"
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <fcntl.h>
#include "paths.h"
#include "libinn.h"
#include "clibrary.h"
#include "dbz.h"
#include "macros.h"


/*
**  A reusable data buffer.
*/
typedef struct _BUFFER {
    char	*Data;
    int		Size;
    int		Used;
} BUFFER;


/*
**  Convert a pathname into a history-like entry.
*/
STATIC void
Splice(name, Line)
    register char	*name;
    BUFFER		*Line;
{
    static char		SPOOL[] = _PATH_SPOOL;
    register char	*last;
    register char	*p;
    register char	*end;
    register int	i;
    register int	j;

    /* Make sure it's a relative pathname. */
    if (name[0] == '/'
     && name[STRLEN(SPOOL)] == '/'
     && EQn(name, SPOOL, STRLEN(SPOOL)))
	name += STRLEN(SPOOL);

    /* Turn foo/bar/baz into foo.bar/baz and get the length. */
    for (last = NULL, p = name; *p; p++)
	if (*p == '/') {
	    last = p;
	    *p = '.';
	}
    if (last)
	*last = '/';
    i = p - name;
    if (i == 0 || i > Line->Used)
	/* No file (shouldn't happen) or line too small to hold it. */
	return;

    /* Look for the string; allow it to appear multiple times. */
    end = &Line->Data[Line->Used - i];
    for (p = Line->Data; p <= end; )
	if (*p == *name && EQn(p, name, i) && (p == end || ISWHITE(p[i])))
	    for (j = i; --j >= 0; )
		*p++ = ' ';
	else
	    p++;
}


/*
**  Print usage message and exit.
*/
STATIC NORETURN
Usage()
{
    (void)fprintf(stderr, "Usage:  prunehistory [-f file] [input]\n");
    exit(1);
}


int
main(ac, av)
    int			ac;
    char		*av[];
{
    static char		SEPS[] = " \t";
    register char	*p;
    register char	*q;
    register FILE	*rfp;
    register int	wfd;
    register int	c;
    register int	i;
    OFFSET_T		where;
    char		buff[BUFSIZ];
    char		*files;
    datum		key;
    datum		value;
    STRING		History;
    BUFFER		Line;
    BOOL		Passing;

    /* Set defaults. */
    History = _PATH_HISTORY;
    Line.Size = BUFSIZ;
    Line.Data = NEW(char, Line.Size);
    Line.Used = 0;
    Passing = FALSE;

    /* Parse JCL. */
    while ((i = getopt(ac, av, "f:p")) != EOF)
	switch (i) {
	default:
	    Usage();
	    /* NOTREACHED */
	case 'f':
	    History = optarg;
	    break;
	case 'p':
	    Passing = TRUE;
	    break;
	}
    ac -= optind;
    av += optind;
    if (ac)
	Usage();

    /* Open files. */
    if (dbminit(History) < 0) {
	(void)fprintf(stderr, "Can't set up \"%s\" database, %s\n",
		History, strerror(errno));
	exit(1);
    }
    if ((rfp = fopen(History, "r")) == NULL) {
	(void)fprintf(stderr, "Can't open \"%s\" for reading, %s\n",
		History, strerror(errno));
	exit(1);
    }
    if ((wfd = open(History, O_WRONLY)) < 0) {
	(void)fprintf(stderr, "Can't open \"%s\" for writing, %s\n",
		History, strerror(errno));
	(void)fclose(rfp);
	exit(1);
    }

    /* Loop over all input. */
    while (fgets(buff, sizeof buff, stdin) != NULL) {
	if ((p = strchr(buff, '\n')) == NULL) {
	    if (Passing)
		(void)printf("%s\n", buff);
	    else
		(void)fprintf(stderr, "Line too long, ignored:\n\t%s\n", buff);
	    continue;
	}
	*p = '\0';

	/* Ignore blank and comment lines. */
	if (buff[0] == '\0' || buff[0] == COMMENT_CHAR) {
	    if (Passing)
		(void)printf("%s\n", buff);
	    continue;
	}

	if (buff[0] != '<' || (p = strchr(buff, '>')) == NULL) {
	    if (Passing)
		(void)printf("%s\n", buff);
	    else
		(void)fprintf(stderr,
		    "Line doesn't start with a <Message-ID>, ignored:\n\t%s\n",
		    buff);
	    continue;
	}
	*++p = '\0';
	files = p + 1;

	/* Loop up the article. */
	key.dsize = p - buff + 1;
	key.dptr = buff;
	value = dbzfetch(key);
	if (value.dptr == NULL || value.dsize != sizeof where) {
	    (void)fprintf(stderr, "No entry for \"%s\", %s\n",
		    buff, strerror(errno));
	    continue;
	}

	/* Copy the value to an aligned spot. */
	for (p = value.dptr, q = (char *)&where, i = sizeof where; --i >= 0; )
	    *q++ = *p++;
	if (fseek(rfp,  where, SEEK_SET) == -1) {
	    (void)fprintf(stderr, "Can't fseek for \"%s\", %s\n",
		    buff, strerror(errno));
	    continue;
	}

	/* Move forward to the filename fields, and note where they start. */
	for (i = 2; (c = getc(rfp)) != EOF && c != '\n'; )
	    if (c == HIS_FIELDSEP && --i == 0)
		break;
	if (c != HIS_FIELDSEP) {
	    (void)fprintf(stderr, "Bad text line for \"%s\", %s\n",
		    buff, strerror(errno));
	    continue;
	}
	where = ftell(rfp);

	/* Read the the first chunk. */
	i = fread((POINTER)Line.Data, (SIZE_T)1, BUFSIZ, rfp);
	if (i <= 0) {
	    (void)fprintf(stderr, "Bad read for \"%s\", %s\n",
		    buff, strerror(errno));
	    continue;
	}
	Line.Used = i;

	/* No newline; get another chunk and try again. */
	while ((p = memchr((POINTER)Line.Data, '\n', (SIZE_T)Line.Used)) == NULL) {
	    if (Line.Used + BUFSIZ >= Line.Size) {
		Line.Size += BUFSIZ;
		RENEW(Line.Data, char, Line.Size);
	    }
	    i = fread((POINTER)&Line.Data[Line.Used], (SIZE_T)1, BUFSIZ, rfp);
	    if (i <= 0)
		break;
	    Line.Used += i;
	    if (Line.Used >= BUFSIZ * 10) {
		(void)fprintf(stderr, "Too long:  ");
		break;
	    }
	}
	if (p == NULL) {
	    (void)fprintf(stderr, "Can't read line for \"%s\", %s\n",
		    buff, strerror(errno));
	    continue;
	}
	Line.Used = p - Line.Data;

	/* Get the write pointer ready. */
	if (lseek(wfd, where, SEEK_SET) == -1) {
	    (void)fprintf(stderr, "Can't fseek back for \"%s\", %s\n",
		    buff, strerror(errno));
	    continue;
	}

	/* If Message-ID was only thing on line, zap the text line. */
	if (*files == '\0') {
	    (void)memset((POINTER)Line.Data, ' ', (SIZE_T)Line.Used);
	    if (xwrite(wfd, Line.Data, Line.Used) < 0)
		(void)fprintf(stderr, "Can't blank \"%s\", %s\n",
			buff, strerror(errno));
	    continue;
	}

	/* Prune out all the found filenames. */
	if ((p = strtok(files, SEPS)) == NULL) {
	    (void)fprintf(stderr, "Bad input, \"%s\"\n", files);
	    continue;
	}
	do {
	    Splice(p, &Line);
	} while ((p = strtok((char *)NULL, SEPS)) != NULL);

	/* Write out the new line. */
	if (xwrite(wfd, Line.Data, Line.Used) < 0)
	    (void)fprintf(stderr, "Can't write new text for \"%s\", %s\n",
		    buff, strerror(errno));
    }

    /* Close files; we're done. */
    if (close(wfd) < 0)
	(void)fprintf(stderr, "Can't close \"%s\", %s\n",
		History, strerror(errno));
    (void)fclose(rfp);
    exit(0);
    /* NOTREACHED */
}
