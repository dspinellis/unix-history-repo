/*  $Revision: 1.3 $
**
**  Read file list on standard input and spew out batchfiles.
*/
#include "configdata.h"
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#if	defined(DO_NEED_TIME)
#include <time.h>
#endif	/* defined(DO_NEED_TIME) */
#include <sys/time.h>
#include "paths.h"
#include "qio.h"
#include "libinn.h"
#include "clibrary.h"
#include "macros.h"


/*
**  Print a usage message and exit.
*/
STATIC NORETURN
Usage()
{
    (void)fprintf(stderr, "convertbatch usage_error.\n");
    exit(1);
}


int
main(ac, av)
    int			ac;
    char		*av[];
{
    static char		SPOOL[] = _PATH_SPOOL;
    static char		HDR[] = "Message-ID:";
    int			i;
    register QIOSTATE	*qp;
    register QIOSTATE	*artp;
    register char	*line;
    register char	*text;
    register char	*format;
    register char	*p;
    register BOOL	Dirty;
    struct stat		Sb;

    /* Parse JCL. */
    format = "nm";
    while ((i = getopt(ac, av, "w:")) != EOF)
	switch (i) {
	default:
	    Usage();
	    /* NOTREACHED */
	case 'w':
	    for (p = format = optarg; *p; p++) {
		switch (*p) {
		case FEED_BYTESIZE:
		case FEED_FULLNAME:
		case FEED_MESSAGEID:
		case FEED_NAME:
		    continue;
		}
		(void)fprintf(stderr, "Ignoring \"%c\" in -w flag.\n", *p);
	    }
	}
    ac -= optind;
    av += optind;
    if (ac)
	Usage();

    if (chdir(SPOOL) < 0) {
	(void)fprintf(stderr, "batchconvert cant chdir %s, %s\n",
		SPOOL, strerror(errno));
	exit(1);
    }

    /* Loop over all input. */
    qp = QIOfdopen((int)fileno(stdin), 0);
    while ((line = QIOread(qp)) != NULL) {
	if (line[0] == '/'
	 && line[STRLEN(SPOOL)] == '/'
	 && EQn(line, SPOOL, STRLEN(SPOOL)))
	    line += STRLEN(SPOOL) + 1;

	for (p = line; *p; p++)
	    if (ISWHITE(*p)) {
		*p = '\0';
		break;
	    }

	if ((artp = QIOopen(line, 0)) == NULL)
	    /* Non-existant article. */
	    continue;

	/* Read article, looking for Message-ID header. */
	while ((text = QIOread(artp)) != NULL) {
	    if (*text == '\0')
		break;
	    if (*text == 'M' && EQn(text, HDR, STRLEN(HDR)))
		break;
	    if ((*text == 'M' || *text == 'm')
	     && caseEQn(text, HDR, STRLEN(HDR)))
		break;
	}
	if (text == NULL || *text == '\0') {
	    QIOclose(artp);
	    continue;
	}

	/* Skip to value of header. */
	for (text += STRLEN(HDR); ISWHITE(*text); text++)
	    continue;
	if (*text == '\0') {
	    QIOclose(artp);
	    continue;
	}

	/* Write the desired info. */
	for (Dirty = FALSE, p = format; *p; p++) {
	    switch (*p) {
	    default:
		continue;
	    case FEED_BYTESIZE:
		if (Dirty)
		    (void)putchar(' ');
		if (stat(line, &Sb) < 0)
		    (void)printf("0");
		else
		    (void)printf("%ld", Sb.st_size);
		break;
	    case FEED_FULLNAME:
		if (Dirty)
		    (void)putchar(' ');
		(void)printf("%s/%s", SPOOL, line);
		break;
	    case FEED_MESSAGEID:
		if (Dirty)
		    (void)putchar(' ');
		(void)printf("%s", text);
		break;
	    case FEED_NAME:
		if (Dirty)
		    (void)putchar(' ');
		(void)printf("%s", line);
		break;
	    }
	    Dirty = TRUE;
	}
	if (Dirty)
	    (void)putchar('\n');

	QIOclose(artp);
    }

    exit(0);
    /* NOTREACHED */
}
