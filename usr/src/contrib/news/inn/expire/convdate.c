/*  $Revision: 1.6 $
**
**  Convert date strings and numbers to numbers and strings.
*/
#include "configdata.h"
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <sys/types.h>
#if	defined(DO_NEED_TIME)
#include <time.h>
#endif	/* defined(DO_NEED_TIME) */
#include <sys/time.h>
#include "libinn.h"
#include "clibrary.h"
#include "macros.h"


    /* This is sloppy, but good enough. */
STATIC STRING	Program = "convdate";


STATIC BOOL
AllDigits(p)
    register char	*p;
{
    for (; *p; p++)
	if (!CTYPE(isdigit, *p))
	    return FALSE;
	return TRUE;
}

/*
**  Print usage message and exit.
*/
STATIC NORETURN
Usage()
{
    (void)fprintf(stderr, "Usage:  %s [-c|-n|-s] arg...\n", Program);
    exit(1);
}


int
main(ac, av)
    int		ac;
    char	*av[];
{
    static char	CANTCONVERT[] = "%s:  Can't convert \"%s\"\n";
    int		Mode;
    int		i;
    int		s;
    time_t	t;
    char	*p;
    TIMEINFO	Now;

    /* Set defaults. */
    Mode = 0;

    /* Parse JCL. */
    while ((i = getopt(ac, av, "cns")) != EOF)
	switch (i) {
	default:
	    Usage();
	    /* NOTREACHED */
	case 'c':
	case 'n':
	case 's':
	    if (Mode != 0) {
		(void)fprintf(stderr,
			"Only one -c -n or -s flag is allowed.\n");
		exit(1);
	    }
	    Mode = i;
	    break;
	}
    ac -= optind;
    av += optind;
    if (ac == 0)
	Usage();

    /* Get the current time. */
    if (Mode != 'c' && GetTimeInfo(&Now) < 0) {
	(void)fprintf(stderr, "%s:  Can't get time, %s\n",
		Program, strerror(errno));
	exit(1);
    }

    for (s = 0; (p = *av++) != NULL; )
	switch (Mode) {
	default:
	case 's':
	    if ((t = parsedate(p, &Now)) == -1) {
		(void)fprintf(stderr, CANTCONVERT, Program, p);
		s++;
	    }
	    else if ((p = ctime(&t)) == NULL) {
		(void)fprintf(stderr, "%s:  Can't convert %ld to string\n",
			Program, (long)t);
		s++;
	    }
	    else
		(void)printf("%s", p);
	    break;
	case 'n':
	    if ((t = parsedate(p, &Now)) == -1) {
		(void)fprintf(stderr, CANTCONVERT, Program, p);
		s++;
	    }
	    else
		(void)printf("%ld\n", (long)t);
	    break;
	case 'c':
	    if (!AllDigits(p)) {
		(void)fprintf(stderr, CANTCONVERT, Program, p);
		s++;
	    }
	    else {
		t = (time_t)atol(p);
		if ((p = ctime(&t)) == NULL) {
		    (void)fprintf(stderr, CANTCONVERT, Program, p);
		    s++;
		}
		else
		    (void)printf("%s", p);
	    }
	    break;
	}

    exit(s);
    /* NOTREACHED */
}
