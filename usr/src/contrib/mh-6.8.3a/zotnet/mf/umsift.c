/* umsift.c - test out uumm */

#include "mf.h"
#include <stdio.h>

/*  */

/* ARGSUSED */

main (argc, argv)
int     argc;
char  **argv;
{
    int     oops = 0;
    char    sobuf[BUFSIZ];
    FILE * fp;

    mts_init (*argv);
    setbuf (stdout, sobuf);
    if (argc < 2)
	sift (stdin);
    else
	while (--argc) {
	    if ((fp = fopen (*++argv)) == NULL) {
		perror (*argv);
		oops++;
		continue;
	    }
	    sift (fp);
	    fclose (fp);
	}

    exit (oops);
}

/*  */

static  sift (f)
        FILE * f;
{
    switch (uucp2mmdf (fileno (f), fileno (stdout), FALSE)) {
	case MFOK: 
	    break;

	case MFPRM: 
	    die ("internal error while filtering UUCP mail");

	case MFSIO: 
	    die ("no free file pointers -- you lose");

	case MFERR: 
	    die ("i/o error while filtering UUCP mail");

	case MFROM: 
	case MFHDR: 
	case MFTXT: 
	    fprintf (stderr, "UUCP mail was in bad format, patched...\n");
	    break;
    }
}

/*  */

/* VARARGS */

die (fmt, a, b, c, d)
char   *fmt,
       *a,
       *b,
       *c,
       *d;
{
    fflush (stdout);

    fprintf (stderr, fmt, a, b, c, d);
    putc ('\n', stderr);

    exit (-1);
}
