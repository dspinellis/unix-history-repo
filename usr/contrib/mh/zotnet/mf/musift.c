/* musift.c - test out mmuu */

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
    switch (mmdf2uucp (fileno (f), fileno (stdout), FALSE)) {
	case MFOK: 
	    break;

	case MFPRM: 
	    die ("internal error while filtering MMDF mail");

	case MFSIO: 
	    die ("no free file pointers -- you lose");

	case MFERR: 
	    die ("i/o error while filtering MMDF mail");

	case MFROM: 
	case MFHDR: 
	case MFTXT: 
	    fprintf (stderr, "MMDF mail was in bad format, patched...\n");
	    break;
    }
}

/*  */

/* VARARGS */

static  die (fmt, a, b, c, d)
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
