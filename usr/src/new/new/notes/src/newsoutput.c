static char *sccsid = "@(#)newsoutput.c	1.1\t1/23/83";

#include "parms.h"
#include "structs.h"
#include "globs.h"
#include "newsgate.h"
/*
 *	newsoutput - place the specified notesfiles out to the
 *	news susbsystem. 
 *
 *	Original Coding:	Ray Essick	April 1982
 */

static int  othersys;			/* dump other people to news */

newsone (nfname)
char   *nfname;
{
    return newsout(nfname, othersys);
}


main (argc, argv)
char  **argv;
{
    char    nf[NNLEN];
    FILE * altfile;
    int     i,
            count,
            c;

#include "main.i"			/* common init code and such */

    if (argc == 1) {
	printf ("Usage: %s [-a] [-f file] topic1 [topic2 ...]\n", argv[0]);
	exit (BAD);
    }

#ifndef DEMANDNEWS
    if (globuid != NOTESUID) {
	printf ("Sorry, only notes 'owner' can send notes to news\n");
	exit (BAD);
    }
#endif

    othersys = 0;			/* default to jus self */

    for (i = 1; i < argc; i++) {
	if (argv[i][0] == '-') {
	    switch (argv[i][1]) {

		case 'a':			/* send -A-ll systems notes */
		    othersys = 1;
		    break;

		case 'f': 				/* process a file */
		    readrc(argv[++i]);
		    break;

		default: 
		    printf ("Bad switch: %c\n", argv[i][1]);
		    exit (BAD);
	    }
	} else {
	    expand(argv[i]);
	}
    }

    for (i = 0; i < last_group; i++) {
	if (group[i].lookat == 1) {
	    newsone(group[i].name);
	}
    }
}
