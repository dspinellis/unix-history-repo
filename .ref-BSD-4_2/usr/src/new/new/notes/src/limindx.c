static char *sccsid = "@(#)limindx.c	1.2 2/1/83";

#include "parms.h"
#include "structs.h"
/*
 *	limindx(io) struct io_f *io;
 *	runs a limited form of the index and prntind functions.
 *	its purpose is to come up with a note number for forwarding
 *	notes as responses.
 *
 *	Returns the note number picked. 
 *		0 means no note.
 *
 *	Original coding:	Ray Essick	January 1982
 *
 */

limindx (io)
struct io_f *io;
{
    int     firstdis,
            lastdis;
    char    c;
    int     i,
            znote,
            zresp;

    firstdis = io->descr.d_nnote - nindex + 1; /* start at the end of the file */
    while (1) {
	prntind(io, &firstdis, &lastdis);		/* show the page */
	at(2, 28);
	printf("---- Limited Index ----");
	at(-1, 1);
	c = gchar();
	switch (c) {
	    case '?': 
	    case 'h': 
		help(LIMHLP);
		break;

	    case '\r': 
	    case '\n': 
	    case ' ': 
	    case '+': 					/* advance 1 page */
		firstdis = lastdis;
		break;

	    case '-': 					/* backwards 1 page */
	    case '\b': 
		firstdis -= nindex;
		break;

	    case '=': 					/* back to the start */
		firstdis = 1;
		break;

	    case '*': 					/* last page of index */
		firstdis = io->descr.d_nnote - nindex + 1;
		break;

	    case '!': 					/* give him a shell */
		gshell ();
		break;

	    case 'q': 					/* leave, giving up */
	    case 'Q': 					/* and some others .. */
	    case 'k': 
	    case 'K': 
		return 0;

	    case 'x': 					/* search for title */
	    case 'X': 
		i = tsearch(io, lastdis, c == 'x'); /* assume lies before here */
		if (i > 0) {
		    firstdis = i;		/* set him on that page */
		}
		continue;			/* otherwise get another key */

	    case 'a': 
	    case 'A': 			/* author search from current spot */
		znote = lastdis;
		zresp = 0;		/* start at the correct place */
		do {
		    i = asearch (io, &znote, &zresp, (c == 'a'));
							/* look */
		    if (i > 0) {
			if (zresp != 0) {
			    continue;			/* not a 'note' */
			}
			firstdis = znote;		/* go there */
			break;				/* out of this loop */
		    }
		}
		while (i > 0);			/* until not found */
		continue;			/* get another command */

	    case '0': 				/* pick a note */
	    case '1': 
	    case '2': 
	    case '3': 
	    case '4': 
	    case '5': 
	    case '6': 
	    case '7': 
	    case '8': 
	    case '9': 
		prompt("Note number > ");
		return(getnum(c));
	}
    }
}
