static char *sccsid = "@(#)getnet.c	1.1\t1/23/83";

#include "parms.h"
#include "structs.h"
#include "net.h"
/*
 *	getnet(sys, xmit, reply)
 *	char *sys, **xmit, **reply)
 *
 *	This routine will scan the net.how file, looking for alternate
 *	routes to the system specified. The xmit and reply pointers are
 *	set appropriately.
 *	(null if no entry found).
 *	The routine returns -1 if the file is not there, otherwise
 *	it returns the count of lines matched (0,1,2)
 *
 *	Original Coding:	Ray Essick	April 1982
 */

static char outgoing[CMDLEN];
static char incoming[CMDLEN];				/* hold net command */

getnet (sys, xmit, reply)
char   *sys;
char  **xmit;
char  **reply;
{

    FILE * nethow;
    char    buf[BUFSIZ];
    char   *p;
    int     direct;
    int     i,
            count;

    count = 0;					/* number of lines we have */
    *xmit = *reply = NULL;
    sprintf (buf, "%s/%s/%s", MSTDIR, UTILITY, NETHOW);

    if ((nethow = fopen(buf, "r")) == NULL) {
	return(-1);
    }

    while (count < 2) {
	p = buf;				/* set up at the beginning */
	while ((*p = getc(nethow)) != '\n' && *p != EOF) {
	    *p++;				/* grab the line */
	}
	if (*p == EOF) {
	    break;				/* ran out of file */
	}
	*p = '\0';				/* null terminate the line */

	p = buf;
	while (*p != ':') {
	    p++;				/* skip first field */
	}
	*p = '\0';				/* null terminate */
	if (strcmp(buf, sys) != 0) {
	    continue;				/* not what we want */
	}

	p++;						/* which direction */
	if (*p == 'x') {
	    direct = 0;					/* out */
	} else {
	    direct = 1;					/* in */
	}

	for (i = 0; i < 3; i++) {
	    while (*p++ != ':');			/* skip fields */
	}

	if (*p == '\0')	{			/* bad line, forget it */
	    continue;
	}

	if (direct == 0) {				/* outgoing */
	    *xmit = outgoing;
	    strcpy(outgoing, p);			/* move it */
	} else {
	    *reply = incoming;
	    strcpy(incoming, p);			/* incoming */
	}

	count++;					/* bump the count */
    }
    fclose(nethow);
    return(count);
}
