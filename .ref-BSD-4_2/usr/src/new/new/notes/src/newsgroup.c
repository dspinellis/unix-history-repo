static char *sccsid = "@(#)newsgroup.c	1.1\t1/23/83";

#include "parms.h"
#include "structs.h"
#include "newsgate.h"
/*
 *	newsgroup(nfname, ngroup,direct)
 *
 *	This routine looks in the file specified by NEWSALIAS
 *	for a correspondence between the notesfile name supplied 
 *	and a news(1) newsgroup.
 *	The resultant match is placed where the second parameter points.
 *	In the event of no match, the same name is passed back.
 *
 *	direct gives us the direction of mapping: 
 *	NFNEWS says that nfname is a notesfile and we find a newsgroup
 *	NEWSNF says that nfname is a newsgroup and we find a nf
 *	in both cases nfname is the input and ngroup is the output
 *
 *	Original Coding:	Ray Essick	April 7, 1982
 */

newsgroup (nfname, ngroup, direct)
char   *nfname;
char   *ngroup;
{

    FILE * groups;
    char    linebuf[CMDLEN];		/* hold lines from file */
    char   *p,
           *q;
    int     c;

    strcpy(ngroup, nfname);		/* arrange for failure */

    sprintf(linebuf, "%s/%s/%s", MSTDIR, UTILITY, NEWSALIAS);
    if ((groups = fopen(linebuf, "r")) == NULL) {
	return(-1);				/* no file, too bad */
    }

    while (1) {
	p = linebuf;				/* start line */
	while ((c = getc(groups)) != EOF && c != '\n') {
	    *p++ = c;
	}
	if (c == EOF) {
	    fclose(groups);
	    return(0);				/* no match */
	}
	*p = '\0';				/* terminate string */

	q = linebuf;				/* find colon */
	while (*q != ':' && *q) {
	    q++;				/* try next */
	}
	if (*q != ':') {			/* properly formatted? */
	    fprintf(stderr, "Bad line in newsgroup file: %s\n", linebuf);
	    continue;				/* skip the line */
	}
	*q++ = '\0';				/* break into two parts */
	if (direct == NFNEWS) {
	    if (strcmp(linebuf, nfname) == 0) {	/* match ? */
		strcpy(ngroup, q);			/* copy it to caller */
		fclose(groups);
		return(1);				/* success */
	    }
	} else {					/* news to nf */
	    if (strcmp(q, nfname) == 0) {
		strcpy(ngroup, linebuf);		/* move find */
		fclose(groups);
		return(1);				/* success */
	    }
	}
    }
}
