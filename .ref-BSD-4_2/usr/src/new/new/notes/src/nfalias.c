static char *sccsid = "@(#)nfalias.c	1.1\t1/23/83";

#include "parms.h"
#include "structs.h"
#include "net.h"
/*
 *	nfalias(local, remote, sysname)
 *	char *local, *remote, *sysname;
 *
 *	nfalias looks in the alias file for the remote system and
 *	tries to find a match for the local notesfile.
 *	If a match is found, it is copied to *remote, otherwise 
 *	the original name is copied over.
 *
 *	The routine returns -1 if there is no file.
 *	A zero is returned if no match is found.
 *	A 1 is returned if a match is found.
 *
 *	Original Coding:	Ray Essick	April 25, 1982
 */

nfalias (local, remote, sysname)
char   *local;
char   *remote;
char   *sysname;
{

    FILE * aliases;
    char    linebuf[CMDLEN];			/* hold lines from file */
    char   *p,
           *q;
    int     c;

    strcpy (remote, local);			/* pessimism - ready to fail */

    sprintf (linebuf, "%s/%s/%s/%s", MSTDIR, UTILITY, ALIASES, sysname);
    if ((aliases = fopen(linebuf, "r")) == NULL) {
	return(-1);					/* no file, too bad */
    }

    while (1) {
	p = linebuf;					/* start line */
	while ((c = getc(aliases)) != EOF && c != '\n') {
	    *p++ = c;
	}
	if (c == EOF) {
	    fclose(aliases);
	    return(0);					/* no match */
	}
	*p = '\0';					/* terminate string */

	q = linebuf;					/* find colon */
	while (*q != ':' && *q) {
	    q++;					/* try next */
	}
	if (*q != ':') {			/* properly formatted? */
	    fprintf(stderr, "Bad line in alias file for system %s: %s\n",
		    sysname, linebuf);
	    continue;				/* skip the line */
	}
	*q++ = '\0';				/* break into two parts */
	if (strcmp(linebuf, local) == 0) {	/* match ? */
	    strcpy(remote, q);			/* copy the name over */
	    fclose(aliases);
	    return(1);					/* success */
	}
    }
}
