static char *sccsid = "@(#)pattern.c	1.1\t1/23/83";

#include "parms.h"
#include "structs.h"
/*
 *	dopat - matches notesfile names and calls the desired routine with
 *	each matched arguement!
 *
 *	Ray Essick			Feb 25, 1982
 */

FILE * popen ();				/* declare the function! */

dopat (p, what)
char   *p;					/* the pattern to match */
int     (*what) ();				/* routine to be called */
{
    char    cmd[CMDLEN];
    int     c,
            i;
    FILE * zfile;

    sprintf(cmd, "(cd %s;echo %s)", MSTDIR, p);
    zfile = popen(cmd, "r");		/* read what it has to say */

    do {
	i = 0;
	while ((c = getc(zfile)) != ' ' && (c != '\n') && (c != EOF)) {
	    cmd[i++] = c;
	}
	if (i == 0) {
	    break;
	}
	cmd[i++] = '\0';
	i = (*what) (cmd);			/* call his routine for him */
    } while ((c != EOF) && (i != QUITFAST) && (i != QUITUPD));
    pclose(zfile);					/* close that file! */

    return(i);					/* return the return code */
}
