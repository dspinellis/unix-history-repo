/* help.c - print the usage line */
#ifndef lint
static char ident[] = "@(#)$Id: help.c,v 1.3 1992/12/15 00:20:22 jromine Exp $";
#endif	/*  lint */

#include "../h/mh.h"
#include <stdio.h>


extern char *options[];


void help (str, swp)
register char  *str;
register struct swit   *swp;
{
    int     nameoutput,
            len,
            linepos,
            outputlinelen;
    register char  *cp,
                  **ap;

    printf ("syntax: %s\n", str);
    printf ("  switches are:\n");
    printsw (ALL, swp, "-");

    if (ssequal ("@(#)", cp = version))
	cp += 4;
    printf ("\nversion: %s\n", cp);

    nameoutput = linepos = 0;
    outputlinelen = OUTPUTLINELEN;
    for (ap = options; *ap; ap++) {
	if (!nameoutput) {
	    fprintf (stdout, "%s: ", cp = "options");
	    linepos += (nameoutput = strlen (cp) + 2);
	}
	len = strlen (cp = *ap);
	if (linepos != nameoutput)
	    if (len + linepos + 3 > outputlinelen)
		fprintf (stdout, "\n%*s", linepos = nameoutput, "");
	    else {
		fputs (" ", stdout);
		linepos++;
	    }
	fprintf (stdout, "[%s]", cp);
	linepos += len + 2;
    }

    if (linepos)
	(void) fputc ('\n', stdout);
}
