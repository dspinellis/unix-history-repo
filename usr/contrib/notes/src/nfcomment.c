#include "parms.h"
#include "structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: nfcomment.c,v 1.7 85/01/18 15:25:46 notes Rel $";
#endif	RCSIDENT

/*
 *	nfcomment(nfname, text, title, dirflag, anonflag)
 *	char *nfname, *text, *title;
 *
 *	Allows the user to insert notes into notesfiles from his programs.
 *	The text is inserted into the notesfile 'nfname' with the 
 *	title specified. If dirflag or anonflag are non-null the
 *	director or anonymous bits are enabled, provided the user has
 *	permission.
 *	If text is NULL, the text for the note will be taken from
 *	standard input.
 *
 *	Original Coding:	Ray Essick	April 1982
 */

FILE * popen ();

nfcomment (nfname, text, title, dirflag, anonflag)
char   *nfname;
char   *text;
char   *title;
{

    FILE * zpipe;
    char    cmdline[CMDLEN];

    if (nfname == NULL)
    {
	fprintf (stderr, "nfcomment: No notesfile specified\n");
	return (-1);
    }
    if (title == NULL)
	title = "From nfcomment";

    sprintf (cmdline, "%s/nfpipe %s -t \"%s\" %s %s",
	    BIN,					/* where nfpipe lives */
	    nfname,
	    title,
	    dirflag ? "-d" : " ",
	    anonflag ? "-a" : " ");

    if (text == NULL)
    {
	system (cmdline);
	fprintf (stderr, "EOT\n");			/* let him know he hit EOT */
    }
    else
    {
	if ((zpipe = popen (cmdline, "w")) == NULL)
	    return (-1);
	while (*text)
	    putc (*text++, zpipe);
	pclose (zpipe);
    }
    return 0;
}
