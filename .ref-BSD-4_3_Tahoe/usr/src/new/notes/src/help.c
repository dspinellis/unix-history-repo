#include "parms.h"
#include "structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: help.c,v 1.7 85/01/18 15:13:16 notes Exp $";
#endif	RCSIDENT

/*
 *	help(file) char *file;
 *	prints the specified help file using more and then
 *	does a pause with 'hit any key to continue.
 *	
 *	Original idea:	Rob Kolstad January 1982
 */

help (file)
char   *file;
{
    char    cmdline[CMDLEN];				/* line buffer */
    char   *command;

    if ((command = getenv ("PAGER")) == NULL)		/* see if overridden */
	command = PAGER;				/* assign default */
#ifndef	FASTFORK
    sprintf (cmdline, "%s < %s/%s/%s", command, Mstdir, UTILITY, file);
    dounix (cmdline, 1, 1);				/* set tty flags */
#else
    sprintf (cmdline, "%s/%s/%s", Mstdir, UTILITY, file);
    dounix (1, 1, command, cmdline, 0, 0, 0);
#endif
							/* and to his uid */
    printf ("  --Hit any key to continue--");
    gchar ();
}
