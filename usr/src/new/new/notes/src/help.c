static char *sccsid = "@(#)help.c	1.2 2/1/83";

#include <sys/types.h>
#include <sys/stat.h>
#include "parms.h"
#include "structs.h"
/*
 *	help(file) char *file;
 *	prints the specified help file using more and then
 *	does a pause with 'hit any key to continue.
 *	
 *	Original idea:	Rob Kolstad January 1982
 */

help(file)
char   *file;
{
    char    cmdline[CMDLEN];			/* line buffer */
    extern char *mypager;

    sprintf(cmdline, "%s/%s/%s", MSTDIR, UTILITY, file);
    dounix(1, 1, mypager, cmdline, 0, 0, 0);
    wfchar();
    return;
}
