/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */
#include <stdio.h>
#include "macro.h"
#include "yesno.h"

/*
 * ch_dir changes current working directory. Returns integer YES if
 * successful, otherwise NO.
 */
ch_dir(pathname)
	char *pathname;			/* pathname of destination directory */
{
	extern int IGNORE_BAD_EXIT;	/* exit if command doesn't return 0 */

	if (!CHDIR(pathname))
		{
		pperror(pathname);
		if (IGNORE_BAD_EXIT == NO)
			pxexit();
		return(NO);
		}
	return(YES);
}



/*
 * getpvindex() returns the location of the PROJECT environment
 * variable in the global cell environ.
 */
getpvindex()
{
	extern char **environ;		/* global environment cell */
	register int i;			/* location of PROJECT env. variable */
	int strncmp();			/* compare n characters */

	for (i = 0; environ[i] != NULL; i++)
		if (strncmp(environ[i], "PROJECT=", 8) == 0)
			return(i);
	return(-1);
}



/*
 * nomorecore() prints a warning error message and then exits.
 */
nomorecore()
{
	warn("out of memory");
	pxexit();
}



/*
 * print_title prints a project directory title.
 */
void
print_title(ppathname)
	char *ppathname;		/* project directory pathname */
{
	static int done_command;	/* has a command been done? */

	printf((done_command) ? "\n==> %s <==\n" : "==> %s <==\n", ppathname);
	fflush(stdout);
	done_command = 1;
}



/*
 * pxexit() calls exit(ERRSTATUS).
 */
pxexit()
{
	extern int ERRSTATUS;		/* pexec error status */

	exit(ERRSTATUS);
}
