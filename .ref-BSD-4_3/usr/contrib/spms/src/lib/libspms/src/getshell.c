/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * getshell() returns the value the SHELL environment variable (usually
 * the pathname of the user's command interpreter). If SHELL is undefined,
 * getshell() returns SH (defined in header file bin.h).
 */
#include "bin.h"
#include "null.h"

char *
getshell()
{
	char *shell;			/* command shell pathname */
	char *getenv();			/* get environment variable */

	if ((shell = getenv("SHELL")) == NULL)
		return(SH);
	else
		return(shell);
}
