/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * gethdir() returns a pointer to a string containing the pathname of the
 * root project directory belonging to username. If username is null or a
 * zero length string, the root project directory belonging to the current
 * user returned. A null pointer is returned on error.
 */
#include <stdio.h>
#include <pwd.h>
#include "null.h"
#include "path.h"

char *
gethdir(username)
	char *username;			/* user's login name */
{
	char *getenv();			/* get environment variable */
	char *hdir;			/* pointer to home directory pathname */
	char *strcpy();			/* string copy */
	struct passwd *getpwnam();	/* get password file entry by name */
	struct passwd *getpwuid();	/* get password file entry by uid */
	struct passwd *pw;		/* passwd struct pointer */
	
	if (username == NULL || *username == '\0')
		{
		if ((hdir = getenv("ROOTPROJECT")) != NULL)
			return(hdir);
		if ((hdir = getenv("HOME")) != NULL)
			return(hdir);
		pw = getpwuid(getuid());
		}
	else if ((pw = getpwnam(username)) == NULL)
		{
		warn("unknown user %s", username);
		return(NULL);
		}
	return(pw->pw_dir);
}
