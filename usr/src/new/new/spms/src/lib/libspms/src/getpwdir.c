/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * getpwdir() searchs from the beginning of the passwd file until a
 * matching home directory is found. If found, a pointer is returned
 * to a passwd struct, otherwise NULL.
 */
#include <pwd.h>
#include "macro.h"
#include "null.h"

struct passwd *
getpwdir(dir)
	char *dir;			/* directory to be matched */
{
	register struct passwd *pw;	/* pointer to current passwd entry */
	int endpwent();			/* close passwd file */
	struct passwd *getpwent();	/* get next passwd entry */

	for (;;)
		{
		if ((pw = getpwent()) == NULL)
			break;
		if (EQUAL(dir, pw->pw_dir))
			break;
		}
	endpwent();
	return(pw);
}
