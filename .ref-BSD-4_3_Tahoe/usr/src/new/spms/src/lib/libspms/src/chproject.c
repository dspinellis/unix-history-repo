/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * chproject() changes the name of the project root directory pathname.
 * Returns integer YES if successful, otherwise NO.
 */
#include "null.h"
#include "path.h"
#include "yesno.h"

chproject(ppathname)
	char *ppathname;		/* new project pathname */
{
	extern char *_PROJECT;		/* project root directory pathname */
	char *strsav();			/* save a string somewhere */
	PATH pathbuf;			/* pathname buffer */

	if (xppath(ppathname, &pathbuf) == -1)
		{
		patherr(ppathname);
		return(NO);
		}
	else	{
		switch (pathbuf.p_mode & P_IFMT)
			{
			case P_IFNEW:
			case P_IFREG:
			case P_IFPDIR:
				warn("%s: no such project", ppathname);
				return(NO);
			case P_IFHOME:
			case P_IFPROOT:
				if (_PROJECT != NULL)
					free(_PROJECT);
				if ((_PROJECT = strsav(pathbuf.p_path)) == NULL)
					{
					warn("out of memory");
					return(NO);
					}
				break;
			}
		}
	return(YES);
}
