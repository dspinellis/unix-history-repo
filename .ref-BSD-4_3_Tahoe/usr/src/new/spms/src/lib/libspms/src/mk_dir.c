/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * mk_dir() creates a directory named by dirname via the mkdir command.
 * Returns exit status of mkdir command.
 */
#include <sys/param.h>
#include "bin.h"
#include "system.h"

mk_dir(dirname)
	char *dirname;			/* directory name */
{
	int pid;			/* process identity */
	int status;			/* child return status */
	int w;				/* a child id */

	if ((pid = FORK()) == 0)
		{
		execl(MKDIR, "mkdir", dirname, 0);
		_exit(1);
		}
	while ((w = wait(&status)) != pid && w != -1)
		continue;
	status >>= NBBY;
	status &=  0xff;
	return(status);
}
