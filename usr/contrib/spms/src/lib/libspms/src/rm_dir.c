/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * rm_dir() removes a directory named by dirname via the rmdir command.
 * Returns exit status of rmdir command.
 */
#include <sys/param.h>
#include "bin.h"
#include "macro.h"
#include "system.h"

rm_dir(dirname)
	char *dirname;			/* directory name */
{
	int pid;			/* process identity */
	int status;			/* child return status */
	int w;				/* a child id */

	if ((pid = FORK()) == 0)
		{
		execl(RMDIR, "rmdir", dirname, 0);
		_exit(1);
		}
	while ((w = wait(&status)) != pid && w != -1)
		continue;
	status >>= NBBY;
	status &=  0xff;
	/* check if the directory exists because rmdir is unreliable */
	if (status == 0 && FILEXIST(dirname))
		{
		warn("%s not removed", dirname);
		status = 1;
		}
	return(status);
}
