/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * diff() compares files and directories via the diff(1) command. Argv is a
 * pointer to an an array of character strings which contain the names of
 * the files and/or directories plus flags for diff. The last argument
 * must be a null pointer. Returns 0 if no differences, 1 if some, 2 if
 * trouble.
 */
#include <sys/param.h>
#include "bin.h"
#include "system.h"

diff(argv)
	char **argv;
{
	int pid;			/* process identity */
	int status;			/* child return status */
	int w;				/* a child id */

	if ((pid = FORK()) == 0)
		{
		execv(DIFF, argv);
		_exit(2);
		}
	while ((w = wait(&status)) != pid && w != -1)
		continue;
	status >>= NBBY;
	status &=  0xff;
	return(status);
}
