/*	utime.c	4.2	83/05/31	*/

#include <sys/time.h>
/*
 * Backwards compatible utime.
 */

utime(name, otv)
	char *name;
	int otv[];
{
	struct timeval tv[2];

	tv[0].tv_sec = otv[0]; tv[0].tv_usec = 0;
	tv[1].tv_sec = otv[1]; tv[1].tv_usec = 0;
	return (utimes(name, tv));
}
