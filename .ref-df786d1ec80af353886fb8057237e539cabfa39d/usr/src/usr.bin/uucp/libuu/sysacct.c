#ifndef lint
static char sccsid[] = "@(#)sysacct.c	5.1 (Berkeley) %G%";
#endif

#include <sys/types.h>

/*******
 *	sysacct(bytes, time)	output accounting info
 *	time_t time;
 *	long bytes;
 */

sysacct(bytes, time)
time_t time;
long bytes;
{
	return;
}
