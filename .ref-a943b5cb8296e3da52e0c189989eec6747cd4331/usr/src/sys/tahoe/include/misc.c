/*	misc.c	1.2	85/07/29	*/

#include "../h/types.h"
#include "../tahoe/mtpr.h"

/*
 * make sure addr is not in cache
 */

uncache(addr)
	char *addr;
{
	mtpr(PDCS, addr);
}
