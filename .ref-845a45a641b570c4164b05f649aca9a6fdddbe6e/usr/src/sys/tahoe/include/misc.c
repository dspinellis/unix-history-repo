/*	misc.c	7.1	88/05/21	*/

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
