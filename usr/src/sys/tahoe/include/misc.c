/*	misc.c	1.1	85/07/21	*/

#include "../h/types.h"
#include "../machine/mtpr.h"

/*
 * make sure addr is not in cache
 */

uncache(addr)
	char *addr;
{
	mtpr(addr, PDCS);
}
