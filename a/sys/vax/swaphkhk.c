#include "../h/param.h"
#include "../h/conf.h"
/*
 * Dual rk07 configuration
 *	root on hk0a
 *	paging on hk0b, then hk1b
 */
dev_t	rootdev	= makedev(3, 0);
dev_t	pipedev	= makedev(3, 0);
dev_t	argdev	= makedev(3, 1);
dev_t	dumpdev = makedev(3, 1);
long	dumplo	= 10032 - 2 * 2048;		/* not enough... */

/*
 * Nswap is the basic number of blocks of swap per
 * swap device, and is multiplied by nswdev after
 * nswdev is determined at boot.
 */
int	nswap = 10032;

struct	swdevt swdevt[] =
{
	makedev(3, 1),	0,		/* hk0b */
	makedev(3, 9),	0,		/* hk1b */
	0,		0,
};
