#include "../h/param.h"
#include "../h/conf.h"
/*
 * Single storage module (UDA50) configuration
 *	root on ra0a
 *	paging on ra0b
 */
dev_t	rootdev	= makedev(9, 0);
dev_t	pipedev	= makedev(9, 0);
dev_t	argdev	= makedev(9, 1);
dev_t	dumpdev	= makedev(9, 1);
long	dumplo	= 33440 - 10 * 2048;

/*
 * Nswap is the basic number of blocks of swap per
 * swap device, and is multiplied by nswdev after
 * nswdev is determined at boot.
 */
int	nswap = 33440;

struct	swdevt swdevt[] =
{
	makedev(9, 1),	0,		/* ra0b */
	0,		0,
};
