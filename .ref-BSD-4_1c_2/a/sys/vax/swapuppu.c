#include "../h/param.h"
#include "../h/conf.h"
/*
 * Dual storage module (ampex or cdc 300M or fujitsu 160m) configuration
 *	root on up0a
 *	paging on up0b, then also up2b
 */
dev_t	rootdev	= makedev(2, 0);
dev_t	pipedev	= makedev(2, 0);
dev_t	argdev	= makedev(2, 2);
dev_t	dumpdev	= makedev(2, 2);
long	dumplo	= 33440 - 10 * 2048;

/*
 * Nswap is the basic number of blocks of swap per
 * swap device, and is multiplied by nswdev after
 * nswdev is determined at boot.
 */
int	nswap = 33440;

struct	swdevt swdevt[] =
{
	makedev(2, 1),	0,		/* up0b */
	makedev(2, 17),	0,		/* up1b */
	0,		0,
};
