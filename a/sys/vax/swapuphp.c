#include "../h/param.h"
#include "../h/conf.h"
/*
 * UP0?+PP0? configuration
 *	root on up0a
 *	paging on up0b and then hp0b
 */
dev_t	rootdev	= makedev(2, 0);
dev_t	pipedev	= makedev(2, 0);
dev_t	argdev	= makedev(2, 1);
dev_t	dumpdev = makedev(2, 1);
int	dumplo	= 33440 - 10 * 2048;

/*
 * Nswap is the basic number of blocks of swap per
 * swap device, and is multiplied by nswdev after
 * nswdev is determined at boot.
 */
int	nswap = 33440;

struct	swdevt swdevt[] =
{
	makedev(2, 1),	0,		/* up0b */
	makedev(0, 1),	0,		/* hp0b */
	0,		0,
};
