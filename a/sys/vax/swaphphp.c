#include "../h/param.h"
#include "../h/conf.h"
/*
 * Dual rp0?/rm?? configuration
 *	root on hp0a
 *	paging on hp0b and then hp1b
 */
dev_t	rootdev	= makedev(0, 0);
dev_t	pipedev	= makedev(0, 0);
dev_t	argdev	= makedev(0, 1);
dev_t	dumpdev = makedev(0, 1);
int	dumplo	= 33440 - 10 * 2048;

/*
 * Nswap is the basic number of blocks of swap per
 * swap device, and is multiplied by nswdev after
 * nswdev is determined at boot.
 */
int	nswap = 33440;

struct	swdevt swdevt[] =
{
	makedev(0, 1),	0,		/* hp0b */
	makedev(0, 9),	0,		/* hp1b */
	0,		0,
};
