/*	confup.c	4.2	11/9/80	*/

#include "../conf/conf.c"
/*
 * Single ampex 9300 configuration
 *	root on up0a
 *	paging on up0b
 */
dev_t	rootdev	= makedev(2, 0);
dev_t	pipedev	= makedev(2, 0);
dev_t	argdev	= makedev(2, 1);

/*
 * Nswap is the basic number of blocks of swap per
 * swap device, and is multiplied by nswdev after
 * nswdev is determined at boot.
 */
int	nswap = 33440;

struct	swdevt swdevt[] =
{
	makedev(2, 1),	0,		/* up0b */
	0,		0,
};
