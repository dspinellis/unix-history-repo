/*	confup2rp.c	4.3	11/13/80	*/

#include "../conf/conf.c"
/*
 * 2 rp06+ 1 9300 configuration
 *	root on rp0a
 *	paging on up0b, then also rp1b and rp0b
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
	makedev(0, 1),	0,		/* rp0b */
	makedev(0, 9),	0,		/* rp1b */
	0,		0,
};
