/*	conf2rpup.c	4.2	11/9/80	*/

#include "../conf/conf.c"
/*
 * 2 rp06+ 1 9300 configuration
 *	root on rp0a
 *	paging on rp0b, then also rp1b and up0b
 */
dev_t	rootdev	= makedev(0, 0);
dev_t	pipedev	= makedev(0, 0);
dev_t	argdev	= makedev(0, 1);

/*
 * Nswap is the basic number of blocks of swap per
 * swap device, and is multiplied by nswdev after
 * nswdev is determined at boot.
 */
int	nswap = 33440;

struct	swdevt swdevt[] =
{
	makedev(0, 1),	0,		/* rp0b */
	makedev(0, 9),	0,		/* rp1b */
	makedev(2, 1),	0,		/* up0b */
	0,		0,
};
