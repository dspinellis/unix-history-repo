/*	confrp.c	4.2	11/9/80	*/

#include "../conf/conf.c"
/*
 * Single rp06/rm03 configuration
 *	root on rp0a
 *	paging on rp0b
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
	0,		0,
};
