/*	conf2up2rp.c	4.2	11/9/80	*/

#include "../conf/conf.c"
/*
 * ERNIE primary configuration with 2 rp's and 2 up's
 *	root on up1
 *	paging on up1 then also rp0 and up0
 */
dev_t	rootdev	= makedev(2, 8);
dev_t	pipedev	= makedev(2, 8);
dev_t	argdev	= makedev(2, 9);

/*
 * Nswap is the basic number of blocks of swap per
 * swap device, and is multiplied by nswdev after
 * nswdev is determined at boot.
 */
int	nswap = 33440;

struct	swdevt swdevt[] =
{
	makedev(2, 9),	0,		/* up1b */
	makedev(0, 9),	0,		/* rp1b */
	makedev(2, 1),	0,		/* up0b */
	0,		0,
};
