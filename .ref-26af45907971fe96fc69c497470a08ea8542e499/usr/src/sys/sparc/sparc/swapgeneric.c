/*
 * dummy swapgeneric.c: not used in new config system.
 *
 *	@(#)swapgeneric.c	7.1 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/conf.h>

dev_t	rootdev = NODEV;
dev_t	dumpdev = NODEV;

struct	swdevt swdevt[] = {
	{ NODEV, 0, 0 },
	{ NODEV, 0, 0 }
};
