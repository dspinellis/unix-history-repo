/*-
 * Copyright (c) 1982, 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 *
 *	@(#)bmap.c	7.4 (Berkeley) %G%
 */

#include <sys/param.h>
#include <stand.att/saio.h>

#define	NBUFS	4
static char	b[NBUFS][MAXBSIZE];
static daddr_t	blknos[NBUFS];

daddr_t
bmap(io, bn)
	register struct iob *io;
	daddr_t bn;
{
	register struct dinode *ip;
	int i, j, sh;
	daddr_t nb, *bap;

	ip = &io->i_ino;
	if (bn < 0) {
		printf("bn negative\n");
		return ((daddr_t)0);
	}

	/* The first NDADDR blocks are direct blocks. */
	if(bn < NDADDR) {
		nb = ip->di_db[bn];
		return (nb);
	}

	/* Determine the number of levels of indirection. */
	sh = 1;
	bn -= NDADDR;
	for (j = NIADDR; j > 0; j--) {
		sh *= NINDIR(&io->i_fs);
		if (bn < sh)
			break;
		bn -= sh;
	}
	if (j == 0) {
		printf("bn ovf %ld\n", bn);
		return ((daddr_t)0);
	}

	/* Get the first indirect block address. */
	nb = ip->di_ib[NIADDR - j];
	if (nb == 0) {
		printf("bn void %ld\n",bn);
		return ((daddr_t)0);
	}

	/* Get the indirect blocks. */
	for (; j <= NIADDR; j++) {
		if (blknos[j] != nb) {
			io->i_bn = fsbtodb(&io->i_fs, nb) + io->i_boff;
			io->i_ma = b[j];
			io->i_cc = io->i_fs.fs_bsize;
			if (devread(io) != io->i_fs.fs_bsize) {
				if (io->i_error)
					errno = io->i_error;
				printf("bn %ld: read error\n", io->i_bn);
				return ((daddr_t)0);
			}
			blknos[j] = nb;
		}
		bap = (daddr_t *)b[j];
		sh /= NINDIR(&io->i_fs);
		i = (bn / sh) % NINDIR(&io->i_fs);
		nb = bap[i];
		if(nb == 0) {
			printf("bn void %ld\n",bn);
			return ((daddr_t)0);
		}
	}
	return (nb);
}
