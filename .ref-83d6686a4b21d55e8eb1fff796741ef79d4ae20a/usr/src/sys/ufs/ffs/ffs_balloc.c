/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)ffs_balloc.c	7.3 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "user.h"
#include "buf.h"
#include "proc.h"
#include "file.h"
#include "vnode.h"
#include "../ufs/inode.h"
#include "../ufs/fs.h"

/*
 * Bmap defines the structure of file system storage
 * by returning the physical block number on a device given the
 * inode and the logical block number in a file.
 * When convenient, it also leaves the physical
 * block number of the next block of the file in rablock
 * for use in read-ahead.
 */
bmap(ip, bn, bnp, rablockp, rasizep)
	register struct inode *ip;
	register daddr_t bn;
	daddr_t	*bnp;
	daddr_t	*rablockp;
	int *rasizep;
{
	register struct fs *fs;
	register daddr_t nb;
	struct buf *bp;
	daddr_t *bap;
	int i, j, sh;
	int error;

	if (bn < 0)
		return (EFBIG);
	fs = ip->i_fs;

	/*
	 * The first NDADDR blocks are direct blocks
	 */
	if (bn < NDADDR) {
		nb = ip->i_db[bn];
		if (nb == 0) {
			*bnp = (daddr_t)-1;
			return (0);
		}
		if (rablockp && rasizep) {
			if (bn < NDADDR - 1) {
				*rablockp = fsbtodb(fs, ip->i_db[bn + 1]);
				*rasizep = blksize(fs, ip, bn + 1);
			} else {
				*rablockp = 0;
				*rasizep = 0;
			}
		}
		*bnp = fsbtodb(fs, nb);
		return (0);
	}

	/*
	 * Determine how many levels of indirection.
	 */
	sh = 1;
	bn -= NDADDR;
	for (j = NIADDR; j > 0; j--) {
		sh *= NINDIR(fs);
		if (bn < sh)
			break;
		bn -= sh;
	}
	if (j == 0)
		return (EFBIG);

	/*
	 * fetch the first indirect block
	 */
	nb = ip->i_ib[NIADDR - j];
	if (nb == 0) {
		*bnp = (daddr_t)-1;
		return (0);
	}

	/*
	 * fetch through the indirect blocks
	 */
	for (; j <= NIADDR; j++) {
		if (error = bread(ip->i_devvp, fsbtodb(fs, nb),
		    (int)fs->fs_bsize, &bp)) {
			brelse(bp);
			return (error);
		}
		bap = bp->b_un.b_daddr;
		sh /= NINDIR(fs);
		i = (bn / sh) % NINDIR(fs);
		nb = bap[i];
		if (nb == 0) {
			*bnp = (daddr_t)-1;
			brelse(bp);
			return (0);
		}
	}

	/*
	 * calculate read-ahead.
	 */
	if (rablockp && rasizep) {
		if (i < NINDIR(fs) - 1) {
			*rablockp = fsbtodb(fs, bap[i + 1]);
			*rasizep = fs->fs_bsize;
		} else {
			*rablockp = 0;
			*rasizep = 0;
		}
	}
	*bnp = fsbtodb(fs, nb);
	brelse(bp);
	return (0);
}

/*
 * Balloc defines the structure of file system storage
 * by returning the physical block number on a device given the
 * inode and the logical block number in a file.
 * When unallocated entries are found, new physical blocks
 * are allocated.
 */
balloc(ip, bn, size, bnp, flags)
	register struct inode *ip;
	register daddr_t bn;
	int size;
	daddr_t	*bnp;
	int flags;
{
	register struct fs *fs;
	register daddr_t nb;
	struct buf *bp, *nbp;
	int osize, nsize, i, j, sh, error;
	daddr_t lbn, *bap, pref, blkpref();

	if (bn < 0)
		return (EFBIG);
	fs = ip->i_fs;

	/*
	 * If the next write will extend the file into a new block,
	 * and the file is currently composed of a fragment
	 * this fragment has to be extended to be a full block.
	 */
	nb = lblkno(fs, ip->i_size);
	if (nb < NDADDR && nb < bn) {
		osize = blksize(fs, ip, nb);
		if (osize < fs->fs_bsize && osize > 0) {
			error = realloccg(ip, ip->i_db[nb],
				blkpref(ip, nb, (int)nb, &ip->i_db[0]),
				osize, (int)fs->fs_bsize, &bp);
			if (error) {
				*bnp = (daddr_t)-1;
				return (error);
			}
			ip->i_size = (nb + 1) * fs->fs_bsize;
			ip->i_db[nb] = dbtofsb(fs, bp->b_blkno);
			ip->i_flag |= IUPD|ICHG;
			bdwrite(bp);
		}
	}
	/*
	 * The first NDADDR blocks are direct blocks
	 */
	if (bn < NDADDR) {
		nb = ip->i_db[bn];
		if (nb == 0 || ip->i_size < (bn + 1) * fs->fs_bsize) {
			if (nb != 0) {
				/* consider need to reallocate a frag */
				osize = fragroundup(fs, blkoff(fs, ip->i_size));
				nsize = fragroundup(fs, size);
				if (nsize <= osize)
					goto gotit;
				error = realloccg(ip, nb,
					blkpref(ip, bn, (int)bn, &ip->i_db[0]),
					osize, nsize, &bp);
			} else {
				if (ip->i_size < (bn + 1) * fs->fs_bsize)
					nsize = fragroundup(fs, size);
				else
					nsize = fs->fs_bsize;
				error = alloc(ip,
					blkpref(ip, bn, (int)bn, &ip->i_db[0]),
					nsize, &bp, flags);
			}
			if (error) {
				*bnp = (daddr_t)-1;
				return (error);
			}
			nb = dbtofsb(fs, bp->b_blkno);
			if ((ip->i_mode & IFMT) == IFDIR)
				/*
				 * Write directory blocks synchronously
				 * so they never appear with garbage in
				 * them on the disk.
				 * 
				 * NB: Should free space and return error
				 * if bwrite returns an error.
				 */
				error = bwrite(bp);
			else
				bdwrite(bp);
			ip->i_db[bn] = nb;
			ip->i_flag |= IUPD|ICHG;
		}
gotit:
		*bnp = fsbtodb(fs, nb);
		return (0);
	}

	/*
	 * Determine how many levels of indirection.
	 */
	pref = 0;
	sh = 1;
	lbn = bn;
	bn -= NDADDR;
	for (j = NIADDR; j > 0; j--) {
		sh *= NINDIR(fs);
		if (bn < sh)
			break;
		bn -= sh;
	}
	if (j == 0)
		return (EFBIG);

	/*
	 * fetch the first indirect block
	 */
	nb = ip->i_ib[NIADDR - j];
	if (nb == 0) {
		pref = blkpref(ip, lbn, 0, (daddr_t *)0);
	        error = alloc(ip, pref, (int)fs->fs_bsize, &bp, B_CLRBUF);
		if (error) {
			*bnp = (daddr_t)-1;
			return (error);
		}
		nb = dbtofsb(fs, bp->b_blkno);
		/*
		 * Write synchronously so that indirect blocks
		 * never point at garbage.
		 * 
		 * NB: Should free space and return error
		 * if bwrite returns an error.
		 */
		error = bwrite(bp);
		ip->i_ib[NIADDR - j] = nb;
		ip->i_flag |= IUPD|ICHG;
	}

	/*
	 * fetch through the indirect blocks
	 */
	for (; j <= NIADDR; j++) {
#ifdef SECSIZE
		bp = bread(ip->i_dev, fsbtodb(fs, nb), (int)fs->fs_bsize,
		    fs->fs_dbsize);
#else SECSIZE
		if (error = bread(ip->i_devvp, fsbtodb(fs, nb),
		    (int)fs->fs_bsize, &bp)) {
			brelse(bp);
			return (error);
		}
		bap = bp->b_un.b_daddr;
		sh /= NINDIR(fs);
		i = (bn / sh) % NINDIR(fs);
		nb = bap[i];
		if (nb == 0) {
			if (pref == 0)
				if (j < NIADDR)
					pref = blkpref(ip, lbn, 0,
						(daddr_t *)0);
				else
					pref = blkpref(ip, lbn, i, &bap[0]);
		        error = alloc(ip, pref, (int)fs->fs_bsize, &nbp,
				(j < NIADDR) ? B_CLRBUF : flags);
			if (error) {
				brelse(bp);
				*bnp = (daddr_t)-1;
				return (error);
			}
			nb = dbtofsb(fs, nbp->b_blkno);
			if (j < NIADDR || (ip->i_mode & IFMT) == IFDIR)
				/*
				 * Write synchronously so indirect blocks
				 * never point at garbage and blocks
				 * in directories never contain garbage.
				 * 
				 * NB: Should free space and return error
				 * if bwrite returns an error.
				 */
				error = bwrite(nbp);
			else
				bdwrite(nbp);
			bap[i] = nb;
			bdwrite(bp);
		} else
			brelse(bp);
	}

	*bnp = fsbtodb(fs, nb);
	return (0);
}
