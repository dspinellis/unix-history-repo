/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)dumptraverse.c	5.4 (Berkeley) 2/23/87";
#endif not lint

#include "dump.h"

pass(fn, map)
	register int (*fn)();
	register char *map;
{
	register int bits;
	ino_t maxino;

	maxino = sblock->fs_ipg * sblock->fs_ncg - 1;
	for (ino = 0; ino < maxino; ) {
		if ((ino % NBBY) == 0) {
			bits = ~0;
			if (map != NULL)
				bits = *map++;
		}
		ino++;
		if (bits & 1)
			(*fn)(getino(ino));
		bits >>= 1;
	}
}

mark(ip)
	struct dinode *ip;
{
	register int f;
	extern int anydskipped;

	f = ip->di_mode & IFMT;
	if (f == 0)
		return;
	BIS(ino, clrmap);
	if (f == IFDIR)
		BIS(ino, dirmap);
	if ((ip->di_mtime >= spcl.c_ddate || ip->di_ctime >= spcl.c_ddate) &&
	    !BIT(ino, nodmap)) {
		BIS(ino, nodmap);
		if (f != IFREG && f != IFDIR && f != IFLNK) {
			esize += 1;
			return;
		}
		est(ip);
	} else if (f == IFDIR)
		anydskipped = 1;
}

add(ip)
	register struct	dinode	*ip;
{
	register int i;
	long filesize;

	if(BIT(ino, nodmap))
		return;
	nsubdir = 0;
	dadded = 0;
	filesize = ip->di_size;
	for (i = 0; i < NDADDR; i++) {
		if (ip->di_db[i] != 0)
			dsrch(ip->di_db[i], dblksize(sblock, ip, i), filesize);
		filesize -= sblock->fs_bsize;
	}
	for (i = 0; i < NIADDR; i++) {
		if (ip->di_ib[i] != 0)
			indir(ip->di_ib[i], i, &filesize);
	}
	if(dadded) {
		nadded++;
		if (!BIT(ino, nodmap)) {
			BIS(ino, nodmap);
			est(ip);
		}
	}
	if(nsubdir == 0)
		if(!BIT(ino, nodmap))
			BIC(ino, dirmap);
}

indir(d, n, filesize)
	daddr_t d;
	int n, *filesize;
{
	register i;
	daddr_t	idblk[MAXNINDIR];

	bread(fsbtodb(sblock, d), (char *)idblk, sblock->fs_bsize);
	if(n <= 0) {
		for(i=0; i < NINDIR(sblock); i++) {
			d = idblk[i];
			if(d != 0)
				dsrch(d, sblock->fs_bsize, *filesize);
			*filesize -= sblock->fs_bsize;
		}
	} else {
		n--;
		for(i=0; i < NINDIR(sblock); i++) {
			d = idblk[i];
			if(d != 0)
				indir(d, n, filesize);
		}
	}
}

dirdump(ip)
	struct dinode *ip;
{
	/* watchout for dir inodes deleted and maybe reallocated */
	if ((ip->di_mode & IFMT) != IFDIR)
		return;
	dump(ip);
}

dump(ip)
	struct dinode *ip;
{
	register int i;
	long size;

	if(newtape) {
		newtape = 0;
		bitmap(nodmap, TS_BITS);
	}
	BIC(ino, nodmap);
	spcl.c_dinode = *ip;
	spcl.c_type = TS_INODE;
	spcl.c_count = 0;
	i = ip->di_mode & IFMT;
	if (i == 0) /* free inode */
		return;
	if ((i != IFDIR && i != IFREG && i != IFLNK) || ip->di_size == 0) {
		spclrec();
		return;
	}
	if (ip->di_size > NDADDR * sblock->fs_bsize)
		i = NDADDR * sblock->fs_frag;
	else
		i = howmany(ip->di_size, sblock->fs_fsize);
	blksout(&ip->di_db[0], i);
	size = ip->di_size - NDADDR * sblock->fs_bsize;
	if (size <= 0)
		return;
	for (i = 0; i < NIADDR; i++) {
		dmpindir(ip->di_ib[i], i, &size);
		if (size <= 0)
			return;
	}
}

dmpindir(blk, lvl, size)
	daddr_t blk;
	int lvl;
	long *size;
{
	int i, cnt;
	daddr_t idblk[MAXNINDIR];

	if (blk != 0)
		bread(fsbtodb(sblock, blk), (char *)idblk, sblock->fs_bsize);
	else
		bzero(idblk, sblock->fs_bsize);
	if (lvl <= 0) {
		if (*size < NINDIR(sblock) * sblock->fs_bsize)
			cnt = howmany(*size, sblock->fs_fsize);
		else
			cnt = NINDIR(sblock) * sblock->fs_frag;
		*size -= NINDIR(sblock) * sblock->fs_bsize;
		blksout(&idblk[0], cnt);
		return;
	}
	lvl--;
	for (i = 0; i < NINDIR(sblock); i++) {
		dmpindir(idblk[i], lvl, size);
		if (*size <= 0)
			return;
	}
}

blksout(blkp, frags)
	daddr_t *blkp;
	int frags;
{
	int i, j, count, blks, tbperdb;

	blks = howmany(frags * sblock->fs_fsize, TP_BSIZE);
	tbperdb = sblock->fs_bsize / TP_BSIZE;
	for (i = 0; i < blks; i += TP_NINDIR) {
		if (i + TP_NINDIR > blks)
			count = blks;
		else
			count = i + TP_NINDIR;
		for (j = i; j < count; j++)
			if (blkp[j / tbperdb] != 0)
				spcl.c_addr[j - i] = 1;
			else
				spcl.c_addr[j - i] = 0;
		spcl.c_count = count - i;
		spclrec();
		for (j = i; j < count; j += tbperdb)
			if (blkp[j / tbperdb] != 0)
				if (j + tbperdb <= count)
					dmpblk(blkp[j / tbperdb],
					    sblock->fs_bsize);
				else
					dmpblk(blkp[j / tbperdb],
					    (count - j) * TP_BSIZE);
		spcl.c_type = TS_ADDR;
	}
}

bitmap(map, typ)
	char *map;
{
	register i;
	char *cp;

	spcl.c_type = typ;
	spcl.c_count = howmany(msiz * sizeof(map[0]), TP_BSIZE);
	spclrec();
	for (i = 0, cp = map; i < spcl.c_count; i++, cp += TP_BSIZE)
		taprec(cp);
}

spclrec()
{
	register int s, i, *ip;

	spcl.c_inumber = ino;
	spcl.c_magic = NFS_MAGIC;
	spcl.c_checksum = 0;
	ip = (int *)&spcl;
	s = 0;
	i = sizeof(union u_spcl) / (4*sizeof(int));
	while (--i >= 0) {
		s += *ip++; s += *ip++;
		s += *ip++; s += *ip++;
	}
	spcl.c_checksum = CHECKSUM - s;
	taprec((char *)&spcl);
}

dsrch(d, size, filesize)
	daddr_t d;
	int size, filesize;
{
	register struct direct *dp;
	long loc;
	char dblk[MAXBSIZE];

	if(dadded)
		return;
	if (filesize > size)
		filesize = size;
	bread(fsbtodb(sblock, d), dblk, filesize);
	for (loc = 0; loc < filesize; ) {
		dp = (struct direct *)(dblk + loc);
		if (dp->d_reclen == 0) {
			msg("corrupted directory, inumber %d\n", ino);
			break;
		}
		loc += dp->d_reclen;
		if(dp->d_ino == 0)
			continue;
		if(dp->d_name[0] == '.') {
			if(dp->d_name[1] == '\0')
				continue;
			if(dp->d_name[1] == '.' && dp->d_name[2] == '\0')
				continue;
		}
		if(BIT(dp->d_ino, nodmap)) {
			dadded++;
			return;
		}
		if(BIT(dp->d_ino, dirmap))
			nsubdir++;
	}
}

struct dinode *
getino(ino)
	daddr_t ino;
{
	static daddr_t minino, maxino;
	static struct dinode itab[MAXINOPB];

	if (ino >= minino && ino < maxino) {
		return (&itab[ino - minino]);
	}
	bread(fsbtodb(sblock, itod(sblock, ino)), itab, sblock->fs_bsize);
	minino = ino - (ino % INOPB(sblock));
	maxino = minino + INOPB(sblock);
	return (&itab[ino - minino]);
}

int	breaderrors = 0;		
#define	BREADEMAX 32

bread(da, ba, cnt)
	daddr_t da;
	char *ba;
	int	cnt;	
{
	int n;

loop:
	if (lseek(fi, (long)(da * dev_bsize), 0) < 0){
		msg("bread: lseek fails\n");
	}
	n = read(fi, ba, cnt);
	if (n == cnt)
		return;
	if (da + (cnt / dev_bsize) > fsbtodb(sblock, sblock->fs_size)) {
		/*
		 * Trying to read the final fragment.
		 *
		 * NB - dump only works in TP_BSIZE blocks, hence
		 * rounds `dev_bsize' fragments up to TP_BSIZE pieces.
		 * It should be smarter about not actually trying to
		 * read more than it can get, but for the time being
		 * we punt and scale back the read only when it gets
		 * us into trouble. (mkm 9/25/83)
		 */
		cnt -= dev_bsize;
		goto loop;
	}
	msg("(This should not happen)bread from %s [block %d]: count=%d, got=%d\n",
		disk, da, cnt, n);
	if (++breaderrors > BREADEMAX){
		msg("More than %d block read errors from %d\n",
			BREADEMAX, disk);
		broadcast("DUMP IS AILING!\n");
		msg("This is an unrecoverable error.\n");
		if (!query("Do you want to attempt to continue?")){
			dumpabort();
			/*NOTREACHED*/
		} else
			breaderrors = 0;
	}
}
