static	char *sccsid = "@(#)traverse.c	1.3 (Berkeley) %G%";
#include "dump.h"

struct	fs	sblock;

pass(fn, map)
	int (*fn)();
	short *map;
{
	struct dinode *dp;
	int bits;
	ino_t maxino;

	sync();
	bread(SBLOCK, &sblock, sizeof sblock);
	if (sblock.fs_magic != FS_MAGIC) {
		msg("bad sblock magic number\n");
		dumpabort();
	}
	maxino = sblock.fs_ipg * sblock.fs_ncg;
	for (ino = 0; ino < maxino; ) {
		if((ino % MLEN) == 0) {
			bits = ~0;
			if(map != NULL)
				bits = *map++;
		}
		ino++;
		if(bits & 1) {
			dp = getino(ino);
			(*fn)(dp);
		}
		bits >>= 1;
	}
}

icat(dp, fn1)
	register struct	dinode	*dp;
	int (*fn1)();
{
	register int i;

	for (i = 0; i < NDADDR; i++) {
		if (dp->di_db[i] != 0)
			(*fn1)(dp->di_db[i]);
	}
	for (i = 0; i < NIADDR; i++) {
		if (dp->di_ib[i] != 0)
			indir(dp->di_ib[i], fn1, i);
	}
}

indir(d, fn1, n)
	daddr_t d;
	int (*fn1)();
	int n;
{
	register i;
	daddr_t	idblk[NINDIR];

	bread(d, (char *)idblk, sizeof(idblk));
	if(n <= 0) {
		for(i=0; i<NINDIR; i++) {
			d = idblk[i];
			if(d != 0)
				(*fn1)(d);
		}
	} else {
		n--;
		for(i=0; i<NINDIR; i++) {
			d = idblk[i];
			if(d != 0)
				indir(d, fn1, n);
		}
	}
}

mark(ip)
	struct dinode *ip;
{
	register f;

	f = ip->di_mode & IFMT;
	if(f == 0)
		return;
	BIS(ino, clrmap);
	if(f == IFDIR)
		BIS(ino, dirmap);
	if(ip->di_mtime >= spcl.c_ddate ||
	   ip->di_ctime >= spcl.c_ddate) {
		BIS(ino, nodmap);
		if (f != IFREG){
			esize += 1;
			return;
		}
		est(ip);
	}
}

add(ip)
	struct dinode *ip;
{

	if(BIT(ino, nodmap))
		return;
	nsubdir = 0;
	dadded = 0;
	icat(ip, dsrch);
	if(dadded) {
		BIS(ino, nodmap);
		est(ip);
		nadded++;
	}
	if(nsubdir == 0)
		if(!BIT(ino, nodmap))
			BIC(ino, dirmap);
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
	if ((i != IFDIR && i != IFREG) || ip->di_size == 0) {
		spclrec();
		return;
	}
	if (ip->di_size > NDADDR * BSIZE)
		i = NDADDR * FRAG;
	else
		i = howmany(ip->di_size, FSIZE);
	blksout(&ip->di_db[0], i);
	size = ip->di_size - NDADDR * BSIZE;
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
	daddr_t idblk[NINDIR];

	if (blk != 0)
		bread(blk, (char *)idblk, sizeof(idblk));
	else
		blkclr(idblk, sizeof(idblk));
	if (lvl <= 0) {
		if (*size < NINDIR * BSIZE)
			cnt = howmany(*size, TP_BSIZE);
		else
			cnt = NINDIR * BLKING * FRAG;
		*size -= NINDIR * BSIZE;
		blksout(&idblk[0], cnt);
		return;
	}
	lvl--;
	for (i = 0; i < NINDIR; i++) {
		dmpindir(idblk[i], lvl, size);
		if (*size <= 0)
			return;
	}
}

blksout(blkp, frags)
	daddr_t *blkp;
	int frags;
{
	int i, j, count, blks;

	blks = frags * BLKING;
	for (i = 0; i < blks; i += TP_NINDIR) {
		if (i + TP_NINDIR > blks)
			count = blks;
		else
			count = i + TP_NINDIR;
		for (j = i; j < count; j++)
			if (blkp[j / (BLKING * FRAG)] != 0)
				spcl.c_addr[j - i] = 1;
			else
				spcl.c_addr[j - i] = 0;
		spcl.c_count = count - i;
		spclrec();
		for (j = i; j < count; j += (BLKING * FRAG))
			if (blkp[j / (BLKING * FRAG)] != 0)
				if (j + (BLKING * FRAG) <= count)
					dmpblk(blkp[j / (BLKING * FRAG)],
					    BSIZE);
				else
					dmpblk(blkp[j / (BLKING * FRAG)],
					    (count - j) * TP_BSIZE);
		spcl.c_type = TS_ADDR;
	}
}

bitmap(map, typ)
	short *map;
{
	register i, n;
	char *cp;

	n = -1;
	for(i=0; i<MSIZ; i++)
		if(map[i])
			n = i;
	if(n < 0)
		return;
	spcl.c_type = typ;
	spcl.c_count = (n*sizeof(map[0]) + TP_BSIZE)/TP_BSIZE;
	spclrec();
	cp = (char *)map;
	for(i=0; i<spcl.c_count; i++) {
		taprec(cp);
		cp += TP_BSIZE;
	}
}

spclrec()
{
	register int s, i, *ip;

	spcl.c_inumber = ino;
	spcl.c_magic = MAGIC;
	spcl.c_checksum = 0;
	ip = (int *)&spcl;
	s = 0;
	for(i = 0; i < sizeof(union u_spcl)/sizeof(int); i++)
		s += *ip++;
	spcl.c_checksum = CHECKSUM - s;
	taprec((char *)&spcl);
}

dsrch(d)
	daddr_t d;
{
	register char *cp;
	register i;
	register ino_t in;
	struct direct dblk[DIRPB];

	if(dadded)
		return;
	bread(d, (char *)dblk, sizeof(dblk));
	for(i=0; i<DIRPB; i++) {
		in = dblk[i].d_ino;
		if(in == 0)
			continue;
		cp = dblk[i].d_name;
		if(cp[0] == '.') {
			if(cp[1] == '\0')
				continue;
			if(cp[1] == '.' && cp[2] == '\0')
				continue;
		}
		if(BIT(in, nodmap)) {
			dadded++;
			return;
		}
		if(BIT(in, dirmap))
			nsubdir++;
	}
}

struct dinode *
getino(ino)
	daddr_t ino;
{
	static daddr_t minino, maxino;
	static struct dinode itab[INOPB];

	if (ino >= minino && ino < maxino) {
		return (&itab[ino - minino]);
	}
	bread(itod(ino, &sblock), itab, BSIZE);
	minino = ino - (ino % INOPB);
	maxino = minino + INOPB;
	return (&itab[ino - minino]);
}

int	breaderrors = 0;		
#define	BREADEMAX 32

bread(da, ba, c)
	daddr_t da;
	char *ba;
	int	c;	
{
	register n;
	register	regc;

	if (lseek(fi, (long)(da*FSIZE), 0) < 0){
		msg("bread: lseek fails\n");
	}
	regc = c;	/* put c someplace safe; it gets clobbered */
	n = read(fi, ba, c);
	if(n != c || regc != c){
		msg("(This should not happen)bread from %s [block %d]: c=0x%x, regc=0x%x, &c=0x%x, n=0x%x\n",
			disk, da, c, regc, &c, n);
#ifdef ERNIE
		msg("Notify Robert Henry of this error.\n");
#endif
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
}

CLR(map)
	register short *map;
{
	register n;

	n = MSIZ;
	do
		*map++ = 0;
	while(--n);
}

blkclr(cp, size)
	char *cp;
	long size;
{
	asm("movc5	$0,(r0),$0,8(ap),*4(ap)");
}
