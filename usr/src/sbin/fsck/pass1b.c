#ifndef lint
static char version[] = "@(#)pass1b.c	3.2 (Berkeley) %G%";
#endif

#include <sys/param.h>
#include <sys/inode.h>
#include <sys/fs.h>
#include "fsck.h"

int	pass1bcheck();

pass1b()
{
	register int c, i;
	register DINODE *dp;
	struct inodesc idesc;
	ino_t inumber;

	bzero((char *)&idesc, sizeof(struct inodesc));
	idesc.id_type = ADDR;
	idesc.id_func = pass1bcheck;
	inumber = 0;
	for (c = 0; c < sblock.fs_ncg; c++) {
		for (i = 0; i < sblock.fs_ipg; i++, inumber++) {
			if (inumber < ROOTINO)
				continue;
			dp = ginode(inumber);
			if (dp == NULL)
				continue;
			idesc.id_number = inumber;
			if (statemap[inumber] != USTATE &&
			    (ckinode(dp, &idesc) & STOP))
				goto out1b;
		}
	}
out1b:
	flush(&dfile, &inoblk);
}

pass1bcheck(idesc)
	register struct inodesc *idesc;
{
	register daddr_t *dlp;
	int nfrags, res = KEEPON;
	daddr_t blkno = idesc->id_blkno;

	for (nfrags = idesc->id_numfrags; nfrags > 0; blkno++, nfrags--) {
		if (outrange(blkno, 1))
			res = SKIP;
		for (dlp = duplist; dlp < muldup; dlp++)
			if (*dlp == blkno) {
				blkerr(idesc->id_number, "DUP", blkno);
				*dlp = *--muldup;
				*muldup = blkno;
				if (muldup == duplist)
					return (STOP);
			}
	}
	return (res);
}
