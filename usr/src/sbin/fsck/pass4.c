#ifndef lint
static char version[] = "@(#)pass4.c	3.1 (Berkeley) %G%";
#endif

#include <sys/param.h>
#include <sys/inode.h>
#include <sys/fs.h>
#include "fsck.h"

int	pass4check();

pass4()
{
	register ino_t inumber, *blp;
	int n;
	struct inodesc idesc;

	bzero((char *)&idesc, sizeof(struct inodesc));
	idesc.id_type = ADDR;
	idesc.id_func = pass4check;
	for (inumber = ROOTINO; inumber <= lastino; inumber++) {
		idesc.id_number = inumber;
		switch (statemap[inumber]) {

		case FSTATE:
			n = lncntp[inumber];
			if (n)
				adjust(&idesc, (short)n);
			else {
				for (blp = badlncnt;blp < badlnp; blp++)
					if (*blp == inumber) {
						clri(&idesc, "UNREF", 1);
						break;
					}
			}
			break;

		case DSTATE:
			clri(&idesc, "UNREF", 1);
			break;

		case CLEAR:
			clri(&idesc, "BAD/DUP", 1);
			break;
		}
	}
	if (imax - ROOTINO - n_files != sblock.fs_cstotal.cs_nifree) {
		pwarn("FREE INODE COUNT WRONG IN SUPERBLK");
		if (preen)
			printf(" (FIXED)\n");
		if (preen || reply("FIX") == 1) {
			sblock.fs_cstotal.cs_nifree = imax - ROOTINO - n_files;
			sbdirty();
		}
	}
	flush(&dfile, &fileblk);
}

pass4check(idesc)
	register struct inodesc *idesc;
{
	register daddr_t *dlp;
	int nfrags, res = KEEPON;
	daddr_t blkno = idesc->id_blkno;

	for (nfrags = idesc->id_numfrags; nfrags > 0; blkno++, nfrags--) {
		if (outrange(blkno, 1))
			res = SKIP;
		else if (getbmap(blkno)) {
			for (dlp = duplist; dlp < enddup; dlp++)
				if (*dlp == blkno) {
					*dlp = *--enddup;
					return (KEEPON);
				}
			clrbmap(blkno);
			n_blks--;
		}
	}
	return (res);
}
