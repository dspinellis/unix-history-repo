#ifndef lint
static char version[] = "@(#)pass1.c	3.1 (Berkeley) %G%";
#endif

#include <sys/param.h>
#include <sys/inode.h>
#include <sys/fs.h>
#include "fsck.h"

int	pass1check();

pass1()
{
	register int c, i, n, j;
	register DINODE *dp;
	int ndb, partial;
	struct inodesc idesc;
	ino_t inumber;

	bzero((char *)&idesc, sizeof(struct inodesc));
	idesc.id_type = ADDR;
	idesc.id_func = pass1check;
	inumber = 0;
	n_blks += howmany(sblock.fs_cssize, sblock.fs_fsize);
	for (c = 0; c < sblock.fs_ncg; c++) {
		if (getblk(&cgblk, cgtod(&sblock, c), sblock.fs_cgsize) == 0)
			continue;
		if (cgrp.cg_magic != CG_MAGIC) {
			pfatal("CG %d: BAD MAGIC NUMBER\n", c);
			bzero((char *)&cgrp, (int)sblock.fs_cgsize);
		}
		n = 0;
		for (i = 0; i < sblock.fs_ipg; i++, inumber++) {
			dp = ginode(inumber);
			if (dp == NULL)
				continue;
			n++;
			if (ALLOC) {
				if (!isset(cgrp.cg_iused, i)) {
					if (debug)
						printf("%d bad, not used\n",
						    inumber);
					inosumbad++;
				}
				n--;
				lastino = inumber;
				if (!preen && (dp->di_mode & IFMT) == IFMT &&
				    reply("HOLD BAD BLOCK") == 1) {
					dp->di_size = sblock.fs_fsize;
					dp->di_mode = IFREG|0600;
					inodirty();
				} else if (ftypeok(dp) == 0)
					goto unknown;
				if (dp->di_size < 0) {
					if (debug)
						printf("bad size %d:",
							dp->di_size);
					goto unknown;
				}
				ndb = howmany(dp->di_size, sblock.fs_bsize);
				if (SPECIAL)
					ndb++;
				for (j = ndb; j < NDADDR; j++)
					if (dp->di_db[j] != 0) {
						if (debug)
							printf("bad direct addr: %d\n",
								dp->di_db[j]);
						goto unknown;
					}
				for (j = 0, ndb -= NDADDR; ndb > 0; j++)
					ndb /= NINDIR(&sblock);
				for (; j < NIADDR; j++)
					if (dp->di_ib[j] != 0) {
						if (debug)
							printf("bad indirect addr: %d\n",
								dp->di_ib[j]);
						goto unknown;
					}
				n_files++;
				lncntp[inumber] = dp->di_nlink;
				if (dp->di_nlink <= 0) {
					if (badlnp < &badlncnt[MAXLNCNT])
						*badlnp++ = inumber;
					else {
						pfatal("LINK COUNT TABLE OVERFLOW");
						if (reply("CONTINUE") == 0)
							errexit("");
					}
				}
				statemap[inumber] = DIRCT ? DSTATE : FSTATE;
				badblk = dupblk = 0; maxblk = 0;
				idesc.id_number = inumber;
				idesc.id_filesize = 0;
				(void)ckinode(dp, &idesc);
				idesc.id_filesize *= btodb(sblock.fs_fsize);
				if (dp->di_blocks != idesc.id_filesize) {
					pwarn("INCORRECT BLOCK COUNT I=%u (%ld should be %ld)",
					    inumber, dp->di_blocks,
					    idesc.id_filesize);
					if (preen)
						printf(" (CORRECTED)\n");
					else if (reply("CORRECT") == 0)
						continue;
					dp->di_blocks = idesc.id_filesize;
					inodirty();
				}
				continue;
		unknown:
				pfatal("UNKNOWN FILE TYPE I=%u", inumber);
				if (reply("CLEAR") == 1) {
					zapino(dp);
					inodirty();
					inosumbad++;
				}
			} else {
				if (isset(cgrp.cg_iused, i)) {
					if (debug)
						printf("%d bad, marked used\n",
						    inumber);
					inosumbad++;
					n--;
				}
				partial = 0;
				for (j = 0; j < NDADDR; j++)
					if (dp->di_db[j] != 0)
						partial++;
				for (j = 0; j < NIADDR; j++)
					if (dp->di_ib[j] != 0)
						partial++;
				if (partial || dp->di_mode != 0 ||
				    dp->di_size != 0) {
					pfatal("PARTIALLY ALLOCATED INODE I=%u",
						inumber);
					if (reply("CLEAR") == 1) {
						zapino(dp);
						inodirty();
						inosumbad++;
					}
				}
			}
		}
		if (n != cgrp.cg_cs.cs_nifree) {
			if (debug)
				printf("cg[%d].cg_cs.cs_nifree is %d; calc %d\n",
				    c, cgrp.cg_cs.cs_nifree, n);
			inosumbad++;
		}
		if (cgrp.cg_cs.cs_nbfree != sblock.fs_cs(&sblock, c).cs_nbfree
		  || cgrp.cg_cs.cs_nffree != sblock.fs_cs(&sblock, c).cs_nffree
		  || cgrp.cg_cs.cs_nifree != sblock.fs_cs(&sblock, c).cs_nifree
		  || cgrp.cg_cs.cs_ndir != sblock.fs_cs(&sblock, c).cs_ndir)
			sbsumbad++;
	}
}

pass1check(idesc)
	register struct inodesc *idesc;
{
	register daddr_t *dlp;
	int res = KEEPON;
	int anyout, nfrags;
	daddr_t blkno = idesc->id_blkno;

	anyout = outrange(blkno, idesc->id_numfrags);
	for (nfrags = idesc->id_numfrags; nfrags > 0; blkno++, nfrags--) {
		if (anyout && outrange(blkno, 1)) {
			blkerr(idesc->id_number, "BAD", blkno);
			if (++badblk >= MAXBAD) {
				pwarn("EXCESSIVE BAD BLKS I=%u",
					idesc->id_number);
				if (preen)
					printf(" (SKIPPING)\n");
				else if (reply("CONTINUE") == 0)
					errexit("");
				return (STOP);
			}
			res = SKIP;
		} else if (getbmap(blkno)) {
			blkerr(idesc->id_number, "DUP", blkno);
			if (++dupblk >= MAXDUP) {
				pwarn("EXCESSIVE DUP BLKS I=%u",
					idesc->id_number);
				if (preen)
					printf(" (SKIPPING)\n");
				else if (reply("CONTINUE") == 0)
					errexit("");
				return (STOP);
			}
			if (enddup >= &duplist[DUPTBLSIZE]) {
				pfatal("DUP TABLE OVERFLOW.");
				if (reply("CONTINUE") == 0)
					errexit("");
				return (STOP);
			}
			for (dlp = duplist; dlp < muldup; dlp++)
				if (*dlp == blkno) {
					*enddup++ = blkno;
					break;
				}
			if (dlp >= muldup) {
				*enddup++ = *muldup;
				*muldup++ = blkno;
			}
		} else {
			n_blks++;
			setbmap(blkno);
		}
		idesc->id_filesize++;
	}
	return (res);
}
