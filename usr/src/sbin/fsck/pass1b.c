/*
 * Copyright (c) 1980, 1986 The Regents of the University of California.
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
 */

#ifndef lint
static char sccsid[] = "@(#)pass1b.c	5.6 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <ufs/dinode.h>
#include <ufs/fs.h>
#include "fsck.h"

int	pass1bcheck();
static  struct dups *duphead;

pass1b()
{
	register int c, i;
	register struct dinode *dp;
	struct inodesc idesc;
	ino_t inumber;

	bzero((char *)&idesc, sizeof(struct inodesc));
	idesc.id_type = ADDR;
	idesc.id_func = pass1bcheck;
	duphead = duplist;
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
				return;
		}
	}
}

pass1bcheck(idesc)
	register struct inodesc *idesc;
{
	register struct dups *dlp;
	int nfrags, res = KEEPON;
	daddr_t blkno = idesc->id_blkno;

	for (nfrags = idesc->id_numfrags; nfrags > 0; blkno++, nfrags--) {
		if (chkrange(blkno, 1))
			res = SKIP;
		for (dlp = duphead; dlp; dlp = dlp->next) {
			if (dlp->dup == blkno) {
				blkerror(idesc->id_number, "DUP", blkno);
				dlp->dup = duphead->dup;
				duphead->dup = blkno;
				duphead = duphead->next;
			}
			if (dlp == muldup)
				break;
		}
		if (muldup == 0 || duphead == muldup->next)
			return (STOP);
	}
	return (res);
}
