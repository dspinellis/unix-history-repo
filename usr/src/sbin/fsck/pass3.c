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
static char sccsid[] = "@(#)pass3.c	5.6 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <ufs/dinode.h>
#include <ufs/fs.h>
#include "fsck.h"

int	pass2check();

pass3()
{
	register struct dinode *dp;
	struct inodesc idesc;
	ino_t inumber, orphan;
	int loopcnt;

	bzero((char *)&idesc, sizeof(struct inodesc));
	idesc.id_type = DATA;
	for (inumber = ROOTINO; inumber <= lastino; inumber++) {
		if (statemap[inumber] == DSTATE) {
			pathp = pathname;
			*pathp++ = '?';
			*pathp = '\0';
			idesc.id_func = findino;
			idesc.id_name = "..";
			idesc.id_parent = inumber;
			loopcnt = 0;
			do {
				orphan = idesc.id_parent;
				if (orphan < ROOTINO || orphan > maxino)
					break;
				dp = ginode(orphan);
				idesc.id_parent = 0;
				idesc.id_number = orphan;
				if ((ckinode(dp, &idesc) & FOUND) == 0)
					break;
				if (loopcnt >= sblock.fs_cstotal.cs_ndir)
					break;
				loopcnt++;
			} while (statemap[idesc.id_parent] == DSTATE);
			if (linkup(orphan, idesc.id_parent) == 1) {
				idesc.id_func = pass2check;
				idesc.id_number = lfdir;
				descend(&idesc, orphan);
			}
		}
	}
}
