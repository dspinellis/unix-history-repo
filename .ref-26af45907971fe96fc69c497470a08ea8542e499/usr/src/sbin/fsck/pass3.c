/*
 * Copyright (c) 1980, 1986, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)pass3.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/time.h>
#include <ufs/ufs/dinode.h>
#include <ufs/ffs/fs.h>
#include "fsck.h"

pass3()
{
	register struct inoinfo **inpp, *inp;
	ino_t orphan;
	int loopcnt;

	for (inpp = &inpsort[inplast - 1]; inpp >= inpsort; inpp--) {
		inp = *inpp;
		if (inp->i_number == ROOTINO ||
		    !(inp->i_parent == 0 || statemap[inp->i_number] == DSTATE))
			continue;
		if (statemap[inp->i_number] == DCLEAR)
			continue;
		for (loopcnt = 0; ; loopcnt++) {
			orphan = inp->i_number;
			if (inp->i_parent == 0 ||
			    statemap[inp->i_parent] != DSTATE ||
			    loopcnt > numdirs)
				break;
			inp = getinoinfo(inp->i_parent);
		}
		(void)linkup(orphan, inp->i_dotdot);
		inp->i_parent = inp->i_dotdot = lfdir;
		lncntp[lfdir]--;
		statemap[orphan] = DFOUND;
		propagate();
	}
}
