/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.noredist.c%
 *
 *	@(#)dlbl.c	7.1 (Berkeley) %G%
 */

#include "../h/param.h"
#include "../h/inode.h"
#include "../h/fs.h"
#include "../h/dkbad.h"
#include "../h/disk.h"

struct bb {
	char bufr[LABELOFFSET];
	struct disklabel w;
} xx,yy;
struct disklabel *dlp;
extern struct disklabel wdsizes[2];


struct	dkbad	dkbad;

main() {
	int fi,x;

	xx.bufr[0] = 126;
	xx.bufr[LABELOFFSET-1] = 125;
	xx.w.dk_magic = 0xabc;
	xx.w.dk_type = 1;
	xx.w.dk_secsize = 512;
	xx.w.dk_nsectors = 17;
	xx.w.dk_ntracks = 8;
	xx.w.dk_ncylinders = 615;
	xx.w.dk_secpercyl = 17*8;
	xx.w.dk_secperunit = (612-290)*17*8;
	xx.w.dk_precompcyl = 616;
	xx.w.dk_partition[0].nblocks = (611-290)*17*8;
	xx.w.dk_partition[0].cyloff = 290;
	xx.w.dk_partition[1].nblocks = (611-290)*17*8;
	xx.w.dk_partition[1].cyloff = 290;
	xx.w.dk_partition[2].nblocks = (611-290)*17*8;
	xx.w.dk_partition[2].cyloff = 290;
	xx.w.dk_name[0]= 'a';
	xx.w.dk_name[1]= 'b';
	xx.w.dk_name[2]= 'c';
	xx.w.dk_name[3]= '\0';
	dlp = &xx.w;
	fi = open("wd0a:", 2);
	wdsizes[0] = *dlp;
	lseek(fi,0,0);
	write(fi,&xx,512);
	/*for (x=0; x <17; x++) */write(fi,&xx, 512);
	/*for (x=0; x <17; x++)*/ read(fi,&yy, 512);
	exit(-1);
}
