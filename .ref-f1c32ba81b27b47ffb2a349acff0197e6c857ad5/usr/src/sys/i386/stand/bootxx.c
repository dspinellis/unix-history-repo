/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.noredist.c%
 *
 *	@(#)bootxx.c	7.1 (Berkeley) %G%
 */

#include "../h/param.h"
#include "../h/inode.h"
#include "../h/fs.h"
#include <a.out.h>
#include "saio.h"
#include "../h/reboot.h"
#include "../h/dir.h"
#include "../h/disk.h"
#include "devvm.h"

extern	int howto, bootdev, unit, cyloffset, boottype;
extern char bootprog[] ;
/*
 * Boot program... arguments passed in r10 and r11
 * are passed through to the full boot program.
 */

main()
{
	register int io, partition; register char *bp ;

#ifdef lint
	howto = 0; devtype = 0;
#endif
	extern struct disklabel disklabel;

	/* are we a disk, if so look at disklabel and do things */
	if (bootdev == 0 || bootdev == 3) {
	    /*
	     * Synthesize bootdev from unit, type and partition
	     * from the ROM monitor.
	     * It's dirty work, but someone's got to do it, and
	     * we always seem to get it.
	     */
	    for (io = 0; io < 8; io++)
		if (bootdev > 0) { /* XXX should check dk_type == DTYPE_SCSI */
			if (disklabel.dk_partition[io].cyloff
				== cyloffset * disklabel.dk_secpercyl)
				break;
		} else {
	
			if (disklabel.dk_partition[io].cyloff == cyloffset)
			break;
		}
	    if (io == 8) io = 0; /* probably a bad or non-existant disklabel */
	    bootdev = makedev(bootdev, make_minor(unit, io));
	} else { io = 0 ; howto = (howto&0x7) | 3 ; }
	bp = bootprog ;
	while (*bp != '0') bp++ ;	/* n-char device names instead of 2 */
	*bp++ = unit % 10 + '0' ;
	*bp += io % 10 ;
/*	bootprog[3] = unit % 10 + '0';
	bootprog[4] = io % 10 + 'a';*/
	printf("loading %s\n", bootprog);
	io = open(bootprog, 0);
	if (io >= 0)
		copyunix(io);
	_stop("boot failed\n");
}

/*ARGSUSED*/
copyunix(io)
	register io;	
{
	struct exec x;
	register int i;
	char *addr;

	i = read(io, (char *)&x, sizeof x);
	if (i != sizeof x ||
	    (x.a_magic != 0407 && x.a_magic != 0413 && x.a_magic != 0410))
		_stop("Bad format\n");
	if ((x.a_magic == 0413 || x.a_magic == 0410) &&
	    lseek(io, 0x400, 0) == -1)
		goto shread;
	if (read(io, (char *)0, x.a_text) != x.a_text)
		goto shread;
	addr = (char *)x.a_text;
	if (x.a_magic == 0413 || x.a_magic == 0410)
		while ((int)addr & CLOFSET)
			*addr++ = 0;
	if (read(io, addr, x.a_data) != x.a_data)
		goto shread;
	addr += x.a_data;
	x.a_bss += 128*512;	/* slop */
	for (i = 0; i < x.a_bss; i++)
		*addr++ = 0;
	setregs();
 	(*((int (*)()) x.a_entry))();
	return;
shread:
	_stop("Short read\n");
}
