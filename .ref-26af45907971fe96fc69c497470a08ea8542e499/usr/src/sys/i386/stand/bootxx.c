/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)bootxx.c	7.3 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/reboot.h>
#include <sys/disklabel.h>

#include <a.out.h>
#include <stand/saio.h>

char *bootprog = "/boot";
extern int opendev, bootdev, cyloffset;
extern struct disklabel disklabel;

/*
 * Boot program... loads /boot out of filesystem indicated by arguements.
 * We assume an autoboot unless we detect a misconfiguration.
 */

main(dev, unit, off)
{
	register struct disklabel *lp;
	register int io, partition, howto;


	/* are we a disk, if so look at disklabel and do things */
	lp = &disklabel;
	if (lp->d_magic == DISKMAGIC) {
	    /*
	     * Synthesize bootdev from dev, unit, type and partition
	     * information from the block 0 bootstrap.
	     * It's dirty work, but someone's got to do it.
	     * This will be used by the filesystem primatives, and
	     * drivers. Ultimately, opendev will be created corresponding
	     * to which drive to pass to top level bootstrap.
	     */
	    for (io = 0; io < 8; io++)
#ifdef notyetSCSI
		if (lp->d_type == DTYPE_SCSI) {
			if (lp->d_partitions[io].p_offset == off)
				break;
		} else
#endif
		if (lp->d_partitions[io].p_offset == off*lp->d_secpercyl)
			break;

	    if (io == 8) goto screwed;
            cyloffset = off;
	} else {
screwed:
		/* probably a bad or non-existant disklabel */
		io = 0 ;
		howto |= RB_SINGLE|RB_ASKNAME ;
	}

	/* construct bootdev */
	/* currently, PC has no way of booting off alternate controllers */
	bootdev = MAKEBOOTDEV(/*i_dev*/ dev, /*i_adapt*/0, /*i_ctlr*/0,
	    unit, /*i_part*/io);

	printf("loading %s\n", bootprog);
	io = open(bootprog, 0);
	if (io >= 0)
		copyunix(io, howto);
	_stop("boot failed\n");
}

/*ARGSUSED*/
copyunix(io, howto)
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
	for (i = 0; i < x.a_bss; i++)
		*addr++ = 0;

 	(*((int (*)()) x.a_entry))(howto, opendev, cyloffset);
	return;
shread:
	_stop("Short read\n");
}
