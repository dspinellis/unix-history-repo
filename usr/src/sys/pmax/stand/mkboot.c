/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1992 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)mkboot.c	7.1 (Berkeley) %G%";
#endif not lint

#include <stdio.h>
#include "types.h"
#include "exec.h"
#include "../include/param.h"
#include "../dev/devDiskLabel.h"

/* this is the size of the standard ULTRIX boot */
#define MAXBOOTSIZE (15 * DEV_BSIZE)

char	block[DEV_BSIZE];
char	*dev, *bootfname;

/*
 * installboot bootprog device
 */
main(argc, argv)
	int argc;
	char *argv[];
{
	register int i, n;
	int ifd, ofd;
	Dec_DiskBoot decBootInfo;
	int nsectors;
	long loadAddr;
	long execAddr;
	long length;

	if (argc != 3)
		usage();
	dev = argv[2];
	i = strlen(dev);
	bootfname = argv[1];
	ifd = open(bootfname, 0, 0);
	if (ifd < 0) {
		perror(bootfname);
		exit(1);
	}
	ofd = open(dev, 2, 0);
	if (ofd < 0) {
	deverr:
		perror(dev);
		exit(1);
	}

	/*
	 * Check for exec header and skip to code segment.
	 */
	if (!DecHeader(ifd, &loadAddr, &execAddr, &length)) {
		fprintf(stderr, "Need impure text format (OMAGIC) file\n");
		exit(1);
	}
	if (length > MAXBOOTSIZE) {
		fprintf(stderr, "boot program is too big (%d > %d)\n",
			length, MAXBOOTSIZE);
		exit(1);
	}

	/*
	 * Write the boot information block.
	 */
	decBootInfo.magic = DEC_BOOT_MAGIC;
	decBootInfo.mode = 0;
	decBootInfo.loadAddr = loadAddr;
	decBootInfo.execAddr = execAddr;
	decBootInfo.map[0].numBlocks = nsectors =
		(length + DEV_BSIZE - 1) >> DEV_BSHIFT;
	decBootInfo.map[0].startBlock = 1;
	decBootInfo.map[1].numBlocks = 0;
	if (lseek(ofd, (long)(DEC_BOOT_SECTOR * DEV_BSIZE), 0) < 0 ||
	    write(ofd, (char *)&decBootInfo, sizeof(decBootInfo)) !=
	    sizeof(decBootInfo)) {
		perror(dev);
		fprintf(stderr, "Sector write %d failed: ", DEC_BOOT_SECTOR);
		exit(1);
	}
	if (lseek(ofd, (long)(1 * DEV_BSIZE), 0) < 0)
		goto deverr;

	/*
	 * Write the remaining code to the correct place on the disk.
	 */
	for (i = 0; i < nsectors && length > 0; i++) {
		bzero(block, DEV_BSIZE);
		n = length < DEV_BSIZE ? length : DEV_BSIZE;
		if (read(ifd, block, n) != n) {
			perror(bootfname);
			break;
		}
		length -= n;
		if (write(ofd, block, DEV_BSIZE) != DEV_BSIZE) {
			perror(dev);
			break;
		}
	}
	printf("Wrote %d sectors\n", i);
	if (length > 0)
		printf("Warning: didn't reach end of boot program!\n");
	exit(0);
}

usage()
{
	printf("Usage: installboot bootprog device\n");
	printf("where:\n");
	printf("\t\"bootprog\" is a -N format file < %d bytes long\n",
	       MAXBOOTSIZE);
	printf("\t\"device\" should be the 'a' partition of a bootable disk\n");
	printf("WARNING!!  If the 'c' partition contains a file system, %s\n",
	       "DON'T RUN THIS!!");
	exit(1);
}

/*
 *----------------------------------------------------------------------
 *
 * DecHeader -
 *
 *	Check if the header is a dec (coff) file.
 *
 * Results:
 *	Return true if all went ok.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */
DecHeader(bootFID, loadAddr, execAddr, length)
	int bootFID;	/* Handle on the boot program */
	long *loadAddr;	/* Address to start loading boot program. */
	long *execAddr;	/* Address to start executing boot program. */
	long *length;	/* Length of the boot program. */
{
	struct exec aout;
	int bytesRead;

	if (lseek(bootFID, 0, 0) < 0) {
		perror(bootfname);
		return 0;
	}
	bytesRead = read(bootFID, (char *)&aout, sizeof(aout));
	if (bytesRead != sizeof(aout) || aout.ex_fhdr.magic != COFF_MAGIC ||
	    aout.a_magic != OMAGIC)
		return 0;
	*loadAddr = aout.ex_aout.codeStart;
	*execAddr = aout.a_entry;
	*length = aout.a_text + aout.a_data;
	if (lseek(bootFID, N_TXTOFF(aout), 0) < 0) {
		perror(bootfname);
		return 0;
	}
	printf("Input file is coff format\n");
	printf("load %x, start %x, len %d\n", *loadAddr, *execAddr, *length);
	return 1;
}
