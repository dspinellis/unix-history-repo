/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)mkboottape.c	7.1 (Berkeley) %G%
 */

#include <stdio.h>
#include <sys/param.h>
#include <sys/exec.h>
#include "../dev/devDiskLabel.h"

char	block[DEV_BSIZE];
char	*tapedev, *rootdev, *bootfname;

/*
 * This program takes a kernel and the name of the special device file that
 * has the mini-root file system stored on it and creates a boot tape.
 * The -b option makes a bootfile that can load the kernel and mini-root
 * over the network using the 'boot 6/tftp/filename -m' PROM command.
 *
 * usage: mkboottape [-b] tapedev vmunix minirootdev size
 */
main(argc, argv)
	int argc;
	char *argv[];
{
	register int i, n;
	int ifd, ofd, rfd;
	Dec_DiskBoot decBootInfo;
	ProcSectionHeader shdr;
	struct exec aout;
	int nsectors;
	long loadAddr;
	long execAddr;
	long textoff;
	long length;
	long rootsize;
	int makebootfile = 0;

	if (argc > 1 && strcmp(argv[1], "-b") == 0) {
		argc--;
		argv++;
		makebootfile = 1;
	}
	if (argc != 5)
		usage();
	tapedev = argv[1];
	bootfname = argv[2];
	rootdev = argv[3];
	rootsize = atoi(argv[4]);
	ifd = open(bootfname, 0, 0);
	if (ifd < 0) {
	bootferr:
		perror(bootfname);
		exit(1);
	}
	rfd = open(rootdev, 0, 0);
	if (rfd < 0) {
		perror(rootdev);
		exit(1);
	}
	if (makebootfile)
		ofd = creat(tapedev, 0666);
	else
		ofd = open(tapedev, 2, 0);
	if (ofd < 0) {
	deverr:
		perror(tapedev);
		exit(1);
	}

	/*
	 * Check for exec header and skip to code segment.
	 */
	i = read(ifd, (char *)&aout, sizeof(aout));
	if (i != sizeof(aout) || aout.ex_fhdr.magic != COFF_MAGIC ||
	    aout.a_magic != OMAGIC) {
		fprintf(stderr, "Need impure text format (OMAGIC) file\n");
		exit(1);
	}
	loadAddr = aout.ex_aout.codeStart;
	execAddr = aout.a_entry;
	length = aout.a_text + aout.a_data;
	textoff = N_TXTOFF(aout);
	printf("Input file is COFF format\n");
	printf("load %x, start %x, len %d\n", loadAddr, execAddr, length);

	/*
	 * Compute size of boot program rounded to page size + mini-root size.
	 */
	nsectors = (((length + aout.a_bss + NBPG - 1) & ~(NBPG - 1)) >>
		DEV_BSHIFT) + rootsize;

	if (makebootfile) {
		/*
		 * Write modified ECOFF header.
		 */
		aout.ex_fhdr.numSections = 1;
		aout.ex_fhdr.numSyms = 0;
		aout.ex_fhdr.symPtr = 0;
		aout.a_text = nsectors << DEV_BSHIFT;
		aout.a_data = 0;
		aout.a_bss = 0;
		aout.ex_aout.heapStart = aout.ex_aout.bssStart =
			aout.ex_aout.codeStart + aout.a_text;
		if (write(ofd, (char *)&aout, sizeof(aout)) != sizeof(aout))
			goto deverr;
		strncpy(shdr.name, ".text", sizeof(shdr.name));
		shdr.physAddr = shdr.virtAddr = loadAddr;
		shdr.size = aout.a_text;
		shdr.sectionPtr = n = (sizeof(aout) + sizeof(shdr) + 15) & ~15;
		shdr.relocPtr = 0;
		shdr.lnnoPtr = 0;
		shdr.numReloc = 0;
		shdr.numLnno = 0;
		shdr.flags = 0x20;
		if (write(ofd, (char *)&shdr, sizeof(shdr)) != sizeof(shdr))
			goto deverr;
		n -= sizeof(aout) + sizeof(shdr);
		if (write(ofd, block, n) != n)
			goto deverr;
	} else {
		/*
		 * Write the boot information block.
		 */
		decBootInfo.magic = DEC_BOOT_MAGIC;
		decBootInfo.mode = 0;
		decBootInfo.loadAddr = loadAddr;
		decBootInfo.execAddr = execAddr;
		decBootInfo.map[0].numBlocks = nsectors;
		decBootInfo.map[0].startBlock = 1;
		decBootInfo.map[1].numBlocks = 0;
		if (write(ofd, (char *)&decBootInfo, sizeof(decBootInfo)) !=
		    sizeof(decBootInfo))
			goto deverr;
	}
	/* seek to start of text */
	if (lseek(ifd, textoff, 0) < 0)
		goto bootferr;

	/*
	 * Write the remaining code to the correct place on the tape.
	 */
	i = length;
	while (i > 0) {
		n = DEV_BSIZE;
		if (n > i)
			n = i;
		if (read(ifd, block, n) != n)
			goto bootferr;
		if (write(ofd, block, n) != n)
			goto deverr;
		i -= n;
	}

	/*
	 * Pad the boot file with zeros to the start of the mini-root.
	 */
	bzero(block, DEV_BSIZE);
	i = ((nsectors - rootsize) << DEV_BSHIFT) - length;
	while (i > 0) {
		n = DEV_BSIZE;
		if (n > i)
			n = i;
		if (write(ofd, block, n) != n)
			goto deverr;
		i -= n;
	}

	/*
	 * Write the mini-root to tape.
	 */
	i = rootsize;
	while (i > 0) {
		if (read(rfd, block, DEV_BSIZE) != DEV_BSIZE) {
			perror(rootdev);
			break;
		}
		if (write(ofd, block, DEV_BSIZE) != DEV_BSIZE)
			goto deverr;
		i--;
	}

	printf("Wrote %d sectors\n", nsectors);
	exit(0);
}

usage()
{
	printf("Usage: mkboottape [-b] tapedev vmunix minirootdev size\n");
	printf("where:\n");
	printf("\t\"tapedev\" is the tape drive device\n");
	printf("\t\"vmunix\" is a -N format file\n");
	printf("\t\"minitrootdev\" is the character device of a mini-root file system disk\n");
	printf("\t\"size\" is the number of 512 byte blocks in the file system\n");
	exit(1);
}
