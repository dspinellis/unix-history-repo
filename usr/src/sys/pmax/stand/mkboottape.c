/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)mkboottape.c	7.5 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/exec.h>

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>

#include <pmax/stand/dec_boot.h>

void err __P((const char *, ...));
void usage __P((void));

struct	Dec_DiskBoot decBootInfo;

/*
 * This program takes a kernel and the name of the special device file that
 * has the mini-root file system stored on it and creates a boot tape.
 * The -b option makes a bootfile that can load the kernel and mini-root
 * over the network using the 'boot 6/tftp/filename -m' PROM command.
 *
 * usage: mkboottape [-b] tapedev vmunix minirootdev size
 */
int
main(argc, argv)
	int argc;
	char *argv[];
{
	register int i, n;
	ProcSectionHeader shdr;
	struct exec aout;
	long loadAddr;
	long execAddr;
	long textoff;
	long length;
	long rootsize;
	int ifd, ofd, rfd;
	int makebootfile;
	int nsectors;
	char block[DEV_BSIZE];

	makebootfile = 0;
	while ((i = getopt(argc, argv, "b")) != EOF)
		switch(i) {
		case 'b':
			makebootfile = 1;
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (argc != 4)
		usage();

	if (makebootfile)
		ofd = open(argv[0], O_CREAT|O_TRUNC|O_WRONLY, DEFFILEMODE);
	else
		ofd = open(argv[0], O_RDWR, 0);
	if (ofd < 0)
deverr:		err("%s: %s", argv[0], strerror(errno));

	if ((ifd = open(argv[1], O_RDONLY, 0)) < 0)
bootferr:	err("%s: %s", argv[1], strerror(errno));

	if ((rfd = open(argv[2], O_RDONLY, 0)) < 0)
rooterr:	err("%s: %s", argv[2], strerror(errno));

	rootsize = atoi(argv[3]);

	/*
	 * Check for exec header and skip to code segment.
	 */
	if (read(ifd, &aout, sizeof(aout)) != sizeof(aout) ||
	    aout.ex_fhdr.magic != COFF_MAGIC || aout.a_magic != OMAGIC)
		err("need impure text format (OMAGIC) file");

	loadAddr = aout.ex_aout.codeStart;
	execAddr = aout.a_entry;
	length = aout.a_text + aout.a_data;
	textoff = N_TXTOFF(aout);
	(void)printf("Input file is COFF format\n");
	(void)printf("load %x, start %x, len %d\n", loadAddr, execAddr, length);

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
	if (lseek(ifd, textoff, SEEK_SET) < 0)
		goto bootferr;

	/*
	 * Write the remaining code to the correct place on the tape.
	 */
	for (i = length; i > 0; i -= n) {
		n = DEV_BSIZE;
		if (n > i)
			n = i;
		if (read(ifd, block, n) != n)
			goto bootferr;
		if (write(ofd, block, DEV_BSIZE) != DEV_BSIZE)
			goto deverr;
	}

	/*
	 * Pad the boot file with zeros to the start of the mini-root.
	 */
	bzero(block, DEV_BSIZE);
	i = ((nsectors - rootsize) << DEV_BSHIFT) -
		((length + DEV_BSIZE - 1) & ~(DEV_BSIZE - 1));
	n = DEV_BSIZE;
	for (; i > 0; i -= n) {
		if (write(ofd, block, n) != n)
			goto deverr;
	}

	/*
	 * Write the mini-root to tape.
	 */
	for (i = rootsize; i > 0; i--) {
		if (read(rfd, block, DEV_BSIZE) != DEV_BSIZE)
			goto rooterr;
		if (write(ofd, block, DEV_BSIZE) != DEV_BSIZE)
			goto deverr;
	}

	(void)printf("mkboottape: wrote %d sectors\n", nsectors);
	exit(0);
}

void
usage()
{
	(void)fprintf(stderr,
	    "usage: mkboottape [-b] tapedev vmunix minirootdev size\n");
	exit(1);
}

#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

void
#if __STDC__
err(const char *fmt, ...)
#else
err(fmt, va_alist)
	char *fmt;
        va_dcl
#endif
{
	va_list ap;
#if __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	(void)fprintf(stderr, "mkboottape: ");
	(void)vfprintf(stderr, fmt, ap);
	va_end(ap);
	(void)fprintf(stderr, "\n");
	exit(1);
	/* NOTREACHED */
}
