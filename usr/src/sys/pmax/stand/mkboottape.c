/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)mkboottape.c	8.1 (Berkeley) 6/10/93
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
#include <err.h>

#include <pmax/stand/dec_boot.h>
#include <pmax/stand/dec_exec.h>

void usage __P((void));

struct	Dec_DiskBoot decBootInfo;
struct	coff_exec dec_exec;
extern char *__progname;		/* Program name, from crt0. */

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
	off_t textoff;
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
deverr:		err(1, "%s", argv[0]);

	if ((ifd = open(argv[1], O_RDONLY, 0)) < 0)
bootferr:	err(1, "%s", argv[1]);

	if ((rfd = open(argv[2], O_RDONLY, 0)) < 0)
rooterr:	err(1, "%s", argv[2]);

	rootsize = atoi(argv[3]);

	/*
	 * Check for exec header and skip to code segment.
	 */
	if (read(ifd, &aout, sizeof(aout)) != sizeof(aout) ||
	    aout.a_magic != OMAGIC) {
		fprintf(stderr, "%s: %s: need old text format (OMAGIC) file\n",
			__progname, argv[1]);
		exit(1);
	}

	loadAddr = aout.a_entry;
	execAddr = aout.a_entry;
	length = aout.a_text + aout.a_data;
	textoff = N_TXTOFF(aout);
	(void)printf("Input file is a.out format\n");
	(void)printf("load %x, start %x, len %d\n", loadAddr, execAddr, length);

	/*
	 * Compute size of boot program rounded to page size + mini-root size.
	 */
	nsectors = (((length + aout.a_bss + NBPG - 1) & ~(NBPG - 1)) >>
		DEV_BSHIFT) + rootsize;

	if (makebootfile) {
		/*
		 * Write the ECOFF header.
		 */
		dec_exec.magic = COFF_MAGIC;
		dec_exec.numSections = 1;
		dec_exec.optHeader = 56;
		dec_exec.flags = 7;
		dec_exec.aout_magic = OMAGIC;
		dec_exec.verStamp = 512;
		dec_exec.codeSize = nsectors << DEV_BSHIFT;
		dec_exec.entry = execAddr;
		dec_exec.codeStart = loadAddr;
		dec_exec.heapStart = dec_exec.bssStart =
			dec_exec.codeStart + aout.a_text;
		if (write(ofd, (char *)&dec_exec, sizeof(dec_exec)) !=
		    sizeof(dec_exec))
			goto deverr;
		strncpy(shdr.name, ".text", sizeof(shdr.name));
		shdr.physAddr = shdr.virtAddr = loadAddr;
		shdr.size = dec_exec.codeSize;
		shdr.sectionPtr = n =
			(sizeof(dec_exec) + sizeof(shdr) + 15) & ~15;
		shdr.relocPtr = 0;
		shdr.lnnoPtr = 0;
		shdr.numReloc = 0;
		shdr.numLnno = 0;
		shdr.flags = 0x20;
		if (write(ofd, (char *)&shdr, sizeof(shdr)) != sizeof(shdr))
			goto deverr;
		n -= sizeof(dec_exec) + sizeof(shdr);
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

	(void)printf("%s: wrote %d sectors\n", __progname, nsectors);
	exit(0);
}

void
usage()
{
	(void)fprintf(stderr,
	    "usage: %s [-b] tapedev vmunix minirootdev size\n", __progname);
	exit(1);
}
