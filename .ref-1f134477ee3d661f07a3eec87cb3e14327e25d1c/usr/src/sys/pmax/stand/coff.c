/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)coff.c	7.1 (Berkeley) %G%
 */

#define COFF
#include <sys/exec.h>

/*
 * Print header info for coff files.
 */
void
main(argc, argv)
	int argc;
	char **argv;
{
	register struct devices *dp;
	register int fd, i, n;
	char *fname;
	struct exec aout;

	if (argc < 2) {
		printf("usage: %s <file>\n");
		goto err;
	}
	if ((fd = open(fname = argv[1], 0)) < 0)
		goto err;

	/* read the COFF header */
	i = read(fd, (char *)&aout, sizeof(aout));
	if (i != sizeof(aout)) {
		printf("No a.out header\n");
		goto cerr;
	}
	printf("HDR: magic 0x%x(0%o) nsec %d nsym %d optheader %d textoff %x\n",
		aout.ex_fhdr.magic,
		aout.ex_fhdr.magic,
		aout.ex_fhdr.numSections,
		aout.ex_fhdr.numSyms,
		aout.ex_fhdr.optHeader,
		N_TXTOFF(aout));
	printf("A.out: magic 0x%x(0%o) ver %d entry %x gprM %x gpV %x\n",
		aout.ex_aout.magic,
		aout.ex_aout.magic,
		aout.ex_aout.verStamp,
		aout.ex_aout.entry,
		aout.ex_aout.gprMask,
		aout.ex_aout.gpValue);
	printf("\tstart %x,%x,%x size %d+%d+%d\n",
		aout.ex_aout.codeStart,
		aout.ex_aout.heapStart,
		aout.ex_aout.bssStart,
		aout.ex_aout.codeSize,
		aout.ex_aout.heapSize,
		aout.ex_aout.bssSize);

cerr:
	close(fd);
err:
	exit(0);
}
