/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)printdata.c	5.3 (Berkeley) %G%";
#endif /* not lint */

/*
 * print contents of data addresses in octal
 *
 * There are two entries:  one is given a range of addresses,
 * the other is given a count and a starting address.
 */

#include "defs.h"
#include "machine.h"
#include "process.h"
#include "object.h"
#include "process/process.rep"
#include "process/pxinfo.h"

#define WORDSPERLINE 4

/*
 * print words from lowaddr to highaddr
 */

printdata(lowaddr, highaddr)
ADDRESS lowaddr;
ADDRESS highaddr;
{
	register int count;
	register ADDRESS addr;
	int val;

	if (lowaddr > highaddr) {
		error("first address larger than second");
	}
	count = 0;
	for (addr = lowaddr; addr <= highaddr; addr += sizeof(int)) {
		if (count == 0) {
			printf("%8x: ", addr);
		}
		dread(&val, addr, sizeof(val));
		printf("  %8x", val);
		if (++count >= WORDSPERLINE) {
			putchar('\n');
			count = 0;
		}
	}
	if (count != 0) {
		putchar('\n');
	}
}
