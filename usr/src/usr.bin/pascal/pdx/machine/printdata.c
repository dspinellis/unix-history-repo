/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)printdata.c 1.1 %G%";

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

/*
 * print count words starting at address
 */

printndata(count, addr)
int count;
ADDRESS addr;
{
	printdata(addr, addr + (count - 1)*sizeof(int));
}
