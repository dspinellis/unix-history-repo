/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)prf.c	7.1 (Berkeley) %G%
 */

#include "sys/param.h"
#include "../tahoe/cp.h"
#include "../include/mtpr.h"

/*
 * Print a character on console.
 */
struct	cpdcb_o cpout;
struct	cpdcb_i cpin;

/* console requires even parity */
#define EVENP

putchar(c)
	char c;
{
	int time;
#ifdef EVENP
	register mask, par;

	for (par = 0, mask = 1; mask != 0200; mask <<= 1, par <<= 1)
		par ^= c&mask;
	c |= par;
#endif /* EVENP */
	cpout.cp_hdr.cp_unit = CPCONS;		/* Resets done bit */
	cpout.cp_hdr.cp_comm = CPWRITE;
	cpout.cp_hdr.cp_count = 1;
	cpout.cp_buf[0] = c;
	mtpr(CPMDCB, &cpout);
	time = 100000;				/* Delay loop */
	while (time--) {
		uncache(&cpout.cp_hdr.cp_unit);
		if (cpout.cp_hdr.cp_unit & CPDONE)
			break;
	}
	if (c == '\n')
		putchar ('\r');
}

scankbd()
{}

getchar()
{
	char c;

	cpin.cp_hdr.cp_unit = CPCONS;		/* Resets done bit */
	cpin.cp_hdr.cp_comm = CPREAD;
	cpin.cp_hdr.cp_count = 1;
	mtpr(CPMDCB, &cpin);
	while ((cpin.cp_hdr.cp_unit & CPDONE) == 0) 
		uncache(&cpin.cp_hdr.cp_unit);
	uncache(&cpin.cpi_buf[0]);
	c = cpin.cpi_buf[0] & 0x7f;
	if (c == '\r')
		c = '\n';
	if (c != '\b' && c != '\177')
		putchar(c);
	return (c);
}

trap(ps)
	int ps;
{
	printf("Trap %o\n", ps);
	for (;;)
		;
}

uncache (addr)
	char *addr;
{
	/* Return *(addr-0x4000); DIRTY assumes this address is valid */
	mtpr(PDCS, addr);
}
