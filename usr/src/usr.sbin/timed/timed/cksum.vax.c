/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)cksum.vax.c	1.1 (Berkeley) %G%";
#endif not lint

#include "../globals.h"
#include <protocols/timed.h>

#define ADD	asm("adwc (r9)+,r8;");

/* computes the checksum for ip packets for the VAX */

in_cksum(addr, len)
u_short *addr;
int len;
{
	register int nleft = len;	/* on vax, (user mode), r11 */
#ifndef lint
	register int xxx;		/* on vax, (user mode), r10 */
#endif not lint
	register u_short *w = addr;	/* on vax, known to be r9 */
	register int sum = 0;		/* on vax, known to be r8 */

	if (((int)w&0x2) && nleft >= 2) {
		sum += *w++;
		nleft -= 2;
	}
	while ((nleft -= 32) >= 0) {
		asm("clrl r0");		/* clears carry */
		ADD; ADD; ADD; ADD; ADD; ADD; ADD; ADD;
		asm("adwc $0,r8");
	}
	nleft += 32;
	while ((nleft -= 8) >= 0) {
		asm("clrl r0");
		ADD; ADD;
		asm("adwc $0,r8");
	}
	nleft += 8;
	{ asm("ashl $-16,r8,r0; addw2 r0,r8");
	  asm("adwc $0,r8; movzwl r8,r8"); }
	while ((nleft -= 2) >= 0) {
		asm("movzwl (r9)+,r0; addl2 r0,r8");
	}
	if (nleft == -1) {
		sum += *(u_char *)w;
	}

	{ asm("ashl $-16,r8,r0; addw2 r0,r8; adwc $0,r8");
	  asm("mcoml r8,r8; movzwl r8,r8"); }
	return (sum);
}
