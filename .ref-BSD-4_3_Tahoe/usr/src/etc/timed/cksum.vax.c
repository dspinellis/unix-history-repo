/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)cksum.vax.c	1.4 (Berkeley) 6/18/88";
#endif /* not lint */

#include <sys/types.h>

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
