/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
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
 *	@(#)prf.c	7.1 (Berkeley) 5/4/91
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
