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
static char sccsid[] = "@(#)cksum.m68000.c	2.3 (Berkeley) 6/18/88";
#endif /* not lint */

#include "../globals.h"
#include <protocols/timed.h>

/* computes the checksum for ip packets for a Motorola 68000 base computer */

in_cksum(w, mlen)
	register u_short *w;
	register int mlen;
{
	register int sum = 0;

	if (mlen > 0) {
		if (((int)w & 1) == 0) {
			sum = ocsum(w, mlen>>1);
			w += mlen>>1;
			if (mlen & 1) {
				sum += *(u_char *)w << 8;
				mlen = -1;
			}
		} else {
			u_short swsum;

			sum = *(u_char *)w << 8;
			mlen--;
			w = (u_short *)(1 + (int)w);
			swsum = ocsum(w, mlen>>1);
			swab((char *)&swsum, (char *)&swsum, sizeof swsum);
			sum += swsum;
			w += mlen>>1;
			if (mlen & 1)
				sum += *(u_char *)w;
		}
	}
	sum = (sum & 0xFFFF) + (sum >> 16);
	sum = (sum & 0xFFFF) + (sum >> 16);
	sum = (~sum) & 0xFFFF;
	return (sum);
}
