/*
 * Copyright (c) 1989 Regents of the University of California.
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

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)iso_addr.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <netiso/iso.h>

struct iso_addr *
iso_addr(addr)
register char *addr;
{
	static struct iso_addr out_addr;
	register char *cp = out_addr.isoa_genaddr;
	char *cplim = cp + sizeof(out_addr.isoa_genaddr);
	register int byte;
	register nibble_cnt = 0;

	bzero((char *)&out_addr, sizeof(out_addr));
	while (*addr && (cp < cplim)) {
		byte <<= 8;
		if ((*addr >= '0') && (*addr <= '9')) {
			byte += *addr - '0';
		} else if ((*addr >= 'a') && (*addr <= 'f')) {
			byte += *addr - 'a' + 10;
		} else if ((*addr >= 'A') && (*addr <= 'F')) {
			byte += *addr - 'A' + 10;
		} else
			nibble_cnt++;
		addr++;
		nibble_cnt++;
		if (nibble_cnt > 1) {
			*cp++ = byte;
			nibble_cnt = byte = 0;
		}
	}
	if (nibble_cnt && (cp < cplim))
		*cp++ = byte;
	out_addr.isoa_len = cp - out_addr.isoa_genaddr;
	return (&out_addr);
}

char *
iso_ntoa(isoa)
struct iso_addr *isoa;
{
	static char hexlist[] = "0123456789abcdef";
	static char obuf[64];
	register char *out = obuf; 
	register int i;
	register u_char *in = (u_char *)isoa->isoa_genaddr;
	u_char *inlim = in + isoa->isoa_len;

	out[1] = 0;
	while (in < inlim) {
		i = *in++;
		*out++ = '.';
		if (i > 0xf) {
			out[1] = hexlist[i & 0xf];
			i >>= 8;
			out[0] = hexlist[i];
			out += 2;
		} else
			*out++ = hexlist[i];
	}
	*out = 0;
	return(obuf + 1);
}
