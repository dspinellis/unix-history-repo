/*
 * Copyright (c) 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)iso_addr.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <netiso/iso.h>
#include <string.h>

/* States*/
#define VIRGIN	0
#define GOTONE	1
#define GOTTWO	2
/* Inputs */
#define	DIGIT	(4*0)
#define	END	(4*1)
#define DELIM	(4*2)

struct iso_addr *
iso_addr(addr)
	register const char *addr;
{
	static struct iso_addr out_addr;
	register char *cp = out_addr.isoa_genaddr;
	char *cplim = cp + sizeof(out_addr.isoa_genaddr);
	register int byte = 0, state = VIRGIN, new;

	bzero((char *)&out_addr, sizeof(out_addr));
	do {
		if ((*addr >= '0') && (*addr <= '9')) {
			new = *addr - '0';
		} else if ((*addr >= 'a') && (*addr <= 'f')) {
			new = *addr - 'a' + 10;
		} else if ((*addr >= 'A') && (*addr <= 'F')) {
			new = *addr - 'A' + 10;
		} else if (*addr == 0) 
			state |= END;
		else
			state |= DELIM;
		addr++;
		switch (state /* | INPUT */) {
		case GOTTWO | DIGIT:
			*cp++ = byte; /*FALLTHROUGH*/
		case VIRGIN | DIGIT:
			state = GOTONE; byte = new; continue;
		case GOTONE | DIGIT:
			state = GOTTWO; byte = new + (byte << 4); continue;
		default: /* | DELIM */
			state = VIRGIN; *cp++ = byte; byte = 0; continue;
		case GOTONE | END:
		case GOTTWO | END:
			*cp++ = byte; /* FALLTHROUGH */
		case VIRGIN | END:
			break;
		}
		break;
	} while (cp < cplim); 
	out_addr.isoa_len = cp - out_addr.isoa_genaddr;
	return (&out_addr);
}
static char hexlist[] = "0123456789abcdef";

char *
iso_ntoa(isoa)
	const struct iso_addr *isoa;
{
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
			i >>= 4;
			out[0] = hexlist[i];
			out += 2;
		} else
			*out++ = hexlist[i];
	}
	*out = 0;
	return(obuf + 1);
}
