/***********************************************************
		Copyright IBM Corporation 1987

                      All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of IBM not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

IBM DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
IBM BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/*
 * ARGO Project, Computer Sciences Dept., University of Wisconsin - Madison
 */
/* $Header: clnp_debug.c,v 4.2 88/06/29 14:58:34 hagens Exp $ */
/* $Source: /usr/argo/sys/netargo/RCS/clnp_debug.c,v $ */

#ifndef lint
static char *rcsid = "$Header: clnp_debug.c,v 4.2 88/06/29 14:58:34 hagens Exp $";
#endif lint

#include "../h/types.h"
#include "../h/param.h"
#include "../h/mbuf.h"
#include "../h/domain.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/errno.h"

#include "../net/if.h"
#include "../net/route.h"

#include "../netiso/iso.h"
#include "../netiso/clnp.h"
#include "../netiso/clnp_stat.h"
#include "../netiso/argo_debug.h"

#ifdef	ARGO_DEBUG

#ifdef	TESTDEBUG
#ifdef notdef
struct addr_37 u_37 = {
	{0x00, 0x02, 0x00, 0x10, 0x20, 0x30, 0x35}, 
	{0x01, 0x02, 0x03, 0x04, 0x50, 0x60, 0x70, 0x80, 0x90}
};
struct addr_osinet u_osinet = {
	{0x00, 0x04},
	{0x00, 0x02, 0x00, 0x01, 0x23, 0x42, 0x78, 0x20, 0x01, 0x05, 0x00}
};
#endif notdef
struct addr_rfc986 u_rfc986 = {
	{0x00, 0x06},
	{0x01, 0xc0, 0x0c, 0x0c, 0xab, 0x11}
};
struct addr_rfc986 u_bad = {
	{0x00, 0x01},
	{0x01, 0xc0, 0x0c, 0x0c, 0xab, 0x11}
};
#include <stdio.h>
main()
{
	struct iso_addr	a;

	a.isoa_afi = AFI_37;
	a.isoa_u.addr_37 = u_37;
	a.isoa_len = 17;
	printf("type 37: %s\n", clnp_iso_addrp(&a));

	a.isoa_afi = AFI_OSINET;
	a.isoa_u.addr_osinet = u_osinet;
	a.isoa_len = 14;
	printf("type osinet: %s\n", clnp_iso_addrp(&a));

	a.isoa_afi = AFI_RFC986;
	a.isoa_u.addr_rfc986 = u_rfc986;
	a.isoa_len = 9;
	printf("type rfc986: %s\n", clnp_iso_addrp(&a));

	a.isoa_afi = 12;
	a.isoa_u.addr_rfc986 = u_rfc986;
	a.isoa_len = 9;
	printf("type bad afi: %s\n", clnp_iso_addrp(&a));

	a.isoa_afi = AFI_RFC986;
	a.isoa_u.addr_rfc986 = u_bad;
	a.isoa_len = 9;
	printf("type bad idi: %s\n", clnp_iso_addrp(&a));
}
#endif	TESTDEBUG

unsigned int	clnp_debug;
static char letters[] = "0123456789abcdef";

/*
 *	Print buffer in hex, return addr of where we left off.
 *	Do not null terminate.
 */
char *
clnp_hexp(src, len, where)
char	*src;		/* src of data to print */
int		len;		/* lengthof src */
char	*where;		/* where to put data */
{
	int i;

	for (i=0; i<len; i++) {
		*where++ = letters[src[i] >> 4];
		*where++ = letters[src[i] & 0x0f];
	}
	return where;
}

/*
 *	Return a ptr to a human readable form of an iso addr 
 */
static char iso_addr_b[50];
#define	DELIM	'.';

char *
clnp_iso_addrp(isoa)
struct iso_addr *isoa;
{
	char	*cp;

	/* print length */
	clnp_sprintf(iso_addr_b, "[%d] ", isoa->isoa_len);

	/* set cp to end of what we have */
	cp = iso_addr_b;
	while (*cp)
		cp++;

	/* print afi */
	cp = clnp_hexp(&isoa->isoa_afi, 1, cp);
	*cp++ = DELIM;

	/* print type specific part */
	switch(isoa->isoa_afi) {
		case AFI_37:
			cp = clnp_hexp(isoa->t37_idi, ADDR37_IDI_LEN, cp);
			*cp++ = DELIM;
			cp = clnp_hexp(isoa->t37_dsp, ADDR37_DSP_LEN, cp);
			break;
		
/* 		case AFI_OSINET:*/
		case AFI_RFC986: {
			u_short	idi;

			/* osinet and rfc986 have idi in the same place */
			/* print idi */
			cp = clnp_hexp(isoa->rfc986_idi, ADDROSINET_IDI_LEN, cp);
			*cp++ = DELIM;
			CTOH(isoa->rfc986_idi[0], isoa->rfc986_idi[1], idi);

			if (idi == IDI_OSINET) {
				struct ovl_osinet *oosi = (struct ovl_osinet *)isoa;
				cp = clnp_hexp(oosi->oosi_orgid, OVLOSINET_ORGID_LEN, cp);
				*cp++ = DELIM;
				cp = clnp_hexp(oosi->oosi_snetid, OVLOSINET_SNETID_LEN, cp);
				*cp++ = DELIM;
				cp = clnp_hexp(oosi->oosi_snpa, OVLOSINET_SNPA_LEN, cp);
				*cp++ = DELIM;
				cp = clnp_hexp(oosi->oosi_nsap, OVLOSINET_NSAP_LEN, cp);
			} else if (idi == IDI_RFC986) {
				struct ovl_rfc986 *o986 = (struct ovl_rfc986 *)isoa;
				cp = clnp_hexp(&o986->o986_vers, 1, cp);
				*cp++ = DELIM;
#ifdef  vax
				clnp_sprintf(cp, "%d.%d.%d.%d.%d", 
				o986->o986_inetaddr[0] & 0xff,
				o986->o986_inetaddr[1] & 0xff,
				o986->o986_inetaddr[2] & 0xff,
				o986->o986_inetaddr[3] & 0xff,
				o986->o986_upid & 0xff);
				return(iso_addr_b);
#else
				cp = clnp_hexp(&o986->o986_inetaddr[0], 1, cp);
				*cp++ = DELIM;
				cp = clnp_hexp(&o986->o986_inetaddr[1], 1, cp);
				*cp++ = DELIM;
				cp = clnp_hexp(&o986->o986_inetaddr[2], 1, cp);
				*cp++ = DELIM;
				cp = clnp_hexp(&o986->o986_inetaddr[3], 1, cp);
				*cp++ = DELIM;
				cp = clnp_hexp(&o986->o986_upid, 1, cp);
#endif vax
			}
			
		} break;

		default:
			*cp++ = '?';
			break;
	}
	*cp = (char)0;
	
	return(iso_addr_b);
}

#define	MAX_COLUMNS	8
/*
 *	Dump the buffer to the screen in a readable format. Format is:
 *
 *		hex/dec  where hex is the hex format, dec is the decimal format.
 *		columns of hex/dec numbers will be printed, followed by the
 *		character representations (if printable).
 */
dump_buf(buf, len)
char	*buf;
int		len;
{
	int		i,j;

	printf("Dump buf 0x%x len 0x%x\n", buf, len);
	for (i = 0; i < len; i += MAX_COLUMNS) {
		printf("+%d:\t", i);
		for (j = 0; j < MAX_COLUMNS; j++) {
			if (i + j < len) {
				printf("%x/%d\t", buf[i+j]&0xff, buf[i+j]);
			} else {
				printf("	");
			}
		}

		for (j = 0; j < MAX_COLUMNS; j++) {
			if (i + j < len) {
				if (((buf[i+j]) > 31) && ((buf[i+j]) < 128))
					printf("%c", buf[i+j]&0xff);
				else
					printf(".");
			}
		}
		printf("\n");
	}
}


/*
 *		The following hacks are a trimmed down version of sprintf.
 */
/*VARARGS1*/
clnp_sprintf(buf, fmt, x1, x2)
	register char *fmt;
	unsigned x1, x2;
{
	clnp_prf(buf, fmt, &x1, 0);
}

clnp_prf(buf, fmt, adx, dummy)
	register char	*buf;
	register char *fmt;
	register unsigned int *adx;
	int	dummy;
{
	register int b, c, i;
	char *s;
	char *clnp_printn();

loop:
	while ((c = *fmt++) != '%') {
		if(c == '\0') {
			*buf++ = (char)0;
			return;
		}
		*buf++ = c;
	}
again:
	c = *fmt++;
	switch (c) {
	case 'l':
		goto again;
	case 'x': case 'X':
		b = 16;
		goto number;
	case 'd': case 'D':
	case 'u':		/* what a joke */
		b = 10;
		goto number;
	case 'o': case 'O':
		b = 8;
number:
		buf = clnp_printn((unsigned long)*adx, b, buf);
		break;
	case 'c':
		b = *adx;
		for (i = 24; i >= 0; i -= 8)
			if (c = (b >> i) & 0x7f)
				*buf++ = c;
		break;

	case 's':
		s = (char *)*adx;
		while (*s)
			*buf++ = *s++;
		break;

	case '%':
		*buf++ = '%';
		break;
	}
	adx++;
	goto loop;
}

char *
clnp_printn(n, b, where)
unsigned long	n;
int		b;
char	*where;
{
	char prbuf[11];
	register char *cp;

	if (b == 10 && (int)n < 0) {
		*where++ = '-';
		n = (unsigned)(-(int)n);
	}
	cp = prbuf;
	do {
		*cp++ = "0123456789abcdef"[n%b];
		n /= b;
	} while (n);
	do {
		*where++ = *--cp;
	} while (cp > prbuf);
	return(where);
}
#endif	ARGO_DEBUG
