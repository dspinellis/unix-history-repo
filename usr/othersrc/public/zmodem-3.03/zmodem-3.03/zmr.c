/*
 * File: zmr.c 12-04-1988
 * Copyright 1988 Omen Technology Inc All Rights Reserved
 *
 *
 *	This code implements ZMODEM Run Length Encoding, not funded
 *	by the original Telenet development contract.  This software,
 *	including these features, may be freely used for non
 *	commercial and educational purposes.  This software may also
 *	be freely used to support file transfer operations to or from
 *	licensed Omen Technology products.  Contact Omen Technology
 *	for licensing for other uses.  Any programs which use part or
 *	all of this software must be provided in source form with this
 *	notice intact except by written permission from Omen
 *	Technology Incorporated.
 *
 *		Omen Technology Inc		FAX: 503-621-3745
 *		Post Office Box 4681
 *		Portland OR 97208
 *
 *	This code is made available in the hope it will be useful,
 *	BUT WITHOUT ANY WARRANTY OF ANY KIND OR LIABILITY FOR ANY
 *	DAMAGES OF ANY KIND.
 *	ZMODEM RLE compression and decompression functions
 */

/* Send data subpacket RLE encoded with 32 bit FCS */
zsdar32(buf, length, frameend)
char *buf;
{
	register int c, l, n;
	register UNSL long crc;

	crc = 0xFFFFFFFFL;  l = *buf++ & 0377;
	if (length == 1) {
		zsendline(l); crc = UPDC32(l, crc);
		if (l == ZRESC) {
			zsendline(1); crc = UPDC32(1, crc);
		}
	} else {
		for (n = 0; --length >= 0; ++buf) {
			if ((c = *buf & 0377) == l && n < 126 && length>0) {
				++n;  continue;
			}
			switch (n) {
			case 0:
				zsendline(l);
				crc = UPDC32(l, crc);
				if (l == ZRESC) {
					zsendline(0100); crc = UPDC32(0100, crc);
				}
				l = c; break;
			case 1:
				if (l != ZRESC) {
					zsendline(l); zsendline(l);
					crc = UPDC32(l, crc);
					crc = UPDC32(l, crc);
					n = 0; l = c; break;
				}
				/* **** FALL THRU TO **** */
			default:
				zsendline(ZRESC); crc = UPDC32(ZRESC, crc);
				if (l == 040 && n < 34) {
					n += 036;
					zsendline(n); crc = UPDC32(n, crc);
				}
				else {
					n += 0101;
					zsendline(n); crc = UPDC32(n, crc);
					zsendline(l); crc = UPDC32(l, crc);
				}
				n = 0; l = c; break;
			}
		}
	}
	xsendline(ZDLE); xsendline(frameend);
	crc = UPDC32(frameend, crc);

	crc = ~crc;
	for (length=4; --length >= 0;) {
		zsendline((int)crc);  crc >>= 8;
	}
}


/* Receive data subpacket RLE encoded with 32 bit FCS */
zrdatr32(buf, length)
register char *buf;
{
	register int c;
	register UNSL long crc;
	register char *end;
	register int d;

	crc = 0xFFFFFFFFL;  Rxcount = 0;  end = buf + length;
	d = 0;	/* Use for RLE decoder state */
	while (buf <= end) {
		if ((c = zdlread()) & ~0377) {
crcfoo:
			switch (c) {
			case GOTCRCE:
			case GOTCRCG:
			case GOTCRCQ:
			case GOTCRCW:
				d = c;  c &= 0377;
				crc = UPDC32(c, crc);
				if ((c = zdlread()) & ~0377)
					goto crcfoo;
				crc = UPDC32(c, crc);
				if ((c = zdlread()) & ~0377)
					goto crcfoo;
				crc = UPDC32(c, crc);
				if ((c = zdlread()) & ~0377)
					goto crcfoo;
				crc = UPDC32(c, crc);
				if ((c = zdlread()) & ~0377)
					goto crcfoo;
				crc = UPDC32(c, crc);
				if (crc != 0xDEBB20E3) {
					zperr(badcrc);
					return ERROR;
				}
				Rxcount = length - (end - buf);
#ifndef DSZ
				vfile("zrdatr32: %d %s", Rxcount,
				  Zendnames[d-GOTCRCE&3]);
#endif
				return d;
			case GOTCAN:
				zperr("Sender Canceled");
				return ZCAN;
			case TIMEOUT:
				zperr("TIMEOUT");
				return c;
			default:
				zperr("Bad data subpacket");
				return c;
			}
		}
		crc = UPDC32(c, crc);
		switch (d) {
		case 0:
			if (c == ZRESC) {
				d = -1;  continue;
			}
			*buf++ = c;  continue;
		case -1:
			if (c >= 040 && c < 0100) {
				d = c - 035; c = 040;  goto spaces;
			}
			if (c == 0100) {
				d = 0;
				*buf++ = ZRESC;  continue;
			}
			d = c;  continue;
		default:
			d -= 0100;
			if (d < 1)
				goto badpkt;
spaces:
			if ((buf + d) > end)
				goto badpkt;
			while ( --d >= 0)
				*buf++ = c;
			d = 0;  continue;
		}
	}
badpkt:
	zperr("Data subpacket too long");
	return ERROR;
}

