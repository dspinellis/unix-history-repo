/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * from: Utah $Hdr: itereg.h 1.2 89/08/25$
 *
 *	@(#)itereg.h	7.1 (Berkeley) 5/8/90
 */

#define REGADDR		ip->regbase
#define FBBASE		((volatile u_char *)ip->fbbase)
#define	FONTROM		((u_char *)(REGADDR+0x3B))
#define ITEREGS		((struct iteregs *)(REGADDR))

/*
 * All of the HP displays use the same font ROM setup. These structures
 * are used to get at them.
 */

struct	iteregs {
	u_short	reset;
	u_short	interrupt;
	u_char	:8,
		fbwidth_h,
		:8,
		fbwidth_l,
		:8,
		fbheight_h,
		:8,
		fbheight_l,
		:8,
		dispwidth_h,
		:8,
		dispwidth_l,
		:8,
		dispheight_h,
		:8,
		dispheight_l;
};

struct	fontinfo {
	u_char	nfonts,	:8,
		fontid,	:8,
		haddr,	:8,
		laddr,	:8;
};

struct	font {
	u_char	fh,	:8,
		fw;
	u_char	pad[7],
		data[256];
};

#define draw_cursor(ip) { \
	WINDOWMOVER(ip, ip->cblanky, ip->cblankx, \
		    ip->cury * ip->ftheight, \
		    ip->curx * ip->ftwidth, \
		    ip->ftheight, ip->ftwidth, RR_XOR); \
        ip->cursorx = ip->curx; \
	ip->cursory = ip->cury; }

#define erase_cursor(ip) \
  	WINDOWMOVER(ip, ip->cblanky, ip->cblankx, \
		    ip->cursory * ip->ftheight, \
		    ip->cursorx * ip->ftwidth, \
		    ip->ftheight, ip->ftwidth, RR_XOR);
