/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
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
 * from: Utah $Hdr: grf_tcreg.h 1.11 92/01/21$
 *
 *	@(#)grf_tcreg.h	8.1 (Berkeley) 6/10/93
 */

#include <hp/dev/iotypes.h>	/* XXX */

#define tccm_waitbusy(regaddr) \
	while (((struct tcboxfb *)(regaddr))->cmap_busy & 0x04) DELAY(100)

#define tc_waitbusy(regaddr,planes) \
	while (((struct tcboxfb *)(regaddr))->busy & planes) DELAY(100)

struct tcboxfb {
  u_char 	:8;
  vu_char 	reset;			/* reset register		0x01 */
  vu_char	fb_address;		/* frame buffer address 	0x02 */
  vu_char	interrupt;		/* interrupt register		0x03 */
  u_char	:8;
  vu_char	fbwmsb;			/* frame buffer width MSB	0x05 */
  u_char	:8;
  vu_char	fbwlsb;			/* frame buffer width MSB	0x07 */
  u_char	:8;
  vu_char	fbhmsb;			/* frame buffer height MSB	0x09 */
  u_char	:8;
  vu_char	fbhlsb;			/* frame buffer height MSB	0x0b */
  u_char	:8;
  vu_char	dwmsb;			/* display width MSB		0x0d */
  u_char	:8;
  vu_char	dwlsb;			/* display width MSB		0x0f */
  u_char	:8;
  vu_char	dhmsb;			/* display height MSB		0x11 */
  u_char	:8;
  vu_char	dhlsb;			/* display height MSB		0x13 */
  u_char	:8;
  vu_char	fbid;			/* Scondary frame buffer id	0x15 */
  u_char	:8;
  vu_char	bits;			/* square(0)/double-high(1) 	0x17 */
  u_char	f1[0x5b-0x17-1];
  vu_char	num_planes;		/* number of color planes       0x5b */
  u_char	:8;
  vu_char	fbomsb;			/* frame buffer offset MSB	0x5d */
  u_char	:8;
  vu_char	fbolsb;			/* frame buffer offset LSB	0x5f */
  u_char	f2[0x4040-0x5f-1];
  vu_char 	vblank;			/* vertical blanking	      0x4040 */
  u_char	:8,:8,:8;
  vu_char	busy;			/* window move active	      0x4044 */
  u_char	:8,:8,:8;
  vu_char 	vtrace_request;		/* vert retrace intr request  0x4048 */
  u_char	:8,:8,:8;
  vu_char	move_request;		/* window move intr request   0x404C */
  u_char	f3[0x4080-0x404c-1];
  vu_char	nblank;			/* display enable planes      0x4080 */
  u_char 	f4[0x4088-0x4080-1];
  vu_char	wen;			/* write enable plane 	      0x4088 */
  u_char 	f5[0x408c-0x4088-1];
  vu_char	ren;			/* read enable plane          0x408c */
  u_char 	f6[0x4090-0x408c-1];
  vu_char	fben;			/* frame buffer write enable  0x4090 */
  u_char 	f7[0x409c-0x4090-1];
  vu_char	wmove;			/* start window move 	      0x409c */
  u_char 	f8[0x40a0-0x409c-1];
  vu_char	blink;			/* enable blink planes 	      0x40a0 */
  u_char 	f9[0x40a8-0x40a0-1];
  vu_char	altframe;		/* enable alternate frame     0x40a8 */
  u_char 	f10[0x40ac-0x40a8-1];
  vu_char	curon;			/* cursor control register    0x40ac */
  u_char	f11[0x40ea-0x40ac-1];
  vu_char	prr;			/* pixel replacement rule     0x40ea */
  u_char	f12[0x40ef-0x40ea-1];
  vu_char	wmrr;			/* move replacement rule      0x40ef */
  u_char 	f13[0x40f2-0x40ef-1];
  vu_short 	source_x;		/* source x pixel # 	      0x40f2 */
  u_char 	f14[0x40f6-0x40f2-2];
  vu_short 	source_y;		/* source y pixel # 	      0x40f6 */
  u_char 	f15[0x40fa-0x40f6-2];
  vu_short	dest_x;			/* dest x pixel # 	      0x40fa */
  u_char 	f16[0x40fe-0x40fa-2];
  vu_short	dest_y;			/* dest y pixel # 	      0x40fe */
  u_char 	f17[0x4102-0x40fe-2];
  vu_short	wwidth;			/* block mover pixel width    0x4102 */
  u_char	f18[0x4106-0x4102-2];
  vu_short 	wheight;		/* block mover pixel height   0x4106 */
  /* Catseye */
  u_char	f19[0x4206-0x4106-2];
  vu_short	rug_cmdstat;		/* RUG Command/Staus	      0x4206 */
  u_char	f20[0x4510-0x4206-2];
  vu_short	vb_select;		/* Vector/BitBlt Select	      0x4510 */
  vu_short	tcntrl;			/* Three Operand Control      0x4512 */
  vu_short	acntrl;			/* BitBlt Mode		      0x4514 */
  vu_short	pncntrl;		/* Plane Control	      0x4516 */
  u_char	f21[0x4800-0x4516-2];
  vu_short	catseye_status;		/* Catseye Status	      0x4800 */
  /* End of Catseye */
  u_char 	f22[0x6002-0x4800-2];
  vu_short	cmap_busy;		/* Color Ram busy	      0x6002 */
  u_char 	f23[0x60b2-0x6002-2];
  vu_short	rdata;			/* color map red data 	      0x60b2 */
  vu_short 	gdata;			/* color map green data       0x60b4 */
  vu_short	bdata;			/* color map blue data 	      0x60b6 */
  vu_short	cindex;			/* color map index 	      0x60b8 */
  vu_short	plane_mask;		/* plane mask select	      0x60ba */
  u_char 	f24[0x60f0-0x60ba-2];
  vu_short 	strobe;			/* color map trigger 	      0x60f0 */
};
