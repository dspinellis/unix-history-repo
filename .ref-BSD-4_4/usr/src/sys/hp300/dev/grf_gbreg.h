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
 * from: Utah $Hdr: grf_gbreg.h 1.11 92/01/21$
 *
 *	@(#)grf_gbreg.h	8.1 (Berkeley) 6/10/93
 */

#include <hp/dev/iotypes.h>	/* XXX */

/*
 * Gatorbox driver regs
 */

#define TILER_ENABLE		0x80
#define LINE_MOVER_ENABLE	0x80
#define UP_LEFT         	0x00
#define DOWN_RIGHT		0x40
#define MOVE_UP_LEFT    	TILER_ENABLE|UP_LEFT
#define MOVE_DOWN_RIGHT 	TILER_ENABLE|DOWN_RIGHT

#define tile_mover_waitbusy(regaddr) \
  	while (((struct gboxfb *)(regaddr))->sec_interrupt & 0x10)

#define line_mover_waitbusy(regaddr) \
  	while ((((struct gboxfb *)(regaddr))->status & 0x80) == 0)

#define gbcm_waitbusy(regaddr) \
	while (((struct gboxfb *)(regaddr))->cmap_busy != 0xff)

struct gboxfb {
  u_char 	:8;
  vu_char 	reset;			/* reset register		0x01 */
  vu_char	sec_interrupt;		/* Secondary interrupt register 0x03 */
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
  u_char	f1[0x5d-0x15-1];
  vu_char	fbomsb;			/* frame buffer offset MSB	0x5d */
  u_char	:8;
  vu_char	fbolsb;			/* frame buffer offset LSB	0x5f */
  u_char	f2[0x4000-0x5f-1];
  vu_char 	crtc_address;		/* CTR controller address reg 0x4000 */
  vu_char	status;			/* Status register	      0x4001 */
  vu_char	crtc_data;		/* CTR controller data reg    0x4002 */
  u_char	f3[6];
  vu_char	line_mover_rep_rule;    /* Line move rep rule	             */
  u_char	:8, :8;
  vu_char	line_mover_width;	/* Line move width	             */
  u_char	f4[0xff3];
  vu_char	width;			/* width in tiles	      0x5001 */
  u_char	:8;
  vu_char 	height;			/* height in tiles	      0x5003 */
  u_char 	f5[3];
  vu_char	rep_rule;		/* replacement rule	      0x5007 */
  u_char 	f6[0x6001-0x5007-1];
  vu_char 	blink1;			/* blink 1		      0x6001 */
  u_char 	f7[3];
  vu_char	blink2;			/* blink 2		      0x6005 */
  u_char 	f8[3];
  vu_char	write_protect;		/* write protect 	      0x6009 */
  u_char	f9[0x6803-0x6009-1];
  vu_char	cmap_busy;		/* color map busy             0x6803 */
  u_char 	f10[0x68b9-0x6803-1];
  vu_char	creg_select;         	/* color map register select  0x68b8 */
  u_char 	f11[0x68f1-0x68b9-1];
  vu_char 	cmap_write;		/* color map write trigger    0x68f1 */
  u_char 	f12[0x69b3-0x68f1-1];
  vu_char	cmap_red;		/* red value register         0x69b3 */
  u_char	:8;
  vu_char	cmap_grn;		/* green value register       0x69b5 */
  u_char	:8;
  vu_char	cmap_blu;		/* blue value register        0x69b6 */
};
