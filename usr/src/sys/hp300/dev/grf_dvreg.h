/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: grf_dvreg.h 1.5 92/01/21$
 *
 *	@(#)grf_dvreg.h	7.4 (Berkeley) %G%
 */

#include <hp/dev/iotypes.h>	/* XXX */

/*
 * Map of the DaVinci frame buffer controller chip in memory ...
 */

#define db_waitbusy(regaddr) \
	while (((struct dvboxfb *)(regaddr))->wbusy || \
	       ((struct dvboxfb *)(regaddr))->as_busy) DELAY(100)

struct rgb {
  u_char :8, :8, :8;
  vu_char red;
  u_char :8, :8, :8;
  vu_char green;
  u_char :8, :8, :8;
  vu_char blue;
};

struct dvboxfb {
  u_char 	:8;
  vu_char 	reset;			/* reset register		0x01 */
  u_char	fb_address;		/* frame buffer address 	0x02 */
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
  vu_char	fbid;			/* frame buffer id		0x15 */
  u_char	f1[0x47];
  vu_char	fbomsb;			/* frame buffer offset MSB	0x5d */
  u_char	:8;
  vu_char	fbolsb;			/* frame buffer offset LSB	0x5f */
  u_char	f2[16359];
  vu_char	wbusy;			/* Window move in progress    0x4047 */
  u_char	f3[0x405b-0x4047-1];
  vu_char	as_busy;		/* Scan accessing frame buf.  0x405B */
  u_char        f4[0x4090-0x405b-1];
  vu_int	fbwen;			/* Frame buffer write enable  0x4090 */
  u_char	f5[0x409f-0x4090-4];
  vu_char	wmove;			/* Initiate window move.      0x409F */
  u_char	f6[0x40b3-0x409f-1];
  vu_char	fold;			/* Byte/longword per pixel    0x40B3 */
  u_char	f7[0x40b7-0x40b3-1];
  vu_char	opwen;			/* Overlay plane write enable 0x40B7 */
  u_char	f8[0x40bf-0x40b7-1];
  vu_char	drive;			/* Select FB vs. Overlay.     0x40BF */

  u_char        f8a[0x40cb-0x40bf-1];
  vu_char	zconfig;		/* Z buffer configuration     0x40CB */
  u_char	f8b[0x40cf-0x40cb-1];
  vu_char	alt_rr;			/* Alternate replacement rule 0x40CF */
  u_char	f8c[0x40d3-0x40cf-1];
  vu_char	zrr;			/* Z replacement rule	      0x40D3 */

  u_char	f9[0x40d7-0x40d3-1];
  vu_char	en_scan;		/* Enable scan DTACK.	      0x40D7 */
  u_char 	f10[0x40ef-0x40d7-1];
  vu_char  	rep_rule;		/* Replacement rule	      0x40EF */
  u_char 	f11[0x40f2-0x40ef-1];
  vu_short	source_x;		/* Window source X origin     0x40F2 */
  u_char	f12[0x40f6-0x40f2-2];
  vu_short	source_y;		/* Window source Y origin     0x40F6 */
  u_char 	f13[0x40fa-0x40f6-2];
  vu_short	dest_x;			/* Window dest X origin       0x40FA */
  u_char 	f14[0x40fe-0x40fa-2];
  vu_short	dest_y;			/* Window dest Y origin       0x40FE */
  u_char 	f15[0x4102-0x40fe-2];
  vu_short 	wwidth;			/* Window width		      0x4102 */
  u_char 	f16[0x4106-0x4102-2];
  vu_short	wheight;		/* Window height	      0x4106 */
  u_char 	f17[0x6003-0x4106-2];
  vu_char	cmapbank;		/* Bank select (0 or 1)       0x6003 */
  u_char 	f18[0x6007-0x6003-1];
  vu_char	dispen;			/* Display enable	      0x6007 */

  u_char	f18a[0x600B-0x6007-1];
  vu_char	fbvenp;			/* Frame buffer video enable  0x600B */
  u_char	f18b[0x6017-0x600B-1];
  vu_char	fbvens;			/* fbvenp blink counterpart   0x6017 */

  u_char 	f19[0x6023-0x6017-1];
  vu_char	vdrive;			/* Video display mode	      0x6023 */
  u_char	f20[0x6083-0x6023-1];
  vu_char	panxh;			/* Pan display in X (high)    0x6083 */
  u_char	f21[0x6087-0x6083-1];
  vu_char	panxl;			/* Pan display in X (low)     0x6087 */
  u_char	f22[0x608b-0x6087-1];
  vu_char	panyh;			/* Pan display in Y (high)    0x608B */
  u_char	f23[0x608f-0x608b-1];
  vu_char	panyl;			/* Pan display in Y (low)     0x608F */
  u_char	f24[0x6093-0x608f-1];
  vu_char	zoom;			/* Zoom factor		      0x6093 */
  u_char 	f25[0x6097-0x6093-1];
  vu_char	pz_trig;		/* Pan & zoom trigger	      0x6097 */
  u_char 	f26[0x609b-0x6097-1];
  vu_char	ovly0p;			/* Overlay 0 primary map      0x609B */
  u_char	f27[0x609f-0x609b-1];
  vu_char	ovly1p;			/* Overlay 1 primary map      0x609F */
  u_char	f28[0x60a3-0x609f-1];
  vu_char	ovly0s;			/* Overlay 0 secondary map    0x60A3 */
  u_char	f29[0x60a7-0x60a3-1];
  vu_char	ovly1s;			/* Overlay 1 secondary map    0x60A7 */
  u_char	f30[0x60ab-0x60a7-1];
  vu_char	opvenp;			/* Overlay video enable	      0x60AB */
  u_char	f31[0x60af-0x60ab-1];
  vu_char	opvens;			/* Overlay blink enable	      0x60AF */
  u_char 	f32[0x60b3-0x60af-1];
  vu_char	fv_trig;		/* Trigger control registers  0x60B3 */
  u_char	f33[0x60b7-0x60b3-1];
  vu_char	cdwidth;		/* Iris cdwidth timing reg.   0x60B7 */
  u_char 	f34[0x60bb-0x60b7-1];
  vu_char	chstart;		/* Iris chstart timing reg.   0x60BB */
  u_char	f35[0x60bf-0x60bb-1];
  vu_char	cvwidth;		/* Iris cvwidth timing reg.   0x60BF */
  u_char 	f36[0x6100-0x60bf-1];
  struct 	rgb rgb[8];		/* overlay color map */
  u_char 	f37[0x6403-0x6100-sizeof(struct rgb)*8];
  vu_char 	red0;
  u_char 	f38[0x6803-0x6403-1];
  vu_char	green0;
  u_char	f39[0x6c03-0x6803-1];
  vu_char	blue0;
  u_char 	f40[0x7403-0x6c03-1];
  vu_char 	red1;
  u_char	f41[0x7803-0x7403-1];
  vu_char	green1;
  u_char 	f42[0x7c03-0x7803-1];
  vu_char 	blue1;
  u_char 	f43[0x8012-0x7c03-1];
  vu_short	status1;		/* Master Status register     0x8012 */
  u_char	f44[0xC226-0x8012-2];
  vu_short	trans;			/* Transparency		      0xC226 */
  u_char	f45[0xC23E-0xC226-2];
  vu_short 	pstop;			/* Pace value control	      0xc23e */
};
