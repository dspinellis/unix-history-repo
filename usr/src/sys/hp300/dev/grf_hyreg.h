/*
 * Copyright (c) 1991 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department and Mark Davies of the Department of Computer
 * Science, Victoria University of Wellington, New Zealand.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: grf_hyreg.h 1.1 92/01/22$
 *
 *	@(#)grf_hyreg.h	7.2 (Berkeley) %G%
 */

#include <hp/dev/iotypes.h>	/* XXX */

struct hyboxfb {
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
  u_char	f2[0x4000-0x5f-1];
  vu_char	nblank;			/* display enable planes      0x4000 */
};

