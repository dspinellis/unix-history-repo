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
 * from: Utah $Hdr: grf_rbreg.h 1.1 90/07/09$
 *
 *	@(#)grf_rbreg.h	7.2 (Berkeley) %G%
 */

/*
 * Map of the Renaissance frame buffer controller chip in memory ...
 */

#define rb_waitbusy(regaddr) \
	while (((struct rboxfb *)(regaddr))->wbusy & 0x01) DELAY(100)

#define	CM1RED	((struct rencm  *)(ip->regbase + 0x6400))
#define	CM1GRN	((struct rencm  *)(ip->regbase + 0x6800))
#define	CM1BLU	((struct rencm  *)(ip->regbase + 0x6C00))
#define	CM2RED	((struct rencm  *)(ip->regbase + 0x7400))
#define	CM2GRN	((struct rencm  *)(ip->regbase + 0x7800))
#define	CM2BLU	((struct rencm  *)(ip->regbase + 0x7C00))

#define	vu_char		volatile u_char
#define	vu_short	volatile u_short
#define	vu_int		volatile u_int

struct	rencm {
	u_char  :8, :8, :8;
	vu_char	value;
};

struct rboxfb {
    u_char	filler1[1];
    vu_char	reset;			/* reset register		0x01 */
    vu_char	fb_address;		/* frame buffer address 	0x02 */
    vu_char	interrupt;		/* interrupt register		0x03 */
    u_char	filler1a;
    vu_char	fbwmsb;			/* frame buffer width MSB	0x05 */
    u_char	filler1b;
    vu_char	fbwlsb;			/* frame buffer width MSB	0x07 */
    u_char	filler1c;
    vu_char	fbhmsb;			/* frame buffer height MSB	0x09 */
    u_char	filler1d;
    vu_char	fbhlsb;			/* frame buffer height MSB	0x0b */
    u_char	filler1e;
    vu_char	dwmsb;			/* display width MSB		0x0d */
    u_char	filler1f;
    vu_char	dwlsb;			/* display width MSB		0x0f */
    u_char	filler1g;
    vu_char	dhmsb;			/* display height MSB		0x11 */
    u_char	filler1h;
    vu_char	dhlsb;			/* display height MSB		0x13 */
    u_char	filler1i;
    vu_char	fbid;			/* frame buffer id		0x15 */
    u_char	filler1j[0x47];
    vu_char	fbomsb;			/* frame buffer offset MSB	0x5d */
    u_char	filler1k;
    vu_char	fbolsb;			/* frame buffer offset LSB	0x5f */
    u_char	filler2[16359];
    vu_char	wbusy;			/* window mover is active     0x4047 */
    u_char      filler3[0x405b - 0x4048];
    vu_char	scanbusy;		/* scan converteris active    0x405B */
    u_char      filler3b[0x4083 - 0x405c];
    vu_char	video_enable;   	/* drive vid. refresh bus     0x4083 */
    u_char	filler4[3];
    vu_char	display_enable;		/* enable the display	      0x4087 */
    u_char	filler5[8];
    vu_int	write_enable;		/* write enable register      0x4090 */
    u_char 	filler6[11];
    vu_char	wmove;			/* start window mover	      0x409f */
    u_char	filler7[3];
    vu_char	blink;			/* blink register	      0x40a3 */
    u_char	filler8[15];
    vu_char	fold;			/* fold  register	      0x40b3 */
    vu_int	opwen;			/* overlay plane write enable 0x40b4 */
    u_char	filler9[3];
    vu_char	tmode;			/* Tile mode size	      0x40bb */
    u_char	filler9a[3];		
    vu_char	drive;			/* drive register	      0x40bf */
    u_char 	filler10[3];
    vu_char	vdrive;			/* vdrive register	      0x40c3 */
    u_char 	filler10a[0x40cb-0x40c4];
    vu_char	zconfig;		/* Z-buffer mode	      0x40cb */
    u_char	filler11a[2];
    vu_short	tpatt;			/* Transparency pattern	      0x40ce */
    u_char	filler11b[3];
    vu_char	dmode;			/* dither mode		      0x40d3 */
    u_char	filler11c[3];
    vu_char	en_scan;		/* enable scan board to DTACK 0x40d7 */
    u_char	filler11d[0x40ef-0x40d8];
    vu_char	rep_rule;		/* replacement rule	      0x40ef */
    u_char 	filler12[2];
    vu_short	source_x;		/* source x		      0x40f2 */
    u_char 	filler13[2];
    vu_short	source_y;		/* source y		      0x40f6 */
    u_char 	filler14[2];
    vu_short	dest_x;			/* dest x		      0x40fa */
    u_char 	filler15[2];
    vu_short	dest_y;			/* dest y		      0x40fe */
    u_char 	filler16[2];
    vu_short	wwidth;			/* window width		      0x4102 */
    u_char 	filler17[2];
    vu_short	wheight;		/* window height	      0x4106 */
    u_char	filler18[18];
    vu_short	patt_x;			/* pattern x		      0x411a */
    u_char	filler19[2];
    vu_short	patt_y;			/* pattern y		      0x411e */
    u_char	filler20[0x8012 - 0x4120];
    vu_short	te_status;		/* transform engine status    0x8012 */
    u_char	filler21[0x1ffff-0x8014];
};
