/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
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
 * from: $Hdr: fbreg.h,v 4.300 91/06/27 20:43:07 root Rel41 $ SONY
 *
 *	@(#)fbreg.h	8.1 (Berkeley) 6/11/93
 */

#ifdef KERNEL

struct fb_softc {
	int	fbs_state;
	int	fbs_device;
	lScrType fbs_type;
	int	fbs_flag;
};

#if defined(news3200) || defined(news3400)
#define	splfb		spl3
#endif
#ifdef news3800
#define	splfb		spl4
#endif

/*
 * state of fbstate
 */
#define	FB_BUSY		01	/* fb is busy */
#define FB_WANTED	02	/* fb is wanted */
#define FB_WAIT		04	/* fb is waiting for command complete */
#define FB_DELAY	010	/* fb is busy, but not wait (inner ioctl) */
#define FB_DELAY2	020	/* fb is busy, but not wait (outer ioctl) */

/*
 * physical page map for IOP
 */
#define FB_MAX_IO	(2048*1024)	/* 2048 KB */
#define NFBMAP	(FB_MAX_IO / NBPG + 1)	/* maximum number of pages */

struct fb_map {
	caddr_t		fm_vaddr;	/* virtual address */
	unsigned	fm_offset;	/* offset into first page */
	unsigned	fm_count;	/* byte counts */
	caddr_t		fm_addr[NFBMAP]; /* first addr into page */
};

extern struct fb_map	fbmap[];

/*
 * fbreg definition
 */
struct fbreg {
	u_char	fb_command;	/* command byte */
	u_char	fb_result;	/* result byte */
	u_char	fb_device;	/* device type */
	u_char	fb_unit;	/* unit no */

	union {
	    lBitblt	_fb_bitblt;		/* bitblt command */
	    lBatchBitblt _fb_batchbitblt;	/* batch bitblt command */
	    lTileBitblt	_fb_tilebitblt;		/* tile bitblt command */
	    lBitblt3	_fb_bitblt3;		/* 3 operand bitblt command */
	    lPrimLine	_fb_polyline;		/* (disjoint) polyline */
	    lPrimRect	_fb_rectangle;		/* rectangle */
	    lPrimFill	_fb_fillscan;		/* fill pattern */
	    lPrimMarker	_fb_polymarker;		/* polymarker */
	    lPrimText	_fb_text;		/* text display */
	    lPrimDot	_fb_polydot;		/* polydot */
	    lScrType	_fb_scrtype;		/* screen type */
	    lCursor2	_fb_cursor;		/* cursor area */
	    lPalette	_fb_palette;		/* get/set palette */
	    lPoint	_fb_point;
	    int		_fb_data;
	    lVideoCtl	_fb_videoctl;
	    lVideoStatus _fb_videostatus;
	    lFbIoctl	_fb_ioctl;
	} fb_un;
};

#define fb_bitblt	fb_un._fb_bitblt
#define fb_batchbitblt	fb_un._fb_batchbitblt
#define fb_tilebitblt	fb_un._fb_tilebitblt
#define fb_bitblt3	fb_un._fb_bitblt3
#define fb_polyline	fb_un._fb_polyline
#define fb_rectangle	fb_un._fb_rectangle
#define fb_fillscan	fb_un._fb_fillscan
#define fb_polymarker	fb_un._fb_polymarker
#define fb_text		fb_un._fb_text
#define fb_polydot	fb_un._fb_polydot
#define fb_scrtype	fb_un._fb_scrtype
#define fb_cursor	fb_un._fb_cursor
#define fb_palette	fb_un._fb_palette
#define fb_point	fb_un._fb_point
#define fb_data		fb_un._fb_data
#define fb_videoctl	fb_un._fb_videoctl
#define fb_videostatus	fb_un._fb_videostatus
#define fb_fbioctl	fb_un._fb_ioctl

/* command definiton */
#define FB_CPROBE	0	/* probe */
#define FB_CATTACH	1	/* attach */
#define FB_COPEN	2	/* read from Frame Buffer */
#define FB_CCLOSE	3	/* write to Frame Buffer */
#define FB_CBITBLT	4	/* bitblt */
#define FB_CBATCHBITBLT	5	/* bitblt */
#define FB_CSETSCR	6	/* set display screen */
#define FB_CGETSCR	7	/* get display screen */
#define FB_CSETDIM	8	/* set dimmer level */
#define FB_CGETDIM	9	/* get dimmer level */
#define FB_CDRAW	10	/* graphic primitive drawing */
#define FB_CPOLYLINE	11
#define FB_CDJPOLYLINE	12
#define FB_CRECTANGLE	13
#define FB_CFILLSCAN	14
#define FB_CPOLYMARKER	15
#define FB_CTEXT	16
#define FB_CPOLYDOT	17
#define FB_CSETSCRMODE	18
#define FB_CGETSCRMODE	19
#define FB_CGETSCRTYPE	20	/* get screen type (b/w or color8 or...) */
#define FB_CSETPALETTE	21
#define FB_CGETPALETTE	22
#define FB_CSELPALETTE	23	/* select current palette */
#define FB_CSETDADDR	24	/* set display address */

#define FB_CTILEBITBLT	30	/* tile bitblt */
#define FB_CBITBLT3	31	/* 3 operand bitblt */

#define FB_CSETCURSOR	50
#define FB_CUNSETCURSOR	51
#define FB_CSHOWCURSOR	52
#define FB_CHIDECURSOR	53
#define FB_CSETXY	54
#define FB_CAUTODIM	55

#define FB_CSETVIDEO	60
#define FB_CGETVIDEO	61
#define FB_CSETPMODE	62
#define FB_CGETPMODE	63

#define FB_CGETPAGE	70

#define FB_CIOCTL	80

/* result definiton */
#define FB_ROK		0	/* OK */
#define FB_RERROR	1	/* some error occurred */

#define	FBUNIT(dev)	(minor(dev) & 0x7f)
#define	FBVIDEO(dev)	(minor(dev) & 0x80)

#define FBPRI	(PRIBIO+1)

#define ASROM_SIZE	0x10000		/* size of auto-select ROM */
#define ASREG_SIZE	0x10000		/* size of auto-select control reg. */
#define ASREG_SIZE2	0x40000		/* size of 2nd area */

#endif /* KERNEL */
