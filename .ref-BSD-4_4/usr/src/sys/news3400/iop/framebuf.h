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
 * from: $Hdr: framebuf.h,v 4.300 91/06/09 06:42:21 root Rel41 $ SONY
 *
 *	@(#)framebuf.h	8.1 (Berkeley) 6/11/93
 */

#ifndef __FRAMEBUF__
#define __FRAMEBUF__ 1

typedef unsigned short	Word;

#define BitsPerWord	16

typedef struct {
	short	x, y;
} sPoint;

typedef struct {
	int	x, y;
} lPoint;

typedef struct {
	sPoint	origin;
	sPoint	extent;
} sRectangle;

typedef struct {
	lPoint	origin;
	lPoint	extent;
} lRectangle;

typedef struct {
	char		type;	/* BM_FB, BM_MEM, BM_0, BM_1 or BM_LBP */
	char		depth;	/* bitmap depth */
	unsigned short	width;	/* width in Words */
	sRectangle	rect;	/* defined area */
	Word		*base;	/* for BM_MEM */
} sBitmap;

typedef struct {
	char		type;	/* BM_FB, BM_MEM, BM_0, BM_1 or BM_LBP */
	char		depth;	/* bitmap depth */
	unsigned short	width;	/* width in Words */
	lRectangle	rect;	/* defined area */
	Word		*base;	/* for BM_MEM */
} lBitmap;

#define BM_FB		0	/* frame buffer */
#define BM_MEM		1	/* bitmap in memory (XY format) */
#define BM_0		2	/* virtual bitmap of data '0' */
#define BM_1		3	/* virtual bitmap of data '1' */
#define BM_LBP		4	/* lbp page buffer (future support) */

/* 2 operand bitblt */
typedef struct {
	unsigned char	func;		/* function code */
	char		transp;		/* transparency */
	int		fore_color;	/* foreground color */
	int		aux_color;	/* auxiliary color */
	int		planemask;	/* select plane */
	sBitmap		srcBitmap;	/* source bitmap */
	sRectangle	srcRect;	/* source rectangle */
	sBitmap		destBitmap;	/* destination bitmap */
	sRectangle	destClip;	/* clip rectangle */
	sPoint		destPoint;	/* destination point */
} sBitblt;

typedef struct {
	unsigned char	func;		/* function code */
	char		transp;		/* transparency */
	int		fore_color;	/* foreground color */
	int		aux_color;	/* auxiliary color */
	int		planemask;	/* select plane */
	lBitmap		srcBitmap;	/* source bitmap */
	lRectangle	srcRect;	/* source rectangle */
	lBitmap		destBitmap;	/* destination bitmap */
	lRectangle	destClip;	/* clip rectangle */
	lPoint		destPoint;	/* destination point */
} lBitblt;

/* tile 2 operand bitblt */
typedef struct {
	unsigned char	func;		/* function code */
	char		transp;		/* transparency */
	int		fore_color;	/* foreground color */
	int		aux_color;	/* auxiliary color */
	int		planemask;	/* select plane */
	sBitmap		ptnBitmap;	/* pattern bitmap */
	sRectangle	ptnRect;	/* pattern rectangle */
	sPoint		refPoint;	/* reference point */
	sBitmap		destBitmap;	/* destination bitmap */
	sRectangle	destClip;	/* clip rectangle */
	sRectangle	destRect;	/* destination rectangle */
} sTileBitblt;

typedef struct {
	unsigned char	func;		/* function code */
	char		transp;		/* transparency */
	int		fore_color;	/* foreground color */
	int		aux_color;	/* auxiliary color */
	int		planemask;	/* select plane */
	lBitmap		ptnBitmap;	/* pattern bitmap */
	lRectangle	ptnRect;	/* pattern rectangle */
	lPoint		refPoint;	/* reference point */
	lBitmap		destBitmap;	/* destination bitmap */
	lRectangle	destClip;	/* clip rectangle */
	lRectangle	destRect;	/* destination rectangle */
} lTileBitblt;

/* 3 operand bitblt */
typedef struct {
	unsigned char	func;		/* function code */
	char		transp;		/* transparency */
	int		fore_color;	/* foreground color */
	int		aux_color;	/* auxiliary color */
	int		planemask;	/* select plane */
	sBitmap		ptnBitmap;	/* pattern bitmap */
	sRectangle	ptnRect;	/* pattern rectangle */
	sPoint		refPoint;	/* reference point */
	sBitmap		srcBitmap;	/* source bitmap */
	sRectangle	srcRect;	/* source rectangle */
	sBitmap		destBitmap;	/* destination bitmap */
	sRectangle	destClip;	/* clip rectangle */
	sPoint		destPoint;	/* destination point */
} sBitblt3;

typedef struct {
	unsigned char	func;		/* function code */
	char		transp;		/* transparency */
	int		fore_color;	/* foreground color */
	int		aux_color;	/* auxiliary color */
	int		planemask;	/* select plane */
	lBitmap		ptnBitmap;	/* pattern bitmap */
	lRectangle	ptnRect;	/* pattern rectangle */
	lPoint		refPoint;	/* reference point */
	lBitmap		srcBitmap;	/* source bitmap */
	lRectangle	srcRect;	/* source rectangle */
	lBitmap		destBitmap;	/* destination bitmap */
	lRectangle	destClip;	/* clip rectangle */
	lPoint		destPoint;	/* destination point */
} lBitblt3;

typedef struct {
	sRectangle	srcRect;	/* source rectangle */
	sPoint		destPoint;	/* destination point */
} sSrcDest;

typedef struct {
	lRectangle	srcRect;	/* source rectangle */
	lPoint		destPoint;	/* destination point */
} lSrcDest;

/*
 * batch bitblt
 */
typedef struct {
	unsigned char	func;		/* function code */
	char		transp;		/* transparency */
	int		fore_color;	/* foreground color */
	int		aux_color;	/* auxiliary color */
	int		planemask;	/* select plane */
	sBitmap		srcBitmap;	/* source bitmap */
	sBitmap		destBitmap;	/* destination bitmap */
	sRectangle	destClip;	/* clip rectangle */
	int		nSrcDest;	/* number of src-dest in list */
	sSrcDest	*srcDestList;	/* pointer to src-dest spec */
} sBatchBitblt;

typedef struct {
	unsigned char	func;		/* function code */
	char		transp;		/* transparency */
	int		fore_color;	/* foreground color */
	int		aux_color;	/* auxiliary color */
	int		planemask;	/* select plane */
	lBitmap		srcBitmap;	/* source bitmap */
	lBitmap		destBitmap;	/* destination bitmap */
	lRectangle	destClip;	/* clip rectangle */
	int		nSrcDest;	/* number of src-dest in list */
	lSrcDest	*srcDestList;	/* pointer to src-dest spec */
} lBatchBitblt;

#define MAX_BATCHBITBLT	1024		/* max number in src-dest list */

/*
 * set screen mode
 */

#define BLACK_ON_WHITE	0	/* white - 0, black - 1 (default) */
#define WHITE_ON_BLACK	1	/* white - 1, black - 0 */

/*
 * graphic primitive drawing
 */

/* scan line array */
typedef struct {
	short	y;
	short	x0, x1;		/* x0 <= x1 */
} sScanl;

typedef struct {
	int	y;
	int	x0, x1;		/* x0 <= x1 */
} lScanl;

/* line */
typedef struct {
	unsigned int	lptn;		/* line pattern */
	short		np;		/* number of points */
	sPoint		*plist;		/* point list */
	int		fore_color;	/* foreground color */
	int		aux_color;	/* auxiliary color */
	int		planemask;	/* select plane */
	char		transp;		/* transparency */
	unsigned char	func;		/* rop function code */
	char		dlpf;		/* draw last point flag */
	sRectangle	clip;		/* clip rectangle */
	sBitmap		drawBM;		/* drawing bitmap */
} sPrimLine;

typedef struct {
	unsigned int	lptn;		/* line pattern */
	short		np;		/* number of points */
	lPoint		*plist;		/* point list */
	int		fore_color;	/* foreground color */
	int		aux_color;	/* auxiliary color */
	int		planemask;	/* select plane */
	char		transp;		/* transparency */
	unsigned char	func;		/* rop function code */
	char		dlpf;		/* draw last point flag */
	lRectangle	clip;		/* clip rectangle */
	lBitmap		drawBM;		/* drawing bitmap */
} lPrimLine;

/* rectangle filling */
typedef struct {
	sRectangle	rect;		/* rectangle */
	sPoint		refPoint;	/* fill reference point */
	sRectangle	ptnRect;	/* pattern rectangle */
	sBitmap		ptnBM;		/* pattern bitmap */
	int		fore_color;	/* foreground color */
	int		aux_color;	/* auxiliary color */
	int		planemask;	/* select plane */
	char		transp;		/* transparency */
	unsigned char	func;		/* rop function code */
	sRectangle	clip;		/* clip rectangle */
	sBitmap		drawBM;		/* drawing bitmap */
} sPrimRect;

typedef struct {
	lRectangle	rect;		/* rectangle */
	lPoint		refPoint;	/* fill reference point */
	lRectangle	ptnRect;	/* pattern rectangle */
	lBitmap		ptnBM;		/* pattern bitmap */
	int		fore_color;	/* foreground color */
	int		aux_color;	/* auxiliary color */
	int		planemask;	/* select plane */
	char		transp;		/* transparency */
	unsigned char	func;		/* rop function code */
	lRectangle	clip;		/* clip rectangle */
	lBitmap		drawBM;		/* drawing bitmap */
} lPrimRect;

/* pattern filling */
typedef struct {
	short		nscan;		/* number of scan element */
	sScanl		*scan;		/* scan line data */
	sPoint		refPoint;	/* fill reference point */
	sRectangle	ptnRect;	/* pattern rectangle */
	sBitmap		ptnBM;		/* pattern bitmap */
	int		fore_color;	/* foreground color */
	int		aux_color;	/* auxiliary color */
	int		planemask;	/* select plane */
	char		transp;		/* transparency */
	unsigned char	func;		/* rop function code */
	sRectangle	clip;		/* clip rectangle */
	sBitmap		drawBM;		/* drawing bitmap */
} sPrimFill;

typedef struct {
	short		nscan;		/* number of scan element */
	lScanl		*scan;		/* scan line data */
	lPoint		refPoint;	/* fill reference point */
	lRectangle	ptnRect;	/* pattern rectangle */
	lBitmap		ptnBM;		/* pattern bitmap */
	int		fore_color;	/* foreground color */
	int		aux_color;	/* auxiliary color */
	int		planemask;	/* select plane */
	char		transp;		/* transparency */
	unsigned char	func;		/* rop function code */
	lRectangle	clip;		/* clip rectangle */
	lBitmap		drawBM;		/* drawing bitmap */
} lPrimFill;

/* marker */
typedef struct {
	short		np;		/* number of points */
	sPoint		*plist;		/* point list */
	sRectangle	ptnRect;	/* pattern rectangle */
	sBitmap		ptnBM;		/* pattern bitmap */
	int		fore_color;	/* foreground color */
	int		aux_color;	/* auxiliary color */
	int		planemask;	/* select plane */
	char		transp;		/* transparency */
	unsigned char	func;		/* rop function code */
	sRectangle	clip;		/* clip rectangle */
	sBitmap		drawBM;		/* drawing bitmap */
} sPrimMarker;

typedef struct {
	short		np;		/* number of points */
	lPoint		*plist;		/* point list */
	lRectangle	ptnRect;	/* pattern rectangle */
	lBitmap		ptnBM;		/* pattern bitmap */
	int		fore_color;	/* foreground color */
	int		aux_color;	/* auxiliary color */
	int		planemask;	/* select plane */
	char		transp;		/* transparency */
	unsigned char	func;		/* rop function code */
	lRectangle	clip;		/* clip rectangle */
	lBitmap		drawBM;		/* drawing bitmap */
} lPrimMarker;

/* text */
#define MAX_STRING	1024

typedef struct {
	char		type;		/* ROM-font, etc */
	short		len;		/* string length (byte) */
	unsigned char	*str;		/* string */
	sPoint		p;
	int		dx, dy;		/* vector (16bit left shifted) */
	char		ex_factor;	/* expansion factor */
	sPoint		fp;		/* bitmap font upper-left */
	short		width, height;	/* font width, font height */
	short		column;		/* number of characters in a row */
	unsigned short	first_chr;	/* first character code */
	unsigned short	last_chr;	/* last character code */
	sBitmap		fontBM;		/* font bitmap */
	int		fore_color;	/* foreground color */
	int		aux_color;	/* auxiliary color */
	int		planemask;	/* select plane */
	char		transp;		/* transparency */
	unsigned char	func;		/* rop function code */
	sRectangle	clip;		/* clip rectangle */
	sBitmap		drawBM;		/* drawing bitmap */
} sPrimText;

typedef struct {
	int		dx, dy;		/* vector (16bit left shifted) */
	lPoint		fp;		/* bitmap font upper-left */
	short		width, height;	/* font width, font height */
	short		column;		/* number of characters in a row */
	unsigned short	first_chr;	/* first character code */
	unsigned short	last_chr;	/* last character code */
	lBitmap		fontBM;		/* font bitmap */
	int		fore_color;	/* foreground color */
	int		aux_color;	/* auxiliary color */
	int		planemask;	/* select plane */
	lBitmap		drawBM;		/* drawing bitmap */
	lRectangle	clip;		/* clip rectangle */
	lPoint		p;		/* output position */
	unsigned char	*str;		/* string */
	short		len;		/* string length (byte) */
	char		type;		/* ROM-font, etc */
	char		transp;		/* transparency */
	unsigned char	func;		/* rop function code */
	char		ex_factor;	/* expansion factor */
} lPrimText;

/* dot */
typedef struct {
	short		np;		/* number of points */
	sPoint		*plist;		/* point list */
	int		fore_color;	/* foreground color */
	int		aux_color;	/* auxiliary color */
	int		planemask;	/* select plane */
	char		transp;		/* transparency */
	unsigned char	func;		/* rop function code */
	sRectangle	clip;		/* clip rectangle */
	sBitmap		drawBM;		/* drawing bitmap */
} sPrimDot;

typedef struct {
	short		np;		/* number of points */
	lPoint		*plist;		/* point list */
	int		fore_color;	/* foreground color */
	int		aux_color;	/* auxiliary color */
	int		planemask;	/* select plane */
	char		transp;		/* transparency */
	unsigned char	func;		/* rop function code */
	lRectangle	clip;		/* clip rectangle */
	lBitmap		drawBM;		/* drawing bitmap */
} lPrimDot;

/*
 *	screen type
 */
typedef struct {
	short		colorwidth;	/* palette color width */
	short		plane;		/* number of planes */
	sRectangle	bufferrect;	/* framebuffer region */
	sRectangle	visiblerect;	/* visible screen region */
} sScrType;

typedef struct {
	short		colorwidth;	/* palette color width */
	short		plane;		/* number of planes */
	char		type;		/* device type */
	char		unit;		/* unit no */
	lRectangle	bufferrect;	/* framebuffer region */
	lRectangle	visiblerect;	/* visible screen region */
} lScrType;

/* device type */
#define	FB_NWB512	1	/* fb is B/W bitmap display (816x1024) */
#define	FB_NWB225	2	/* fb is color bitmap display (1280x1024) */
#define	FB_POPM		3	/* fb is B/W bitmap display (816x1024) */
#define	FB_POPC		4	/* fb is color bitmap display (1024x768) */
#define FB_NWB514	5	/* fb is GrayScale bitmap display (1280x1280) */
#define FB_NWB251	6	/* fb is New color bitmap display (1280x1024) */
#define	FB_LCDM		7	/* fb is lcd bitmap (1120x780) */
#define	FB_LCDC		8	/* fb is LCD color bitmap display (?) */
#define	FB_NWB518	9	/* fb is B/W bitmap display (1024x768) */
#define	FB_NWB252	10	/* fb is color bitmap display (1024x768) */
#define	FB_NWB253	11	/* fb is B/W bitmap display (816x1024) */
#define	FB_NWB254	12	/* fb is color bitmap display (1024x768) */
#define	FB_NWB255	13	/* fb is B/W bitmap display (1280x1280) */
#define	FB_SLB101	14	/* fb is color bitmap display (1920x1035) */
#define FB_NWB256	15	/* fb is 3D-Rendaring Board (1280x1024) */
#define	FB_NWB257	16	/* fb is color bitmap display (1280x1024) */

#define FB_NWB240	200	/* fb is A3 LPB/IR interface (3136x4516) */
#define FB_NWB241	201	/* fb is A4 LBP/IR interface (3904x5600) */
#define FB_NWB242	202	/* fb is A3 Image Processor (3136x4516) */

#define FB_FB2015	254	/* fb is full color big display (2048x1536) */
#define FB_FB2020	255	/* fb is full color big display (2048x2048) */

#define FB_MONO0	1	/* fb is B/W (compat-3.0) */
#define FB_COLOR0	2	/* fb is color (compat-3.0) */

/*
 *	color
 */
typedef struct {
	short	index;		/* palette number */
	struct {
		short	r, g, b;
	} rgb;
} sPalette;

typedef struct {
	int		count;
	sPalette	*palette;
} lPalette;

/*
 *	cursor
 */
typedef struct {
	unsigned char	func;
	char		cursor_color;
	char		mask_color;
	sPoint		hot;
	sPoint		size;
	sRectangle	cursorRect;	/* Cursor Pattern rea */
	sRectangle	maskRect;	/* Mask Pattern Area */
	sRectangle	saveRect;	/* Save Pattern Area */
	sRectangle	moveArea;
} sCursor;

typedef struct {
	unsigned char	func;
	char		cursor_color;
	char		mask_color;
	lPoint		hot;
	lPoint		size;
	lRectangle	cursorRect;	/* Cursor Pattern Area */
	lRectangle	maskRect;	/* Mask Pattern Area */
	lRectangle	saveRect;	/* Save Pattern Area */
	lRectangle	moveArea;
} lCursor;

typedef struct {
	unsigned char	func;
	int		cursor_color;
	int		mask_color;
	lPoint		hot;
	lPoint		size;
	lRectangle	cursorRect;	/* Cursor Pattern Area */
	lRectangle	maskRect;	/* Mask Pattern Area */
	lRectangle	saveRect;	/* Save Pattern Area */
	lRectangle	moveArea;
} lCursor2;

typedef struct {
	int request;
	lPoint sp;
	lPoint dp;
	lPoint size;
	int refresh_rate;
	int func;
	int planemask;
} lVideoCtl;

typedef struct {
	int request;
	int mode;
	int status;
	lRectangle vframe;
	int refresh_rate;
	int func;
	int planemask;
} lVideoStatus;

/* Video Control Request */
#define VIDEO_RESET     0x01
#define VIDEO_RUN       0x02
#define VIDEO_STOP      0x04
#define VIDEO_CONT      0x08
#define VIDEO_FRAME     0x10
#define VIDEO_ROP       0x20

/* Video Status */
#define VIDEO_STATUS    0x01

#define VIDEO_SIG_NTSC  0x01
#define VIDEO_SIG_PAL   0x02

#define VIDEO_STATUS_RUN        1
#define VIDEO_STATUS_STOP       2
#define VIDEO_STATUS_ERROR      4

#define VIDEO_ERROR_SIZE        0x100
#define VIDEO_ERROR_POINT       0x200
#define VIDEO_ERROR_PARAM       0x400

typedef struct {
	int	request;
	int	param[8];
} lFbIoctl;


/* func */
#define BF_0	0x0	/* 0 */
#define BF_SDA	0x1	/* Src & Dest */
#define BF_SDIA	0x2	/* Src & ~Dest */
#define BF_S	0x3	/* Src */
#define BF_SIDA	0x4	/* ~Src & Dest */
#define BF_D	0x5	/* Dest */
#define BF_SDX	0x6	/* Src ^ Dest */
#define BF_SDO	0x7	/* Src | Dest */
#define BF_SDOI	0x8	/* ~(Src | Dest) */
#define BF_SDXI	0x9	/* ~(Src ^ Dest) */
#define BF_DI	0xa	/* ~Dest */
#define BF_SDIO	0xb	/* Src | ~Dest */
#define BF_SI	0xc	/* ~Src */
#define BF_SIDO	0xd	/* ~Src | Dest */
#define BF_SDAI	0xe	/* ~(Src & Dest) */
#define BF_1	0xf	/* 1 */

#define BF_NOP(f)	((f) == BF_D)
#define BF_INV(f)	((f) == BF_DI)
#define BF_CON(f)	((f) == BF_0 || (f) == BF_1)
#define BF_SRC(f)	(!BF_NOP(f)&&!BF_INV(f)&&!BF_CON(f))

/* Bitmap Font Type */
#define ROM_ASCII	0
#define ROM_KANJI	1
#define ASCII		2
#define ROM_CONS	3

/* Plane Mask */
#define FB_PLANEALL	0xffffff
#define FB_PLANE0	0x000001
#define FB_PLANE1	0x000002
#define FB_PLANE2	0x000004
#define FB_PLANE3	0x000008
#define FB_PLANE4	0x000010
#define FB_PLANE5	0x000020
#define FB_PLANE6	0x000040
#define FB_PLANE7	0x000080
#define FB_PLANE8	0x000100
#define FB_PLANE9	0x000200
#define FB_PLANE10	0x000400
#define FB_PLANE11	0x000800
#define FB_PLANE12	0x001000
#define FB_PLANE13	0x002000
#define FB_PLANE14	0x004000
#define FB_PLANE15	0x008000
#define FB_PLANE16	0x010000
#define FB_PLANE17	0x020000
#define FB_PLANE18	0x040000
#define FB_PLANE19	0x080000
#define FB_PLANE20	0x100000
#define FB_PLANE21	0x200000
#define FB_PLANE22	0x400000
#define FB_PLANE23	0x800000

/* Line Pattern */
#define LINE_SLD	(unsigned)0xffffffff	/* solid */
#define LINE_DSH	(unsigned)0xfcfcfcfc	/* dash */
#define LINE_DOT	(unsigned)0xcccccccc	/* dot */
#define LINE_DSHDOT	(unsigned)0xfff18fff	/* dash dot */
#define LINE_DSHDOTDOT	(unsigned)0xff8c63ff	/* dash dot dot */

/*
 * FB IOCTL
 */
#include <sys/ioctl.h>

#define FBIOCSETSCR		_IOW('F', 0, int)
#define FBIOCGETSCR		_IOR('F', 1, int)
#define FBIOCSETDIM		_IOW('F', 2, int)
#define FBIOCGETDIM		_IOR('F', 3, int)
#define FBIOCSETSCRMODE		_IOW('F', 4, int)
#define FBIOCGETSCRMODE		_IOR('F', 5, int)
#define FBIOCAUTODIM		_IOW('F', 6, int)

#define FBIOCBITBLT 		_IOW('F', 10, sBitblt)
#define FBIOCBATCHBITBLT	_IOW('F', 11, sBatchBitblt)
#define FBIOCTILEBITBLT		_IOW('F', 12, sTileBitblt)
#define FBIOCBITBLT3		_IOW('F', 13, sBitblt3)

#define FBIOCPOLYLINE		_IOW('F', 20, sPrimLine)
#define FBIOCDJPOLYLINE		_IOW('F', 21, sPrimLine)
#define FBIOCPOLYMARKER		_IOW('F', 22, sPrimMarker)
#define FBIOCRECTANGLE		_IOW('F', 23, sPrimRect)
#define FBIOCFILLSCAN		_IOW('F', 24, sPrimFill)
#define FBIOCTEXT		_IOW('F', 25, sPrimText)
#define FBIOCPOLYDOT		_IOW('F', 26, sPrimDot)

#define FBIOCGETSCRTYPE		_IOR('F', 30, sScrType)

/* for color display */
#define FBIOCSETPALETTE		_IOW('F', 31, sPalette)
#define FBIOCGETPALETTE		_IOWR('F', 32, sPalette)
#define FBIOCSETDADDR		_IOW('F', 33, int)

#define FBIOCENABLE		_IO('F', 40)
#define FBIOCDISABLE		_IO('F', 41)

#define FBIOCSETCURSOR		_IOW('F', 42, sCursor)
#define FBIOCUNSETCURSOR	_IO('F', 43)
#define FBIOCSHOWCURSOR		_IO('F', 44)
#define FBIOCHIDECURSOR		_IO('F', 45)
#define FBIOCSETXY		_IOW('F', 46, sPoint)

#define FBIOCNBITBLT 		_IOW('F', 50, lBitblt)
#define FBIOCNBATCHBITBLT	_IOW('F', 51, lBatchBitblt)
#define FBIOCNTILEBITBLT	_IOW('F', 52, lTileBitblt)
#define FBIOCNBITBLT3		_IOW('F', 53, lBitblt3)

#define FBIOCNPOLYLINE		_IOW('F', 60, lPrimLine)
#define FBIOCNDJPOLYLINE	_IOW('F', 61, lPrimLine)
#define FBIOCNPOLYMARKER	_IOW('F', 62, lPrimMarker)
#define FBIOCNRECTANGLE		_IOW('F', 63, lPrimRect)
#define FBIOCNFILLSCAN		_IOW('F', 64, lPrimFill)
#define FBIOCNTEXT		_IOW('F', 65, lPrimText)
#define FBIOCNPOLYDOT		_IOW('F', 66, lPrimDot)

#define FBIOCNGETSCRTYPE	_IOR('F', 70, lScrType)
#define FBIOCNSETPALETTE	_IOW('F', 71, lPalette)
#define FBIOCNGETPALETTE	_IOWR('F', 72, lPalette)
#define FBIOCNSETPALETTEMODE	_IOW('F', 73, int)
#define FBIOCNGETPALETTEMODE	_IOR('F', 74, int)

#define FBIOCNSETCURSOR		_IOW('F', 80, lCursor)
#define FBIOCNUNSETCURSOR	_IO('F', 81)
#define FBIOCNSHOWCURSOR	_IO('F', 82)
#define FBIOCNHIDECURSOR	_IO('F', 83)
#define FBIOCNSETXY		_IOW('F', 84, lPoint)
#define FBIOCNSETCURSOR2	_IOW('F', 85, lCursor2)

#define FBIOCNSETVIDEO		_IOW('F', 90, lVideoCtl)
#define FBIOCNGETVIDEO		_IOR('F', 91, lVideoStatus)

#define FBIOCNIOCTL		_IOWR('F', 100, lFbIoctl)

#endif /* !__FRAMEBUF__ */
