/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratory.
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
 *	@(#)fbvar.h	8.1 (Berkeley) 6/11/93
 *
 * from: $Header: fbvar.h,v 1.15 92/11/26 02:04:34 torek Exp $
 */

/*
 * Frame buffer variables.  All frame buffer drivers must provide the
 * following in order to participate.
 */
struct fbdriver {
	/* device unblank function (force kernel output to display) */
	void	(*fbd_unblank)(struct device *);
#ifdef notyet
	void	(*fbd_wrrop)();		/* `write region' rasterop */
	void	(*fbd_cprop)();		/* `copy region' rasterop */
	void	(*fbd_clrop)();		/* `clear region' rasterop */
#endif
};

struct fbdevice {
	int	fb_major;		/* XXX */
	struct	fbtype fb_type;		/* what it says */
	caddr_t	fb_pixels;		/* display RAM */
	int	fb_linebytes;		/* bytes per display line */

	struct	fbdriver *fb_driver;	/* pointer to driver */
	struct	device *fb_device;	/* parameter for fbd_unblank */

	/* Raster console emulator state */
	u_int	fb_bits;		/* see defines below */
	int	fb_ringing;		/* bell currently ringing */
	int	fb_belldepth;		/* audible bell depth */
	int	fb_scroll;		/* stupid sun scroll mode */

	int	fb_p0;			/* escape sequence parameter 0 */
	int	fb_p1;			/* escape sequence parameter 1 */

	int	*fb_row;		/* emulator row */
	int	*fb_col;		/* emulator column */

	int	fb_maxrow;		/* emulator height of screen */
	int	fb_maxcol;		/* emulator width of screen */

	int	fb_emuwidth;		/* emulator screen width  */
	int	fb_emuheight;		/* emulator screen height */

	int	fb_xorigin;		/* x origin for first column */
	int	fb_yorigin;		/* y origin for first row */

	struct	raster *fb_sp;		/* frame buffer raster */
	struct	raster *fb_cursor;	/* optional cursor */
	int	fb_ras_blank;		/* current screen blank raster op */

	struct	raster_font *fb_font;	/* font and related info */
	int	fb_font_ascent;		/* distance from font to char origin */
};

#define FB_INESC	0x001		/* processing an escape sequence */
#define FB_STANDOUT	0x002		/* standout mode */
/* #define FB_BOLD	0x?		/* boldface mode */
#define FB_INVERT	0x008		/* white on black mode */
#define FB_VISBELL	0x010		/* visual bell */
#define FB_CURSOR	0x020		/* cursor is visible */
#define FB_P0_DEFAULT	0x100		/* param 0 is defaulted */
#define FB_P1_DEFAULT	0x200		/* param 1 is defaulted */
#define FB_P0		0x400		/* working on param 0 */
#define FB_P1		0x800		/* working on param 1 */

void	fbattach __P((struct fbdevice *));
