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
 * from: $Hdr: fbdefs.h,v 4.300 91/06/09 06:33:54 root Rel41 $ SONY
 *
 *	@(#)fbdefs.h	8.1 (Berkeley) 6/11/93
 */

#ifndef mips
#define volatile
#endif

#ifdef CPU_SINGLE

#if defined(news3400) || defined(news3700)
#define splbitmap	spl3
#endif
#define	FB_LOCK		lock_bitmap()
#define FB_UNLOCK	unlock_bitmap()

#else /* CPU_SINGLE */

#undef PRE_EMPT
#define PRE_EMPT
#define FB_LOCK
#define FB_UNLOCK
#include <news3400/fb/fbsem.h> /* semaphore name */

#endif /* CPU_SINGLE */

#if defined(news3400) || defined(news3700)
#define WB_FLUSH	MachEmptyWriteBuffer()
#else
#define WB_FLUSH
#endif

/*
 * macro to compute modulo
 * 	MOD(-1, 16) --> 15
 *	DIV(-1, 16) --> -1
 */
#define MOD(x, d, t)	(((t)=(x)%(d))>=0?(t):(t)+(d))
#define TRUNC(x, d, t)	((x)-MOD(x,d,t))
#define DIV(x, d, t)	(((x)-MOD(x,d,t))/(d))

/* BitsPerWord should be 2^x */
#define MODWORD(x)	((x)&((BitsPerWord)-1))
#define TRUNCWORD(x)	((x)-MODWORD(x))
#define DIVWORD(x)	(((x)-MODWORD(x))/BitsPerWord)


#define AllOnes		0xffff

/* transparency */
#define TRANS(t,f)	((t) ? ((f) & 0x3 | 0x4) : (f))

/*
 *	color definitions
 */
#define MODE_1to1	0
#define MODE_1toN	1
#define MODE_Nto1	2
#define MODE_NtoN	3

#define PARALLEL	0
#define INTERNAL	1

#define NODRAW		0
#define DRAW		1

typedef struct _mergeRopBits {
	unsigned long	ca1, cx1, ca2, cx2;
} mergeRopRec;

extern mergeRopRec	mergeRopBits[];

#define DoRop(f, s, d, r) \
{  \
	switch(f) {  \
	case BF_0: \
		(r) = 0; break;  \
	case BF_SDA: \
		(r) = (s)&(d); break;  \
	case BF_SDIA: \
		(r) = (s)&~(d); break;  \
	case BF_S: \
		(r) = (s); break;  \
	case BF_SIDA: \
		(r) = ~(s)&(d); break;  \
	case BF_D: \
		(r) = (d); break;  \
	case BF_SDX: \
		(r) = (s)^(d); break;  \
	case BF_SDO: \
		(r) = (s)|(d); break;  \
	case BF_SDOI: \
		(r) = ~((s)|(d)); break;  \
	case BF_SDXI: \
		(r) = ~((s)^(d)); break;  \
	case BF_DI: \
		(r) = ~(d); break;  \
	case BF_SDIO: \
		(r) = (s)|~(d); break;  \
	case BF_SI: \
		(r) = ~(s); break;  \
	case BF_SIDO: \
		(r) = ~(s)|(d); break;  \
	case BF_SDAI: \
		(r) = ~((s)&(d)); break;  \
	case BF_1: \
		(r) = 0xffffffff; break;  \
	}  \
}

#define DoMergeRop(src, dst) \
	((dst) & ((src) & _ca1 ^ _cx1) ^ ((src) & _ca2 ^ _cx2))
#define DoMergeRopMask(src, dst, mask) \
	((dst) & (((src) & _ca1 ^ _cx1) | ~(mask)) ^ \
	(((src) & _ca2 ^ _cx2) & (mask)))
#define DoMergeFill(dst)	(((dst) & (rop_and)) ^ (rop_xor))
#define DoMergeFillMask(dst, mask) \
	(((dst) & ((rop_and) | ~(mask))) ^ ((rop_xor) & (mask)))
#define DoMultiRop(src, dst) \
{ \
	DoRop(funcs[0], src, dst, tmp0) \
	DoRop(funcs[1], src, dst, tmp1) \
	DoRop(funcs[2], src, dst, tmp2) \
	DoRop(funcs[3], src, dst, tmp3) \
	(dst) = tmp0 & masks[0] | tmp1 & masks[1] | \
	    tmp2 & masks[2] | tmp3 & masks[3]; \
}
#define DoMultiRopMask(src, dst, mask) \
{ \
	DoRop(funcs[0], src, dst, tmp0) \
	DoRop(funcs[1], src, dst, tmp1) \
	DoRop(funcs[2], src, dst, tmp2) \
	DoRop(funcs[3], src, dst, tmp3) \
	(dst) = dst & ~(mask) | \
	    (tmp0 & masks[0] | tmp1 & masks[1] | \
		tmp2 & masks[2] | tmp3 & masks[3]) & mask; \
}

#define TypeAt(m, p)		((m)->fm_vaddr + (p) - (m)->fm_offset)

#ifdef CPU_DOUBLE
# define _TypeAt(m, p)		((m)->fm_addr[(p)>>CLSHIFT] + (CLOFSET&(p)))
#else /* CPU_DOUBLE */
# define _TypeAt(m, p)		((m)->fm_vaddr + (p) - (m)->fm_offset)
#endif /* CPU_DOUBLE */

#define WordAt(m, p)		(*(Word *)_TypeAt(m, p))

/*
*	Device Dependent Structure
*/

#define	MAXPLANE	24

struct fbdev {
	struct fbdev_ops *fbbm_op;
	int		type;
	int		unit;
	lRectangle	FrameRect;
	lRectangle	VisRect;
	lRectangle	CursorRect;
	lRectangle	MaskRect;
	lRectangle	SaveRect;
	lBitmap		Krom_BM0;
	lBitmap		Krom_BM1;
	char *		Krom_base;
	lPoint		Krom_font_extent0;
	lPoint		Krom_font_extent1;
	char		funcvec[MAXPLANE];
	int		Mode;
	int		Pmask;
	unsigned int	pat;
	char		func;
	int		fore;
	int		aux;
	int		trans;
	int		cache_off;
	int		font_w;
	int		font_h;
	int		char_w;
	int		char_h;
	int		scr_w;
	int		scr_h;
	int		ch_pos;
	int		ul_pos;
	int		x_offset;
	int		y_offset;
	int		rit_m;
	int		btm_m;
	lRectangle	moveArea;
	lPoint		size;
	lPoint		hot;
	char		curfuncv[MAXPLANE];
	char		maskfuncv[MAXPLANE];
	int		cursorSet;
	int		cursorShow;
	int		cursorVis;
	lPoint		cursorP;
	char		*rcont_base;
	char		*rcont_reg;
	int		fbNplane;
	int		Colorwidth;
	int		planemask;
	int		Mono;
	int		Dimmer;
	int		DispVer;
	unsigned short	status_flag;
	unsigned short	run_flag;
	char		*private;
	int		hard_cursor;
};

#ifdef CPU_SINGLE
struct mfbdev {
	char		*vram_start;
	int		vram_width;
};
#endif /* CPU_SINGLE */

struct fbdev_ops {
	void	(*fb_rop_init)();
	void	(*fb_rop_copy)();
	void	(*fb_rop_winit)();
	void	(*fb_rop_write)();
	void	(*fb_rop_read)();
	void	(*fb_rop_cinit)();
	void	(*fb_rop_clear)();
	void	(*fb_rop_vect)();
	void	(*fb_rop_dot)();
	void	(*fb_rop_fillscan)();
	void	(*fb_rop_wait)();
	void	(*fb_rop_reset)();
	char 	*(*fb_Krom_addr)();
	void	(*fb_init_palette)();
	int	(*fb_set_palette)();
	int	(*fb_get_palette)();
	int	(*fb_get_pixel)();
	int	(*fb_set_dimmer)();
	int	(*fb_get_dimmer)();
	int	(*fb_open)();
	int	(*fb_close)();
	int	(*fb_ioctl)();
	int	(*fb_get_page)();
	void	(*fb_cursor_set)();
	void	(*fb_cursor_on)();
	void	(*fb_cursor_off)();
	void	(*fb_cursor_move)();
};

struct autodev {
	int	type;
	char	*base;
	char	*reg;
};

struct fbdevsw {
	int	num;
	int	(*fb_probe)();
	void	(*fb_setup)();
};

extern struct autodev	autodev[];
extern struct fbdev	fbdev[];
extern int		nfbdev;
extern struct fbdev	*consfb;
extern unsigned short	fb_color_pallet_def[];
extern unsigned short	fb_gray_pallet_def[];

/*	fb_ioctl Command	*/

#define	FB_INTCHECK		1
#define	FB_INTCLEAR		2
#define	FB_INTENABLE		3
#define	FB_STATUSCHECK		4

#define FB_SETVIDEOCTL		10
#define FB_GETVIDEOSTATUS	11
#define FB_SETPALETTEMODE	12
#define FB_GETPALETTEMODE	13

#define	FB_INT_VSYNC		1
#define	FB_INT_ROPDONE		2

#define	FB_STATUS_ROPEXEC	4
#define	FB_STATUS_ROPWAIT	8
#define	FB_STATUS_ROPVSYNC	16

/*	run_flag	*/

#define	FB_ACTIVE		1
#define	FB_WAITING		2
#define	FB_DONE			4

/*
*	Pseudo Frame Buffer 
*	
*	unit no
*	   0		Console Device and old Interface (/dev/fb)
*	   1		B/W Display and New Interface	(/dev/mfb)
*	   2		Color Display and New Interface	(/dev/cfb)
*
*/

#define	fbbm_rop_init(fb, func) \
	(*(fb)->fbbm_op->fb_rop_init)(fb, func)

#define	fbbm_rop_copy(fb, s, d, rp, wp) \
	(*(fb)->fbbm_op->fb_rop_copy)(fb, s, d, rp, wp)

#define	fbbm_rop_winit(fb) \
	(*(fb)->fbbm_op->fb_rop_winit)(fb)

#define	fbbm_rop_write(fb, map, p, width, srp, drp, wplane) \
	(*(fb)->fbbm_op->fb_rop_write)(fb, map, p, width, srp, drp, wplane) 

#define	fbbm_rop_read(fb, map, p, width, srp, drp, rplane, wplane) \
	(*(fb)->fbbm_op->fb_rop_read)(fb, map, p, width, srp, drp, rplane, wplane)

#define	fbbm_rop_cinit(fb, wplane, bw) \
	(*(fb)->fbbm_op->fb_rop_cinit)(fb, wplane, bw)

#define	fbbm_rop_clear(fb, dr) \
	(*(fb)->fbbm_op->fb_rop_clear)(fb, dr)

#define	fbbm_rop_vect(fb, dr, func, forc, auxc, transp, wplane, n, p, lptn, lpf, sw) \
	(*(fb)->fbbm_op->fb_rop_vect)(fb, dr, func, forc, auxc, transp, wplane, n, p, lptn, lpf, sw)

#define	fbbm_rop_dot(fb, dr, func, forc, auxc, transp, wplane, n, p) \
	(*(fb)->fbbm_op->fb_rop_dot)(fb, dr, func, forc, auxc, transp, wplane, n, p)

#define fbbm_rop_fillscan(fb, ls, ns, clip, sw) \
	(*(fb)->fbbm_op->fb_rop_fillscan)(fb, ls, ns, clip, sw)

#define fbbm_Krom_addr(fb, c, sr) \
	(*(fb)->fbbm_op->fb_Krom_addr)(fb, c, sr)

#define fbbm_rop_wait(fb) \
	(*(fb)->fbbm_op->fb_rop_wait)(fb)
#define fbbm_rop_reset(fb) \
	(*(fb)->fbbm_op->fb_rop_reset)(fb)

#define fbbm_init_palette(fb) \
	(*(fb)->fbbm_op->fb_init_palette)(fb)
#define fbbm_set_palette(fb, palette) \
	(*(fb)->fbbm_op->fb_set_palette)(fb, palette)
#define fbbm_get_palette(fb, palette) \
	(*(fb)->fbbm_op->fb_get_palette)(fb, palette)
#define fbbm_get_pixel(fb, pixel) \
	(*(fb)->fbbm_op->fb_get_pixel)(fb, pixel)

#define fbbm_set_dimmer(fb, n) \
	(*(fb)->fbbm_op->fb_set_dimmer)(fb, n)
#define fbbm_get_dimmer(fb) \
	(*(fb)->fbbm_op->fb_get_dimmer)(fb)

#define fbbm_open(fb) \
	(*(fb)->fbbm_op->fb_open)(fb)
#define fbbm_close(fb) \
	(*(fb)->fbbm_op->fb_close)(fb)
#define fbbm_ioctl(fb, cmd, data) \
	(*(fb)->fbbm_op->fb_ioctl)(fb, cmd, data)

#define fbbm_get_page(fb, off) \
	(*(fb)->fbbm_op->fb_get_page)(fb, off)

#define fbbm_cursor_set(fb, fc, bc) \
	(*(fb)->fbbm_op->fb_cursor_set)(fb, fc, bc)
#define fbbm_cursor_on(fb) \
	(*(fb)->fbbm_op->fb_cursor_on)(fb)
#define fbbm_cursor_off(fb) \
	(*(fb)->fbbm_op->fb_cursor_off)(fb)
#define fbbm_cursor_move(fb) \
	(*(fb)->fbbm_op->fb_cursor_move)(fb)

extern unsigned short mfbstarttab16[16];
extern unsigned short mfbendtab16[16];
extern unsigned short mfbpartmasks16[16][16];
extern unsigned int mfbstarttab32[32];
extern unsigned int mfbendtab32[32];
extern unsigned int mfbpartmasks32[32][32];
extern void mfb_copy_area32();
extern void mfb_copyinv_area32();
extern void mfb_or_area32();
extern void mfb_xor_area32();
extern void mfb_general_area32();
extern void mfb_clr_area32();
extern void mfb_inv_area32();
extern void mfb_set_area32();
extern void mfb_copy_area16();
extern void mfb_copyinv_area16();
extern void mfb_or_area16();
extern void mfb_xor_area16();
extern void mfb_general_area16();
extern void mfb_clr_area16();
extern void mfb_inv_area16();
extern void mfb_set_area16();
extern void mfb_clrvvector32();
extern void mfb_clrhvector32();
extern void mfb_clrvector32();
extern void mfb_invvvector32();
extern void mfb_invhvector32();
extern void mfb_invvector32();
extern void mfb_setvvector32();
extern void mfb_sethvector32();
extern void mfb_setvector32();
extern void mfb_point();
extern void mfb_vector32();
