/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 * from: $Hdr: fbbm_lcdm.c,v 4.300 91/06/09 06:33:38 root Rel41 $ SONY
 *
 *	@(#)fbbm_lcdm.c	7.4 (Berkeley) %G%
 */

/*
 * LCD frame buffer driver
 */

#include "lfbm.h"

#if NLFBM > 0

#include <sys/param.h>
#include <news3400/iop/framebuf.h>
#include <news3400/iop/fbreg.h>

#include <news3400/fb/fbdefs.h>

extern int nofunc();
extern int error();
extern char *ext_fnt24_addr[];
extern char *ext_fnt_addr[];

extern short zero[];

extern unsigned int mfbmask32[];
extern unsigned int mfbrmask32[];

#define NOP	for (j = 0; j < 40; j++)
	
#ifdef news3200
#define LCD_PORT	(volatile unsigned long *)(0xb0000000)
#define DIMMER_PORT	(volatile unsigned long *)(0xb0100000)
#define LCD_CRTC	(volatile unsigned char *)(0xbff60000)
#define	KROM_START	(char *)(0x90000000)
#define	VRAM_START	(unsigned long *)(0x90200000)
#define VRAM_WIDTH	(1120/32)
#endif

void
lfbmattach(i)
{
	/* temporary hack for pseudo-device initialization */;
}

#ifdef news3200
static caddr_t
fblfbm_Krom_addr(fb, c, sr)
	struct fbdev *fb;
	register int c;
	lRectangle *sr;
{
	register int i;
	register u_short *tmp;
	static int tmpfnt[24];

	if ((c >= 0x20) && (c <= 0x7e)) {
		/*
		 * ASCII char
		 */
		c -= ' ';
		c = ((c & 0x1f) | ((c & 0xe0) << 2)) << 7;
		tmp = (u_short *)(fb->Krom_base + (c + (sr->extent.y > 16 ? 0 : 96)) * 2);
	} else if ((c >= 0xa1) && (c <= 0xdf)) {
		/*
		 * KANA char
		 */
		if (sr->extent.y > 16)
			tmp =  (u_short *)ext_fnt24_addr[c + 64];
		else
			tmp =  (u_short *)ext_fnt_addr[c + 64];
	} else if ((c >= 0x2020) && (c <= 0x7e7e)) {
		/*
		 * KANJI char
		 */
		switch (c & 0x7000) {
		case 0x2000:
			c = ((c & 0x1f) | ((c & 0x60) << 5) | ((c & 0x700) >> 1)) << 7;
			break;
		case 0x3000:
		case 0x4000:
			c = ((c & 0x7f) | ((c & 0xf00) >> 1) | ((c & 0x4000) >> 3)) << 7;
			break;
		case 0x5000:
		case 0x6000:
			c = ((c & 0x7f) | ((c & 0xf00) >> 1) | ((c & 0x2000) >> 2)
				| 0x1000) << 7;
			break;
		case 0x7000:
			c = ((c & 0x1f) | ((c & 0x60) << 5) | ((c & 0x700) >> 1)
				| 0x1000) << 7;
			break;
		}
		tmp = (u_short *)(fb->Krom_base + (c + (sr->extent.y > 16 ? 0 : 96)) * 2);
	} else {
		/*
		 * UNKNOWN char
		 */
		return ((caddr_t)zero);
	}

	if (sr->extent.y > 16) {
		for (i = 0; i < 24; i++) {
			tmpfnt[i] = (*tmp << 16) | *(tmp + 2);
			tmp += 4;
		}
	} else {
		for (i = 0; i < 16; i++) {
			tmpfnt[i] = (*tmp << 16) | *(tmp + 2);
			tmp += 4;
		}
	}

	return ((caddr_t)tmpfnt);
}
#endif /* news3200 */

static int
fblfbm_set_dimmer(fb, dim)
	struct fbdev *fb;
	int dim;
{
	int s;

	fb->status_flag = dim ? 0xf1: 0xf0;
#ifdef news3200
	*DIMMER_PORT = fb->status_flag;
#endif
	return (FB_ROK);
}

static int
fblfbm_get_dimmer(fb)
	struct fbdev *fb;
{
	return (fb->status_flag & 0x1);
}

int
fblfbm_get_pixel(fb, pixel)
	struct fbdev *fb;
	register int pixel;
{
	return (pixel);
}

int
fblfbm_get_page(fb, off)
	struct fbdev *fb;
	off_t off;
{
	if (off < 1120/8 * 930) {		/* X/8 * Y */
#ifdef news3200
		return (((unsigned int)0x10200000 + off) >> PGSHIFT);
#endif
	} else
		return (-1);
}

int
fblfbm_probe(unit)
	int unit;
{
#ifdef news3200
	if (unit < NLFBM) {
		if (badaddr(0xbff50000, 1))
			return 0;
		else {
			if (*(volatile u_char *)(0xbff50000) == 0xff)
				return FB_LCDM;
			else
				return 0;
		}
	}
#endif /* news3200 */
	return 0;
}

void fbmem_rop_init();
void fbmem_rop_copy();
void fbmem_rop_winit();
void fbmem_rop_write();
void fbmem_rop_read();
void fbmem_rop_cinit();
void fbmem_rop_clear();
void fbmem_rop_vect();
void fbmem_rop_dot();

struct fbdev_ops fblfbm_ops = {
	fbmem_rop_init,			/* (*fb_rop_init)() */
	fbmem_rop_copy,			/* (*fb_rop_copy)() */
	fbmem_rop_winit,		/* (*fb_rop_winit)() */
	fbmem_rop_write,		/* (*fb_rop_write)() */
	fbmem_rop_read,			/* (*fb_rop_read)() */
	fbmem_rop_cinit,		/* (*fb_rop_cinit)() */
	fbmem_rop_clear,		/* (*fb_rop_clear)() */
	fbmem_rop_vect,			/* (*fb_rop_vect)() */
	fbmem_rop_dot,			/* (*fb_rop_dot)() */
	(void (*)())nofunc,		/* (*fb_rop_fillscan)() */
	(void (*)())error,		/* (*fb_rop_wait)() */
	(void (*)())error,		/* (*fb_rop_reset)() */
	fblfbm_Krom_addr,		/* *(*fb_Krom_addr)() */
	(void (*)())error,		/* (*fb_init_palette)() */
	error,				/* (*fb_set_palette)() */
	error,				/* (*fb_get_palette)() */
	fblfbm_get_pixel,		/* (*fb_get_pixel)() */
	fblfbm_set_dimmer,		/* (*fb_set_dimmer)() */
	fblfbm_get_dimmer,		/* (*fb_get_dimmer)() */
	nofunc,				/* (*fb_open)() */
	nofunc,				/* (*fb_close)() */
	nofunc,				/* (*fb_ioctl)() */
	fblfbm_get_page,		/* (*fb_get_page)() */
	(void (*)())nofunc,		/* (*fb_cursor_set)() */
	(void (*)())nofunc,		/* (*fb_cursor_on)() */
	(void (*)())nofunc,		/* (*fb_cursor_off)() */
	(void (*)())nofunc,		/* (*fb_cursor_move)() */
};

static char lctc_data[] = {
	0, 47,
	1, 35,
	9, 0,
	10, 0,
	11, 0,
	12, 0,
	13, 0,
	14, 0,
	15, 0,
	18, 35,
	19, 0x01,
	20, 0x85,
	21, 0,
	22, 0x10
};

struct mfbdev fblcd = { (caddr_t)VRAM_START, VRAM_WIDTH };

void
fblfbm_setup(fb)
	register struct fbdev *fb;
{
	register int i;
	register volatile unsigned char *lctreg = LCD_CRTC;

	if (fb->type) {
		fb->Mono = 1;
		fb->Colorwidth = 1;
		fb->fbNplane = 1;
		fb->planemask = 0x1;
		fb->Dimmer = 1;

		fb->FrameRect.origin.x = 0;
		fb->FrameRect.origin.y = 0;
		fb->FrameRect.extent.x = 1120;
		fb->FrameRect.extent.y = 930;
		fb->VisRect.origin.x = 0;
		fb->VisRect.origin.y = 0;
		fb->VisRect.extent.x = 1120;
		fb->VisRect.extent.y = 780;
		fb->Krom_BM0.type = BM_MEM;
		fb->Krom_BM0.depth = 1;
		fb->Krom_BM0.width = 1;
		fb->Krom_BM0.rect.origin.x = 0;
		fb->Krom_BM0.rect.origin.y = 0;
		fb->Krom_BM0.rect.extent.x = 16;
		fb->Krom_BM0.rect.extent.y = 16;
		fb->Krom_BM1.type = BM_MEM;
		fb->Krom_BM1.depth = 1;
		fb->Krom_BM1.width = 2;
		fb->Krom_BM1.rect.origin.x = 0;
		fb->Krom_BM1.rect.origin.y = 0;
		fb->Krom_BM1.rect.extent.x = 32;
		fb->Krom_BM1.rect.extent.y = 32;
		fb->Krom_base = KROM_START;
		fb->Krom_font_extent0.x = 16;
		fb->Krom_font_extent0.y = 16;
		fb->Krom_font_extent1.x = 24;
		fb->Krom_font_extent1.y = 24;
		fb->font_w = 12;
		fb->font_h = 24;
		fb->char_w = 12;
		fb->char_h = 26;
		fb->scr_w = 1120;
		fb->scr_h = 780;
		fb->ch_pos = 0;
		fb->ul_pos = 24;
		fb->x_offset = (1120 - (80 * 12)) / 2;
		fb->y_offset = ( 780 - (29 * 26)) / 2;
		fb->rit_m = 80;
		fb->btm_m = 29;
		fb->cursorSet = 0;
		fb->cursorVis = 0;
		fb->cursorShow = 0;
		fb->cursorP.x = 1120/2;
		fb->cursorP.y = 780/2;
		fb->status_flag = 1;
		fb->run_flag = 0;
		fb->hard_cursor = 0;

		fb->fbbm_op = &fblfbm_ops;

		fb->private = (caddr_t)&fblcd;

		for (i = 0; i < 28; i++)
			*lctreg++ = lctc_data[i];
		for (i = 0; i < 500000; i ++)
			;
		*LCD_PORT = 1;
	}
}
#endif /* NLFBM > 0 */
