/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 * from: $Hdr: fbbm_253.c,v 4.300 91/06/09 06:33:12 root Rel41 $ SONY; */
 *
 *	@(#)fbbm_253.c	7.2 (Berkeley) %G%
 */

/*
 * NWB-253 frame buffer driver
 */

#include <machine/fix_machine_type.h>

#include "nwb253.h"

#if NNWB253 > 0
#include <sys/param.h>
#include <news3400/iop/framebuf.h>
#include <news3400/iop/fbreg.h>

#include <news3400/fb/fbdefs.h>

extern int error();
extern int nofunc();
extern char *ext_fnt24_addr[];
extern char *ext_fnt_addr[];

extern short zero[];

#define NOP		{ int j; for (j = 0; j < 40; j++); }
	
#define	VRAM_START	(unsigned int *)(0x88000000)
#define VRAM_WIDTH	(2048/32)

static caddr_t
fb253_Krom_addr(fb, c, sr)
	struct fbdev *fb;
	register int c;
	lRectangle *sr;
{
	unsigned int cvcode16();

	if ((c >= 0x20) && (c <= 0x7e)) {
		/*
		 * ASCII char
		 */
		c -= ' ';
		c = ((c & 0x1f) | ((c & 0xe0) << 2)) << 7;
		return (caddr_t)(c + fb->Krom_base + (sr->extent.y > 16 ? 0 : 96));
	} else if ((c >= 0xa1) && (c <= 0xdf)) {
		/*
		 * KANA char
		 */
		if (sr->extent.y > 16)
			return ((caddr_t)ext_fnt24_addr[c + 64]);
		else
			return ((caddr_t)ext_fnt_addr[c + 64]);
	} else if ((c >= 0x2020) && (c <= 0x7e7e)) {
		/*
		 * KANJI char
		 */
		switch (c & 0x7000) {
		case 0x2000:
			c = ((c&0x1f)|((c&0x60)<<5)|((c&0x700)>>1))<<7;
			break;
		case 0x3000:
		case 0x4000:
			c = ((c&0x7f)|((c&0xf00)>>1)|((c&0x4000)>>3))<<7;
			break;
		case 0x5000:
		case 0x6000:
			c = ((c&0x7f)|((c&0xf00)>>1)|((c&0x2000)>>2)|0x1000)<<7;
			break;
		case 0x7000:
			c = ((c&0x1f)|((c&0x60)<<5)|((c&0x700)>>1)|0x1000)<<7;
			break;
		}
		return (caddr_t)(c + fb->Krom_base + (sr->extent.y > 16 ? 0 : 96));
	} else {
		/*
		 * UNKNOWN char
		 */
		return (caddr_t)zero;
	}
}

static int
fb253_set_dimmer(fb, dim)
	struct fbdev *fb;
	int dim;
{
	int s;

	fb->status_flag = (fb->status_flag & 0xf3) | ((dim & 3) << 2);
	*(volatile u_short *)(0xb8ff0000) = fb->status_flag;

	return (FB_ROK);
}

static int
fb253_get_dimmer(fb)
	struct fbdev *fb;
{
	int dim;

	dim = (fb->status_flag >> 2) & 3;
	return (dim);
}

int
fb253_get_pixel(fb, pixel)
	struct fbdev *fb;
	register int pixel;
{
	return (pixel);
}

fb253_ioctl(fb, cmd, data)
	struct fbdev *fb;
	int cmd;
	int *data;
{
	register int result = 0;

	switch (cmd) {
	case FB_INTCHECK:
		if (*(volatile u_short *)(0xb8ff0000) & 0x08)
			result |= FB_INT_VSYNC;
		break;
	case FB_INTENABLE:
		if (*data & FB_INT_VSYNC) {
			fb->status_flag |= 1;
			*(volatile u_short *)(0xb8ff0000) = fb->status_flag;
			WB_FLUSH;
		}
		break;
	case FB_INTCLEAR:
		if (*data & FB_INT_VSYNC) {
			fb->status_flag &= ~1;
			*(volatile u_short *)(0xb8ff0000) = fb->status_flag;
			WB_FLUSH;
		}
		break;
	case FB_STATUSCHECK:
		break;
	default:
		result = -1;
	}
	return (result);
}

int
fb253_get_page(fb, off)
	struct fbdev *fb;
	off_t off;
{
	if (off < 2048/8 * 2048)		/* X/8 * Y */
		return (((unsigned int)0x8000000 + off) >> PGSHIFT);
	else
		return (-1);
}

int
fb253_probe(unit)
	int unit;
{
	if (unit >= NNWB253)
		return 0;
	if (badaddr(0xb8ff0000, 2) || badaddr(0xb8e00000, 2))
		return 0;
	if ((*(volatile u_short *)(0xb8ff0000) & 7) == 4)
		return FB_NWB253;
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

static struct fbdev_ops
fb253_ops = {
	fbmem_rop_init,
	fbmem_rop_copy,
	fbmem_rop_winit,
	fbmem_rop_write,
	fbmem_rop_read,
	fbmem_rop_cinit,
	fbmem_rop_clear,
	fbmem_rop_vect,
	fbmem_rop_dot,
	(void (*)())nofunc,
	(void (*)())error,
	(void (*)())error,
	fb253_Krom_addr,
	(void (*)())error,
	error,
	error,
	fb253_get_pixel,
	fb253_set_dimmer,
	fb253_get_dimmer,
	nofunc,
	nofunc,
	fb253_ioctl,
	fb253_get_page,
	(void (*)())nofunc,
	(void (*)())nofunc,
	(void (*)())nofunc,
	(void (*)())nofunc,
};

static u_short
nwp512_data1[] = {
	0x00, 0x44,
	0x01, 0x33,
	0x02, 0x3c,
	0x03, 0x38,
	0x04, 0x84,
	0x05, 0x03,
	0x06, 0x80,
	0x07, 0x80,
	0x08, 0x10,
	0x09, 0x07,
	0x0a, 0x20,
	0x0c, 0x00,
	0x0d, 0x00,
	0x1b, 0x03
};

static u_short
nwp512_data2[] = {
	0x1e, 0x08,
	0x20, 0x08,
	0x21, 0x0d
};

static u_short
nwp518_data1[] = {
	0x00, 0x52,
	0x01, 0x40,
	0x02, 0x4a,
	0x03, 0x49,
	0x04, 0x63,
	0x05, 0x02,
	0x06, 0x60,
	0x07, 0x60,
	0x08, 0x10,
	0x09, 0x07,
	0x0a, 0x20,
	0x0c, 0x00,
	0x0d, 0x00,
	0x1b, 0x04
};

static u_short
nwp518_data2[] = {
	0x1e, 0x08,
	0x20, 0x00,
	0x21, 0x00
};

static u_short
nwe501_data1[] = {
	0x00, 0x4b,
	0x01, 0x40,
	0x02, 0x4a,
	0x03, 0x43,
	0x04, 0x64,
	0x05, 0x02,
	0x06, 0x60,
	0x07, 0x60,
	0x08, 0x10,
	0x09, 0x07,
	0x0a, 0x20,
	0x0c, 0x00,
	0x0d, 0x00,
	0x1b, 0x04
};

static u_short
nwe501_data2[] = {
	0x1e, 0x08,
	0x20, 0x00,
	0x21, 0x00
};

static u_short
*crtc_data[3][2] = {
	nwp512_data1, nwp512_data2,
	nwp518_data1, nwp518_data2,
	nwe501_data1, nwe501_data2,
};

static void
fb253_init(fb, id)
	struct fbdev *fb;
	int id;
{
	register int i;
	register volatile u_short *ctlreg = (u_short *)(0xb8ff0000);
	register volatile u_short *crtreg = (u_short *)(0xb8fe0000);
	register volatile u_short *p;
	u_short	dummy;

	*ctlreg = 0;			/* stop crtc */
	NOP;

	/* initialize crtc without R3{0,1,2} */
	p = crtc_data[id][0];
	for (i = 0; i < 28; i++) {
		*crtreg++ = *p++;
		NOP;
	}

	*ctlreg = 0x02;			/* start crtc */
	NOP;

	/* set crtc control reg */
	p = crtc_data[id][1];
	for (i = 0; i < 6; i++) {
		*crtreg++ = *p++;
		NOP;
	}
}

struct mfbdev fb253 = { (caddr_t)VRAM_START, VRAM_WIDTH };

void
fb253_setup(fb)
	struct fbdev *fb;
{
	int id;

	if (fb->type) {
		fb->Mono = 1;
		fb->Colorwidth = 1;
		fb->fbNplane = 1;
		fb->planemask = 0x1;
		fb->Dimmer = 1;

		id = ((*(volatile u_short *)(0xb8ff0000)) >> 8) & 0xf;

		switch (id) {
		case 0:
			fb->FrameRect.extent.x = 2048;
			fb->FrameRect.extent.y = 2048;
			fb->VisRect.extent.x = 816;
			fb->VisRect.extent.y = 1024;
			fb->cursorP.x = 816/2;
			fb->cursorP.y = 1024/2;
			fb->font_w = 8;
			fb->font_h = 16;
			fb->char_w = 10;
			fb->char_h = 24;
			fb->scr_w = 816;
			fb->scr_h = 1024;
			fb->ch_pos = 5;
			fb->ul_pos = 22;
			fb->x_offset = 8;
			fb->y_offset = 8;
			fb->rit_m = 80;
			fb->btm_m = 42;
			break;
		case 1:
		case 2:
			fb->FrameRect.extent.x = 2048;
			fb->FrameRect.extent.y = 2048;
			fb->VisRect.extent.x = 1024;
			fb->VisRect.extent.y = 768;
			fb->cursorP.x = 1024/2;
			fb->cursorP.y = 768/2;
			fb->font_w = 8;
			fb->font_h = 16;
			fb->char_w = 12;
			fb->char_h = 22;
			fb->scr_w = 1024;
			fb->scr_h = 768;
			fb->ch_pos = 2;
			fb->ul_pos = 20;
			fb->x_offset = 32;
			fb->y_offset = 21;
			fb->rit_m = 80;
			fb->btm_m = 33;
			break;
		}
		fb->FrameRect.origin.x = 0;
		fb->FrameRect.origin.y = 0;
		fb->VisRect.origin.x = 0;
		fb->VisRect.origin.y = 0;
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
		fb->Krom_BM1.rect.extent.x = 24;
		fb->Krom_BM1.rect.extent.y = 24;
		fb->Krom_base = (char *)(0xb8e00000);
		fb->Krom_font_extent0.x = 16;
		fb->Krom_font_extent0.y = 16;
		fb->Krom_font_extent1.x = 24;
		fb->Krom_font_extent1.y = 24;
		fb->cursorSet = 0;
		fb->cursorVis = 0;
		fb->cursorShow = 0;
		fb->status_flag = 2;
		fb->run_flag = 0;
		fb->hard_cursor = 0;

		fb->fbbm_op = &fb253_ops;

		fb->private = (caddr_t)&fb253;

		fb253_init(fb, id);
	}
}
#endif /* NNWB253 > 0 */
