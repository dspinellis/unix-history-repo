/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 * from: $Hdr: bitmapif.c,v 4.300 91/06/09 06:14:40 root Rel41 $ SONY
 *
 *	@(#)bitmapif.c	7.3 (Berkeley) %G%
 */

#ifdef IPC_MRX
#include <sys/param.h>
#include <sys/types.h>
#include <news3400/iop/framebuf.h>
#include <news3400/iop/fbreg.h>
#else
#include <sys/param.h>
#include <sys/types.h>
#include <news3400/iop/framebuf.h>
#include <news3400/iop/fbreg.h>
#endif

#include <news3400/fb/fbdefs.h>

#include <news3400/bm/vt100.h>
#include <news3400/bm/bitmapif.h>

extern int bm_todo;
extern int tmode;

#ifdef CPU_SINGLE
extern char *ext_fnt_addr[];
extern char *ext_fnt24_addr[];
#else /* CPU_SINGLE */
extern char **ext_fnt_addr;
extern char **ext_fnt24_addr;
#define lock_bitmap()
#define unlock_bitmap()
#endif /* CPU_SINGLE */

extern	SCREEN	screen;

#ifdef IPC_MRX
extern int bitmap_use;
#endif

#ifdef CPU_SINGLE
#include <machine/cpu.h>
#define PRE_EMPT	need_resched()
#else
#define	PRE_EMPT
#endif

short zero[32 * 2];
struct	csr_buf local_csr_buf;

#ifdef CPU_SINGLE
struct fb_map rommap;
#endif

bitmapinit()
{
	fbbm_rop_reset(consfb);
	if (!consfb->Mono) {
		lock_bitmap();
		fbbm_init_palette(consfb);
		unlock_bitmap();
	}
}

bm_pallet_read(entry)
	int entry;
{
	lPalette lp;
	sPalette palette;
#ifdef CPU_SINGLE
	struct fb_map rommap;
#endif

	lock_bitmap();

	lp.count = 1;
#ifdef CPU_SINGLE
	lp.palette = (sPalette *)&rommap;
	rommap.fm_vaddr = (caddr_t)&palette;
	rommap.fm_offset = 0;
#else
	lp.palette = &palette;
#endif
	palette.index = entry;

	fbbm_get_palette(consfb, &lp);

	unlock_bitmap();

	return((palette.rgb.r << 16) | (palette.rgb.g << 8) | palette.rgb.b);
}

bm_pallet_write(entry, val)
	int entry;
	int val;
{
	lPalette lp;
	sPalette palette;
#ifdef CPU_SINGLE
	struct fb_map rommap;
#endif

	lock_bitmap();

	lp.count = 1;
#ifdef CPU_SINGLE
	lp.palette = (sPalette *)&rommap;
	rommap.fm_vaddr = (caddr_t)&palette;
	rommap.fm_offset = 0;
#else
	lp.palette = &palette;
#endif
	palette.index = entry;
	palette.rgb.r = ((val >> 16) & 0xff);
	palette.rgb.g = ((val >> 8) & 0xff);
	palette.rgb.b = (val & 0xff);
	fbbm_set_palette(consfb, &lp);

	unlock_bitmap();

	return(val);
}

unsigned
sftjis_to_jis(h, l)
	register unsigned int h, l;
{
	if ((h >= JVR1S) && (h <= JVR1E))
		h -= JVR1S;
	else if ((h >= JVR2S) && (h <= JVR2E))
		h = h - JVR2S + 0x1f;
	else
		return (0);

	h <<= 1;

	if ((l >= JHR1S) && (l <= JHR1E))
		l -= JHR1S;
	else if ((l >= JHR2S) && (l <= JHR2E))
		l = l - JHR2S + 0x3f;
	else if ((l >= JHR3S) && (l <= JHR3E)) {
		l -= JHR3S;
		h++;
	} else
		return (0);

	return(((h + 0x21) << 8) + l + 0x21);
}

setropfunc(func, transp, fore, aux)
	int func, fore, aux;
{
	char tmp[4];
	register int i = consfb->fbNplane;
	register char *funcp = consfb->funcvec;

	consfb->func = func;
	consfb->fore = fore;
	consfb->aux = aux;
	consfb->trans = transp;

	tmp[0] = TRANS(transp, (func & 0x0c) | (func>>2));
	tmp[1] = TRANS(transp, (func>>2) | ((func<<2) & 0x0c));
	tmp[2] = TRANS(transp, func);
	tmp[3] = TRANS(transp, (func<<2) & 0x0c | func & 3);

	while (--i >= 0) {
		*funcp++ = tmp[((fore & 1) << 1) | (aux & 1)];
		fore >>= 1; aux >>= 1;
	}
}

move_rect(src_x, src_y, width, height, dst_x, dst_y, rop)
	int src_x, src_y;
	int width, height;
	int dst_x, dst_y;
	int rop;
{
	lRectangle sr;
	lPoint dp;
		
	sr.origin.x = src_x;
	sr.origin.y = src_y;
	sr.extent.x = width;
	sr.extent.y = height;

	dp.x = dst_x;
	dp.y = dst_y;

	lock_bitmap();
	setropfunc(rop, 0, consfb->planemask, 0);
	fbbm_rop_init(consfb, consfb->funcvec);
	fbbm_rop_copy(consfb, &sr, &dp, 0, consfb->planemask);
	unlock_bitmap();
}

clear_rect(x, y, width, height, rop, fore, aux)
	int x, y;
	int width, height;
	int rop;
	int fore;
	int aux;
{
	lRectangle dr;

	dr.origin.x = x;
	dr.origin.y = y;
	dr.extent.x = width;
	dr.extent.y = height;

	lock_bitmap();
	setropfunc(rop, 0, fore, aux);
	fbbm_rop_cinit(consfb, consfb->planemask, 1);
	fbbm_rop_clear(consfb, &dr);
	unlock_bitmap();
}

line(param)
	short *param;
{
	lPoint p[2];
	lRectangle clip;

	p[0].x = param[0];
	p[0].y = param[1];
	p[1].x = param[2];
	p[1].y = param[3];
	clip = consfb->VisRect;

	lock_bitmap();
	fbbm_rop_vect(consfb, &clip, param[4] & 0xf,
		      fbbm_get_pixel(consfb, param[5]), 0, 1,
		      consfb->planemask, 2, p, LINE_SLD, 1, 0);
	unlock_bitmap();
}

/*
 *  cursor on
 *  cursor_on(x, y, plane) puts cursor at position (x, y) with color = plane, 
 *  if cursor sw is off.
 *  At the same time the image of cursor position is saved.
 */
cursor_on(p)
	lPoint *p;
{
	register struct csr_buf *csr_bp = &local_csr_buf;
	register lRectangle *dr;

	if (screen.s_term_mode & DECCSR_ACTV &&
	    bm_todo <= 0 && csr_bp->csr_sw == C_OFF) {

		if (csr_bp->csr_number == 2 && p->x != rit_m)
			dr = &char_r2;
		else
			dr = &char_r1;

		dr->origin = *p;

		lock_bitmap();
		setropfunc(BF_DI, 0, consfb->planemask, 0);
		fbbm_rop_init(consfb, consfb->funcvec);
		fbbm_rop_copy(consfb, dr, dr, 0, (fcolor^bcolor) & consfb->planemask);
		unlock_bitmap();

		csr_bp->csr_sw = C_ON;
		csr_bp->csr_p = *p;
	}
}

/*
 *  cursor off
 *  cursor_off() turns off cursor.
 *  The image of cursor position which has previously saved by cursor_on
 *  is restored.
 */
cursor_off()
{
	register  struct  csr_buf  *csr_bp = &local_csr_buf;
	register lRectangle *dr;

	if (screen.s_term_mode & DECCSR_ACTV && csr_bp->csr_sw == C_ON) {
		if (csr_bp->csr_number == 2 && csr_bp->csr_x != rit_m)
			dr = &char_r2;
		else
			dr = &char_r1;

		dr->origin = csr_bp->csr_p;

		lock_bitmap();
		setropfunc(BF_DI, 0, consfb->planemask, 0);
		fbbm_rop_init(consfb, consfb->funcvec);
		fbbm_rop_copy(consfb, dr, dr, 0, (fcolor^bcolor) & consfb->planemask);
		unlock_bitmap();

		csr_bp->csr_sw = C_OFF;
	}
}

/*
 *  move lines
 *  move_lines(sl, nl, dl)  moves nl lines starting at line sl to line dl.
 */
move_lines(sl, nl, dl)
	int sl, nl, dl;
{
	move_rect(x_ofst, char_h*(sl - 1) + y_ofst, char_w*(rit_m - LFT_M + 1),
		char_h*nl, x_ofst, char_h*(dl - 1) + y_ofst, BF_S);
	PRE_EMPT;
}

/*
 *  move chars
 *  move_chars(sx, sy, nchar, dx) moves nchar characters at position (sx, sy)
 *  to (dx, sy).
 */
move_chars(sx, sy, nchar, dx)
	int sx, sy;
	int nchar;
	int dx;
{
	move_rect(char_w*(sx - 1) + x_ofst, char_h*(sy - 1) + y_ofst,
	    char_w * nchar, char_h,
	    char_w*(dx - 1) + x_ofst, char_h*(sy - 1) + y_ofst, BF_S);
}

/*
 *  clear lines
 *  clear_lines(sl, nl, rev) clears nl lines starting at line sl with rev
 *  mode. If rev = 0 then normal clear else reverse clear.
 */
clear_lines(sl, nl, rev, fcol, bcol)
	int sl, nl;
	int rev;
	int fcol, bcol;
{
	if (nl == btm_m) {
		clear_rect(0, 0, scr_w, scr_h, BF_S, rev?fcol:bcol, bcol);
	} else if (nl > 0) {
		clear_rect(x_ofst, char_h*(sl - 1) + y_ofst,
			char_w*(rit_m - LFT_M + 1), char_h*nl,
			BF_S, rev?fcol:bcol, bcol);
	}
}

/*
 *  Clear chars
 *  clear_chars(x, y, nchar, rev) clears nchar characters following the
 *  position (x, y) with rev mode. If rev = 0 then normal clear else 
 *  reverse clear.
 */
clear_chars(x, y, nchar, rev, fcol, bcol)
	int x, y;
	int nchar;
	int rev;
	int fcol, bcol;
{
	if (nchar > 0) {
		clear_rect(char_w*(x - 1) + x_ofst, char_h*(y - 1) + y_ofst,
			char_w*nchar, char_h, BF_S, rev?fcol:bcol, bcol);
	}
}

reverse_rec(fcol, bcol)
	int fcol, bcol;
{
	clear_rect(0, 0, scr_w, scr_h, BF_SDX, fcol^bcol, 0);
}

copy_char(sp, c, kanji)
	register SCREEN *sp;
	register unsigned int c;
	int kanji;
{
	register char *f_addr;
	register int len;
	register lRectangle *sr, *dr; 
	lRectangle udr;
	register char **fnt_addr;
	extern struct fb_map rommap;

	lock_bitmap();

	if (consfb->font_h == 24)
		fnt_addr = ext_fnt24_addr;
	else
		fnt_addr = ext_fnt_addr;

	if (kanji) {
		dr = &char_r2;
		sr = &font_r2;
		len = font_len2;
	} else {
		dr = &char_r1;
		sr = &font_r1;
		len = font_len1;
	}

	dr->origin = sp->s_csr.csr_p;

	setropfunc(BF_S, 0, fcolor, bcolor);
	fbbm_rop_cinit(consfb, consfb->planemask, 0);
	fbbm_rop_clear(consfb, dr);

	if (kanji) {
		/*
		 * KANJI code... kanji char
		 */
		f_addr = (char *)fbbm_Krom_addr(consfb, c, sr);
#ifdef CPU_DOUBLE
	} else if (fnt_addr == 0) {
		/*
		 * no external fonts... try to use ROM fonts
		 */
		len = font_len2;
		f_addr = (char *)fbbm_Krom_addr(consfb, c, sr);
#endif
#ifdef KM_ASCII
	} else if (tmode == KM_ASCII) {
		/*
		 * terminal mode is ASCII... ASCII (ISO) char
		 */
		if ((c >= 0x20) && (c <= 0x7e)) {
			/*
			 * ASCII char
			 */
			f_addr = fnt_addr[c];
		} else if ((c >= 0xa0) && (c <= 0xff)) {
			/*
			 * ISO char
			 */
			f_addr = fnt_addr[c - 32];
		} else {
			/*
			 * UNKNOWN char
			 */
			f_addr = (caddr_t)zero;
		}
#endif /* KM_ASCII */
	} else {
		/*
		 * terminal mode is not ASCII... JIS, SJIS, EUC, ...
		 */
		if ((c >= 0x20) && (c <= 0x7e)) {
			/*
			 * ASCII char
			 */
			f_addr = fnt_addr[c];
		} else if ((c >= 0xa1) && (c <= 0xdf)) {
			/*
			 * KANA char
			 */
			f_addr = fnt_addr[c + 32];
		} else {
			/*
			 * UNKNOWN char
			 */
			f_addr = (caddr_t)zero;
		}
	}

	dr->origin.y += ch_pos;

	setropfunc(BF_S, 1, fcolor, bcolor);

	if (f_addr != 0) {
		fbbm_rop_winit(consfb);
		rommap.fm_vaddr = f_addr;
		rommap.fm_offset = 0;
		fbbm_rop_write(consfb, &rommap, rommap.fm_offset, len,
				sr, &dr->origin, consfb->planemask);
		if (sp->s_csr.csr_attributes & BOLD) {
			dr->origin.x += 1;
			fbbm_rop_write(consfb, &rommap, rommap.fm_offset, len,
					sr, &dr->origin, consfb->planemask);
		}
	} else {
		fbbm_rop_init(consfb, consfb->funcvec);
		fbbm_rop_copy(consfb, sr, &dr->origin, 1, consfb->planemask);
		if (sp->s_csr.csr_attributes & BOLD) {
			dr->origin.x += 1;
			fbbm_rop_copy(consfb, sr, &dr->origin, 1, consfb->planemask);
		}
	}

	if (sp->s_csr.csr_attributes & USCORE) {

		udr.origin.x = sp->s_csr.csr_p.x;
		udr.origin.y = sp->s_csr.csr_p.y + ul_pos;
		udr.extent.x = char_w;
		udr.extent.y = 1;

		setropfunc(BF_S, 1, fcolor, bcolor);
		fbbm_rop_cinit(consfb, consfb->planemask, 1);
		fbbm_rop_clear(consfb, &udr);
	}
	unlock_bitmap();
}

vt_flush(spc)
	struct cursor *spc;
{
	register char *f_addr;
	register int len;
	register lRectangle *sr, *dr; 
	lRectangle fr, cr;
	register int i;
	register unsigned int c;
	lRectangle udr;
	register char **fnt_addr;
	char *oldf_addr = (char *)-1;
	extern struct fb_map rommap;

	if (fp == 0)
		return;

	cursor_off();

	lock_bitmap();

	if (consfb->font_h == 24)
		fnt_addr = ext_fnt24_addr;
	else
		fnt_addr = ext_fnt_addr;

	udr.origin = fpp;
	udr.extent.x = fpn * char_w;
	udr.extent.y = char_h;

	setropfunc(BF_S, 0, fcolor, bcolor);
	fbbm_rop_cinit(consfb, consfb->planemask, 0);
	fbbm_rop_clear(consfb, &udr);

	if (fpa & BOLD)
		setropfunc(BF_S, 1, fcolor, bcolor);
	else
		setropfunc(BF_S, 0, fcolor, bcolor);

	fbbm_rop_winit(consfb);

	sr = &fr;
	dr = &cr;
	sr->origin.x = 0;
	sr->origin.y = 0;
	sr->extent.y = font_h;
	dr->origin.x = fpp.x;
	dr->origin.y = fpp.y + ch_pos;

	for (i = 0; i < fp; i++) {
		c = fbuf[i];

		sr->extent.x = font_w;
		dr->extent.x = char_w;

		if (c & 0xff00) {
			/*
			 * KANJI code... kanji char
			 */
			sr->extent.x = font_r2.extent.x;
			dr->extent.x = char_r2.extent.x;
			len = font_len2;
			f_addr = (char *)fbbm_Krom_addr(consfb, c, sr);
#ifdef CPU_DOUBLE
		} else if (fnt_addr == 0) {
			/*
			 * no external fonts... try to use ROM fonts
			 */
			sr->extent.x = font_r1.extent.x;	/*XXX*/
			dr->extent.x = char_r1.extent.x;	/*XXX*/
			len = font_len2;
			f_addr = (char *)fbbm_Krom_addr(consfb, c, sr);
#endif
#ifdef KM_ASCII
		} else if (tmode == KM_ASCII) {
			/*
			 * terminal mode is ASCII... ASCII (ISO) char
			 */
			len = font_len1;
			if ((c >= 0x20) && (c <= 0x7e)) {
				/*
				 * ASCII char
				 */
				f_addr = fnt_addr[c];
			} else if ((c >= 0xa0) && (c <= 0xff)) {
				/*
				 * ISO char
				 */
				f_addr = fnt_addr[c - 32];
			} else {
				/*
				 * UNKNOWN char
				 */
				f_addr = (caddr_t)zero;
			}
#endif /* KM_ASCII */
		} else {
			/*
			 * terminal mode is not ASCII... JIS, SJIS, EUC, ...
			 */
			len = font_len1;
			if ((c >= 0x20) && (c <= 0x7e)) {
				/*
				 * ASCII char
				 */
				f_addr = fnt_addr[c];
			} else if ((c >= 0xa1) && (c <= 0xdf)) {
				/*
				 * KANA char
				 */
				f_addr = fnt_addr[c + 64];
			} else {
				/*
				 * UNKNOWN char
				 */
				f_addr = (caddr_t)zero;
			}
		}

		if (f_addr != 0) {
			if (oldf_addr == 0)
				fbbm_rop_winit(consfb);

			rommap.fm_vaddr = f_addr;
			rommap.fm_offset = 0;
			fbbm_rop_write(consfb, &rommap, rommap.fm_offset, len,
					sr, &dr->origin, consfb->planemask);
			if (fpa & BOLD) {
				/*
				 * Bold char
				 */
				dr->origin.x += 1;
				fbbm_rop_write(consfb, &rommap,
						rommap.fm_offset, len,
						sr, &dr->origin, consfb->planemask);
				dr->origin.x -= 1;
			}
		} else {
			if (oldf_addr != 0)
				fbbm_rop_init(consfb, consfb->funcvec);
	
			fbbm_rop_copy(consfb, sr, &dr->origin, 1, consfb->planemask);
			if (fpa & BOLD) {
				/*
				 * Bold char
				 */
				dr->origin.x += 1;
				fbbm_rop_copy(consfb, sr, &dr->origin,
						1, consfb->planemask);
				dr->origin.x -= 1;
			}

		}
		dr->origin.x += dr->extent.x;
		oldf_addr = f_addr;

		/*
		 * sr->origin.x and sr->origin.y were changed by
		 * fbpop_Krom_addr(), fb254_Krom_addr().
		 */
		sr->origin.x = 0;
		sr->origin.y = 0;
	}

	if (fpa & USCORE) {
		udr.origin.y += ul_pos;
		udr.extent.y = 1;

		setropfunc(BF_S, 1, fcolor, bcolor);
		fbbm_rop_cinit(consfb, consfb->planemask, 1);
		fbbm_rop_clear(consfb, &udr);
	}
	fp = 0;

	unlock_bitmap();

	dr->origin.y -= ch_pos;
	cursor_on(&(spc->csr_p));
}
