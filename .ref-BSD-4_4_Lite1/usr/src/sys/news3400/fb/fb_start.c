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
 * from: $Hdr: fb_start.c,v 4.300 91/06/27 20:42:40 root Rel41 $ SONY
 *
 *	@(#)fb_start.c	8.1 (Berkeley) 6/11/93
 */

#include <sys/param.h>
#include <sys/systm.h>

#ifdef IPC_MRX
#include "../../iop/framebuf.h"
#include "../../iop/fbreg.h"
#include "page.h"
#else
#include <news3400/iop/framebuf.h>
#include <news3400/iop/fbreg.h>
#endif

#include <news3400/fb/fbdefs.h>

#ifdef CPU_SINGLE
#include <machine/cpu.h>
extern struct tty cons;
extern int cnstart();
#define PRE_EMPT	need_resched()
#endif

static struct fbdev	*cfb = 0;
static lPoint		mp;
#ifdef CPU_SINGLE
static int		curs_pending = 0;
#endif

extern struct fbdevsw	fbdevsw[];
extern int		nfbdev;

#ifdef CPU_SINGLE
extern char	*ext_fnt_addr[];
extern char	*ext_fnt24_addr[];
#else
extern char	**ext_fnt_addr;
extern char	**ext_fnt24_addr;
#endif

static char copyfuncv[MAXPLANE] = {
	BF_S, BF_S, BF_S, BF_S, BF_S, BF_S, BF_S, BF_S,		/* SRC */
	BF_S, BF_S, BF_S, BF_S, BF_S, BF_S, BF_S, BF_S,		/* SRC */
	BF_S, BF_S, BF_S, BF_S, BF_S, BF_S, BF_S, BF_S		/* SRC */
};

unsigned short fb_color_pallet_def[48] = {	/* define initial color */
/*	R,	G,	B	*/
	0,	0,	0,
	0,	0,	0x44,
	0,	0x44,	0,
	0,	0x44,	0x44,
	0x44,	0,	0,
	0x44,	0,	0x44,
	0x44,	0x44,	0,
	0x44,	0x44,	0x44,
	0x88,	0x88,	0x88,
	0,	0,	0xff,
	0,	0xff,	0,
	0,	0xff,	0xff,
	0xff,	0,	0,
	0xff,	0,	0xff,
	0xff,	0xff,	0,
	0xff,	0xff,	0xff
};

unsigned short fb_gray_pallet_def[48] = {	/* define initial color */
/*	R,	G,	B	*/
	0xff,	0xff,	0xff,
	0xff,	0xff,	0,
	0xff,	0,	0xff,
	0xff,	0,	0,
	0,	0xff,	0xff,
	0,	0xff,	0,
	0,	0,	0xff,
	0x88,	0x88,	0x88,
	0x44,	0x44,	0x44,
	0x44,	0x44,	0,
	0x44,	0,	0x44,
	0x44,	0,	0,
	0,	0x44,	0x44,
	0,	0x44,	0,
	0,	0,	0x44,
	0,	0,	0
};

static int bitmap_use;		/* shared variable for bitmap exclusion ctrl */

#ifdef IPC_MRX
struct fb_map rommap;
#endif

#ifdef CPU_SINGLE
void
lock_bitmap()
{
	int s;

	/* wait(bitmap_use) */
	s = splbitmap();
	while (bitmap_use & FB_BUSY) {
		bitmap_use |= FB_WANTED;
		sleep((caddr_t)&bitmap_use, FBPRI);
	}
	bitmap_use |= FB_BUSY;
	splx(s);
}

void
unlock_bitmap()
{
	int s;

	/* signal(bitmap_use) */
	s = splbitmap();
	if (bitmap_use & FB_WANTED)
		wakeup((caddr_t)&bitmap_use);
	bitmap_use &= ~(FB_BUSY|FB_WANTED);
	splx(s);
}

lock_bitmap_poll()
{
	int s;

	/* wait(bitmap_use) */

	s = splbitmap();
	if (bitmap_use & (FB_BUSY|FB_WANTED)) {
		splx(s);
		return (1);
	}
	bitmap_use |= FB_BUSY;
	splx(s);
	return (0);
}

void
unlock_bitmap_poll()
{
	int s;

	/* signal(bitmap_use) */
	s = splbitmap();
	if (bitmap_use & FB_WANTED)
		wakeup((caddr_t)&bitmap_use);
	bitmap_use &= ~(FB_BUSY|FB_WANTED);
	splx(s);
}

bmlockedp()
{
	return (bitmap_use & (FB_WANTED|FB_BUSY));
}

#ifdef NOTDEF /* KU:XXX not necessary for news3200 */
void
rop_wait(fb)
	struct fbdev *fb;
{
	register int s;
	int i;

	s = splbitmap();
/* KU:XXX trick! */
#define in_interrupt()	((caddr_t)&fb < (caddr_t)MACH_CODE_START)
	if (in_interrupt() || (fb->run_flag & FB_WAITING)) {
		splx(s);
		fbbm_rop_wait(fb);
	} else {
		if (fbbm_ioctl(fb, FB_STATUSCHECK, 0) &
		    (FB_STATUS_ROPWAIT|FB_STATUS_ROPEXEC)) {

			i = FB_INT_ROPDONE;
			fbbm_ioctl(fb, FB_INTENABLE, &i);

			if (!(fbbm_ioctl(fb, FB_STATUSCHECK, 0) &
			    (FB_STATUS_ROPWAIT|FB_STATUS_ROPEXEC))) {
				i = FB_INT_ROPDONE;
				fbbm_ioctl(fb, FB_INTCLEAR, &i);
			} else {
				fb->run_flag |= FB_WAITING;
				sleep((caddr_t)&fb->run_flag, FBPRI);
			}
		}
		splx(s);
	}
}
#endif /* NOTDEF */
#else /* CPU_SINGLE */
#ifdef IPC_MRX
struct page {
	char bytes[NBPG];
};
extern struct page *page_base;
extern struct page *page_max;
extern struct map pagemap[];
extern struct pte_iop page_pt[];
extern int mapped_page;

caddr_t
fb_map_page(map, n, prot)
	register int *map;
	register int n;
	register int prot;
{
	register int x;
	register struct pte_iop *p;
	register struct page *addr;
	register int s = spl7();
	static int last_x, last_n;

	if (last_n >= n) {
		x = last_x;
	} else {
		rmfree(pagemap, last_n, last_x);
		mapped_page -= last_n;
		last_x = 0;
		last_n = 0;
		if ((x = rmalloc(pagemap, n)) <= 0) {
			splx(s);
			return (NULL);
		}
		mapped_page += n;
		last_x = x;
		last_n = n;
	}
	addr = page_base + x;
	prot |= PG_PAGE;

	for (p = page_pt + x; n > 0; p++, n--) {
		*(int *)p = prot | *map++;
		tbis((caddr_t)addr);
		addr++;
	}

	splx(s);
	return ((caddr_t)(page_base + x));
}

caddr_t
fb_map_page2(map, n, prot)
	register int *map;
	register int n;
	register int prot;
{
	register int x;
	register struct pte_iop *p;
	register struct page *addr;
	register int s;

	if (n == 0)
		return (NULL);
	s = spl7();
	if ((x = rmalloc(pagemap, n)) <= 0) {
		splx(s);
		return (NULL);
	}
	mapped_page += n;
	addr = page_base + x;
	prot |= PG_PAGE;

	for (p = page_pt + x; n > 0; p++, n--) {
		*(int *)p = prot | (*map++);
		tbis((caddr_t)addr);
		addr++;
	}

	splx(s);
	return ((caddr_t)(page_base + x));
}
#endif /* IPC_MRX */
#endif /* CPU_SINGLE */

iopmemfbmap(addr, len, map)
	register caddr_t addr;
	register int len;
	register struct fb_map *map;
{
	register caddr_t *p;
	register int i;

	map->fm_vaddr = addr;
	map->fm_offset = (unsigned)addr & CLOFSET;
	map->fm_count = len;
	len += map->fm_offset;
	p = map->fm_addr;
	addr -= map->fm_offset;

	for (i = 0; i < NFBMAP && len > 0; i++) {
		*p++ = addr;
		addr += CLBYTES;
		len -= CLBYTES;
	}
}

int
nofunc()
{
	return 0;
}

int
error()
{
	return FB_RERROR;
}

void
checkArea(fb, x, y)
	register struct fbdev *fb;
	register int *x, *y;
{
	if (*x < fb->moveArea.origin.x)
		*x = fb->moveArea.origin.x;
	if (*y < fb->moveArea.origin.y)
		*y = fb->moveArea.origin.y;
	if (*x >= (fb->moveArea.origin.x + fb->moveArea.extent.x))
		*x = (fb->moveArea.origin.x + fb->moveArea.extent.x) - 1;
	if (*y >= (fb->moveArea.origin.y + fb->moveArea.extent.y))
		*y = (fb->moveArea.origin.y + fb->moveArea.extent.y) - 1;
}

cursorIn(fb, clip)
	register struct fbdev *fb;
	register lRectangle *clip;
{
	if (clip == 0)
		return (1);
	if (cfb != fb)
		return (0);

	return (clip->origin.x < fb->cursorP.x + fb->size.x &&
		clip->origin.x + clip->extent.x > fb->cursorP.x &&
		clip->origin.y < fb->cursorP.y + fb->size.y &&
		clip->origin.y + clip->extent.y > fb->cursorP.y);
}

void
fbcopy1(src, dst, fv)
	lPoint src;
	lPoint dst;
	char *fv;
{
	lRectangle sr, dr;

	sr.origin = src;
	sr.extent = cfb->size;
	dr.origin = dst;
	if (cliprect2(&sr, &cfb->FrameRect, &dr, &cfb->VisRect)) {
		fbbm_rop_init(cfb, fv);
		fbbm_rop_copy(cfb, &sr, &dr.origin, 1, FB_PLANEALL);
	}
}

void
fbcopy2(src, dst)
	lPoint src;
	lPoint dst;
{
	lRectangle sr, dr;

	sr.origin = src;
	sr.extent = cfb->size;
	dr.origin = dst;
	if (cliprect2(&sr, &cfb->FrameRect, &dr, &cfb->FrameRect)) {
		fbbm_rop_init(cfb, copyfuncv);
		fbbm_rop_copy(cfb, &sr, &dr.origin, 0, FB_PLANEALL);
	}
}

void
fbcopy3(src, dst)
	lPoint src;
	lPoint dst;
{
	lRectangle sr, dr;

	sr.origin = src;
	sr.extent = cfb->size;
	dr.origin = dst;
	if (cliprect2(&sr, &cfb->FrameRect, &dr, &cfb->VisRect)) {
		fbbm_rop_init(cfb, copyfuncv);
		fbbm_rop_copy(cfb, &sr, &dr.origin, 0, FB_PLANEALL);
	}
}

void
cursorOn(fb)
	register struct fbdev *fb;
{
#ifdef CPU_SINGLE
	int s = splbitmap();
#endif

	if (cfb == fb && fb->cursorShow && !fb->cursorVis) {
		if (fb->hard_cursor) {
			fbbm_cursor_on(fb);
		} else {
			fbcopy2(fb->cursorP, fb->SaveRect.origin);
			fbcopy1(fb->MaskRect.origin, fb->cursorP,
				fb->maskfuncv);
			fbcopy1(fb->CursorRect.origin, fb->cursorP,
				fb->curfuncv);
		}
		fb->cursorVis = 1;
	}
#ifdef CPU_SINGLE
	splx(s);
#endif
}

void
cursorOff(fb)
	register struct fbdev *fb;
{
#ifdef CPU_SINGLE
	int s = splbitmap();
#endif

	if (cfb == fb && fb->cursorShow && fb->cursorVis) {
		if (fb->hard_cursor)
			fbbm_cursor_off(fb);
		else
			fbcopy3(fb->SaveRect.origin, fb->cursorP);
		fb->cursorVis = 0;
	}
#ifdef CPU_SINGLE
	splx(s);
#endif
}

void
softCursorCheck(fb, stype, srect, dtype, drect)
	struct fbdev *fb;
	char stype, dtype;
	lRectangle *srect, *drect;
{
	if (cfb == fb  && cfb->cursorVis &&
	    ((stype == BM_FB && cursorIn(fb, srect)) ||
	    (dtype == BM_FB && cursorIn(fb, drect))))
		cursorOff(cfb);
}

void
cursorCheck(fb, stype, srect, dtype, drect)
	struct fbdev *fb;
	char stype, dtype;
	lRectangle *srect, *drect;
{
	if (!fb->hard_cursor)
		softCursorCheck(fb, stype, srect, dtype, drect);
}

int
redrawCursor(fb)
	register struct fbdev *fb;
{
	int s;
	lPoint	tmp;

	if (cfb == fb && fb->cursorSet) {
		s = spl7();
		tmp = mp;
		splx(s);

#ifdef CPU_SINGLE
		s = splbitmap();
#endif
		if (fb->cursorP.x != tmp.x || fb->cursorP.y != tmp.y) {
			if (fb->cursorVis) {
				if (! fb->hard_cursor) {
					fbcopy3(fb->SaveRect.origin,
						fb->cursorP);
				}
			}
			fb->cursorP = tmp;
			if (fb->hard_cursor) {
				fbbm_cursor_off(fb);
				fbbm_cursor_move(fb);
			}
			if (fb->cursorVis) {
				if (fb->hard_cursor) {
					fbbm_cursor_on(fb);
				} else {
					fbcopy2(fb->cursorP,
						fb->SaveRect.origin);
					fbcopy1(fb->MaskRect.origin,
						fb->cursorP, fb->maskfuncv);
					fbcopy1(fb->CursorRect.origin,
						fb->cursorP, fb->curfuncv);
				}
			}
		}
#ifdef CPU_SINGLE
		splx(s);
#endif
	}
	return (0);
}

void
updateCursor(x, y, flag)
	int *x, *y;
	int flag;
{
	int s;

	if (cfb && cfb->cursorSet) {
		checkArea(cfb, x, y);
		s = spl7();
		mp.x = *x - cfb->hot.x;
		mp.y = *y - cfb->hot.y;
		splx(s);
#ifdef CPU_SINGLE
		if (flag || cfb->hard_cursor) {
			curs_pending = 0;
			redrawCursor(cfb);
		} else if (cfb->type == FB_LCDM) {
			if (!lock_bitmap_poll()) {
				curs_pending = 0;
				redrawCursor(cfb);
				unlock_bitmap_poll();
			} else {
				curs_pending = 1;
			}
		}
#else
		redrawCursor(cfb);
#endif
	}
}

setCursor(fb, cursor)
	register struct fbdev *fb;
	register lCursor2  *cursor;
{
	register char *fv;
	register int f0, f1, i, color;
#ifdef CPU_SINGLE
	register int s = splbitmap();
#endif
	int	data;

	if (cfb == fb) {
		cursorOff(cfb);
		fb->cursorShow = 0;
		fb->cursorP.x += cfb->hot.x;
		fb->cursorP.y += cfb->hot.y;
#ifdef CPU_SINGLE
		data = FB_INT_VSYNC;
		fbbm_ioctl(fb, FB_INTCLEAR, &data);
#endif
		cfb = NULL;
	}

	if (cursor) {
		fb->CursorRect = cursor->cursorRect;
		fb->MaskRect = cursor->maskRect;
		fb->SaveRect = cursor->saveRect;
		fb->moveArea = cursor->moveArea;
		fb->hot = cursor->hot;
		fb->size = cursor->size;

		f0 = 0x4 | ((cursor->func >> 2) & 0x3);
		f1 = 0x4 | (cursor->func & 0x3);

		i = fb->fbNplane;
		fv = fb->curfuncv;
		color = cursor->cursor_color;
		while (i-- > 0) {
			*fv++ = (color & 1) ? f1 : f0;
			color >>= 1;
		}

		i = fb->fbNplane;
		fv = fb->maskfuncv;
		color = cursor->mask_color;
		while (i-- > 0) {
			*fv++ = (color & 1) ? f1 : f0;
			color >>= 1;
		}

		checkArea(fb, &fb->cursorP.x, &fb->cursorP.y);
		fb->cursorP.x -= fb->hot.x;
		fb->cursorP.y -= fb->hot.y;
		fb->cursorSet = 1;
		fb->cursorShow = 0;
		fb->cursorVis = 0;
		if (fb->hard_cursor) {
			fbbm_cursor_off(fb);
			fbbm_cursor_set(fb, cursor->cursor_color, cursor->mask_color);
			fbbm_cursor_move(fb);
		}
	} else {
		fb->cursorP.x = fb->VisRect.extent.x / 2;
		fb->cursorP.y = fb->VisRect.extent.y / 2;
		fb->cursorSet = 0;
		fb->cursorShow = 0;
		fb->cursorVis = 0;
		if (fb->hard_cursor)
			fbbm_cursor_off(fb);
	}
#ifdef CPU_SINGLE
	splx(s);
#endif
	return (FB_ROK);
}

showCursor(fb)
	register struct fbdev *fb;
{
	int data;
#ifdef CPU_SINGLE
	register int s = splbitmap();
#endif

	if (fb->cursorSet && !fb->cursorShow) {
		if (cfb && cfb != fb) {
			cursorOff(cfb);
			cfb->cursorShow = 0;
		}
		cfb = fb;
		fb->cursorShow = 1;
		mp = fb->cursorP;
		cursorOn(fb);
#ifdef CPU_SINGLE
		data = FB_INT_VSYNC;
		fbbm_ioctl(fb, FB_INTENABLE, &data);
		splx(s);
#endif
		return (FB_ROK);
	}
#ifdef CPU_SINGLE
	splx(s);
#endif
	return (FB_RERROR);
}


hideCursor(fb)
	register struct fbdev *fb;
{
	int data;
#ifdef CPU_SINGLE
	int s = splbitmap();
#endif

	if (cfb == fb) {
		cursorOff(fb);
		fb->cursorShow = 0;
#ifdef CPU_SINGLE
		data = FB_INT_VSYNC;
		fbbm_ioctl(fb, FB_INTCLEAR, &data);
		splx(s);
#endif
		return (FB_ROK);
	}
#ifdef CPU_SINGLE
	splx(s);
#endif
	return (FB_RERROR);
}


moveCursor(fb, point)
	struct fbdev *fb;
	lPoint *point;
{
	if (cfb == fb) {
		updateCursor(&point->x, &point->y, 1);
		return (FB_ROK);
	}
	return (FB_RERROR);
}

#ifdef CPU_SINGLE
rop_xint()
{
	register struct fbdev *fb;
	register int i;
	register int done = 0;
	int event, data;
	int s = splbitmap();

	for (i = 0, fb = fbdev; i < nfbdev; i++, fb++) {
		if (fb->type && (event = fbbm_ioctl(fb, FB_INTCHECK, 0))) {
#ifdef notyet /* KU:XXX */
			intrcnt[INTR_BITMAP]++;
#endif
			done = 1;
			if (event & FB_INT_VSYNC) {
				data = FB_INT_VSYNC;
				fbbm_ioctl(fb, FB_INTCLEAR, &data);
				if (!lock_bitmap_poll()) {
					curs_pending = 0;
					redrawCursor(fb);
					unlock_bitmap_poll();
				} else {
					curs_pending = 1;
				}
				data = FB_INT_VSYNC;
				fbbm_ioctl(fb, FB_INTENABLE, &data);
			}
			if (event & FB_INT_ROPDONE) {
				if(fb->run_flag & FB_WAITING) {
					data = FB_INT_ROPDONE;
					fbbm_ioctl(fb, FB_INTCLEAR, &data);
					if (!(fbbm_ioctl(fb, FB_STATUSCHECK, 0)
					& (FB_STATUS_ROPWAIT|FB_STATUS_ROPEXEC))) {
						fb->run_flag &= ~FB_WAITING;
						wakeup(&(fb->run_flag));
					} else {
						data = FB_INT_ROPDONE|0x100;
						fbbm_ioctl(fb, FB_INTENABLE, &data);
					}
				}
			}
		}
	}
	splx(s);
	return (done);
}
#endif /* CPU_SINGLE */

cliprect2(sr, sc, dr, dc)
	register lRectangle *sr;
	register lRectangle *sc;
	register lRectangle *dr;	
	register lRectangle *dc;	
{
	register int d;

	/* src left/right edge */
	if ((d = sr->origin.x - sc->origin.x) < 0) {
		sr->extent.x += d;
		sr->origin.x -= d;
		dr->origin.x -= d;
		d = sr->extent.x - sc->extent.x;
	} else
		d += sr->extent.x - sc->extent.x;
	if (d > 0)
		sr->extent.x -= d;

	/* src top/bottom edge */
	if ((d = sr->origin.y - sc->origin.y) < 0) {
		sr->extent.y += d;
		sr->origin.y -= d;
		dr->origin.y -= d;
		d = sr->extent.y - sc->extent.y;
	} else
		d += sr->extent.y - sc->extent.y;
	if (d > 0)
		sr->extent.y -= d;

	if (sr->extent.x <= 0 || sr->extent.y <= 0)
		return (0);

	/* dst left/right edge */
	if ((d = dr->origin.x - dc->origin.x) < 0) {
		dr->origin.x -= d;
		sr->extent.x += d;
		sr->origin.x -= d;
		d = sr->extent.x - dc->extent.x;
	} else
		d += sr->extent.x - dc->extent.x;
	if (d > 0)
		sr->extent.x -= d;

	/* dst top/bottom edge */
	if ((d = dr->origin.y - dc->origin.y) < 0) {
		dr->origin.y -= d;
		sr->extent.y += d;
		sr->origin.y -= d;
		d = sr->extent.y - dc->extent.y;
	} else
		d += sr->extent.y - dc->extent.y;
	if (d > 0)
		sr->extent.y -= d;

	if (sr->extent.x <= 0 || sr->extent.y <= 0)
		return (0);

	dr->extent = sr->extent;
	return (1);
}

cliprect(r, crp, p)
	register lRectangle *r;
	register lRectangle *crp;
	register lRectangle *p;	
{
	register int d;

	/* left edge */
	if ((d = r->origin.x - crp->origin.x) < 0) {
		r->extent.x += d;
		r->origin.x -= d;
		if (p) {
			p->extent.x += d;
			p->origin.x -= d;
		}
		d = r->extent.x - crp->extent.x;
	} else
		d += r->extent.x - crp->extent.x;

	/* right edge */
	if (d > 0) {
		r->extent.x -= d;
		if (p)
			p->extent.x -= d;
	}

	/* top edge */
	if ((d = r->origin.y - crp->origin.y) < 0) {
		r->extent.y += d;
		r->origin.y -= d;
		if (p) {
			p->extent.y += d;
			p->origin.y -= d;
		}
		d = r->extent.y - crp->extent.y;
	} else
		d += r->extent.y - crp->extent.y;

	/* bottom edge */
	if (d > 0) {
		r->extent.y -= d;
		if (p)
			p->extent.y -= d;
	}

	return (r->extent.x > 0 && r->extent.y > 0);
}

getclip(fb, bmp, crp)
	struct fbdev *fb;
	lBitmap *bmp;
	lRectangle *crp;
{
	/* limit clip rectangle to bitmap rectangle */
	if (!cliprect(crp, &bmp->rect, (lRectangle*)0))
		return (0);

	/* limit clip rectangle to frame buffer */
	if ((bmp->type == BM_FB) &&
	    !cliprect(crp, &fb->FrameRect, (lRectangle*)0))
		return (0);
	return (1);
}


clipsrc(fb, bmp)
	struct fbdev *fb;
	lBitmap *bmp;
{
	/* limit clip rectangle to frame buffer */
	if (bmp->type == BM_FB &&
	    !cliprect(&bmp->rect, &fb->FrameRect, (lRectangle*)0))
		return (0);
	return (1);
}


setrop(fb, func, pmask, fore, aux, trans, sbp, dbp)
	register struct fbdev *fb;
	register unsigned int func;
	int pmask;
	register int fore, aux;
	int trans;
	lBitmap *sbp, *dbp;
{
	register char *funcp;
	register int i;
	char tmp[4];

	/* set plane register */

	fb->Mode = 0;
	fb->Pmask = pmask;
	fb->func = func;
	fb->fore = fore;
	fb->aux = aux;
	fb->trans = trans;

	if (sbp->depth > 1)
		fb->Mode |= 2;

	if (dbp->depth > 1)
		fb->Mode |= 1;

	/* set rop function register */
	func &= 0xf;

	tmp[0] = TRANS(trans, (func & 0x0c) | (func>>2));
	tmp[1] = TRANS(trans, (func>>2) | ((func<<2) & 0x0c));
	tmp[2] = TRANS(trans, func);
	tmp[3] = TRANS(trans, (func<<2) & 0x0c | func & 3);

	funcp = fb->funcvec;
	for (i = fb->fbNplane; --i >= 0;) {
		*funcp++ = tmp[((fore & 1) << 1) | (aux & 1)];
		fore >>= 1; aux >>= 1;
	}
	return (0);
}

/*
 * bitblt within frame buffer
 */

bitblt_fb(fb, sbp, srp, dbp, dpp, crp)
	register struct fbdev *fb;
	register lBitmap *sbp;	/* source bitmap (FB) */
	lRectangle *srp;	/* source rectangle */
	lBitmap *dbp;		/* destination bitmap (FB) */
	lPoint *dpp;		/* destination point */
	lRectangle *crp;	/* clip region in destination */
{
	lRectangle sr;
	lRectangle dr;
	register int wplane, i, j;

	sr = *srp;
	dr.origin = *dpp;
	if (crp && !cliprect2(&sr, &sbp->rect, &dr, crp))
		return (0);

	fbbm_rop_init(fb, fb->funcvec);

	switch (fb->Mode) {

	case MODE_1to1:
		fb->Pmask &= 1;

	case MODE_NtoN:

		fbbm_rop_copy(fb, &sr, &dr.origin, 0, fb->Pmask);
		break;

	case MODE_1toN:
		fbbm_rop_copy(fb, &sr, &dr.origin, 1, fb->Pmask);
		break;

	case MODE_Nto1:
		wplane = 1;
		for (i = 0, j = sbp->depth; i < j; i++) {
			if (fb->Pmask & wplane) {
				fbbm_rop_copy(fb, &sr, &dr.origin, i + 1,
				    fb->Pmask >> 16);
				break;
			}
			wplane <<= 1;
		}
		break;
	default:
		return (-1);
	}
	return (0);
}

/*
 * bitblt from main memory to frame buffer
 */

bitblt_tofb(fb, sbp, srp, dbp, dpp, crp)
	register struct fbdev *fb;
	register lBitmap *sbp;	/* source bitmap (MEM) */
	lRectangle *srp;	/* source rectangle */
	lBitmap *dbp;		/* destination bitmap (FB) */
	lPoint *dpp;		/* destination point */
	lRectangle *crp;	/* clip region in destination */
{
	register unsigned p;
	register struct fb_map *smap;
	register int i, n, m;
	lRectangle sr;
	lRectangle dr;
	register int wplane;
#ifdef IPC_MRX
	extern struct fb_map rommap;
	register int pages;
#endif

	smap = (struct fb_map*)sbp->base;

	sr = *srp;
	dr.origin = *dpp;
	if (crp && !cliprect2(&sr, &sbp->rect, &dr, crp))
		return (0);
	dr.extent = sr.extent;

	/* transform source rectangle */
	sr.origin.x -= sbp->rect.origin.x;
	sr.origin.y -= sbp->rect.origin.y;

	/*
	 * check memory map specification
	 */
	p = smap->fm_offset;
#ifdef IPC_MRX
	pages = btoc(smap->fm_offset + smap->fm_count);
	rommap.fm_vaddr = fb_map_page(smap->fm_addr, pages,
		    fb->cache_off ? PG_S|PG_WP|PG_CI : PG_S|PG_WP);
	rommap.fm_offset = 0;
	smap = &rommap;
#endif

	wplane = 1;

	fbbm_rop_winit(fb);

	switch (fb->Mode) {
	case MODE_1to1:
		fbbm_rop_write(fb, smap, p, sbp->width,
			       &sr, &dr, fb->Pmask & 0x01);
		break;
	case MODE_1toN:
		fbbm_rop_write(fb, smap, p, sbp->width,
			       &sr, &dr, fb->Pmask);
		break;
	case MODE_Nto1:
		m = sbp->width * sbp->rect.extent.y;
		for (i = 0; i < sbp->depth; i++, wplane <<= 1) {
			if (fb->Pmask & wplane) {
				p += (m * i) << 1;
				fbbm_rop_write(fb, smap, p, sbp->width,
					       &sr, &dr, wplane);
				break;
			}
			wplane <<= 1;
		}
		break;
	case MODE_NtoN:
		n = min(sbp->depth, fb->fbNplane);
		m = sbp->width * sbp->rect.extent.y;
		p += (m << 1) * n;
		wplane = 1 << (n - 1);
		for (i = n; i > 0; i--) {
			/* get next plane */
			p -= m << 1;
			if (fb->Pmask & wplane)
				fbbm_rop_write(fb, smap, p, sbp->width,
					       &sr, &dr, wplane);
			/* next plane mask */
			wplane >>= 1;
		}
		break;
	default:
		return (-1);
	}
	return (0);
}

/*
 * bitblt from frame buffer to main memroy
 */

bitblt_tomem(fb, sbp, srp, dbp, dpp, crp)
	struct fbdev *fb;
	lBitmap *sbp;		/* source bitmap (FB) */
	lRectangle *srp;	/* source rectangle */
	lBitmap *dbp;		/* destination bitmap (MEM) */
	lPoint *dpp;		/* destination point */
	lRectangle *crp;	/* clip region in destination */
{
	register struct fb_map *dmap;
	register unsigned p;
	register int i, n, m;
	lRectangle sr;
	lRectangle dr;
	int plane;
#ifdef IPC_MRX
	extern struct fb_map rommap;
	register int pages;
#endif

	dmap = (struct fb_map*)dbp->base;

	sr = *srp;
	dr.origin = *dpp;
	if (crp && !cliprect2(&sr, &sbp->rect, &dr, crp))
		return (0);
	dr.extent = sr.extent;

	dr.origin.x -= dbp->rect.origin.x;
	dr.origin.y -= dbp->rect.origin.y;

	p = dmap->fm_offset;
#ifdef IPC_MRX
	pages = btoc(dmap->fm_offset + dmap->fm_count);
	rommap.fm_vaddr = fb_map_page(dmap->fm_addr, pages, PG_S);
	rommap.fm_offset = 0;
	dmap = &rommap;
#endif

	plane = 1;

/*	Wait for rop busy */

	switch (fb->Mode) {

	case MODE_1to1:
		if (fb->Pmask & plane)
			fbbm_rop_read(fb, dmap, p, dbp->width,
				      &sr, &dr, 0, 0);
		break;

	case MODE_1toN:
		m = (dbp->width * dbp->rect.extent.y) << 1;
		for (i = 0; i < dbp->depth; i++) {
			if (fb->Pmask & plane)
				fbbm_rop_read(fb, dmap, p, dbp->width,
					      &sr, &dr, 0, i);
			/* next plane */
			p += m;
			plane <<= 1;
		}
		break;

	case MODE_Nto1:
		for (i = 0; i < sbp->depth; i++, plane <<= 1) {
			if (fb->Pmask & plane) {
				fbbm_rop_read(fb, dmap, p, dbp->width,
					      &sr, &dr, i, 0);
				break;
			}
		}
		break;

	case MODE_NtoN:
		n = min(dbp->depth, fb->fbNplane);
		m = (dbp->width * dbp->rect.extent.y) << 1;
		for (i = 0; i < n; i++) {
			if (fb->Pmask & plane)
				fbbm_rop_read(fb, dmap, p, dbp->width,
					      &sr, &dr, i, i);
			/* next plane */
			p += m;
			plane <<= 1;
		}

		break;

	default:
		return (-1);
	}

	return (0);
}

bitblt_mem(fb, sbp, srp, dbp, dpp, crp)
	struct fbdev *fb;
	register lBitmap *sbp;
	lRectangle *srp;
	register lBitmap *dbp;
	lPoint *dpp;
	lRectangle *crp;
{
	register int i;
	register int plane;
	register struct fb_map *smap, *dmap;
	register unsigned int ps, pd;
	lRectangle sr;
	lRectangle dr;
#ifdef IPC_MRX
	static struct fb_map drommap;
	int spages, dpages;
#endif

	smap = (struct fb_map*)sbp->base;
	dmap = (struct fb_map*)dbp->base;

	sr = *srp;
	dr.origin = *dpp;
	if (crp && !cliprect2(&sr, &sbp->rect, &dr, crp))
		return (0);

	/* normalize source/destination coordinates */
	sr.origin.x -= sbp->rect.origin.x;
	sr.origin.y -= sbp->rect.origin.y;
	dr.origin.x -= dbp->rect.origin.x;
	dr.origin.y -= dbp->rect.origin.y;

	ps = smap->fm_offset;
	pd = dmap->fm_offset;
#ifdef IPC_MRX
	spages = btoc(smap->fm_offset + smap->fm_count);
	dpages = btoc(dmap->fm_offset + dmap->fm_count);
	rommap.fm_vaddr = fb_map_page2(smap->fm_addr, spages, PG_S|PG_WP);
	rommap.fm_offset = 0;
	drommap.fm_vaddr = fb_map_page2(dmap->fm_addr, dpages, PG_S);
	drommap.fm_offset = 0;
	smap = &rommap;
	dmap = &drommap;
#endif

	plane = 0x1;	/* plane 0 */

	switch (fb->Mode) {

	case MODE_1to1:
		if (fb->Pmask & plane) {
			mem_to_mem(fb->funcvec[0],
				   smap, ps, sbp->width, dmap, pd, dbp->width,
				   &sr, &dr.origin);
		}
		break;

	case MODE_1toN:
		for (i = 0; i < dbp->depth; i++) {
			if (fb->Pmask & plane) {
				mem_to_mem(fb->funcvec[i],
					   smap, ps, sbp->width,
					   dmap, pd, dbp->width,
					   &sr, &dr.origin);
			}
			pd += (dbp->width * dbp->rect.extent.y) << 1;
			plane <<= 1;
		}
		break;

	case MODE_Nto1:
		for (i = 0; i < sbp->depth; i++, plane <<= 1) {
			if (fb->Pmask & plane)
				break;
		}
		if (i < sbp->depth) {
			ps += (sbp->width * sbp->rect.extent.y * i) << 1;
			mem_to_mem(fb->funcvec[i],
				   smap, ps, sbp->width, dmap, pd, dbp->width,
				   &sr, &dr.origin);
		}
		break;

	case MODE_NtoN:
		for (i = 0; i < dbp->depth; i++) {
			if (fb->Pmask & plane) {
				mem_to_mem(fb->funcvec[i],
					   smap, ps, sbp->width,
					   dmap, pd, dbp->width,
					   &sr, &dr.origin);
			}
			ps += (sbp->width * sbp->rect.extent.y) << 1;
			pd += (dbp->width * dbp->rect.extent.y) << 1;
			plane <<= 1;
		}
		break;

	default:
		return (-1);
	}
#ifdef IPC_MRX
	page_unmap(rommap.fm_vaddr, spages);
	page_unmap(drommap.fm_vaddr, dpages);
#endif
	return (0);
}

bitblt_nop()
{
	return (0);
}

/*
 * bitblt from '0' bitmap to frame buffer
 */

bitblt_0tofb(fb, sbp, srp, dbp, dpp, crp)
	register struct fbdev *fb;
	lBitmap *sbp;		/* source bitmap (0) */
	lRectangle *srp;	/* source rectangle */
	lBitmap *dbp;		/* destination bitmap (FB) */
	lPoint *dpp;		/* destination point */
	lRectangle *crp;	/* clip region in destination */
{
	lRectangle sr;
	lRectangle dr;

	sr = *srp;
	dr.origin = *dpp;
	if (crp && !cliprect2(&sr, &sbp->rect, &dr, crp))
		return (0);
	dr.extent = sr.extent;

	switch (fb->Mode) {

	case MODE_1to1:
	case MODE_Nto1:
		fb->Pmask &= 1;
		break;
	case MODE_1toN:
	case MODE_NtoN:
		break;

	default:
		return (-1);
	}

	/*
	 * write data into ROP data register
	 */

	fbbm_rop_cinit(fb, fb->Pmask, 0);
	fbbm_rop_clear(fb, &dr);

	return (0);
}

/*
 * bitblt from '1' bitmap to frame buffer
 */

bitblt_1tofb(fb, sbp, srp, dbp, dpp, crp)
	register struct fbdev *fb;
	lBitmap *sbp;		/* source bitmap (1) */
	lRectangle *srp;	/* source rectangle */
	lBitmap *dbp;		/* destination bitmap (FB) */
	lPoint *dpp;		/* destination point */
	lRectangle *crp;	/* clip region in destination */
{
	lRectangle sr;
	lRectangle dr;

	sr = *srp;
	dr.origin = *dpp;
	if (crp && !cliprect2(&sr, &sbp->rect, &dr, crp))
		return (0);
	dr.extent = sr.extent;

	switch (fb->Mode) {

	case MODE_1to1:
	case MODE_Nto1:
		/* plane mask set */
		fb->Pmask &= 0x1;
		break;

	case MODE_1toN:
	case MODE_NtoN:
		break;

	default:
		return (-1);
	}

	/*
	 * write data into ROP data register
	 */

	fbbm_rop_cinit(fb, fb->Pmask, 1);
	fbbm_rop_clear(fb, &dr);

	return (0);
}

#ifndef CPU_DOUBLE
/*
 * bitblt from '0' bitmap to main memory
 */

bitblt_0tomem(fb, sbp, srp, dbp, dpp, crp)
	register struct fbdev *fb;
	lBitmap *sbp;		/* source bitmap (0) */
	lRectangle *srp;	/* source rectangle */
	register lBitmap *dbp;	/* destination bitmap (MEM) */
	lPoint *dpp;		/* destination point */
	lRectangle *crp;	/* clip region in destination */
{
	register struct fb_map *dmap;
	register unsigned int p;
	register int i, j;
	register int plane;
	lRectangle sr;
	lRectangle dr;
	int skip;

	dmap = (struct fb_map*)dbp->base;

	sr = *srp;
	dr.origin = *dpp;
	if (crp && !cliprect2(&sr, &sbp->rect, &dr, crp))
		return (0);
	dr.extent = sr.extent;

	dr.origin.x -= dbp->rect.origin.x;
	dr.origin.y -= dbp->rect.origin.y;

	p = dmap->fm_offset;

	plane = 0x1;

	switch (fb->Mode) {

	case MODE_1to1:
		if (fb->Pmask & plane)
			mem_clear(fb->funcvec[0], dmap, p, dbp->width, &dr, 0);
		break;

	case MODE_1toN:
	case MODE_NtoN:
		skip = (dbp->width * dbp->rect.extent.y) << 1;
		for (i = 0, j = dbp->depth; i < j; i++) {
			if (fb->Pmask & plane)
				mem_clear(fb->funcvec[i], dmap, p, dbp->width,
				    &dr, 0);
			/* next plane */
			p += skip;
			plane <<= 1;
		}
		break;

	case MODE_Nto1:
		for (i = 0, j = sbp->depth; i < j; i++) {
			if (fb->Pmask & plane) {
				mem_clear(fb->funcvec[i], dmap, p, dbp->width,
				    &dr, 0);
				break;
			}
			plane <<= 1;
		}
		break;

	default:
		return (1);
	}

	return (0);
}

/*
 * bitblt from '1' bitmap to main memory
 */

bitblt_1tomem(fb, sbp, srp, dbp, dpp, crp)
	register struct fbdev *fb;
	lBitmap *sbp;		/* source bitmap (1) */
	lRectangle *srp;	/* source rectangle */
	register lBitmap *dbp;	/* destination bitmap (MEM) */
	lPoint *dpp;		/* destination point */
	lRectangle *crp;	/* clip region in destination */
{
	register struct fb_map *dmap;
	register unsigned p;
	register int i, j;
	register int plane;
	lRectangle sr;
	lRectangle dr;
	int skip;

	dmap = (struct fb_map*)dbp->base;

	sr = *srp;
	dr.origin = *dpp;
	if (crp && !cliprect2(&sr, &sbp->rect, &dr, crp))
		return (0);
	dr.extent = sr.extent;

	dr.origin.x -= dbp->rect.origin.x;
	dr.origin.y -= dbp->rect.origin.y;

	p = dmap->fm_offset;

	plane = 0x1;

	switch (fb->Mode) {

	case MODE_1to1:
		if (fb->Pmask & plane)
			mem_clear(fb->funcvec[0], dmap, p, dbp->width, &dr, 1);
		break;

	case MODE_1toN:
	case MODE_NtoN:
		skip = (dbp->width * dbp->rect.extent.y) << 1;
		for (i = 0, j = dbp->depth; i < j; i++) {
			if (fb->Pmask & plane)
				mem_clear(fb->funcvec[i], dmap, p, dbp->width,
				    &dr, 1);
			/* next plane */
			p += skip;
			plane <<= 1;
		}
		break;

	case MODE_Nto1:
		for (i = 0, j = sbp->depth; i < j; i++) {
			if (fb->Pmask & plane) {
				mem_clear(fb->funcvec[i], dmap, p, dbp->width,
				    &dr, 1);
				break;
			}
			plane <<= 1;
		}
		break;

	default:
		return (1);
	}

	return (0);
}
#endif /* !CPU_DOUBLE */

int
(*sel_ropfunc(stype, dtype))()
	int stype;	/* source bitmap type */
	int dtype;	/* dest bitmap type */
{
	if (dtype == BM_0)
		return (bitblt_nop);
	if (dtype == BM_1)
		return (bitblt_nop);

#ifdef CPU_DOUBLE
	switch (stype) {
	case BM_FB:
		return (dtype == BM_FB) ? bitblt_fb : bitblt_tomem;
		break;

	case BM_MEM:
		return (dtype == BM_FB) ? bitblt_tofb : bitblt_mem;
		break;

	case BM_0:
		return (dtype == BM_FB) ? bitblt_0tofb : bitblt_nop;
		break;
	case BM_1:
		return (dtype == BM_FB) ? bitblt_1tofb : bitblt_nop;
		break;
	}
#else /* CPU_DOUBLE */
	switch (stype) {
	case BM_FB:
		return (dtype == BM_FB) ? bitblt_fb : bitblt_tomem;
		break;

	case BM_MEM:
		return (dtype == BM_FB) ? bitblt_tofb : bitblt_mem;
		break;

	case BM_0:
		return (dtype == BM_FB) ? bitblt_0tofb : bitblt_0tomem;
		break;
	case BM_1:
		return (dtype == BM_FB) ? bitblt_1tofb : bitblt_1tomem;
		break;
	}
#endif /* CPU_DOUBLE */

	return (bitblt_nop);
}

bitbltcmd(fb, cmd)
	register struct fbdev *fb;
	register lBitblt *cmd;
{
	lRectangle cr;
	int ret;

	cr = cmd->destClip;

	if (!getclip(fb, &cmd->destBitmap, &cr))
		return (0);
	if (!clipsrc(fb, &cmd->srcBitmap))
		return (0);

	if (setrop(fb, cmd->func, cmd->planemask, cmd->fore_color, cmd->aux_color,
		cmd->transp, &cmd->srcBitmap, &cmd->destBitmap) < 0)
		return (FB_RERROR);

	cursorCheck(fb, cmd->srcBitmap.type, &cmd->srcRect,
			cmd->destBitmap.type, &cr);

	ret = (*sel_ropfunc(cmd->srcBitmap.type, cmd->destBitmap.type))
	    (fb, &cmd->srcBitmap, &cmd->srcRect, &cmd->destBitmap, &cmd->destPoint, &cr);

	cursorOn(fb);

	return (FB_ROK);
}

static
batch_bitblt_01tofb(fb, sbp, clip, sdp, n, sw)
	register struct fbdev *fb;
	lBitmap *sbp;	/* source bitmap (MEM) */
	register lRectangle *clip;
	register lSrcDest *sdp;
	register int n;
	int sw;
{
	register void (*rop_clear)();
	lRectangle *srect = &sbp->rect;

	switch (fb->Mode) {

	case MODE_1to1:
	case MODE_Nto1:
		fb->Pmask &= 1;
		break;

	case MODE_1toN:
	case MODE_NtoN:
		break;

	default:
		return (FB_RERROR);
	}
	fbbm_rop_cinit(fb, fb->Pmask, sw);
	rop_clear = fb->fbbm_op->fb_rop_clear;
	while (--n >= 0) {
		lRectangle sr;
		lRectangle dr;

		sr = sdp->srcRect;
		dr.origin = sdp->destPoint;
		if (cliprect2(&sr, srect, &dr, clip))
			(*rop_clear)(fb, &dr);
		sdp++;
	}
	return (FB_ROK);
}

static
batch_bitblt_fb(fb, sbp, clip, sdp, n)
	register struct fbdev *fb;
	register lBitmap *sbp;
	register lRectangle *clip;
	register lSrcDest *sdp;
	register int n;
{
	register int wplane, i, j;
	lRectangle sr;
	lRectangle dr;

	fbbm_rop_init(fb, fb->funcvec);
	switch (fb->Mode) {

	case MODE_1to1:
		fb->Pmask &= 1;
		while (--n >= 0) {
			sr = sdp->srcRect;
			dr.origin = sdp->destPoint;
			if (cliprect2(&sr, &sbp->rect, &dr, clip))
				fbbm_rop_copy(fb, &sr, &dr.origin, 0, fb->Pmask);
			sdp++;
		}
		break;

	case MODE_NtoN:
		while (--n >= 0) {
			sr = sdp->srcRect;
			dr.origin = sdp->destPoint;
			if (cliprect2(&sr, &sbp->rect, &dr, clip))
				fbbm_rop_copy(fb, &sr, &dr.origin, 0, fb->Pmask);
			sdp++;
		}
		break;

	case MODE_1toN:
		while (--n >= 0) {
			sr = sdp->srcRect;
			dr.origin = sdp->destPoint;
			if (cliprect2(&sr, &sbp->rect, &dr, clip))
				fbbm_rop_copy(fb, &sr, &dr.origin, 1, fb->Pmask);
			sdp++;
		}
		break;

	case MODE_Nto1:
		for (; --n >= 0; sdp++) {
			sr = sdp->srcRect;
			dr.origin = sdp->destPoint;
			if (!cliprect2(&sr, &sbp->rect, &dr, clip))
				continue;
			wplane = 1;
			for (i = 0, j = sbp->depth; i < j; i++) {
				if (fb->Pmask & wplane) {
					fbbm_rop_copy(fb, &sr, &dr.origin,
							i + 1, fb->Pmask >> 16);
					break;
				}
				wplane <<= 1;
			}
		}
		break;

	default:
		return (FB_RERROR);
	}
}

static
batch_bitblt_tofb(fb, sbp, dbp, crp, sdp, n)
	register struct fbdev *fb;
	register lBitmap *sbp;	/* source bitmap (MEM) */
	lBitmap *dbp;		/* destination bitmap (FB) */
	lRectangle *crp;	/* clip region in destination */
	register lSrcDest *sdp;
	register int n;
{
	register unsigned p;
	register struct fb_map *smap;
	register int i, j, m;
	lRectangle sr;
	lRectangle dr;
	register int wplane;
#ifdef IPC_MRX
	extern struct fb_map rommap;
	register int pages;
#endif

	fbbm_rop_winit(fb);
	while (--n >= 0) {
		sr = sdp->srcRect;
		dr.origin = sdp->destPoint;
		if (crp && !cliprect2(&sr, &sbp->rect, &dr, crp)) {
			sdp++;
			continue;
		}
		dr.extent = sr.extent;

		/* transform source rectangle */
		sr.origin.x -= sbp->rect.origin.x;
		sr.origin.y -= sbp->rect.origin.y;

		/*
		 * check memory map specification
		 */
		smap = (struct fb_map*)sbp->base;
		p = smap->fm_offset;
#ifdef IPC_MRX
		pages = btoc(smap->fm_offset + smap->fm_count);
		rommap.fm_vaddr = fb_map_page(smap->fm_addr, pages,
			    fb->cache_off ? PG_S|PG_WP|PG_CI : PG_S|PG_WP);
		rommap.fm_offset = 0;
		smap = &rommap;
#endif

		wplane = 1;

		switch (fb->Mode) {
		case MODE_1to1:
			fbbm_rop_write(fb, smap, p, sbp->width,
				       &sr, &dr, fb->Pmask & 0x01);
			break;
		case MODE_1toN:
			fbbm_rop_write(fb, smap, p, sbp->width,
				       &sr, &dr, fb->Pmask);
			break;
		case MODE_Nto1:
			m = sbp->width * sbp->rect.extent.y;
			for (i = 0; i < sbp->depth; i++, wplane <<= 1) {
				if (fb->Pmask & wplane) {
					p += (m * i) << 1;
					fbbm_rop_write(fb, smap, p, sbp->width,
						       &sr, &dr, wplane);
					break;
				}
				wplane <<= 1;
			}
			break;
		case MODE_NtoN:
			j = min(sbp->depth, fb->fbNplane);
			m = sbp->width * sbp->rect.extent.y;
			p += (m << 1) * j;
			wplane = 1 << (j - 1);
			for (i = j; i > 0; i--) {
				/* get next plane */
				p -= m << 1;
				if (fb->Pmask & wplane)
					fbbm_rop_write(fb, smap, p, sbp->width,
						       &sr, &dr, wplane);
				/* next plane mask */
				wplane >>= 1;
			}
			break;
		default:
			return (-1);
		}
		sdp++;
	}
	return (0);
}

batchbitbltcmd(fb, cmd)
	register struct fbdev *fb;
	register lBatchBitblt *cmd;
{
	register int n;
	register lSrcDest *sdp;
	register int (*blt)();
	lRectangle cr;
#ifdef CPU_SINGLE
	struct fb_map *map;
	unsigned int p;
#endif
	int error;

	if (setrop(fb, cmd->func, cmd->planemask,
	    cmd->fore_color, cmd->aux_color,
	    cmd->transp, &cmd->srcBitmap, &cmd->destBitmap) < 0)
		return (FB_RERROR);

	cr = cmd->destClip;

	if (!getclip(fb, &cmd->destBitmap, &cr))
		return (FB_ROK);
	if (!clipsrc(fb, &cmd->srcBitmap))
		return (0);
#ifdef CPU_SINGLE
	map = (struct fb_map *)(cmd->srcDestList);
	p = map->fm_offset;
	sdp = (lSrcDest *)TypeAt(map, p);
#else
	sdp = cmd->srcDestList;
#endif
	n = cmd->nSrcDest;

	cursorCheck(fb, cmd->srcBitmap.type, &cmd->srcBitmap.rect,
	    cmd->destBitmap.type, &cr);

	blt = sel_ropfunc(cmd->srcBitmap.type, cmd->destBitmap.type);
	if (blt == bitblt_0tofb || blt == bitblt_1tofb) {
		if (error =
		    batch_bitblt_01tofb(fb, &cmd->srcBitmap, &cr, sdp, n,
		    blt == bitblt_1tofb)) {
			cursorOn(fb);
			return (error);
		}
	} else if (blt == bitblt_fb) {
		if (error =
		    batch_bitblt_fb(fb, &cmd->srcBitmap, &cr, sdp, n)) {
			cursorOn(fb);
			return (error);
		}
	} else if (blt == bitblt_tofb) {
		if (error =
		    batch_bitblt_tofb(fb, &cmd->srcBitmap, &cmd->destBitmap,
		    &cr, sdp, n)) {
			cursorOn(fb);
			return (error);
		}
	} else
		while (--n >= 0) {
			if ((*blt)(fb, &cmd->srcBitmap, &sdp->srcRect,
			    &cmd->destBitmap, &sdp->destPoint, &cr) < 0) {
				cursorOn(fb);
				return (FB_RERROR);
			}
			PRE_EMPT;
			sdp++;
		}
	cursorOn(fb);
	return (FB_ROK);
}

tilebitbltcmd(fb, cmd)
	struct fbdev *fb;
	register lTileBitblt *cmd;
{
	lRectangle trect, rect, prect;
	lPoint dp;
	register int dx;
	int dy;
	register int offx, offy;
	register int xlen, ylen;
	int first;
	register int (*blt)();
	int t;

	rect = cmd->destRect;
	prect = cmd->ptnRect;

	if (prect.extent.x <= 0 || prect.extent.y <= 0)
		return;

	if (cmd->ptnBitmap.type == BM_FB &&
		!cliprect(&cmd->ptnBitmap.rect, &fb->FrameRect, (lRectangle*)0))
		return;

	/* clip pattern rectangle */
	if (!cliprect(&prect, &cmd->ptnBitmap.rect, (lRectangle *)0))
		return;

	if (!getclip(fb, &cmd->destBitmap, &cmd->destClip)) return;

	if (!cliprect(&rect, &cmd->destClip, (lRectangle *)0))
		return;

	if (setrop(fb, cmd->func, cmd->planemask, cmd->fore_color, cmd->aux_color,
		cmd->transp, &cmd->ptnBitmap, &cmd->destBitmap) < 0)
		return (FB_RERROR);

	blt = sel_ropfunc(cmd->ptnBitmap.type, cmd->destBitmap.type);

	offx = MOD(rect.origin.x - cmd->refPoint.x, prect.extent.x, t);
	offy = MOD(rect.origin.y - cmd->refPoint.y, prect.extent.y, t);

	dp = rect.origin;

	trect.origin.x = prect.origin.x + offx;
	trect.origin.y = prect.origin.y + offy;

	dy = rect.extent.y;

	cursorCheck(fb, cmd->ptnBitmap.type, &prect, cmd->destBitmap.type, &rect);

	first = 1;
	while (dy > 0) {
		if (first) {	/* for the first time */
			ylen = prect.extent.y - offy;
			ylen = min(ylen, dy);
			trect.extent.y = ylen;
			trect.origin.y = prect.origin.y + offy;
			first = 0;
		} else {
			ylen = min(prect.extent.y, dy);
			trect.extent.y = ylen;
			trect.origin.y = prect.origin.y;
		}

		dp.x = rect.origin.x;
		dx = rect.extent.x;
		xlen = prect.extent.x - offx;
		trect.origin.x = prect.origin.x + offx;

		if (dx < xlen) {
			trect.extent.x = dx;
			(*blt)(fb, &cmd->ptnBitmap, &trect, &cmd->destBitmap, &dp, (lRectangle *)0);
		} else {
			trect.extent.x = xlen;
			(*blt)(fb, &cmd->ptnBitmap, &trect, &cmd->destBitmap, &dp, (lRectangle *)0);
			dp.x += xlen;
			dx -= xlen;
			trect.origin.x = prect.origin.x;
			while (dx > 0) {
				xlen = min(dx, prect.extent.x);
				trect.extent.x = xlen;
				(*blt)(fb, &cmd->ptnBitmap, &trect, &cmd->destBitmap, &dp, (lRectangle *)0);
				dp.x += xlen;
				dx -= xlen;
			}
		}

		dp.y += ylen;
		dy -= ylen;
	}

	cursorOn(fb);
}

bitblt3cmd(fb, cmd)
	struct fbdev fb;
	lBitblt3 *cmd;
{
	return (FB_ROK);
}

draw_rectangle(fb, dp)
	struct fbdev *fb;
	lPrimRect *dp;
{
	lRectangle trect, rect, prect;
	lPoint p;
	register int dx;
	int dy;
	register int offx, offy;
	register int xlen, ylen;
	int first;
	register int (*blt)();
	int t;

	rect = dp->rect;
	prect = dp->ptnRect;

	if (prect.extent.x <= 0 || prect.extent.y <= 0)
		return;

	if (dp->ptnBM.type == BM_FB &&
		!cliprect(&dp->ptnBM.rect, &fb->FrameRect, (lRectangle*)0))
		return;

	/* clip pattern rectangle */
	if (!cliprect(&prect, &dp->ptnBM.rect, (lRectangle *)0))
		return;

	if (!getclip(fb, &dp->drawBM, &dp->clip)) return;

	if (!cliprect(&rect, &dp->clip, (lRectangle *)0))
		return;

	if (setrop(fb, dp->func, dp->planemask, dp->fore_color, dp->aux_color,
			dp->transp, &dp->ptnBM, &dp->drawBM) < 0)
		return (FB_RERROR);

	blt = sel_ropfunc(dp->ptnBM.type, dp->drawBM.type);

	offx = MOD(rect.origin.x - dp->refPoint.x, prect.extent.x, t);
	offy = MOD(rect.origin.y - dp->refPoint.y, prect.extent.y, t);

	p = rect.origin;

	trect.origin.x = prect.origin.x + offx;
	trect.origin.y = prect.origin.y + offy;

	dy = rect.extent.y;

	cursorCheck(fb, dp->ptnBM.type, &prect, dp->drawBM.type, &rect);

	first = 1;
	while (dy > 0) {
		if (first) {	/* for the first time */
			ylen = prect.extent.y - offy;
			ylen = min(ylen, dy);
			trect.extent.y = ylen;
			trect.origin.y = prect.origin.y + offy;
			first = 0;
		} else {
			ylen = min(prect.extent.y, dy);
			trect.extent.y = ylen;
			trect.origin.y = prect.origin.y;
		}

		p.x = rect.origin.x;
		dx = rect.extent.x;
		xlen = prect.extent.x - offx;
		trect.origin.x = prect.origin.x + offx;

		if (dx < xlen) {
			trect.extent.x = dx;
			(*blt)(fb, &dp->ptnBM, &trect, &dp->drawBM, &p, (lRectangle *)0);
		} else {
			trect.extent.x = xlen;
			(*blt)(fb, &dp->ptnBM, &trect, &dp->drawBM, &p, (lRectangle *)0);
			p.x += xlen;
			dx -= xlen;
			trect.origin.x = prect.origin.x;
			while (dx > 0) {
				xlen = min(dx, prect.extent.x);
				trect.extent.x = xlen;
				(*blt)(fb, &dp->ptnBM, &trect, &dp->drawBM, &p, (lRectangle *)0);
				p.x += xlen;
				dx -= xlen;
			}
		}

		p.y += ylen;
		dy -= ylen;
	}

	cursorOn(fb);
}

draw_polymarker(fb, dp)
	struct fbdev *fb;
	register lPrimMarker *dp;
{
	register lPoint *ps;
	register int np;
	lRectangle cr;
	register int (*blt)();
#ifdef CPU_SINGLE
	struct fb_map *map;
	unsigned int p;
#endif

	cr = dp->clip;

	if ((dp->drawBM.type == BM_FB) &&
			!getclip(fb, &dp->drawBM, &cr))
		return (FB_ROK);

	if (dp->ptnBM.type == BM_FB &&
		!cliprect(&dp->ptnBM.rect, &fb->FrameRect, (lRectangle*)0))
		return (FB_ROK);

	if (setrop(fb, dp->func, dp->planemask, dp->fore_color, dp->aux_color,
			dp->transp, &dp->ptnBM, &dp->drawBM) < 0)
		return (FB_RERROR);

	blt = sel_ropfunc(dp->ptnBM.type, dp->drawBM.type);

	cursorCheck(fb, dp->ptnBM.type, &(dp->ptnRect), dp->drawBM.type, &cr);

#ifdef CPU_SINGLE
	map = (struct fb_map *)(dp->plist);
	p = map->fm_offset;
	ps = (lPoint *)TypeAt(map, p);
#else
	ps = dp->plist;
#endif
	np = dp->np;
	while (--np >= 0) {
		(*blt)(fb, &dp->ptnBM, &dp->ptnRect, &dp->drawBM, ps++, &cr);
		PRE_EMPT;
	}

	cursorOn(fb);

	return (FB_ROK);
}

static int patternx;
static int patterny;
static int patternwidth;
static lBitmap *pbm;		/* pattern bitmap */
static lBitmap *drawbm;		/* drawing bitmap */
static int (*blt)();

static
fill_line(fb, len, dp, offx, offy)
register struct fbdev *fb;
register int len;
register lPoint *dp;
int offx, offy;
{
	register int plen;
	static lRectangle srec = { 0, 0, 0, 1 };

	srec.origin.x = patternx + offx;
	srec.origin.y = patterny + offy;

	if ((plen = patternwidth - offx) > len) {
		srec.extent.x = len;
		(*blt)(fb, pbm, &srec, drawbm, dp, (lRectangle *)0);
		return;
	}

	srec.extent.x = plen;
	(*blt)(fb, pbm, &srec, drawbm, dp, (lRectangle *)0);
	dp->x += plen;
	len -= plen;
	srec.origin.x = patternx;
	plen = patternwidth;

	while (len > 0) {
		srec.extent.x = min(plen, len);
		(*blt)(fb, pbm, &srec, drawbm, dp, (lRectangle *)0);
		dp->x += plen;
		len -= plen;
	}
}

fill_scan(fb, fdata)
	register struct fbdev *fb;
	register lPrimFill *fdata;
{
	register lScanl	*ls;
	int nscan;
	lRectangle clip;
	lRectangle prect;
	register int minx, maxx, miny, maxy;
#ifdef CPU_SINGLE
	struct fb_map *map;
#endif
	register void (*rop_clear)();
	int (*sel_ropfunc())();

	if ((nscan = fdata->nscan) <= 0)
		return (FB_RERROR);
	
	/* clip pattern rectangle */
	prect = fdata->ptnRect;
	if (!getclip(fb, &fdata->ptnBM, &prect))
		return (0);

	if (prect.extent.x <= 0 || prect.extent.y <= 0)
		return (FB_RERROR);

	/* clip clip rectangle */
	clip = fdata->clip;
	if (!getclip(fb, &fdata->drawBM, &clip))
		return (0);

	if (setrop(fb, fdata->func, fdata->planemask,
	    fdata->fore_color, fdata->aux_color, fdata->transp,
	    &fdata->ptnBM, &fdata->drawBM) < 0)
		return (FB_RERROR);

#ifdef CPU_SINGLE
	map = (struct fb_map *)(fdata->scan);
	ls = (lScanl *)TypeAt(map, map->fm_offset);
#else
	ls = fdata->scan;
#endif

	minx = clip.origin.x;
	maxx = minx + clip.extent.x - 1;
	miny = clip.origin.y;
	maxy = miny + clip.extent.y - 1;

	cursorCheck(fb, fdata->ptnBM.type, &prect, fdata->drawBM.type, &clip);

	blt = sel_ropfunc(fdata->ptnBM.type, fdata->drawBM.type);
	if (blt == bitblt_1tofb || blt == bitblt_0tofb) {
		lRectangle dr;

		if (fb->fbbm_op->fb_rop_fillscan != (void (*)())nofunc) {
			fbbm_rop_fillscan(fb, ls, nscan, &clip,
			    blt == bitblt_1tofb);
			goto out;
		}
		dr.extent.y = 1;
		fbbm_rop_cinit(fb, fb->Pmask, blt == bitblt_1tofb);
		rop_clear = fb->fbbm_op->fb_rop_clear;
		while (--nscan >= 0) {
			if ((dr.origin.y = ls->y) >= miny &&
			    dr.origin.y <= maxy) {
				dr.origin.x = max(ls->x0, minx);
				if ((dr.extent.x =
				    min(ls->x1, maxx) - dr.origin.x + 1) > 0)
					(*rop_clear)(fb, &dr);
			}
			ls++;
		}
	} else {
		int len;
		int refx, refy;
		lPoint dp;
		int sizex, sizey;
		int t;

		sizex = prect.extent.x;
		sizey = prect.extent.y;
		refx = fdata->refPoint.x;
		refy = fdata->refPoint.y;

		patternx = prect.origin.x;
		patterny = prect.origin.y;
		patternwidth = sizex;

		pbm = &fdata->ptnBM;
		drawbm = &fdata->drawBM;

		while (--nscan >= 0) {
			if ((dp.y = ls->y) >= miny && dp.y <= maxy) {
				dp.x = max(ls->x0, minx);
				if ((len = min(ls->x1, maxx) - dp.x + 1) > 0)
					fill_line(fb, len, &dp,
					    MOD((dp.x - refx), sizex, t),
					    MOD((dp.y - refy), sizey, t));
			}
			ls++;
		}
	}
out:
	cursorOn(fb);
	return (FB_ROK);
}

put_string(fb, sdata)
	struct fbdev *fb;
	lPrimText *sdata;
{
	register int x, y;
	register int ex_factor = sdata->ex_factor;
	register unsigned c;
	register unsigned char *str;
	int len = sdata->len;
	int flen;
	int i, j, k, l;
	unsigned fchar = sdata->first_chr;
	unsigned lchar = sdata->last_chr;
	lRectangle cr, save;
	register int (*bltfunc)();
	register char *f_addr;		/* font address */
	register char **fnt_addr;
	static struct fb_map rommap;
#ifdef CPU_SINGLE
	struct fb_map *map;
	unsigned int p;
#endif

	lBitmap *fontBM;
	lRectangle srec;
	lPoint dp;

	extern int tmode;	/* in ../bm/vt100if.c */

	x = sdata->p.x << 16;
	y = sdata->p.y << 16;

	srec.extent.x = sdata->width;
	srec.extent.y = sdata->height;

	switch (sdata->type) {

	case ASCII:
		fontBM = &sdata->fontBM;

		break;

	case ROM_ASCII:
	case ROM_CONS:
		if (sdata->width >= 12 && sdata->height >= 24) {
			if (fb->Krom_BM1.type == (char)0xff) {
				fontBM = &fb->Krom_BM0;
				srec.extent.x = fb->Krom_font_extent0.x>>1;
				srec.extent.y = fb->Krom_font_extent0.y;
				fnt_addr = ext_fnt_addr;
			} else {
				fontBM = &fb->Krom_BM1;
				srec.extent.x = fb->Krom_font_extent1.x>>1;
				srec.extent.y = fb->Krom_font_extent1.y;
				fnt_addr = ext_fnt24_addr;
			}
		} else {
			if (fb->Krom_BM0.type == (char)0xff) {
				fontBM = &fb->Krom_BM1;
				srec.extent.x = fb->Krom_font_extent1.x>>1;
				srec.extent.y = fb->Krom_font_extent1.y;
				fnt_addr = ext_fnt24_addr;
			} else {
				fontBM = &fb->Krom_BM0;
				srec.extent.x = fb->Krom_font_extent0.x>>1;
				srec.extent.y = fb->Krom_font_extent0.y;
				fnt_addr = ext_fnt_addr;
			}
		}

		if (srec.extent.x > sdata->width)
			srec.extent.x = sdata->width;
		if (srec.extent.y > sdata->height)
			srec.extent.y = sdata->height;
		flen = (fontBM->width<<1) * fontBM->rect.extent.y;
		fontBM->base = (Word *)&rommap;
		break;

	case ROM_KANJI:
		if (sdata->width >= 24 && sdata->height >= 24) {
			if (fb->Krom_BM1.type == (char)0xff) {
				fontBM = &fb->Krom_BM0;
				srec.extent = fb->Krom_font_extent0;
				fnt_addr = ext_fnt_addr;
			} else {
				fontBM = &fb->Krom_BM1;
				srec.extent = fb->Krom_font_extent1;
				fnt_addr = ext_fnt24_addr;
			}
		} else {
			if (fb->Krom_BM0.type == (char)0xff) {
				fontBM = &fb->Krom_BM1;
				srec.extent = fb->Krom_font_extent1;
				fnt_addr = ext_fnt24_addr;
			} else {
				fontBM = &fb->Krom_BM0;
				srec.extent = fb->Krom_font_extent0;
				fnt_addr = ext_fnt_addr;
			}
		}

		if (srec.extent.x > sdata->width)
			srec.extent.x = sdata->width;
		if (srec.extent.y > sdata->height)
			srec.extent.y = sdata->height;
		save.extent.x = srec.extent.x;
		flen = (fontBM->width<<1) * fontBM->rect.extent.y;
		fontBM->base = (Word *)&rommap;
		break;

	default:
		return (FB_RERROR);
	}

	/* get clipping rectangle */
	cr = sdata->clip;

	if (!getclip(fb, &sdata->drawBM, &cr))
		return (FB_ROK);

	/* set rop code */
	if (setrop(fb, sdata->func, sdata->planemask,
			sdata->fore_color, sdata->aux_color,
			sdata->transp, fontBM, &sdata->drawBM) < 0)
		return (FB_RERROR);

	/* select rop function */
	bltfunc = sel_ropfunc(fontBM->type, sdata->drawBM.type);

#ifdef CPU_SINGLE
	map = (struct fb_map *)(sdata->str);
	p = map->fm_offset;
	str = (unsigned char *)TypeAt(map, p);
#else
	str = sdata->str;
#endif

	cursorCheck(fb, fontBM->type, &fontBM->rect, sdata->drawBM.type, &cr);

	switch (sdata->type) {

	case ASCII:
		if (sdata->column == 0)
			return (FB_RERROR);
		while (len-- > 0) {
			c = *str++;

			if (c < fchar || c > lchar)
				continue;

			c -= fchar;
			srec.origin.x = sdata->fp.x
				+ sdata->width * (c % sdata->column);
			srec.origin.y = sdata->fp.y
				+ sdata->height * (c / sdata->column);
			dp.x = x >> 16;
			dp.y = y >> 16;

			if (ex_factor == 1) {
				(*bltfunc)(fb, fontBM, &srec, &sdata->drawBM,
					&dp, &cr);
			} else {
				srec.extent.x = 1;

				for (i = 0; i < sdata->width; i++) {
					for (j = 0; j < ex_factor; j++) {
						(*bltfunc)(fb, fontBM, &srec,
							&sdata->drawBM,
							&dp, &cr);
						dp.x++;
						PRE_EMPT;
					}
					srec.origin.x++;
				}
			}
			x += sdata->dx;
			y += sdata->dy;
		}
		break;

	case ROM_ASCII:
	case ROM_CONS:
#ifdef IPC_MRX
		if (fb->type == FB_NWB251)
			fb->cache_off = 1;
#endif
		while (len-- > 0) {
			c = *str++;
			dp.x = x >> 16;
			dp.y = y >> 16;
			k = 0;
			srec.origin.x = srec.origin.y = 0;

			f_addr = 0;

			if ((c >= 0x20) && (c <= 0x7e)) {
				/*
				 * ASCII char
				 */
				f_addr = fnt_addr[c];
				goto disp;
			}

			if (sdata->type == ROM_ASCII) {
				if ((c >= 0xa1) && (c <= 0xdf)) {
					/*
					 * KANA char
					 */
					f_addr = fnt_addr[c + 64];
					goto disp;
				}
			}

			if (sdata->type == ROM_CONS) {
#ifdef KM_ASCII
				if (tmode == KM_ASCII) {
#endif
					if ((c >= 0xa0) && (c <= 0xff)) {
						/*
						 * ISO char
						 */
						f_addr = fnt_addr[c - 32];
						goto disp;
					}
#ifdef KM_ASCII
				} else {
					if ((c >= 0xa1) && (c <= 0xdf)) {
						/*
						 * KANA char
						 */
						f_addr = fnt_addr[c + 64];
						goto disp;
					}
				}
#endif
			}

disp:

			if (f_addr) {
				/*
				 * not ROM font
				 *	(font is in kernel data area)
				 */
				bltfunc = sel_ropfunc(BM_MEM,
						sdata->drawBM.type);
				rommap.fm_vaddr = f_addr;
				rommap.fm_offset = 0;
#ifdef IPC_MRX
				iopmemfbmap(f_addr, flen, &rommap);
#endif
				k = 1;
				l = fontBM->width;
				fontBM->width = 1;
				save = fontBM->rect;
				fontBM->rect.origin = srec.origin;
				fontBM->rect.extent.x = 12;
			} else if (fontBM->type == BM_MEM) {
				/*
				 * KANJI ROM except pop[cm]fb
				 */
				f_addr = fbbm_Krom_addr(fb, c, &srec);
				rommap.fm_vaddr = f_addr;
				rommap.fm_offset = 0;
#ifdef IPC_MRX
				iopmemfbmap(f_addr, flen, &rommap);
#endif
			} else {
				/*
				 * XXX
				 * fontBM->type == BM_FB -> fbbm_pop[cm]
				 *
				 * see fbpop[cm]_setup() routine
				 * in fbbm_pop[cm].c
				 */
				bltfunc = sel_ropfunc(fontBM->type,
							sdata->drawBM.type);

				bzero((caddr_t)fontBM->base,
						sizeof (struct fb_map));
				fbbm_Krom_addr(fb, c, &srec);
				fontBM->rect.origin = srec.origin;
			}

			if (ex_factor == 1) {
				(*bltfunc)(fb, fontBM, &srec, &sdata->drawBM,
					&dp, &cr);
			} else {
				srec.extent.x = 1;

				for (i = 0; i < sdata->width; i++) {

					for (j = 0; j < ex_factor; j++) {
						(*bltfunc)(fb, fontBM, &srec,
							&sdata->drawBM,
							&dp, &cr);
						dp.x++;
					}
					srec.origin.x++;
				}
			}
			PRE_EMPT;
			if (k != 0) {
				fontBM->rect = save;
				fontBM->width = l;
			}
			x += sdata->dx;
			y += sdata->dy;
		}
#ifdef IPC_MRX
		fb->cache_off = 0;
#endif

		break;

	case ROM_KANJI:
#ifdef IPC_MRX
		if (fb->type == FB_NWB251)
			fb->cache_off = 1;
#endif
		while (len > 1) {
			c = *str++;
			c <<= 8;
			c |= *str++;
			dp.x = x >> 16;
			dp.y = y >> 16;
			srec.origin.x = srec.origin.y = 0;

			if (fontBM->type == BM_MEM) {
				/*
				 * KANJI ROM except pop[cm]fb
				 */
				f_addr = fbbm_Krom_addr(fb, c, &srec);
				rommap.fm_vaddr = f_addr;
				rommap.fm_offset = 0;
#ifdef IPC_MRX
				iopmemfbmap(f_addr, flen, &rommap);
#endif
			} else {
				/*
				 * XXX
				 * fontBM->type == BM_FB ---> fbbm_pop[cm]
				 *
				 * see fbpop[cm]_setup() in fbbm_pop[cm].c
				 */
				bzero((caddr_t)fontBM->base,
						sizeof (struct fb_map));
				fbbm_Krom_addr(fb, c, &srec);
				fontBM->rect.origin = srec.origin;
			}

			if (ex_factor == 1) {
				(*bltfunc)(fb, fontBM, &srec, &sdata->drawBM,
					&dp, &cr);
			} else {
				srec.extent.x = 1;
				for (i = 0; i < sdata->width; i++) {
					for (j = 0; j < ex_factor; j++) {
						(*bltfunc)(fb, fontBM, &srec,
							&sdata->drawBM,
							&dp, &cr);
						dp.x++;
					}
					srec.origin.x++;
				}
				srec.extent.x = save.extent.x;
			}
			PRE_EMPT;
			x += sdata->dx;
			y += sdata->dy;
			len -= 2;
		}
#ifdef IPC_MRX
		fb->cache_off = 0;
#endif
		break;

	default:
		cursorOn(fb);
		return (FB_RERROR);
	}

	cursorOn(fb);

	return (FB_ROK);
}

void
linerop(fb, func, fore, aux, trans)
	struct fbdev	*fb;
	register unsigned func;
	register int fore;
	register int aux;
	int trans;
{
	register char *funcv;
	register int i;
	char tmp[4];

	/* set rop function register */
	func &= 0xf;
	tmp[0] = TRANS(trans, (func & 0x0c) | (func >> 2));
	tmp[1] = TRANS(trans, (func >> 2) | ((func << 2) & 0x0c));
	tmp[2] = TRANS(trans, func);
	tmp[3] = TRANS(trans, (func << 2) & 0x0c | func & 3);

	funcv = fb->funcvec;
	for (i = fb->fbNplane; --i >= 0;) {
		*funcv++ = tmp[((fore & 1) << 1) | (aux & 1)];
		fore >>= 1; aux >>= 1;
	}
}

/*
 * line clipping routine
 *
 *	DRAW	visual
 *	NODRAW	not visual
 */
lineclip(p0, p1, r)
	register lPoint *p0;
	register lPoint *p1;
	register lRectangle *r;		/* clipping rectangle */
{
	register lPoint *ptmp;
	register int d0, d1, d2, limit;

	/* sort 2 points by x-coordinate */
	if (p0->x > p1->x) {
		ptmp = p1;
		p1 = p0;
		p0 = ptmp;
	}
	limit = r->origin.x;
	d0 = p1->y - p0->y;
	d1 = p1->x - p0->x;
	if ((d2 = limit - p0->x) > 0) {
		if (p1->x < limit)
			return (NODRAW);
		p0->y += d2 * d0 / d1;
		p0->x = limit;
	}
	limit += r->extent.x - 1;
	if ((d2 = limit - p1->x) < 0) {
		if (p0->x > limit)
			return (NODRAW);
		p1->y += d2 * d0 / d1;
		p1->x = limit;
	}

	/* sort 2 points by y-coordinate */
	if (p0->y > p1->y) {
		ptmp = p1;
		p1 = p0;
		p0 = ptmp;
	}
	limit = r->origin.y;
	d0 = p1->x - p0->x;
	d1 = p1->y - p0->y;
	if ((d2 = limit - p0->y) > 0) {
		if (p1->y < limit)
			return (NODRAW);
		p0->x += d2 * d0 / d1;
		p0->y = limit;
	}
	limit += r->extent.y - 1;
	if ((d2 = limit - p1->y) < 0) {
		if (p0->y > limit)
			return (NODRAW);
		p1->x += d2 * d0 / d1;
		p1->y = limit;
	}
	return (DRAW);
}

#ifndef CPU_DOUBLE
/*
void
point(p, x, s, fp)
	register char *p;
	register int x;
	register int s;
	register char *fp;
{
	x = 7 - (x & 7);
	if ((1 << (3 - (((s & 1) << 1) | ((*p >> x) & 1)))) & *fp)
		*p |= (1 << x);
	else
		*p &= ~(1 << x);
}
*/
#define point(p, x, s, fp) { \
	int xx = 7 - ((x) & 7); \
	if ((1 << (3 - ((((s) & 1) << 1) | ((*(p) >> xx) & 1)))) & *(fp)) \
		*(p) |= (1 << xx); \
	else \
		*(p) &= ~(1 << xx); \
}

mem_vector(fb, p0, p1, mask, dbmp, lpf)
	struct fbdev	*fb;
	lPoint *p0, *p1;
	int mask;		/* plane mask */
	lBitmap *dbmp;		/* drawing bitmap */
	int lpf;		/* if 0, don't draw last point */
{
	register struct fb_map *map = (struct fb_map *)dbmp->base;
	register char *funcv = fb->funcvec;		/* rop function */
	int p = (int)map->fm_offset;
	register int pmask;
	register unsigned int pat;
	register int x = p0->x;
	register int y = p0->y;
	register char *fp;
	int width = dbmp->width << 1;
	int lim;
	int size = width * dbmp->rect.extent.y;
	int ddx, ddy;
	int s, d, c;
	int dx = p1->x - x;
	int dy = p1->y - y;
	int i, j;
	int depth = dbmp->depth;

	/* transformation */
	x -= dbmp->rect.origin.x;
	y -= dbmp->rect.origin.y;

	pat = fb->pat;

	ddx = 1;
	ddy = dbmp->width << 1;
	y = (int)p + y * ddy;

	if (dx == 0)
		ddx = 0;
	else if (dx < 0) {
		dx = -dx;
		ddx = -ddx;
	}

	if (dy == 0)
		ddy = 0;
	else if (dy < 0) {
		dy = -dy;
		ddy = -ddy;
	}
	
	if (dx > dy) {	/* case x */
		lim = dx;
		if (lpf)
			lim++;

		s = -dx;
		d = dx << 1;
		c = dy << 1;

		for (i = lim; i > 0; i--) {
			(int)p = y + (x >> 3);

			pat = (pat << 1) | ((pat & 0x80000000) ? 1: 0);

			fp = funcv;
			pmask = mask;

			for (j = depth; j > 0; j--) {
				if (pmask & 1) {
					point(_TypeAt(map, p), x, pat, fp);
				}

				p += size;
				pmask >>= 1;
				fp++;
			}

			if ((s += c) >= 0) {
				s -= d;
				y += ddy;
			}

			x += ddx;
		}
	} else {			/* case y */
		lim = dy;
		if (lpf)
			lim++;
		s = -dy;
		d = dy << 1;
		c = dx << 1;

		for (i = lim; i > 0; i--) {
			(int)p = y + (x >> 3);
			pat = (pat << 1) | ((pat & 0x80000000) ? 1: 0);

			fp = funcv;
			pmask = mask;

			for (j = depth; j > 0; j--) {
				if (pmask & 1) {
					point(_TypeAt(map, p), x, pat, fp);
				}

				p += size;
				pmask >>= 1;
				fp++;
			}

			if ((s += c) >= 0) {
				s -= d;
				x += ddx;
			}

			y += ddy;
		}
	}
	
	/* rotate pattern */
	pat = fb->pat;

	{
		register int tmp;

		tmp = lim & 31;
		pat = (pat << tmp) | (pat >> (32 - tmp));
	}

	fb->pat = pat;
}
#endif /* !CPU_DOUBLE */

/* polyline drawing */
draw_polyline(fb, dp)
	struct fbdev *fb;
	register lPrimLine *dp;
{
	register lPoint *ps;
	lPoint p0, p1;
	register int np;
	lRectangle clip, *clipp;
#ifdef CPU_SINGLE
	struct fb_map *map;
	unsigned int p;
#endif

	/* clip rectangle */
	clip = dp->clip;

	if (clip.origin.x == -1)
		clipp = 0;
	else {
		clipp = &clip;
		if (!getclip(fb, &dp->drawBM, clipp)) return 0;
	}
#ifdef CPU_SINGLE
	map = (struct fb_map *)(dp->plist);
	p = map->fm_offset;
	ps = (lPoint *)TypeAt(map, p);
#else
	ps = dp->plist;
#endif
	if (dp->drawBM.type == BM_FB) {

		cursorCheck(fb, ~BM_FB, 0, dp->drawBM.type, clipp);
		fbbm_rop_vect(fb, clipp, dp->func, dp->fore_color,
				dp->aux_color, dp->transp, dp->planemask,
				dp->np, ps, dp->lptn, (dp->dlpf)?1:0, 1);
		cursorOn(fb);

		return(FB_ROK);
	}
#ifndef CPU_DOUBLE
	linerop(fb, dp->func, dp->fore_color, dp->aux_color, dp->transp);
	p0 = *ps++;
	np = dp->np - 1;
	fb->pat = dp->lptn;
	if (clipp) {
		while (--np > 0) {
			p1 = *ps;
			if (lineclip(&p0, &p1, clipp)) {
				mem_vector(fb, &p0, &p1,
					dp->planemask, &dp->drawBM,
					ps->x != p1.x || ps->y != p1.y);
				PRE_EMPT;
			}
			p0 = *ps++;
		}
		p1 = *ps;
		if (lineclip(&p0, &p1, clipp)) {
			mem_vector(fb, &p0, &p1, dp->planemask, &dp->drawBM,
				ps->x != p1.x || ps->y != p1.y || dp->dlpf);
		}
	} else {
		while (--np > 0) {
			p1 = *ps;
			mem_vector(fb, &p0, &p1, dp->planemask, &dp->drawBM, 0);
			PRE_EMPT;
			p0 = *ps++;
		}
		p1 = *ps;
		mem_vector(fb, &p0, &p1, dp->planemask, &dp->drawBM, dp->dlpf);
	}
#endif /* !CPU_DOUBLE */
	return (FB_ROK);
}

/* disjoint polyline drawing */

draw_dj_polyline(fb, dp)
	struct fbdev *fb;
	register lPrimLine *dp;
{
	register lPoint *ps;
	lPoint p0, p1;
	register int np;
	lRectangle clip, *clipp;
#ifdef CPU_SINGLE
	struct fb_map *map;
	unsigned int p;
#endif

	int lpf = (dp->dlpf)?1:0;

	/* clip rectangle */
	clip = dp->clip;

	if (clip.origin.x == -1)
		clipp = 0;
	else {
		clipp = &clip;
		if(!getclip(fb, &dp->drawBM, clipp)) return (0);
	}
#ifdef CPU_SINGLE
	map = (struct fb_map *)(dp->plist);
	p = map->fm_offset;
	ps = (lPoint *)TypeAt(map, p);
#else
	ps = dp->plist;
#endif
	if (dp->drawBM.type == BM_FB) {

		cursorCheck(fb, ~BM_FB, 0, dp->drawBM.type, clipp);
		fbbm_rop_vect(fb, clipp, dp->func, dp->fore_color,
				dp->aux_color, dp->transp, dp->planemask,
						dp->np, ps, dp->lptn, lpf, 0);
		cursorOn(fb);
		PRE_EMPT;

		return (FB_ROK);
	}
#ifndef CPU_DOUBLE
	linerop(fb, dp->func, dp->fore_color, dp->aux_color, dp->transp);
	np = dp->np >> 1;
	if (lpf) {
		if (clipp) {
			while (--np >= 0) {
				p0 = *ps++;
				p1 = *ps++;
				fb->pat = dp->lptn;
				if (lineclip(&p0, &p1, clipp)) {
					mem_vector(fb, &p0, &p1,
						dp->planemask, &dp->drawBM, 1);
					PRE_EMPT;
				}
			}
		} else {
			while (--np >= 0) {
				p0 = *ps++;
				p1 = *ps++;
				fb->pat = dp->lptn;
				mem_vector(fb, &p0, &p1,
					dp->planemask, &dp->drawBM, 1);
				PRE_EMPT;
			}
		}
	} else {
		if (clipp) {
			while (--np >= 0) {
				p0 = *ps++;
				p1 = *ps;
				fb->pat = dp->lptn;
				if (lineclip(&p0, &p1, clipp)) {
					mem_vector(fb, &p0, &p1,
						dp->planemask, &dp->drawBM,
						ps->x != p1.x || ps->y != p1.y);
					PRE_EMPT;
				}
				ps++;
			}
		} else {
			while (--np >= 0) {
				p0 = *ps++;
				p1 = *ps++;
				fb->pat = dp->lptn;
				mem_vector(fb, &p0, &p1,
					dp->planemask, &dp->drawBM, 0);
				PRE_EMPT;
			}
		}
	}
#endif /* !CPU_DOUBLE */
	return (FB_ROK);
}

static lRectangle	dotRect = {{ 0, 0 }, { 1, 1 }};

emulate_polydot(fb, dp)
	struct fbdev *fb;
	register lPrimDot *dp;
{
	lPrimMarker marker;
	lPrimMarker *cmdp;
	register lPoint *ps;
	register int np;
	lRectangle cr;
	register int (*blt)();
#ifdef CPU_SINGLE
	struct fb_map *map;
	unsigned int p;
#endif

	cmdp = &marker;

	cmdp->func = dp->func;
        cmdp->transp = dp->transp;
        cmdp->fore_color = dp->fore_color;
        cmdp->aux_color = dp->aux_color;
        cmdp->planemask = dp->planemask;
        cmdp->ptnRect = dotRect;
        cmdp->ptnBM.type = BM_1;
        cmdp->ptnBM.depth = 1;
        cmdp->ptnBM.rect = dotRect;
        cmdp->drawBM = dp->drawBM;
        cmdp->clip = dp->clip;
        cmdp->np = dp->np;
        cmdp->plist = dp->plist;

	return (draw_polymarker(fb, cmdp));
}

#ifndef CPU_DOUBLE
mem_dot(fb, p0, mask, dbmp)
	struct fbdev	*fb;
	lPoint		*p0;
	register int	mask;		/* plane mask */
	lBitmap		*dbmp;		/* drawing bitmap */
{
	register struct fb_map *map = (struct fb_map *)dbmp->base;
	register char *funcv;	/* rop function */
	register int p = (int)map->fm_offset;
	register int depth;
	int size;
	int x, y;

	x = p0->x - dbmp->rect.origin.x;
	y = p0->y - dbmp->rect.origin.y;

	size = (dbmp->width * dbmp->rect.extent.y) << 1;

	p += y * (dbmp->width << 1) + (x >> 3);

	funcv = fb->funcvec;
	for (depth = dbmp->depth; --depth >= 0;) {
		if (mask & 1) {
			point(_TypeAt(map, p), x, ~0, funcv);
		}
		p += size;
		mask >>= 1;
		funcv++;
	}
}
#endif /* !CPU_DOUBLE */

draw_polydot(fb, dp)
	struct fbdev *fb;
	register lPrimDot *dp;
{
	register lPoint *ps;
	lRectangle clip, *clipp;
	register int np;
#ifdef CPU_SINGLE
	struct fb_map *map;
	unsigned int p;
#endif

	if (fb->fbbm_op->fb_rop_dot == (void (*)())nofunc)
		return (emulate_polydot(fb, dp));
		
	/* clip rectangle */
	clip = dp->clip;

	if (clip.origin.x == -1)
		clipp = 0;
	else {
		clipp = &clip;
		if (!getclip(fb, &dp->drawBM, clipp)) return 0;
	}

#ifdef CPU_SINGLE
	map = (struct fb_map *)(dp->plist);
	p = map->fm_offset;
	ps = (lPoint *)TypeAt(map, p);
#else
	ps = dp->plist;
#endif

	if (dp->drawBM.type == BM_FB) {
		cursorCheck(fb, ~BM_FB, 0, dp->drawBM.type, clipp);
		fbbm_rop_dot(fb, clipp, dp->func, dp->fore_color,
				dp->aux_color, dp->transp, dp->planemask,
				dp->np, ps);
		cursorOn(fb);

		return(FB_ROK);
	}
#ifndef CPU_DOUBLE
	linerop(fb, dp->func, dp->fore_color, dp->aux_color, dp->transp);

	np = dp->np;
	if (clipp) {
		register int x0, y0, x1, y1;

		x0 = clipp->origin.x;
		y0 = clipp->origin.y;
		x1 = x0 + clipp->extent.x - 1;
		y1 = y0 + clipp->extent.y - 1;
		if (x1 <= 0 || y1 <= 0) return;

		while (--np >= 0) {
			if ((ps->x >= x0) && (ps->y >= y0) &&
			    (ps->x <= x1) && (ps->y <= y1)) {
				mem_dot(fb, ps, dp->planemask, &dp->drawBM);
				PRE_EMPT;
			}
			ps++;
		}
	} else {
		while (--np >= 0) {
			mem_dot(fb, ps, dp->planemask, &dp->drawBM);
			PRE_EMPT;
			ps++;
		}
	}
#endif /* !CPU_DOUBLE */
	return (FB_ROK);
}

get_scrtype(fb, cmd)
	register struct fbdev *fb;
	register lScrType *cmd;
{
	cmd->colorwidth = fb->Colorwidth;
	cmd->plane = fb->fbNplane;
	cmd->bufferrect = fb->FrameRect;
	cmd->visiblerect = fb->VisRect;
	cmd->type = fb->type;
	cmd->unit = fb->unit;

	return FB_ROK;
}

fbstart(fbaddr, dummy)
	register struct fbreg *fbaddr;
	int dummy;
{
	register struct fbdev *fb = &fbdev[fbaddr->fb_device];
	register int s;

	FB_LOCK;

	if (!fb) {
		return (FB_RERROR);
	}

	/* reset dimmer count */
	rst_dimmer_cnt();

	switch(fbaddr->fb_command) {
	case FB_CPROBE:
		fbaddr->fb_data = search_fbdev(fbaddr->fb_device,
						fbaddr->fb_unit);
		fbaddr->fb_result = FB_ROK;
		break;
	case FB_CATTACH:
		fbaddr->fb_result = get_scrtype(fb, &fbaddr->fb_scrtype);
		break;
	case FB_COPEN:
		fbaddr->fb_result = fbbm_open(fb);
		break;
	case FB_CCLOSE:
		fbaddr->fb_result = fbbm_close(fb);
		break;
	case FB_CSETDIM:
		fbaddr->fb_result = fbbm_set_dimmer(fb, fbaddr->fb_data);
		break;
	case FB_CGETDIM:
		if ((fbaddr->fb_data = fbbm_get_dimmer(fb)) == FB_RERROR)
			fbaddr->fb_result = FB_RERROR;
		else
			fbaddr->fb_result = FB_ROK;
		break;
	case FB_CBITBLT:
		fbaddr->fb_result = bitbltcmd(fb, &fbaddr->fb_bitblt);
		break;
	case FB_CBATCHBITBLT:
		fbaddr->fb_result = batchbitbltcmd(fb, &fbaddr->fb_batchbitblt);
		break;
	case FB_CTILEBITBLT:
		fbaddr->fb_result = tilebitbltcmd(fb, &fbaddr->fb_tilebitblt);
		break;
	case FB_CBITBLT3:
		fbaddr->fb_result = bitblt3cmd(fb, &fbaddr->fb_bitblt3);
		break;
	case FB_CPOLYLINE:
		fbaddr->fb_result = draw_polyline(fb, &fbaddr->fb_polyline);
		break;
	case FB_CDJPOLYLINE:
		fbaddr->fb_result = draw_dj_polyline(fb, &fbaddr->fb_polyline);
		break;
	case FB_CRECTANGLE:
		fbaddr->fb_result = draw_rectangle(fb, &fbaddr->fb_rectangle);
		break;
	case FB_CFILLSCAN:
		fbaddr->fb_result = fill_scan(fb, &fbaddr->fb_fillscan);
		break;
	case FB_CPOLYMARKER:
		fbaddr->fb_result = draw_polymarker(fb, &fbaddr->fb_polymarker);
		break;
	case FB_CTEXT:
		fbaddr->fb_result = put_string(fb, &fbaddr->fb_text);
		break;
	case FB_CPOLYDOT:
		fbaddr->fb_result = draw_polydot(fb, &fbaddr->fb_polydot);
		break;
	case FB_CGETSCRTYPE:
		fbaddr->fb_result = get_scrtype(fb, &fbaddr->fb_scrtype);
		break;
	case FB_CSETPALETTE:
		fbaddr->fb_result = fbbm_set_palette(fb, &fbaddr->fb_palette);
		break;
	case FB_CGETPALETTE:
		fbaddr->fb_result = fbbm_get_palette(fb, &fbaddr->fb_palette);
		break;
	case FB_CSETCURSOR:
		fbaddr->fb_result = setCursor(fb, &fbaddr->fb_cursor);
		break;
	case FB_CUNSETCURSOR:
		fbaddr->fb_result = setCursor(fb, NULL);
		break;
	case FB_CSHOWCURSOR:
		fbaddr->fb_result = showCursor(fb);
		break;
	case FB_CHIDECURSOR:
		fbaddr->fb_result = hideCursor(fb);
		break;
	case FB_CSETXY:
		fbaddr->fb_result = moveCursor(fb, &fbaddr->fb_point);
		break;
	case FB_CAUTODIM:
		if (fbaddr->fb_data)
			auto_dimmer_on(fb);
		else
			auto_dimmer_off(fb);
		fbaddr->fb_result = FB_ROK;
		break;
	case FB_CSETVIDEO:
		fbaddr->fb_result =
			fbbm_ioctl(fb, FB_SETVIDEOCTL, &fbaddr->fb_videoctl);
		break;
	case FB_CGETVIDEO:
		fbaddr->fb_result =
			fbbm_ioctl(fb, FB_GETVIDEOSTATUS, &fbaddr->fb_videostatus);
		break;
	case FB_CSETPMODE:
		fbaddr->fb_result =
			fbbm_ioctl(fb, FB_SETPALETTEMODE, &fbaddr->fb_data);
		break;
	case FB_CGETPMODE:
		fbaddr->fb_result =
			fbbm_ioctl(fb, FB_GETPALETTEMODE, &fbaddr->fb_data);
		break;
#ifdef CPU_SINGLE
	case FB_CGETPAGE:
		fbaddr->fb_data = fbbm_get_page(fb, fbaddr->fb_data);
		if (fbaddr->fb_data == -1)
			fbaddr->fb_result = FB_RERROR;
		else
			fbaddr->fb_result = FB_ROK;
		break;
#endif
	case FB_CIOCTL:
		fbaddr->fb_fbioctl.request = fbbm_ioctl(fb,
			fbaddr->fb_fbioctl.request, fbaddr->fb_fbioctl.param);
		if (fbaddr->fb_fbioctl.request == -1)
			fbaddr->fb_result = FB_RERROR;
		else
			fbaddr->fb_result = FB_ROK;
		break;

	default:
		fbaddr->fb_result = FB_RERROR;
		break;
	}

#ifdef CPU_SINGLE
	if (cfb && curs_pending) {
		curs_pending = 0;
		redrawCursor(cfb);
	}
#endif

	FB_UNLOCK;
}
