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
 * from: Utah $Hdr: ite_hy.c 1.2 92/12/20$
 *
 *	@(#)ite_hy.c	7.4 (Berkeley) %G%
 */

#include "ite.h"
#if NITE > 0

#include <sys/param.h>
#include <sys/conf.h>
#include <sys/proc.h>
#include <sys/ioctl.h>
#include <sys/tty.h>
#include <sys/systm.h>
#include <sys/uio.h>

#include <hp300/dev/grf_hyreg.h>
#include <hp/dev/itereg.h>
#include <hp/dev/itevar.h>

#include <machine/cpu.h>

/* XXX */
#include <hp/dev/grfioctl.h>
#include <hp/dev/grfvar.h>

#define REGBASE	    	((struct hyboxfb *)(ip->regbase))
#define WINDOWMOVER 	hyper_windowmove

#undef charX
#define	charX(ip,c)	\
	(((c) % (ip)->cpl) * ((((ip)->ftwidth + 7) / 8) * 8) + (ip)->fontx)

hyper_init(ip)
	register struct ite_softc *ip;
{
	int width;

	/* XXX */
	if (ip->regbase == NULL) {
		struct grf_softc *gp = ip->grf;

		ip->regbase = gp->g_regkva;
		ip->fbbase = gp->g_fbkva;
		ip->fbwidth = gp->g_display.gd_fbwidth;
		ip->fbheight = gp->g_display.gd_fbheight;
		ip->dwidth = gp->g_display.gd_dwidth;
		ip->dheight = gp->g_display.gd_dheight;
	}

	ite_fontinfo(ip);
	width = ((ip->ftwidth + 7) / 8) * 8;
	ip->cpl      = (ip->fbwidth - ip->dwidth) / width;
	ip->cblanky  = ip->fonty + ((128 / ip->cpl) +1) * ip->ftheight;

	/*
	 * Clear the framebuffer on all planes.
	 */
	hyper_windowmove(ip, 0, 0, 0, 0, ip->fbheight, ip->fbwidth, RR_CLEAR);

	hyper_ite_fontinit(ip);

	REGBASE->nblank = 0x05;

	/*
	 * Stash the inverted cursor.
	 */
	hyper_windowmove(ip, charY(ip, ' '), charX(ip, ' '),
			 ip->cblanky, ip->cblankx, ip->ftheight,
			 ip->ftwidth, RR_COPYINVERTED);
}

hyper_deinit(ip)
	register struct ite_softc *ip;
{
	hyper_windowmove(ip, 0, 0, 0, 0, ip->fbheight, ip->fbwidth, RR_CLEAR);

	REGBASE->nblank = 0x05;
   	ip->flags &= ~ITE_INITED;
}

hyper_ite_fontinit(ip)
	register struct ite_softc *ip;
{
	register u_char *fbmem, *dp;
	int c, l, b;
	int stride, width;

	dp = (u_char *)(getword(ip, getword(ip, FONTROM) + FONTADDR) +
	    ip->regbase) + FONTDATA;
	stride = ip->fbwidth >> 3;
	width = (ip->ftwidth + 7) / 8;

	for (c = 0; c < 128; c++) {
		fbmem = (u_char *) FBBASE +
			(ip->fonty + (c / ip->cpl) * ip->ftheight) *
			stride;
		fbmem += (ip->fontx >> 3) + (c % ip->cpl) * width;
		for (l = 0; l < ip->ftheight; l++) {
			for (b = 0; b < width; b++) {
				*fbmem++ = *dp;
				dp += 2;
			}
			fbmem -= width;
			fbmem += stride;
		}
	}
}

hyper_putc(ip, c, dy, dx, mode)
	register struct ite_softc *ip;
	int c, dy, dx, mode;
{
        int wmrr = ((mode == ATTR_INV) ? RR_COPYINVERTED : RR_COPY);
	
	hyper_windowmove(ip, charY(ip, c), charX(ip, c),
			 dy * ip->ftheight, dx * ip->ftwidth,
			 ip->ftheight, ip->ftwidth, wmrr);
}

hyper_cursor(ip, flag)
	register struct ite_softc *ip;
	register int flag;
{
	if (flag == DRAW_CURSOR)
		draw_cursor(ip)
	else if (flag == MOVE_CURSOR) {
		erase_cursor(ip)
		draw_cursor(ip)
	}
	else
		erase_cursor(ip)
}

hyper_clear(ip, sy, sx, h, w)
	register struct ite_softc *ip;
	register int sy, sx, h, w;
{
	hyper_windowmove(ip, sy * ip->ftheight, sx * ip->ftwidth,
			 sy * ip->ftheight, sx * ip->ftwidth, 
			 h  * ip->ftheight, w  * ip->ftwidth,
			 RR_CLEAR);
}

hyper_scroll(ip, sy, sx, count, dir)
        register struct ite_softc *ip;
        register int sy, count;
        int dir, sx;
{
	register int dy;
	register int dx = sx;
	register int height = 1;
	register int width = ip->cols;

	if (dir == SCROLL_UP) {
		dy = sy - count;
		height = ip->rows - sy;
	}
	else if (dir == SCROLL_DOWN) {
		dy = sy + count;
		height = ip->rows - dy - 1;
	}
	else if (dir == SCROLL_RIGHT) {
		dy = sy;
		dx = sx + count;
		width = ip->cols - dx;
	}
	else {
		dy = sy;
		dx = sx - count;
		width = ip->cols - sx;
	}		

	hyper_windowmove(ip, sy * ip->ftheight, sx * ip->ftwidth,
			 dy * ip->ftheight, dx * ip->ftwidth,
			 height * ip->ftheight,
			 width  * ip->ftwidth, RR_COPY);
}

#include <hp300/dev/maskbits.h>

/* NOTE:
 * the first element in starttab could be 0xffffffff.  making it 0
 * lets us deal with a full first word in the middle loop, rather
 * than having to do the multiple reads and masks that we'd
 * have to do if we thought it was partial.
 */
int starttab[32] =
    {
	0x00000000,
	0x7FFFFFFF,
	0x3FFFFFFF,
	0x1FFFFFFF,
	0x0FFFFFFF,
	0x07FFFFFF,
	0x03FFFFFF,
	0x01FFFFFF,
	0x00FFFFFF,
	0x007FFFFF,
	0x003FFFFF,
	0x001FFFFF,
	0x000FFFFF,
	0x0007FFFF,
	0x0003FFFF,
	0x0001FFFF,
	0x0000FFFF,
	0x00007FFF,
	0x00003FFF,
	0x00001FFF,
	0x00000FFF,
	0x000007FF,
	0x000003FF,
	0x000001FF,
	0x000000FF,
	0x0000007F,
	0x0000003F,
	0x0000001F,
	0x0000000F,
	0x00000007,
	0x00000003,
	0x00000001
    };

int endtab[32] =
    {
	0x00000000,
	0x80000000,
	0xC0000000,
	0xE0000000,
	0xF0000000,
	0xF8000000,
	0xFC000000,
	0xFE000000,
	0xFF000000,
	0xFF800000,
	0xFFC00000,
	0xFFE00000,
	0xFFF00000,
	0xFFF80000,
	0xFFFC0000,
	0xFFFE0000,
	0xFFFF0000,
	0xFFFF8000,
	0xFFFFC000,
	0xFFFFE000,
	0xFFFFF000,
	0xFFFFF800,
	0xFFFFFC00,
	0xFFFFFE00,
	0xFFFFFF00,
	0xFFFFFF80,
	0xFFFFFFC0,
	0xFFFFFFE0,
	0xFFFFFFF0,
	0xFFFFFFF8,
	0xFFFFFFFC,
	0xFFFFFFFE
    };

hyper_windowmove(ip, sy, sx, dy, dx, h, w, func)
	struct ite_softc *ip;
	int sy, sx, dy, dx, h, w, func;
{
	int width;		/* add to get to same position in next line */

	unsigned int *psrcLine, *pdstLine;
                                /* pointers to line with current src and dst */
	register unsigned int *psrc;  /* pointer to current src longword */
	register unsigned int *pdst;  /* pointer to current dst longword */

                                /* following used for looping through a line */
	unsigned int startmask, endmask;  /* masks for writing ends of dst */
	int nlMiddle;		/* whole longwords in dst */
	register int nl;	/* temp copy of nlMiddle */
	register unsigned int tmpSrc;
                                /* place to store full source word */
	register int xoffSrc;	/* offset (>= 0, < 32) from which to
                                   fetch whole longwords fetched
                                   in src */
	int nstart;		/* number of ragged bits at start of dst */
	int nend;		/* number of ragged bits at end of dst */
	int srcStartOver;	/* pulling nstart bits from src
                                   overflows into the next word? */

	if (h == 0 || w == 0)
		return;

	width = ip->fbwidth >> 5;

	if (sy < dy) /* start at last scanline of rectangle */
	{
	    psrcLine = ((unsigned int *) ip->fbbase) + ((sy+h-1) * width);
	    pdstLine = ((unsigned int *) ip->fbbase) + ((dy+h-1) * width);
	    width = -width;
	}
	else /* start at first scanline */
	{
	    psrcLine = ((unsigned int *) ip->fbbase) + (sy * width);
	    pdstLine = ((unsigned int *) ip->fbbase) + (dy * width);
	}

	/* x direction doesn't matter for < 1 longword */
	if (w <= 32)
	{
	    int srcBit, dstBit;     /* bit offset of src and dst */

	    pdstLine += (dx >> 5);
	    psrcLine += (sx >> 5);
	    psrc = psrcLine;
	    pdst = pdstLine;

	    srcBit = sx & 0x1f;
	    dstBit = dx & 0x1f;

	    while(h--)
	    {
                getandputrop(psrc, srcBit, dstBit, w, pdst, func)
	        pdst += width;
		psrc += width;
	    }
	}
	else
        {
	    maskbits(dx, w, startmask, endmask, nlMiddle)
	    if (startmask)
	      nstart = 32 - (dx & 0x1f);
	    else
	      nstart = 0;
	    if (endmask)
	      nend = (dx + w) & 0x1f;
	    else
	      nend = 0;

	    xoffSrc = ((sx & 0x1f) + nstart) & 0x1f;
	    srcStartOver = ((sx & 0x1f) + nstart) > 31;

	    if (sx >= dx) /* move left to right */
	    {
	        pdstLine += (dx >> 5);
		psrcLine += (sx >> 5);

		while (h--)
		{
		    psrc = psrcLine;
		    pdst = pdstLine;

		    if (startmask)
		    {
			getandputrop(psrc, (sx & 0x1f),
				     (dx & 0x1f), nstart, pdst, func)
			    pdst++;
			if (srcStartOver)
			    psrc++;
		    }

		    /* special case for aligned operations */
		    if (xoffSrc == 0)
		    {
			nl = nlMiddle;
			while (nl--)
			{
			    DoRop (*pdst, func, *psrc++, *pdst);
			    pdst++;
			}
		    }
		    else
		    {
			nl = nlMiddle + 1;
			while (--nl)
			{
			    getunalignedword (psrc, xoffSrc, tmpSrc)
				DoRop (*pdst, func, tmpSrc, *pdst);
			    pdst++;
			    psrc++;
			}
		    }

		    if (endmask)
		    {
			getandputrop0(psrc, xoffSrc, nend, pdst, func);
		    }

		    pdstLine += width;
		    psrcLine += width;
		}
	    }
	    else /* move right to left */
	    {
		pdstLine += (dx+w >> 5);
		psrcLine += (sx+w >> 5);
		/* if fetch of last partial bits from source crosses
		   a longword boundary, start at the previous longword
		   */
		if (xoffSrc + nend >= 32)
		    --psrcLine;

		while (h--)
		{
		    psrc = psrcLine;
		    pdst = pdstLine;

		    if (endmask)
		    {
			getandputrop0(psrc, xoffSrc, nend, pdst, func);
		    }

		    nl = nlMiddle + 1;
		    while (--nl)
		    {
			--psrc;
			--pdst;
			getunalignedword(psrc, xoffSrc, tmpSrc)
                        DoRop(*pdst, func, tmpSrc, *pdst);
		    }

		    if (startmask)
		    {
			if (srcStartOver)
			    --psrc;
			--pdst;
			getandputrop(psrc, (sx & 0x1f),
				     (dx & 0x1f), nstart, pdst, func)
                    }

		    pdstLine += width;
		    psrcLine += width;
		}
	    } /* move right to left */
	}
}
#endif
