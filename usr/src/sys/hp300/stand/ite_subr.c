/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
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
 * from: Utah $Hdr: ite_subr.c 1.2 92/01/20$
 *
 *	@(#)ite_subr.c	8.1 (Berkeley) 6/10/93
 */

#include <hp300/stand/samachdep.h>

#ifdef ITECONSOLE

#include <sys/param.h>
#include <hp/dev/itevar.h>
#include <hp/dev/itereg.h>

ite_fontinfo(ip)
	struct ite_softc *ip;
{
	u_long fontaddr = getword(ip, getword(ip, FONTROM) + FONTADDR);

	ip->ftheight = getbyte(ip, fontaddr + FONTHEIGHT);
	ip->ftwidth  = getbyte(ip, fontaddr + FONTWIDTH);
	ip->rows     = ip->dheight / ip->ftheight;
	ip->cols     = ip->dwidth / ip->ftwidth;

	if (ip->fbwidth > ip->dwidth) {
		/*
		 * Stuff goes to right of display.
		 */
		ip->fontx    = ip->dwidth;
		ip->fonty    = 0;
		ip->cpl      = (ip->fbwidth - ip->dwidth) / ip->ftwidth;
		ip->cblankx  = ip->dwidth;
		ip->cblanky  = ip->fonty + ((128 / ip->cpl) +1) * ip->ftheight;
	}
	else {
		/*
		 * Stuff goes below the display.
		 */
		ip->fontx   = 0;
		ip->fonty   = ip->dheight;
		ip->cpl     = ip->fbwidth / ip->ftwidth;
		ip->cblankx = 0;
		ip->cblanky = ip->fonty + ((128 / ip->cpl) + 1) * ip->ftheight;
	}
}

ite_fontinit(ip)
	register struct ite_softc *ip;
{
	int bytewidth = (((ip->ftwidth - 1) / 8) + 1);
	int glyphsize = bytewidth * ip->ftheight;
	u_char fontbuf[500];
	u_char *dp, *fbmem;
	int c, i, romp;

	romp = getword(ip, getword(ip, FONTROM) + FONTADDR) + FONTDATA;
	for (c = 0; c < 128; c++) {
		fbmem = (u_char *)
		    (FBBASE +
		     (ip->fonty + (c / ip->cpl) * ip->ftheight) * ip->fbwidth +
		     (ip->fontx + (c % ip->cpl) * ip->ftwidth));
		dp = fontbuf;
		for (i = 0; i < glyphsize; i++) {
			*dp++ = getbyte(ip, romp);
			romp += 2;
		}
		writeglyph(ip, fbmem, fontbuf);
	}
}

/*
 * Display independent versions of the readbyte and writeglyph routines.
 */
u_char
ite_readbyte(ip, disp)
	struct ite_softc *ip;
	int disp;
{
	return((u_char) *(((u_char *)ip->regbase) + disp));
}

ite_writeglyph(ip, fbmem, glyphp)
	register struct ite_softc *ip;
	register u_char *fbmem, *glyphp;
{
	register int bn;
	int c, l, b;

	for (l = 0; l < ip->ftheight; l++) {
		bn = 7;
		for (b = 0; b < ip->ftwidth; b++) {
			if ((1 << bn) & *glyphp)
				*fbmem++ = 1;
			else
				*fbmem++ = 0;
			if (--bn < 0) {
				bn = 7;
				glyphp++;
			}
		}
		if (bn < 7)
			glyphp++;
		fbmem -= ip->ftwidth;
		fbmem += ip->fbwidth;
	}
}
#endif
