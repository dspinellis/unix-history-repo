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
 * from: $Hdr: askrom.c,v 4.300 91/06/09 06:32:39 root Rel41 $ SONY
 *
 *	@(#)askrom.c	8.1 (Berkeley) 6/10/93
 */

#include "nwb514.h"
#include "nwb251.h"
#include "nwb256.h"

#if NNWB514 > 0 || NNWB251 > 0 || NNWB256 > 0

#ifdef IPC_MRX
#include "../../h/param.h"
#include "../../iop/framebuf.h"
#include "../../iop/fbreg.h"
#else
#include <sys/types.h>
#include <sys/param.h>
#include <news3400/iop/framebuf.h>
#include <news3400/iop/fbreg.h>
#endif

#include <news3400/fb/fbdefs.h>

#include <news3400/fb/qpdm.h>

extern short zero[];

extern char *ext_fnt_addr[];
extern char *ext_fnt24_addr[];

int krom_enable = 0;

caddr_t
autos_Krom_addr(fb, c, sr)
	struct fbdev *fb;
	register int c;
	lRectangle *sr;
{
	unsigned int cvcode24();

	if ((c >= 0x20) && (c <= 0x7e)) {
		/*
		 * ASCII char
		 */
		c -= ' ';
		c = ((c & 0x1f) | ((c & 0xe0) << 2)) << 7;
		return ((caddr_t)(c + QP_KJROM + (sr->extent.y > 16 ? 0 : 96)));
	} else if ((c >= 0xa1) && (c <= 0xdf)) {
		/*
		 * KANA char
		 */
		if (sr->extent.y > 16)
			return ((caddr_t)ext_fnt24_addr[c + 64]);
		else
			return ((caddr_t)ext_fnt_addr[c + 64]);
	} else if ((c >= 0x2000) && (c <= 0x7fff)) {
		/*
		 * KANJI char
		 */
		switch (c & 0x7000) {
		case 0x2000:
			c = ((c & 0x1f)|((c & 0x60)<<5)|((c & 0x700)>>1))<<7;
			break;
		case 0x3000:
		case 0x4000:
			c = ((c & 0x7f)|((c & 0xf00)>>1)|((c & 0x4000)>>3))<<7;
			break;
		case 0x5000:
		case 0x6000:
			c = ((c & 0x7f)|((c & 0xf00)>>1)
					|((c & 0x2000)>>2)|0x1000) << 7;
			break;
		case 0x7000:
			c = ((c & 0x1f)|((c & 0x60)<<5)
					| ((c & 0x700)>>1)|0x1000) << 7;
			break;
		}
		return ((caddr_t)(c + QP_KJROM + (sr->extent.y > 16 ? 0 : 96)));
	} else {
		/*
		 * UNKNOWN char
		 */
		return ((caddr_t)zero);
	}
}
#endif /* NNWB514 > 0 || NNWB251 > 0 || NNWB256 > 0 */
