/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 * from: $Hdr: askrom.c,v 4.300 91/06/09 06:32:39 root Rel41 $ SONY
 *
 *	@(#)askrom.c	7.3 (Berkeley) %G%
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
