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
 * from: $Hdr: fb_conf.c,v 4.300 91/06/09 06:32:46 root Rel41 $ SONY
 *
 *	@(#)fb_conf.c	8.1 (Berkeley) 6/10/93
 */

#include "nwb512.h"
#include "nwb514.h"
#include "nwb518.h"
#include "nwb225.h"
#include "nwb251.h"
#include "nwb252.h"
#include "nwb253.h"
#include "nwb254.h"
#include "nwb255.h"
#ifdef news3800
#include "nwb256.h"
#else
#define NNWB256 0
#endif
#include "nwb257.h"
#include "lfbm.h"
#include "pfbc.h"
#include "pfbm.h"
#include "slb101.h"

#ifdef IPC_MRX
#include "../../iop/framebuf.h"
#else
#include <news3400/iop/framebuf.h>
#endif

#include <news3400/fb/fbdefs.h>

#define NFBDEV \
	NNWB225 + NNWB512 + NNWB518 + NPFBM + NPFBC + NNWB514 + NNWB251 + \
	NLFBM + NNWB252 + NNWB253 + NNWB254 + NNWB255 + NNWB256 + NNWB257 + \
	NSLB101

#if NFBDEV > 0
struct fbdev	fbdev[NFBDEV];
#endif

#if NNWB512 > 0
int	fb512_probe();
void	fb512_setup();
#endif

#if NNWB225 > 0
int	fb225_probe();
void	fb225_setup();
#endif

#if NPFBM > 0
int	fbpopm_probe();
void	fbpopm_setup();
#endif

#if NNWB518 > 0
int	fb518_probe();
void	fb518_setup();
#endif

#if NPFBC > 0
int	fbpopc_probe();
void	fbpopc_setup();
#endif

#if NNWB514 > 0
int	fb514_probe();
void	fb514_setup();
#endif

#if NNWB251 > 0
int	fb251_probe();
void	fb251_setup();
#endif

#if NLFBM > 0
int	fblfbm_probe();
void	fblfbm_setup();
#endif

#if NNWB252 > 0
int	fb252_probe();
void	fb252_setup();
#endif

#if NNWB253 > 0
int	fb253_probe();
void	fb253_setup();
#endif

#if NNWB254 > 0
int	fb254_probe8();
int	fb254_probe16();
void	fb254_setup8();
void	fb254_setup16();
#endif

#if NNWB255 > 0
int	fb255_probe();
void	fb255_setup();
#endif

#if NNWB256 > 0
int	fb256_probe();
void	fb256_setup();
#endif

#if NNWB257 > 0
int	fb257_probe();
void	fb257_setup();
#endif

#if NSLB101 > 0
int	fb101_probe8();
int	fb101_probe24();
void	fb101_setup8();
void	fb101_setup24();
#endif

struct fbdevsw	fbdevsw[] = {

#if NNWB512 > 0
	NNWB512,	fb512_probe,	fb512_setup,
#endif

#if NNWB225 > 0
	NNWB225,	fb225_probe,	fb225_setup,
#endif

#if NPFBM > 0
	NPFBM,		fbpopm_probe,	fbpopm_setup,
#endif

#if NNWB518 > 0
	NNWB518,	fb518_probe,	fb518_setup,
#endif

#if NPFBC > 0
	NPFBC,		fbpopc_probe,	fbpopc_setup,
#endif

#if NNWB514 > 0
	NNWB514,	fb514_probe,	fb514_setup,
#endif

#if NNWB251 > 0
	NNWB251,	fb251_probe,	fb251_setup,
#endif

#if NLFBM > 0
	NLFBM,		fblfbm_probe,	fblfbm_setup,
#endif

#if NNWB252 > 0
	NNWB252,	fb252_probe,	fb252_setup,
#endif

#if NNWB253 > 0
	NNWB253,	fb253_probe,	fb253_setup,
#endif

#if NNWB254 > 0
	NNWB254,	fb254_probe16,	fb254_setup16,
	NNWB254,	fb254_probe8,	fb254_setup8,
#endif

#if NNWB255 > 0
	NNWB255,	fb255_probe,	fb255_setup,
#endif

#if NNWB256 > 0
	NNWB256,	fb256_probe,	fb256_setup,
#endif

#if NNWB257 > 0
	NNWB257,	fb257_probe,	fb257_setup,
#endif

#if NSLB101 > 0
	NSLB101,	fb101_probe24,	fb101_setup24,
	NSLB101,	fb101_probe8,	fb101_setup8,
#endif

	0,		(int(*)())(-1),	(void(*)())(-1),
};
