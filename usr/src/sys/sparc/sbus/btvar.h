/*
 * Copyright (c) 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratory.
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
 *	@(#)btvar.h	8.2 (Berkeley) 1/21/94
 *
 * from: $Header: btvar.h,v 1.1 93/10/12 15:29:01 torek Exp $
 */

/*
 * Brooktree color frame buffer state variables (see btreg.h).
 *
 * Unfortunately, remarkably little code can be shared between the
 * cg3 and cg6 drivers here, as the cg3 registers do longword-ops
 * `as expected', but the cg6 ones use only the upper byte.
 *
 * Still, the software color map manipulation is not completely trivial.
 */
union bt_cmap {
	u_char  cm_map[256][3];		/* 256 R/G/B entries */
	u_int   cm_chip[256 * 3 / 4];	/* the way the chip gets loaded */
};

/*
 * Routines in bt_subr.c.
 */
int	bt_getcmap __P((struct fbcmap *, union bt_cmap *, int));
int	bt_putcmap __P((struct fbcmap *, union bt_cmap *, int));

/*
 * Compute (x / 4) * 3 and (x / 4) * 4.  These are used in turning
 * RGB indices (which are in multiples of three) into `chip RGB' values
 * (which are in multiples of four).
 */
#define	BT_D4M3(x) ((((x) >> 2) << 1) + ((x) >> 2))	/* (x / 4) * 3 */
#define	BT_D4M4(x) ((x) & ~3)				/* (x / 4) * 4 */
